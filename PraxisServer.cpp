
#include "PraxisServer.h"

#ifdef __PRAXIS_WINDOWS__
#include <windows.h>
#include <mmsystem.h>
#include <winuser.h>
#include <winsock.h>
#endif

#ifdef __PRAXIS_LINUX__
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include <string.h>

#include <string>
#include <iostream>

using namespace std;

namespace PraxisServer
{
    SOCKET ServerSocket;
    SOCKET LastAcceptedClientSocket;
}

bool PraxisServer::Start()
{
    ServerSocket              = INVALID_SOCKET;
    LastAcceptedClientSocket  = INVALID_SOCKET;

#ifdef __PRAXIS_WINDOWS__
    // Start Winsock up
    WSAData wsaData;
    int nCode;
    if ((nCode = WSAStartup(MAKEWORD(1, 1), &wsaData)) != 0) {
        cerr << "WSAStartup() returned error code " << nCode << "." <<
                endl;
        return false;
    }
#endif

    // Begin listening for connections
    cout << "Establishing the listener..." << endl;
    ServerSocket = INVALID_SOCKET;
    u_long nInterfaceAddr = inet_addr("127.0.0.1");
    if (nInterfaceAddr != INADDR_NONE) {
        ServerSocket = socket(AF_INET, SOCK_STREAM, 0);
        if (ServerSocket != INVALID_SOCKET) {

            sockaddr_in sinInterface;
            sinInterface.sin_family = AF_INET;
            sinInterface.sin_addr.s_addr = nInterfaceAddr;
            sinInterface.sin_port = htons(4242);
            if (bind(ServerSocket, (sockaddr*)&sinInterface,
                    sizeof(sockaddr_in)) != SOCKET_ERROR) {
                listen(ServerSocket, 1);
            }
        }
    }

    if (ServerSocket == INVALID_SOCKET)
        return false;

#ifdef __PRAXIS_WINDOWS__
    u_long iMode = 1;
    ioctlsocket(ServerSocket, FIONBIO, &iMode);
#endif

#ifdef __PRAXIS_LINUX__
    u_long iMode = 1;
    ioctl(ServerSocket, FIONBIO, &iMode);
#endif

    return true;
}

SOCKET PraxisServer::Accept()
{
    if(ServerSocket == INVALID_SOCKET)
        return INVALID_SOCKET;

    //cout << "Waiting for a connection..." << flush;
    sockaddr_in sinRemote;
    int nAddrSize = sizeof(sinRemote);
#ifdef __PRAXIS_WINDOWS__
    SOCKET clientSocket = accept(ServerSocket, (sockaddr*)&sinRemote, &nAddrSize);
#endif
#ifdef __PRAXIS_LINUX__
    SOCKET clientSocket = accept(ServerSocket, (struct sockaddr*)&sinRemote, (socklen_t *)&nAddrSize);
#endif
    if (clientSocket != INVALID_SOCKET) {
        cout << "Accepted connection from " <<
                inet_ntoa(sinRemote.sin_addr) << ":" <<
                ntohs(sinRemote.sin_port) << "." << endl;

#ifdef __PRAXIS_WINDOWS__
        u_long iMode = 1;
        ioctlsocket(clientSocket, FIONBIO, &iMode);
#endif

#ifdef __PRAXIS_LINUX__
        u_long iMode = 1;
        ioctl(clientSocket, FIONBIO, &iMode);
#endif
    }

    LastAcceptedClientSocket = clientSocket;

    return clientSocket;
}

std::string PraxisServer::Receive()
{
    return Receive(LastAcceptedClientSocket);
}

std::string PraxisServer::Receive(SOCKET s)
{
    char buf[65536];

    int nBytes = recv(s, buf, 65536, 0);
    if(nBytes > 0)
    {
        cout << "Received " << nBytes << " bytes." << endl;
        buf[nBytes] = '\0';
    }
    else
    {
        //cout << "Received no bytes, recv returned " << nBytes << endl;
        buf[0] = '\0';
    }

    return std::string(buf);
}

void PraxisServer::Send(std::string & data)
{
    Send(LastAcceptedClientSocket, data);
}

void PraxisServer::Send(SOCKET s, std::string & data)
{
    int nReadBytes = data.length();
    const char * buf = data.c_str();
    //strcpy(buf, data.c_str());

    int nSentBytes = 0;
    while (nSentBytes < nReadBytes) {
        int nTemp = send(s, buf + nSentBytes,
                nReadBytes - nSentBytes, 0);
        if (nTemp > 0) {
            cout << "Sent " << nTemp << " bytes back to client." << endl;
            nSentBytes += nTemp;
        }
        else if (nTemp == SOCKET_ERROR) {
            cout << "Socket error" << endl;
            return;
        }
        else {
            // Client closed connection before we could reply to
            // all the data it sent, so bomb out early.
            cout << "Peer unexpectedly dropped connection!" <<
                    endl;
            return;
        }
    }

    return;
}

bool PraxisServer::SocketIsValid(SOCKET s)
{
    return (s != INVALID_SOCKET);
}

void PraxisServer::SetBlockingOption(u_long mode)
{
    SetBlockingOption(LastAcceptedClientSocket, mode);
}

void PraxisServer::SetBlockingOption(SOCKET s, u_long mode)
{
#ifdef __PRAXIS_WINDOWS__
        ioctlsocket(s, FIONBIO, &mode);
#endif

#ifdef __PRAXIS_LINUX__
        ioctl(s, FIONBIO, &mode);
#endif
}
