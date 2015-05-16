#ifndef PRAXISSERVER_H
#define PRAXISSERVER_H

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

typedef int SOCKET;
static int INVALID_SOCKET = -1;
static int SOCKET_ERROR = -1;
#endif

#include <string>

namespace PraxisServer
{
    bool         Start();

    SOCKET       Accept();
    bool         SocketIsValid(SOCKET s);

    std::string  Receive();
    void         Send(std::string & data);
    void         SetBlockingOption(u_long mode);

    std::string  Receive(SOCKET s);
    void         Send(SOCKET s, std::string & data);
    void         SetBlockingOption(SOCKET s, u_long mode);
}

#endif // PRAXISSERVER_H
