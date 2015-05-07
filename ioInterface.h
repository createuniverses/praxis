
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/


#ifndef IOINTERFACE_H
#define IOINTERFACE_H

#pragma warning(disable:4311)
#pragma warning(disable:4244)
#pragma warning(disable:4273)

#include <stdio.h>
#include <string.h>

#include <string>

// #include "ficl-4.1.0/ficl.h"

void ioInit();

std::string ioCall(std::string sCmd);

//std::string & forthGetError();
//std::string & forthGetOutput();

//void forthClearError();
//void forthClearOutput();

void ioClose();

//bool forthIsCommandComplete(std::string sCmd);

#endif // IOINTERFACE_H

