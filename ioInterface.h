
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

void ioInit();

void ioCall(std::string sCmd);
void ioCallWithReply(std::string sCmd);

std::string & ioGetReply();
std::string & ioGetTrace();

void ioClose();

#endif // IOINTERFACE_H

