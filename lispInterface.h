
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/


#ifndef LISPINTERFACE_H
#define LISPINTERFACE_H

#pragma warning(disable:4311)
#pragma warning(disable:4244)
#pragma warning(disable:4273)

#include <stdio.h>
#include <string.h>

#include <string>

#include "s7/s7.h"

void lispInit();

bool lispCall(std::string sCmd);

std::string & lispGetError();
std::string & lispGetOutput();

void lispClearError();
void lispClearOutput();

void lispClose();

//bool lispIsCommandComplete(std::string sCmd);

#endif // LISPINTERFACE_H

