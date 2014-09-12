#ifndef UT_STRING_H
#define UT_STRING_H

#pragma warning(disable:4786)
#pragma warning(disable:4800)

#include "ML_Vector.h"
#include "ML_Quaternion.h"

#include <string>
#include <vector>

#include <iostream>
using namespace std;

std::string utFormatString(const char format[], ...);

void utCommaSeparatedToStringVector(const std::string & commaSeparatedString, std::vector<std::string> & outputStringVector);

std::string utGetTokenInStream(istream & stream, int maxLength, char delimiter);
bool utVerifyTokenInStream(istream & stream, char delimiter, std::string token);

int utGetIntInStream(istream & stream);
float utGetFloatInStream(istream & stream);
bool utGetBoolInStream(istream & stream);
mlVector2D utGetVector2DInStream(istream & stream);
mlVector3D utGetVector3DInStream(istream & stream);
mlQuaternion utGetQuaternionInStream(istream & stream);

void utPutLabelInStream(ostream & stream, const std::string & label);
void utPutStringInStream(ostream & stream, const std::string & label, const std::string & value);
void utPutIntInStream(ostream & stream, const std::string & label, int value);
void utPutBoolInStream(ostream & stream, const std::string & label, bool value);
void utPutFloatInStream(ostream & stream, const std::string & label, mlFloat value);
void utPutVector2DInStream(ostream & stream, const std::string & label, const mlVector2D & value);
void utPutVector3DInStream(ostream & stream, const std::string & label, const mlVector3D & value);
void utPutQuaternionInStream(ostream & stream, const std::string & label, const mlQuaternion & value);

#endif // UT_STRING_H
