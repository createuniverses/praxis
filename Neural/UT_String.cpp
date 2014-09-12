#include "UT_String.h"

#include <stdio.h>      /* printf */
#include <stdarg.h>     /* va_list, va_start, va_arg, va_end */
#include <stdlib.h>

#include <cstring>


std::string utFormatString(const char format[], ...)
{
	ASSERT(format);

	va_list argList;
	va_start(argList, format);

	const unsigned int utFormatStringMax = 4000;

	char messageBuffer[utFormatStringMax];

	int charCount = vsprintf(messageBuffer, format, argList);

	messageBuffer[utFormatStringMax - 1] = '\0';

	ASSERT(charCount >= 0 && charCount <= utFormatStringMax - 1);
	UNUSED(charCount);

	va_end(argList);

	return std::string(messageBuffer);
}

void utCommaSeparatedToStringVector(const std::string & commaSeparatedString, std::vector<std::string> & outputStringVector)
{
	outputStringVector.clear();

	char buffer[256];

	strcpy(buffer, commaSeparatedString.c_str());

	char seps[] = " ,\t\n";
	char *token;

	token = strtok( buffer, seps );
	while(token != NULL)
	{
		outputStringVector.push_back(std::string(token));

		token = strtok( NULL, seps );
	}
}

// taken from Stroustrup C++ Programming Language pg 620
istream & utEatWhite(istream &is)
{
	char c;
	while (is.get(c))
	{
		if(!isspace(c))
		{
			is.putback(c);
			break;
		}
	}
	return is;
}

std::string utGetTokenInStream(istream & stream, int maxLength, char delimiter)
{
	ASSERT(maxLength <= 1024);

	char StringBuffer[1024];
	utEatWhite(stream);
	stream.getline(StringBuffer, maxLength, delimiter);
	return StringBuffer;
}

bool utVerifyTokenInStream(istream & stream, char delimiter, std::string token)
{
	std::string serialisedText = utGetTokenInStream(stream, 1024, delimiter);
	ASSERT(serialisedText == token);

	return (serialisedText == token);
}

int utGetIntInStream(istream & stream)
{
	int number;

	stream >> number;

	return number;
}

float utGetFloatInStream(istream & stream)
{
	float number;

	stream >> number;

	return number;
}

bool utGetBoolInStream(istream & stream)
{
	int number;

	stream >> number;

	return static_cast<bool>(number);
}

mlVector2D utGetVector2DInStream(istream & stream)
{
	mlVector2D vector2D;

	vector2D.x = utGetFloatInStream(stream);
	vector2D.y = utGetFloatInStream(stream);

	return vector2D;
}

mlVector3D utGetVector3DInStream(istream & stream)
{
	mlVector3D vector3D;

	vector3D.x = utGetFloatInStream(stream);
	vector3D.y = utGetFloatInStream(stream);
	vector3D.z = utGetFloatInStream(stream);

	return vector3D;
}

mlQuaternion utGetQuaternionInStream(istream & stream)
{
	mlQuaternion value;

	value.w = utGetFloatInStream(stream);
	value.x = utGetFloatInStream(stream);
	value.y = utGetFloatInStream(stream);
	value.z = utGetFloatInStream(stream);

	return value;
}

void utPutLabelInStream(ostream & stream, const std::string & label)
{
	stream << label.c_str() << "\n";
}

void utPutStringInStream(ostream & stream, const std::string & label, const std::string & value)
{
	stream << label.c_str() << " " << value.c_str() << "\n";
}

void utPutIntInStream(ostream & stream, const std::string & label, int value)
{
	stream << label.c_str() << " " << value << "\n";
}

void utPutBoolInStream(ostream & stream, const std::string & label, bool value)
{
	stream << label.c_str() << " " << value << "\n";
}

void utPutFloatInStream(ostream & stream, const std::string & label, mlFloat value)
{
	stream << label.c_str() << " " << value << "\n";
}

void utPutVector2DInStream(ostream & stream, const std::string & label, const mlVector2D & value)
{
	stream << label.c_str() << " " << value.x << " " << value.y << "\n";
}

void utPutVector3DInStream(ostream & stream, const std::string & label, const mlVector3D & value)
{
	stream << label.c_str() << " " << value.x << " " << value.y << " " << value.z << "\n";
}

void utPutQuaternionInStream(ostream & stream, const std::string & label, const mlQuaternion & value)
{
	stream << label.c_str() << " " << value.w << " " << value.x << " " << value.y << " " << value.z << "\n";
}
