
#ifndef ML_TYPES_H
#define ML_TYPES_H

#include <stddef.h>

#include <assert.h>

#define ASSERT(x) assert(x)
#define TRACE(x)
#define UNUSED(x) ((void)(x))

typedef	float			mlFloat32;
typedef double			mlFloat64;

typedef mlFloat32		mlFloat;
typedef mlFloat64		mlDouble;

typedef mlFloat			mlAngle;

const mlFloat			mlFloatMax = 3.402823466e+38f * 0.1f;
const mlFloat			mlFloatMin = 1.175494351e-38f * 10.0f;

typedef unsigned char		UInt8,	U08;
typedef signed char		Int8,	S08;

typedef unsigned short		UInt16,	U16;
typedef signed short		Int16,	S16;

typedef unsigned int		UInt32,	U32;
typedef signed int		Int32,	S32;

enum mlAxis
{
	mlAxisX,
	mlAxisY,
	mlAxisZ
};

#endif // ML_TYPES_H

