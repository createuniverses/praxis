
#include "ML_Maths.h"

void mlSinCos(mlAngle angle, mlFloat * sinResult, mlFloat * cosResult)
{
	*sinResult = mlSin(angle);
	*cosResult = mlCos(angle);
}

mlAngle mlAngleNormalise(mlAngle x)
{
	mlFloat t = x * mlHalfInversePi + 100.5f;

	mlFloat f = t - mlFloat(int(t));
	mlFloat result = (f - 0.5f) * mlTwoPi;
	return result;
}

static UInt32 mlReinterpretFloatAsInteger(mlFloat x)
{
	return reinterpret_cast<UInt32 &>(x);
}

static bool mlInternal_IsNan(mlFloat32 x)
{
	assert(sizeof(x) == sizeof(mlFloat32));
	
	UInt32 i = mlReinterpretFloatAsInteger(x);
	
	return	((i & 0x7f800000L) == 0x7f800000L) &&
		 	((i & 0x007fffffL) != 0000000000L);
}

static bool mlInternal_IsInfinity(mlFloat32 x)
{
	assert(sizeof(x) == sizeof(mlFloat32));

	UInt32 i = mlReinterpretFloatAsInteger(x);
	
	return 	((i & 0x7f800000L) == 0x7f800000L) &&
		 	((i & 0x007fffffL) == 0000000000L);
}

bool mlIsValid(mlFloat x)
{
	return !mlInternal_IsNan(x) && !mlInternal_IsInfinity(x);
}

