
#ifndef ML_MATHS_H
#define ML_MATHS_H

#include <math.h>
#include <float.h>

#include "ML_Types.h"

const mlFloat		mlPi			= 3.14159265358979323846f;
const mlFloat		mlTwoPi			= mlPi * 2.0f;
const mlFloat		mlHalfPi		= mlPi * 0.5f;
const mlFloat		mlInversePi		= 1.0f / mlPi;
const mlFloat 		mlHalfInversePi = mlInversePi * 0.5f;

const mlFloat 		mlSqrtTwo		= 1.414213562f;
const mlFloat 		mlHalfSqrtTwo	= 0.707106781f;

const mlAngle		mlDegrees10 = mlPi / 18.0f;
const mlAngle		mlDegrees15 = mlPi / 12.0f;
const mlAngle		mlDegrees20 = mlPi / 9.0f;
const mlAngle		mlDegrees30 = mlPi / 6.0f;
const mlAngle		mlDegrees45 = mlPi / 4.0f;
const mlAngle		mlDegrees60 = mlPi / 3.0f;
const mlAngle		mlDegrees90 = mlPi / 2.0f;
const mlAngle		mlDegrees180 = mlPi;
const mlAngle		mlDegrees360 = mlPi * 2.0f;

const mlFloat		mlDegreesPerRadian = 180.0f / mlPi;
const mlFloat		mlRadiansPerDegree = mlPi / 180.0f;

inline mlAngle	mlDegreesToRadians(mlFloat degrees) { return degrees * mlRadiansPerDegree; }
inline mlAngle	mlRadiansToDegrees(mlFloat radians) { return radians * mlDegreesPerRadian; }

inline mlAngle	mlDegrees(mlFloat degrees)		{ return mlDegreesToRadians(degrees); }
inline mlAngle	mlRadians(mlFloat radians)		{ return radians; }

inline mlFloat	mlMin(mlFloat a, mlFloat b)		{ return (a <= b) ? a : b; }
inline mlFloat	mlMax(mlFloat a, mlFloat b)		{ return (a >= b) ? a : b; }

inline Int32	mlMin(Int32 a, Int32 b)		{ return (a <= b) ? a : b; }
inline Int32	mlMax(Int32 a, Int32 b)		{ return (a >= b) ? a : b; }

mlAngle 		mlAngleNormalise(mlAngle x);

inline bool 	mlAngleIsNormalised(mlAngle x)		{ return (x >= -mlPi) && (x <= mlPi); }

inline mlFloat	mlFabs(mlFloat x)			{ return ::fabsf(x); }
inline mlFloat	mlSign(mlFloat fValue)
{
	if(fValue > 0.0f)
	{
		return 1.0f;
	}

	if(fValue < 0.0f)
	{
		return -1.0f;
	}

	return 0.0f;
}

inline mlFloat	mlSqrt(mlFloat x)			{ return ::sqrtf(x); }
inline mlFloat	mlInvSqrt(mlFloat x)		{ assert(x != 0.0f); return 1.0f / ::sqrtf(x); }
inline mlFloat	mlSin(mlAngle x)			{ return ::sinf(x); }
inline mlFloat	mlCos(mlAngle x)			{ return ::cosf(x); }
inline mlFloat	mlTan(mlAngle x)			{ return ::tanf(x); }

void mlSinCos(mlAngle angle, mlFloat * sinResult, mlFloat * cosResult);

inline mlFloat	mlFloor(mlFloat x)				{ return floorf(x); }
inline mlFloat	mlCeil(mlFloat x)				{ return ceilf(x); }

inline mlAngle	mlArcSin(mlFloat x)				{ return asinf(x); }
inline mlAngle	mlArcCos(mlFloat x)				{ return acosf(x); }
inline mlAngle	mlArcTan(mlFloat x)				{ return atanf(x); }
inline mlAngle	mlArcTan2(mlFloat y, mlFloat x) { return atan2f(y, x); }

inline mlFloat	mlLog(mlFloat x)				{ return logf(x); }
inline mlFloat	mlPow(mlFloat x, mlFloat y)		{ return powf(x, y); }
inline mlFloat	mlFmod(mlFloat x, mlFloat y)	{ return fmodf(x, y); }
inline mlFloat	mlExp(mlFloat x)				{ return expf(x); }

inline mlFloat mlClamp(mlFloat value, mlFloat min_, mlFloat max_)
{
	mlFloat	result;
	
	if (value < min_)
		result = min_;	
	else if (max_ < value)
		result = max_;
	else
		result = value;
		
	return result;
}

inline mlFloat mlFmodUniform(mlFloat x, mlFloat y)
{
	mlFloat modulo = mlFmod(x, y);

	if(modulo < 0.0f)
	{
		return modulo + y;
	}
	else
	{
		return modulo;
	}
}

inline bool mlEquivalent(mlFloat a, mlFloat b, mlFloat tolerance)
{
	mlFloat delta = a - b;
	return mlFabs(delta) < tolerance;
}

inline mlFloat mlInterpolate(mlFloat alpha, mlFloat start, mlFloat end)
{
	return (end - start) * alpha + start;
}

template <typename InType, typename OutType>
OutType mlLinearMapping(InType input, InType inLower, InType inUpper, OutType outLower, OutType outUpper)
{
	const InType inDelta = inUpper - inLower;
	const OutType outDelta = outUpper - outLower;
	mlFloat alpha = static_cast<mlFloat>(input - inLower) * (1.0f / inDelta);
	OutType outResult = (alpha * outDelta) + outLower;
	return outResult;
}

bool mlIsValid(mlFloat x);

#endif // ML_MATHS_H

