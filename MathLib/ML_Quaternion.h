
#ifndef ML_QUATERNION_H
#define ML_QUATERNION_H

#include "ML_Types.h"
#include "ML_Maths.h"
#include "ML_Vector.h"

class mlMatrix3x3;

class mlQuaternion
{
public:
	mlFloat w;
	mlFloat x;
	mlFloat y;
	mlFloat z;
	
	mlQuaternion() { SetIdentity(); }
	mlQuaternion(mlFloat w, mlFloat x, mlFloat y, mlFloat z);
	mlQuaternion(mlAxis axis, mlAngle angle);
	mlQuaternion(const mlVector3D &normalisedAxis, mlAngle angle);
	
	void				SetIdentity();
	
	void				SetRotationAxisAndAngle(const mlVector3D & normalisedAxis, mlAngle angle);
	
	void				Invert();
	void				Normalise();
	
	mlFloat				Magnitude() const;
	mlFloat				MagnitudeSquared() const;
	
    mlQuaternion		operator*(const mlQuaternion &q) const;
	
	mlVector3D			TransformVector(const mlVector3D &v) const;
	
	bool				operator==(const mlQuaternion &) const;
	bool				operator!=(const mlQuaternion &) const;
};

extern const mlQuaternion 		mlQuaternionIdentity;

mlQuaternion			mlQuaternionFromDirection(const mlVector3D & vForward, const mlVector3D & vUp);

mlQuaternion			mlQuaternionFromRotationMatrix(const mlMatrix3x3 & matrix);

mlQuaternion 			mlQuaternionFromVectorRotation(const mlVector3D & normalisedVectorA, const mlVector3D & normalisedVectorB);

mlQuaternion			mlQuaternionInterpolate(mlFloat alpha, const mlQuaternion &from, const mlQuaternion &to, int numSpin = 0);

mlQuaternion			mlQuaternionInterpolateFromIdentity(mlFloat alpha, const mlQuaternion &to);

mlQuaternion			mlQuaternionInterpolateCubic(mlFloat alpha, const mlQuaternion & q0, const mlQuaternion & q1, const mlQuaternion & q2, const mlQuaternion & q3);

mlQuaternion			mlQuaternionLogarithm(const mlQuaternion & quaternion);

mlQuaternion			mlQuaternionExponential(const mlQuaternion & quaternion);

mlFloat				mlQuaternionDotProduct(const mlQuaternion &a, const mlQuaternion &b);

inline mlQuaternion::mlQuaternion(mlFloat wv, mlFloat xv, mlFloat yv, mlFloat zv)
: w(wv), x(xv), y(yv), z(zv)
{ 
}

inline void mlQuaternion::Invert()
{
	w = -w;
}

inline bool mlQuaternion::operator==(const mlQuaternion & other) const
{
	return (w == other.w) && (x == other.x) && (y == other.y) && (z == other.z);
}

inline bool mlQuaternion::operator!=(const mlQuaternion & other) const
{
	return (w != other.w) || (x != other.x) || (y != other.y) || (z != other.z);
}

inline bool mlIsValid(const mlQuaternion & q)
{
	return	mlIsValid(q.x) &&
			mlIsValid(q.y) &&
			mlIsValid(q.z) &&
			mlIsValid(q.w);
}

#endif // ML_QUATERNION_H

