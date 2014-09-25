
#include "ML_Quaternion.h"

#include "ML_Maths.h"
#include "ML_Vector.h"
#include "ML_Matrix.h"

extern const mlQuaternion mlQuaternionIdentity(1.0f, 0.0f, 0.0f, 0.0f);

mlQuaternion::mlQuaternion(const mlVector3D &normalisedAxis, mlAngle angle)
{
	SetRotationAxisAndAngle(normalisedAxis, angle);
}

mlQuaternion::mlQuaternion(mlAxis axis, mlAngle angle)
{
	mlFloat sinHalfAngle;
	mlFloat cosHalfAngle;
	
	mlSinCos(angle * 0.5f, &sinHalfAngle, &cosHalfAngle);
	
	switch (axis)
	{
		case mlAxisX:
			x = sinHalfAngle;
			y = 0.0f;
			z = 0.0f;
			w = cosHalfAngle;
			break;

		case mlAxisY:
			x = 0.0f;
			y = sinHalfAngle;
			z = 0.0f;
			w = cosHalfAngle;
			break;

		case mlAxisZ:
			x = 0.0f;
			y = 0.0f;
			z = sinHalfAngle;
			w = cosHalfAngle;
			break;

		default:
			assert(0);
			break;
	}
}

void mlQuaternion::Normalise()
{
	mlFloat invMag = mlInvSqrt(w*w + x*x + y*y + z*z);
	w *= invMag;
	x *= invMag;
	y *= invMag;
	z *= invMag;
}

void mlQuaternion::SetIdentity()
{ 
	x = y = z = 0.0f;
	w = 1.0f;
}

mlQuaternion mlQuaternion::operator*(const mlQuaternion &q) const
{
	return mlQuaternion(	w * q.w -  x * q.x - y * q.y - z * q.z,
							w * q.x +  x * q.w + y * q.z - z * q.y,
							w * q.y +  y * q.w + z * q.x - x * q.z,
							w * q.z +  z * q.w + x * q.y - y * q.x	);
}

void	mlQuaternion::SetRotationAxisAndAngle(const mlVector3D & normalisedAxis, mlAngle angle)
{
	if (angle != 0.0f)
	{
		mlFloat sinHalfAngle, cosHalfAngle;

		mlSinCos(angle * 0.5f, &sinHalfAngle, &cosHalfAngle);
		
		x = sinHalfAngle * normalisedAxis.x;
		y = sinHalfAngle * normalisedAxis.y;
		z = sinHalfAngle * normalisedAxis.z;
		w = cosHalfAngle;
	}
	else
	{
		x = 0.0f;
		y = 0.0f;
		z = 0.0f;
		w = 1.0f;
	}
}

mlQuaternion mlQuaternionInterpolate(mlFloat alpha, const mlQuaternion &from, const mlQuaternion &to, int numSpin)
{
	const mlFloat EPSILON = 1.0E-2f;
	
	mlFloat	cos_t = mlQuaternionDotProduct(from, to);
	
	bool bflip;
	if (cos_t < 0.0)
	{
		cos_t = -cos_t;
		bflip = true;
	} 
	else
		bflip = false;
	
	mlFloat beta;
	if(1.0f - cos_t < EPSILON) 
	{
		beta = 1.0f - alpha;
 	} 
	else 
	{
		mlFloat theta		= mlArcCos(cos_t);
		mlFloat phi			= theta + numSpin * mlPi;
		mlFloat oo_sin_t	= 1.0f / mlSin(theta);
		mlFloat alphaphi	= alpha * phi;
		alpha =  mlSin(alphaphi) * oo_sin_t;
		beta  =  mlSin(theta - alphaphi) * oo_sin_t;
	}
	
	if(bflip)
		alpha = -alpha;
		
	mlQuaternion result(beta * from.w + alpha * to.w,
						beta * from.x + alpha * to.x,
						beta * from.y + alpha * to.y,
						beta * from.z + alpha * to.z);
						
	result.Normalise();
	
	return result;
}

mlQuaternion mlQuaternionInterpolateFromIdentity(mlFloat alpha, const mlQuaternion &to)
{
	mlFloat beta;
	mlFloat theta;
	mlFloat oo_sin_t, cos_t;
	mlFloat phi;
	const mlFloat EPSILON = 1.0E-2f;
	
	bool bflip;
	
	cos_t = to.w;
	
	if (cos_t < 0.0) 
	{
		cos_t = -cos_t;
		bflip = true;
	} 
	else {
		bflip = false;
	}
	
	if (1.0f - cos_t < EPSILON) 
	{
		beta = 1.0f - alpha;
	} 
	else 
	{
		theta = mlArcCos(cos_t);
		phi = theta;
		oo_sin_t = 1.0f /  mlSin(theta);
		beta  =  mlSin((theta - alpha*phi)) * oo_sin_t;
		alpha =  mlSin((alpha*phi)) * oo_sin_t;
	}
	
	if (bflip)
		alpha = -alpha;

	mlQuaternion result;
	result.x = mlFloat(alpha * to.x);
	result.y = mlFloat(alpha * to.y);
	result.z = mlFloat(alpha * to.z);
	result.w = mlFloat(beta + alpha * to.w);
	
	result.Normalise();

	return result;
}

mlVector3D mlQuaternion::
TransformVector(const mlVector3D &v) const
{
	const mlQuaternion ViQ(	v.x * x + v.y * y + v.z * z,
							v.x * w - v.y * z + v.z * y,
							v.y * w - v.z * x + v.x * z,
							v.z * w - v.x * y + v.y * x );
	
	return mlVector3D(	w * ViQ.x +  x * ViQ.w + y * ViQ.z - z * ViQ.y,
						w * ViQ.y +  y * ViQ.w + z * ViQ.x - x * ViQ.z,
						w * ViQ.z +  z * ViQ.w + x * ViQ.y - y * ViQ.x );
}

mlQuaternion mlQuaternionFromDirection(const mlVector3D & vForward, const mlVector3D & vUp)
{
	mlMatrix3x3 result;
	mlMatrixUtility::MatrixFromDirection(result, vForward, vUp);

	mlQuaternion quat = mlQuaternionFromRotationMatrix(result);

	return quat;
}

mlQuaternion mlQuaternionFromVectorRotation(const mlVector3D & normalisedVectorA, const mlVector3D & normalisedVectorB)
{
	mlVector3D crossProduct = mlVectorCross(normalisedVectorA, normalisedVectorB);

	const mlFloat crossLengthSquared = crossProduct.MagnitudeSquared();
	const mlFloat tooSmall = 0.0000001f;
	
	if (crossLengthSquared < tooSmall)
	{
		return mlQuaternionIdentity;
	}
	
	mlFloat cosTheta = mlClamp(normalisedVectorA * normalisedVectorB, -1.0f, 1.0f);
	
	{
		crossProduct *= mlInvSqrt(crossLengthSquared);
		mlAngle angle = mlArcCos(cosTheta);
		return mlQuaternion(crossProduct, angle);
	}
}

mlQuaternion mlQuaternionFromRotationMatrix(const mlMatrix3x3 & rotationMatrix)
{
	mlQuaternion result;
	
	mlFloat  tr, s, q[4];
	int    i, j, k;
	
	int nxt[3] = {1, 2, 0};
	
	tr = rotationMatrix[0][0] + rotationMatrix[1][1] + rotationMatrix[2][2];
	
	if (tr > 0.0) 
	{
		s = mlSqrt (tr + 1.0f);
		result.w = s * 0.5f;
		s = 0.5f / s;
		result.x = (rotationMatrix[1][2] - rotationMatrix[2][1]) * s;
		result.y = (rotationMatrix[2][0] - rotationMatrix[0][2]) * s;
		result.z = (rotationMatrix[0][1] - rotationMatrix[1][0]) * s;
	} 
	else 
	{
		i = 0;
		if (rotationMatrix[1][1] > rotationMatrix[0][0]) i = 1;
		if (rotationMatrix[2][2] > rotationMatrix[i][i]) i = 2;
		j = nxt[i];
		k = nxt[j];
		
		s = mlSqrt ((rotationMatrix[i][i] - (rotationMatrix[j][j] + rotationMatrix[k][k])) + 1.0f);
		
		q[i] = s * 0.5f;
		
		if (s != 0.0f) s = 0.5f / s;
		
		q[3] = (rotationMatrix[j][k] - rotationMatrix[k][j]) * s;
		q[j] = (rotationMatrix[i][j] + rotationMatrix[j][i]) * s;
		q[k] = (rotationMatrix[i][k] + rotationMatrix[k][i]) * s;
		
		result.x = q[0];
		result.y = q[1];
		result.z = q[2];
		result.w = q[3];
	}

	return result;
}

mlQuaternion mlQuaternionInterpolateCubic(mlFloat alpha, const mlQuaternion & q0, const mlQuaternion & q1, const mlQuaternion & q2, const mlQuaternion & q3)
{
	mlQuaternion qa = mlQuaternionInterpolate(alpha, q0, q3);
	mlQuaternion qb = mlQuaternionInterpolate(alpha, q1, q2);
	
	mlFloat u = 2.0f * alpha * (1.0f - alpha);
	
	return mlQuaternionInterpolate(u, qa, qb);
}

mlQuaternion mlQuaternionLogarithm(const mlQuaternion & quaternion)
{
	mlVector3D vector(quaternion.x, quaternion.y, quaternion.z);
	
	mlQuaternion result;

	if (vector == mlVector3DZero)
	{
		result.w = 0.0f;
		result.x = 0.0f;
		result.y = 0.0f;
		result.z = 0.0f;
	}
	else
	{
		mlFloat cosAngle = quaternion.w;
		mlFloat angle = mlArcCos(cosAngle);
		
		vector.Normalise();
		vector *= angle;
	
		result.w = 0.0f;
		result.x = vector.x;
		result.y = vector.y;
		result.z = vector.z;
	}
	
	return result;
}

mlQuaternion mlQuaternionExponential(const mlQuaternion & quaternion)
{
	mlVector3D vector(quaternion.x, quaternion.y, quaternion.z);
	
	mlAngle angle = vector.Magnitude();
	
	mlFloat sinAngle;
	mlFloat cosAngle;
	mlSinCos(angle, &sinAngle, &cosAngle);
	
	return mlQuaternion(	cosAngle,
							sinAngle * quaternion.x,
							sinAngle * quaternion.y,
							sinAngle * quaternion.z	);
}

mlFloat	mlQuaternion::Magnitude() const
{
	mlFloat magnitudeSquared = x * x + y * y + z * z + w * w;
	return mlSqrt(magnitudeSquared);
}

mlFloat	mlQuaternion::MagnitudeSquared() const
{
	mlFloat magnitudeSquared = x * x + y * y + z * z + w * w;		
	return magnitudeSquared;	
}

mlFloat mlQuaternionDotProduct(const mlQuaternion &a, const mlQuaternion &b)
{
	return a.w * b.w + a.x * b.x + a.y * b.y + a.z * b.z;
}
