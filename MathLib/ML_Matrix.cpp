
#include "ML_Matrix.h"

#include "ML_Maths.h"
#include "ML_Quaternion.h"

extern const mlMatrix3x3	mlMatrix3x3Identity
(
	mlVector3D(1.0f, 0.0f, 0.0f),
	mlVector3D(0.0f, 1.0f, 0.0f),
	mlVector3D(0.0f, 0.0f, 1.0f)
);

extern const mlMatrix3x4	mlMatrix3x4Identity
(
	mlVector3D(1.0f, 0.0f, 0.0f),
	mlVector3D(0.0f, 1.0f, 0.0f),
	mlVector3D(0.0f, 0.0f, 1.0f),
	mlVector3D(0.0f, 0.0f, 0.0f)
);

extern const mlMatrix4x4	mlMatrix4x4Identity
(
	mlVector4D(1.0f, 0.0f, 0.0f, 0.0f),
	mlVector4D(0.0f, 1.0f, 0.0f, 0.0f),
	mlVector4D(0.0f, 0.0f, 1.0f, 0.0f),
	mlVector4D(0.0f, 0.0f, 0.0f, 1.0f)
);

mlMatrix3x4::mlMatrix3x4(const mlQuaternion &quaternion)
{
	SetRotation(quaternion);
	SetTranslation(mlVector3DZero);
}

mlMatrix4x4::mlMatrix4x4(const mlMatrix3x3& matrixCloneFrom, const mlVector3D& translation)
{
	I.x = matrixCloneFrom.I.x;
	I.y = matrixCloneFrom.I.y;
	I.z = matrixCloneFrom.I.z;
	I.w = 0.0f;
	J.x = matrixCloneFrom.J.x;
	J.y = matrixCloneFrom.J.y;
	J.z = matrixCloneFrom.J.z;
	J.w = 0.0f;
	K.x = matrixCloneFrom.K.x;
	K.y = matrixCloneFrom.K.y;
	K.z = matrixCloneFrom.K.z;
	K.w = 0.0f;
	T.x = translation.x;
	T.y = translation.y;
	T.z = translation.z;
	T.w = 1.0f;
}

mlMatrix4x4::mlMatrix4x4(const mlMatrix3x3 &cloneFrom)
{
	I.x = cloneFrom.I.x;
	I.y = cloneFrom.I.y;
	I.z = cloneFrom.I.z;
	I.w = 0.0f;

	J.x = cloneFrom.J.x;
	J.y = cloneFrom.J.y;
	J.z = cloneFrom.J.z;
	J.w = 0.0f;

	K.x = cloneFrom.K.x;
	K.y = cloneFrom.K.y;
	K.z = cloneFrom.K.z;
	K.w = 0.0f;

	T.x = 0.0f;
	T.y = 0.0f;
	T.z = 0.0f;
	T.w = 1.0f;
}

mlMatrix4x4::mlMatrix4x4(const mlMatrix3x4 &cloneFrom)
{
	I.x = cloneFrom.I.x;
	I.y = cloneFrom.I.y;
	I.z = cloneFrom.I.z;
	I.w = 0.0f;
	
	J.x = cloneFrom.J.x;
	J.y = cloneFrom.J.y;
	J.z = cloneFrom.J.z;
	J.w = 0.0f;
	
	K.x = cloneFrom.K.x;
	K.y = cloneFrom.K.y;
	K.z = cloneFrom.K.z;
	K.w = 0.0f;
	
	T.x = cloneFrom.T.x;
	T.y = cloneFrom.T.y;
	T.z = cloneFrom.T.z;
	T.w = 1.0f;
}

void mlMatrix3x3::SetIdentity()
{
	I = mlVector3DX;
	J = mlVector3DY;
	K = mlVector3DZ;
}

void mlMatrixUtility::MatrixFromVectorRotation(mlMatrix3x3 & result, const mlVector3D & normalisedVectorA, const mlVector3D & normalisedVectorB)
{
	mlVector3D v = mlVectorCross(normalisedVectorA, normalisedVectorB);
	mlFloat vSquared = v.MagnitudeSquared();

	if (vSquared > mlFloatMin)
	{
		mlFloat e = normalisedVectorA * normalisedVectorB;
		mlFloat h = (1.0f - e) / vSquared;

		result.I.x = e + h * (v.x * v.x);
		result.I.y = h * v.x * v.y + v.z;
		result.I.z = h * v.x * v.z - v.y;
		result.J.x = h * v.x * v.y - v.z;
		result.J.y = e + h * (v.y * v.y);
		result.J.z = h * v.y * v.z + v.x;
		result.K.x = h * v.x * v.z + v.y;
		result.K.y = h * v.y * v.z - v.x;
		result.K.z = e + h * (v.z * v.z);
	}
	else
	{
		result.SetIdentity();
	}
}

mlVector3D mlMatrix3x3::Transform(const mlVector3D &v) const
{
	return mlVector3D(
				I.x * v.x + J.x * v.y + K.x * v.z,
				I.y * v.x + J.y * v.y + K.y * v.z,
				I.z * v.x + J.z * v.y + K.z * v.z
			);
}

mlVector3D mlMatrix3x3::TransformByTranspose(const mlVector3D &v) const
{
	return mlVector3D(
				I.x * v.x + I.y * v.y + I.z * v.z,
				J.x * v.x + J.y * v.y + J.z * v.z,
				K.x * v.x + K.y * v.y + K.z * v.z
			);
}

mlVector3D mlMatrix3x4::TransformVector(const mlVector3D &v) const
{
	return mlVector3D(
				I.x * v.x + J.x * v.y + K.x * v.z,
				I.y * v.x + J.y * v.y + K.y * v.z,
				I.z * v.x + J.z * v.y + K.z * v.z
			);
}

mlVector3D mlMatrix3x4::TransformVectorByTranspose(const mlVector3D &v) const
{
	return mlVector3D(
				I.x * v.x + I.y * v.y + I.z * v.z,
				J.x * v.x + J.y * v.y + J.z * v.z,
				K.x * v.x + K.y * v.y + K.z * v.z
			);
}

mlVector3D mlMatrix3x4::TransformPoint(const mlVector3D &v) const
{
	return mlVector3D(
				I.x * v.x + J.x * v.y + K.x * v.z + T.x,
				I.y * v.x + J.y * v.y + K.y * v.z + T.y,
				I.z * v.x + J.z * v.y + K.z * v.z + T.z
			);
}

mlVector3D mlMatrix3x4::TransformPointByTranspose(const mlVector3D &v) const
{
	mlVector3D temp = v - T;
	return mlVector3D(
				I.x * temp.x + I.y * temp.y + I.z * temp.z,
				J.x * temp.x + J.y * temp.y + J.z * temp.z,
				K.x * temp.x + K.y * temp.y + K.z * temp.z
			);
}

void mlMatrix3x4::TransformVectors(mlVector3D *out, const mlVector3D *in, int numVec) const
{
	for (int i = 0; i < numVec; i++)
	{
		*out++ = TransformVector(*in++);
	}
}

void mlMatrix3x4::TransformPoints(mlVector3D *out, const mlVector3D *in, int numPt) const
{
	for (int i = 0; i < numPt; i++)
	{
		*out++ = TransformPoint(*in++);
	}
}

void mlMatrix4x4::TransformVectors(mlVector4D *out, const mlVector4D *in, int numVec) const
{
	for (int i = 0; i < numVec; i++)
	{
		*out++ = TransformVector(*in++);
	}
}

void 	mlMatrixTranspose(mlMatrix3x3 & result, const mlMatrix3x3 & source)
{
	result = mlMatrix3x3(
		mlVector3D(source.I.x, source.J.x, source.K.x),
		mlVector3D(source.I.y, source.J.y, source.K.y),
		mlVector3D(source.I.z, source.J.z, source.K.z)
		);
}

void 	mlMatrixTranspose(mlMatrix3x4 &dest, const mlMatrix3x4 &src)
{
	dest.I.x = src.I.x;		dest.J.x = src.I.y;		dest.K.x = src.I.z;
	dest.I.y = src.J.x;		dest.J.y = src.J.y;		dest.K.y = src.J.z;
	dest.I.z = src.K.x;		dest.J.z = src.K.y;		dest.K.z = src.K.z;

	dest.T.x = -( dest.I.x * src.T.x  +  dest.J.x * src.T.y  +  dest.K.x * src.T.z );
	dest.T.y = -( dest.I.y * src.T.x  +  dest.J.y * src.T.y  +  dest.K.y * src.T.z );
	dest.T.z = -( dest.I.z * src.T.x  +  dest.J.z * src.T.y  +  dest.K.z * src.T.z );
}

void 	mlMatrixTranspose(mlMatrix4x4 & result, const mlMatrix4x4 & source)
{
	result = mlMatrix4x4(
		mlVector4D(source.I.x, source.J.x, source.K.x, source.T.x),
		mlVector4D(source.I.y, source.J.y, source.K.y, source.T.y),
		mlVector4D(source.I.z, source.J.z, source.K.z, source.T.z),
		mlVector4D(source.I.w, source.J.w, source.K.w, source.T.w)
		);
}

void	mlMatrixMultiply(mlMatrix3x3 &dest, const mlMatrix3x4 &A, const mlMatrix3x3 &B)
{
	if(&B != &dest)
	{
		dest.I.x  =  A.I.x * B.I.x  +  A.J.x * B.I.y  +  A.K.x * B.I.z;
		dest.I.y  =  A.I.y * B.I.x  +  A.J.y * B.I.y  +  A.K.y * B.I.z;
		dest.I.z  =  A.I.z * B.I.x  +  A.J.z * B.I.y  +  A.K.z * B.I.z;

		dest.J.x  =  A.I.x * B.J.x  +  A.J.x * B.J.y  +  A.K.x * B.J.z;
		dest.J.y  =  A.I.y * B.J.x  +  A.J.y * B.J.y  +  A.K.y * B.J.z;
		dest.J.z  =  A.I.z * B.J.x  +  A.J.z * B.J.y  +  A.K.z * B.J.z;

		dest.K.x  =  A.I.x * B.K.x  +  A.J.x * B.K.y  +  A.K.x * B.K.z;
		dest.K.y  =  A.I.y * B.K.x  +  A.J.y * B.K.y  +  A.K.y * B.K.z;
		dest.K.z  =  A.I.z * B.K.x  +  A.J.z * B.K.y  +  A.K.z * B.K.z;
	}
	else
	{
		mlMatrix3x3 temp;

		temp.I.x  =  A.I.x * B.I.x  +  A.J.x * B.I.y  +  A.K.x * B.I.z;
		temp.I.y  =  A.I.y * B.I.x  +  A.J.y * B.I.y  +  A.K.y * B.I.z;
		temp.I.z  =  A.I.z * B.I.x  +  A.J.z * B.I.y  +  A.K.z * B.I.z;

		temp.J.x  =  A.I.x * B.J.x  +  A.J.x * B.J.y  +  A.K.x * B.J.z;
		temp.J.y  =  A.I.y * B.J.x  +  A.J.y * B.J.y  +  A.K.y * B.J.z;
		temp.J.z  =  A.I.z * B.J.x  +  A.J.z * B.J.y  +  A.K.z * B.J.z;

		temp.K.x  =  A.I.x * B.K.x  +  A.J.x * B.K.y  +  A.K.x * B.K.z;
		temp.K.y  =  A.I.y * B.K.x  +  A.J.y * B.K.y  +  A.K.y * B.K.z;
		temp.K.z  =  A.I.z * B.K.x  +  A.J.z * B.K.y  +  A.K.z * B.K.z;

		dest = temp;
	}
}

void	mlMatrixMultiply(mlMatrix3x3 &dest, const mlMatrix3x3 &A, const mlMatrix3x3 &B)
{
	if((&A != &dest) && (&B != &dest))
	{
		dest.I.x  =  A.I.x * B.I.x  +  A.J.x * B.I.y  +  A.K.x * B.I.z;
		dest.I.y  =  A.I.y * B.I.x  +  A.J.y * B.I.y  +  A.K.y * B.I.z;
		dest.I.z  =  A.I.z * B.I.x  +  A.J.z * B.I.y  +  A.K.z * B.I.z;

		dest.J.x  =  A.I.x * B.J.x  +  A.J.x * B.J.y  +  A.K.x * B.J.z;
		dest.J.y  =  A.I.y * B.J.x  +  A.J.y * B.J.y  +  A.K.y * B.J.z;
		dest.J.z  =  A.I.z * B.J.x  +  A.J.z * B.J.y  +  A.K.z * B.J.z;

		dest.K.x  =  A.I.x * B.K.x  +  A.J.x * B.K.y  +  A.K.x * B.K.z;
		dest.K.y  =  A.I.y * B.K.x  +  A.J.y * B.K.y  +  A.K.y * B.K.z;
		dest.K.z  =  A.I.z * B.K.x  +  A.J.z * B.K.y  +  A.K.z * B.K.z;
	}
	else
	{
		mlMatrix3x3 temp;

		temp.I.x  =  A.I.x * B.I.x  +  A.J.x * B.I.y  +  A.K.x * B.I.z;
		temp.I.y  =  A.I.y * B.I.x  +  A.J.y * B.I.y  +  A.K.y * B.I.z;
		temp.I.z  =  A.I.z * B.I.x  +  A.J.z * B.I.y  +  A.K.z * B.I.z;

		temp.J.x  =  A.I.x * B.J.x  +  A.J.x * B.J.y  +  A.K.x * B.J.z;
		temp.J.y  =  A.I.y * B.J.x  +  A.J.y * B.J.y  +  A.K.y * B.J.z;
		temp.J.z  =  A.I.z * B.J.x  +  A.J.z * B.J.y  +  A.K.z * B.J.z;

		temp.K.x  =  A.I.x * B.K.x  +  A.J.x * B.K.y  +  A.K.x * B.K.z;
		temp.K.y  =  A.I.y * B.K.x  +  A.J.y * B.K.y  +  A.K.y * B.K.z;
		temp.K.z  =  A.I.z * B.K.x  +  A.J.z * B.K.y  +  A.K.z * B.K.z;

		dest = temp;
	}
}

void	mlMatrixMultiply(mlMatrix4x4 &dest, const mlMatrix4x4 &A, const mlMatrix4x4 &B)
{
	if((&A != &dest) && (&B != &dest))
	{
		dest.I.x   =  A.I.x * B.I.x  +  A.J.x * B.I.y  +  A.K.x * B.I.z  +  A.T.x * B.I.w;
		dest.I.y   =  A.I.y * B.I.x  +  A.J.y * B.I.y  +  A.K.y * B.I.z  +  A.T.y * B.I.w;
		dest.I.z   =  A.I.z * B.I.x  +  A.J.z * B.I.y  +  A.K.z * B.I.z  +  A.T.z * B.I.w;
		dest.I.w   =  A.I.w * B.I.x  +  A.J.w * B.I.y  +  A.K.w * B.I.z  +  A.T.w * B.I.w;

		dest.J.x   =  A.I.x * B.J.x  +  A.J.x * B.J.y  +  A.K.x * B.J.z  +  A.T.x * B.J.w;
		dest.J.y   =  A.I.y * B.J.x  +  A.J.y * B.J.y  +  A.K.y * B.J.z  +  A.T.y * B.J.w;
		dest.J.z   =  A.I.z * B.J.x  +  A.J.z * B.J.y  +  A.K.z * B.J.z  +  A.T.z * B.J.w;
		dest.J.w   =  A.I.w * B.J.x  +  A.J.w * B.J.y  +  A.K.w * B.J.z  +  A.T.w * B.J.w;

		dest.K.x   =  A.I.x * B.K.x  +  A.J.x * B.K.y  +  A.K.x * B.K.z  +  A.T.x * B.K.w;
		dest.K.y   =  A.I.y * B.K.x  +  A.J.y * B.K.y  +  A.K.y * B.K.z  +  A.T.y * B.K.w;
		dest.K.z   =  A.I.z * B.K.x  +  A.J.z * B.K.y  +  A.K.z * B.K.z  +  A.T.z * B.K.w;
		dest.K.w   =  A.I.w * B.K.x  +  A.J.w * B.K.y  +  A.K.w * B.K.z  +  A.T.w * B.K.w;
	
		dest.T.x   =  A.I.x * B.T.x  +  A.J.x * B.T.y  +  A.K.x * B.T.z  +  A.T.x * B.T.w;
		dest.T.y   =  A.I.y * B.T.x  +  A.J.y * B.T.y  +  A.K.y * B.T.z  +  A.T.y * B.T.w;
		dest.T.z   =  A.I.z * B.T.x  +  A.J.z * B.T.y  +  A.K.z * B.T.z  +  A.T.z * B.T.w;
		dest.T.w   =  A.I.w * B.T.x  +  A.J.w * B.T.y  +  A.K.w * B.T.z  +  A.T.w * B.T.w;
	}
	else
	{
		mlMatrix4x4 temp;
		
		temp.I.x   =  A.I.x * B.I.x  +  A.J.x * B.I.y  +  A.K.x * B.I.z  +  A.T.x * B.I.w;
		temp.I.y   =  A.I.y * B.I.x  +  A.J.y * B.I.y  +  A.K.y * B.I.z  +  A.T.y * B.I.w;
		temp.I.z   =  A.I.z * B.I.x  +  A.J.z * B.I.y  +  A.K.z * B.I.z  +  A.T.z * B.I.w;
		temp.I.w   =  A.I.w * B.I.x  +  A.J.w * B.I.y  +  A.K.w * B.I.z  +  A.T.w * B.I.w;

		temp.J.x   =  A.I.x * B.J.x  +  A.J.x * B.J.y  +  A.K.x * B.J.z  +  A.T.x * B.J.w;
		temp.J.y   =  A.I.y * B.J.x  +  A.J.y * B.J.y  +  A.K.y * B.J.z  +  A.T.y * B.J.w;
		temp.J.z   =  A.I.z * B.J.x  +  A.J.z * B.J.y  +  A.K.z * B.J.z  +  A.T.z * B.J.w;
		temp.J.w   =  A.I.w * B.J.x  +  A.J.w * B.J.y  +  A.K.w * B.J.z  +  A.T.w * B.J.w;

		temp.K.x   =  A.I.x * B.K.x  +  A.J.x * B.K.y  +  A.K.x * B.K.z  +  A.T.x * B.K.w;
		temp.K.y   =  A.I.y * B.K.x  +  A.J.y * B.K.y  +  A.K.y * B.K.z  +  A.T.y * B.K.w;
		temp.K.z   =  A.I.z * B.K.x  +  A.J.z * B.K.y  +  A.K.z * B.K.z  +  A.T.z * B.K.w;
		temp.K.w   =  A.I.w * B.K.x  +  A.J.w * B.K.y  +  A.K.w * B.K.z  +  A.T.w * B.K.w;
	
		temp.T.x   =  A.I.x * B.T.x  +  A.J.x * B.T.y  +  A.K.x * B.T.z  +  A.T.x * B.T.w;
		temp.T.y   =  A.I.y * B.T.x  +  A.J.y * B.T.y  +  A.K.y * B.T.z  +  A.T.y * B.T.w;
		temp.T.z   =  A.I.z * B.T.x  +  A.J.z * B.T.y  +  A.K.z * B.T.z  +  A.T.z * B.T.w;
		temp.T.w   =  A.I.w * B.T.x  +  A.J.w * B.T.y  +  A.K.w * B.T.z  +  A.T.w * B.T.w;

		dest = temp;
	}
}

void	mlMatrixMultiply(mlMatrix3x4 &dest, const mlMatrix3x4 &A, const mlMatrix3x4 &B)
{
	if((&A != &dest) && (&B != &dest))
	{
		dest.I.x =  A.I.x * B.I.x  +  A.J.x * B.I.y  +  A.K.x * B.I.z;
		dest.I.y =  A.I.y * B.I.x  +  A.J.y * B.I.y  +  A.K.y * B.I.z;
		dest.I.z =  A.I.z * B.I.x  +  A.J.z * B.I.y  +  A.K.z * B.I.z;

		dest.J.x =  A.I.x * B.J.x  +  A.J.x * B.J.y  +  A.K.x * B.J.z;
		dest.J.y =  A.I.y * B.J.x  +  A.J.y * B.J.y  +  A.K.y * B.J.z;
		dest.J.z =  A.I.z * B.J.x  +  A.J.z * B.J.y  +  A.K.z * B.J.z;

		dest.K.x =  A.I.x * B.K.x  +  A.J.x * B.K.y  +  A.K.x * B.K.z;
		dest.K.y =  A.I.y * B.K.x  +  A.J.y * B.K.y  +  A.K.y * B.K.z;
		dest.K.z =  A.I.z * B.K.x  +  A.J.z * B.K.y  +  A.K.z * B.K.z;

		dest.T.x =  A.I.x * B.T.x  +  A.J.x * B.T.y  +  A.K.x * B.T.z  +  A.T.x;
		dest.T.y =  A.I.y * B.T.x  +  A.J.y * B.T.y  +  A.K.y * B.T.z  +  A.T.y;
		dest.T.z =  A.I.z * B.T.x  +  A.J.z * B.T.y  +  A.K.z * B.T.z  +  A.T.z;
	}
	else
	{
		mlMatrix3x4 temp;


		temp.I.x =  A.I.x * B.I.x  +  A.J.x * B.I.y  +  A.K.x * B.I.z;
		temp.I.y =  A.I.y * B.I.x  +  A.J.y * B.I.y  +  A.K.y * B.I.z;
		temp.I.z =  A.I.z * B.I.x  +  A.J.z * B.I.y  +  A.K.z * B.I.z;

		temp.J.x =  A.I.x * B.J.x  +  A.J.x * B.J.y  +  A.K.x * B.J.z;
		temp.J.y =  A.I.y * B.J.x  +  A.J.y * B.J.y  +  A.K.y * B.J.z;
		temp.J.z =  A.I.z * B.J.x  +  A.J.z * B.J.y  +  A.K.z * B.J.z;

		temp.K.x =  A.I.x * B.K.x  +  A.J.x * B.K.y  +  A.K.x * B.K.z;
		temp.K.y =  A.I.y * B.K.x  +  A.J.y * B.K.y  +  A.K.y * B.K.z;
		temp.K.z =  A.I.z * B.K.x  +  A.J.z * B.K.y  +  A.K.z * B.K.z;

		temp.T.x =  A.I.x * B.T.x  +  A.J.x * B.T.y  +  A.K.x * B.T.z  +  A.T.x;
		temp.T.y =  A.I.y * B.T.x  +  A.J.y * B.T.y  +  A.K.y * B.T.z  +  A.T.y;
		temp.T.z =  A.I.z * B.T.x  +  A.J.z * B.T.y  +  A.K.z * B.T.z  +  A.T.z;

		dest = temp;
	}
}

mlFloat mlMatrix3x3::GetDeterminant() const
{
	return 	I.x * (J.y * K.z - J.z * K.y) +
			J.x * (K.y * I.z - K.z * I.y) +
			K.x * (I.y * J.z - I.z * J.y);
}

void mlMatrix3x3::OrthoNormaliseOnX()
{
	I.Normalise();
	
	J -= I * (J * I);
	J.Normalise();
	
	K -= (K * I) * I;
	K -= (K * J) * J;
	K.Normalise();
}

void mlMatrix3x3::OrthoNormaliseOnY()
{
	J.Normalise();
	
	I -= J * (I * J);
	I.Normalise();
	
	K -= (K * J) * J;
	K -= (K * I) * I;
	K.Normalise();
}

void mlMatrix3x3::OrthoNormaliseOnZ()
{
	K.Normalise();
	
	I -= K * (I * K);
	I.Normalise();
	
	J -= (J * K) * K;
	J -= (J * I) * I;
	J.Normalise();
}

mlFloat mlMatrix3x4::GetDeterminant3x3() const
{
	return 	I.x * (J.y * K.z - J.z * K.y) +
			J.x * (K.y * I.z - K.z * I.y) +
			K.x * (I.y * J.z - I.z * J.y);			
}


void mlMatrixInvert(mlMatrix3x3 & result, const mlMatrix3x3 & source)
{
	assert(&result != &source);
	
	mlFloat inverseDeterminant = 1.0f / source.GetDeterminant();
	
	const mlMatrix3x3 & M = source;
	
	result.I.x = (M.J.y * M.K.z - M.K.y * M.J.z) * inverseDeterminant;
	result.J.x = (M.J.z * M.K.x - M.K.z * M.J.x) * inverseDeterminant;
	result.K.x = (M.J.x * M.K.y - M.K.x * M.J.y) * inverseDeterminant;
	
	result.I.y = (M.K.y * M.I.z - M.I.y * M.K.z) * inverseDeterminant;
	result.J.y = (M.K.z * M.I.x - M.I.z * M.K.x) * inverseDeterminant;
	result.K.y = (M.K.x * M.I.y - M.I.x * M.K.y) * inverseDeterminant;
	
	result.I.z = (M.I.y * M.J.z - M.J.y * M.I.z) * inverseDeterminant;
	result.J.z = (M.I.z * M.J.x - M.J.z * M.I.x) * inverseDeterminant;
	result.K.z = (M.I.x * M.J.y - M.J.x * M.I.y) * inverseDeterminant;
}

/****************************************************************************/

void mlMatrixInvert(mlMatrix3x4 & result, const mlMatrix3x4 & source)
{
	assert(&result != &source);
	
	mlFloat inverseDeterminant = 1.0f / source.GetDeterminant3x3();
	
	const mlMatrix3x4 & M = source;
	
	result.I.x = (M.J.y * M.K.z - M.K.y * M.J.z) * inverseDeterminant;
	result.J.x = (M.J.z * M.K.x - M.K.z * M.J.x) * inverseDeterminant;
	result.K.x = (M.J.x * M.K.y - M.K.x * M.J.y) * inverseDeterminant;
	
	result.I.y = (M.K.y * M.I.z - M.I.y * M.K.z) * inverseDeterminant;
	result.J.y = (M.K.z * M.I.x - M.I.z * M.K.x) * inverseDeterminant;
	result.K.y = (M.K.x * M.I.y - M.I.x * M.K.y) * inverseDeterminant;
	
	result.I.z = (M.I.y * M.J.z - M.J.y * M.I.z) * inverseDeterminant;
	result.J.z = (M.I.z * M.J.x - M.J.z * M.I.x) * inverseDeterminant;
	result.K.z = (M.I.x * M.J.y - M.J.x * M.I.y) * inverseDeterminant;
	
	result.T = result.TransformVector(M.T * -1.0f);
}

/****************************************************************************/

void mlMatrix3x3::SetRotation(const mlQuaternion &orientation)
{
	mlFloat x2 = orientation.x + orientation.x;
	mlFloat y2 = orientation.y + orientation.y;
	mlFloat z2 = orientation.z + orientation.z;
	
	mlFloat twoAsq = x2 * orientation.x;
	mlFloat twoBsq = y2 * orientation.y;
	mlFloat twoCsq = z2 * orientation.z;
	
	mlFloat twoAB = x2 * orientation.y;
	mlFloat twoAC = x2 * orientation.z;
	mlFloat twoBC = y2 * orientation.z;
	
	mlFloat twoSA = x2 * orientation.w;
	mlFloat twoSB = y2 * orientation.w;
	mlFloat twoSC = z2 * orientation.w;
	
	I.x = 1.0f - twoBsq - twoCsq;
	J.y = 1.0f - twoAsq - twoCsq;
	K.z = 1.0f - twoAsq - twoBsq;
	
	K.y = twoBC - twoSA;
	J.z = twoBC + twoSA;
	
	K.x = twoAC + twoSB;
	I.z = twoAC - twoSB;
	
	J.x = twoAB - twoSC;
	I.y = twoAB + twoSC;
}

void mlMatrix3x4::SetTranslation(const mlVector3D &translation)
{
	T = translation;
}

void mlMatrix3x4::SetIdentity()
{
	I = mlVector3D(1.0f, 0.0f, 0.0f);
	J = mlVector3D(0.0f, 1.0f, 0.0f);
	K = mlVector3D(0.0f, 0.0f, 1.0f);
	
	T.SetZero();
}

// The side effect of this is that the scale is reset to 1.0
void mlMatrix3x4::SetRotation(const mlQuaternion &orientation)
{
	mlFloat x2 = 2.0f * orientation.x;
	mlFloat y2 = 2.0f * orientation.y;
	mlFloat z2 = 2.0f * orientation.z;
	
	mlFloat twoAsq = x2 * orientation.x;
	mlFloat twoBsq = y2 * orientation.y;
	mlFloat twoCsq = z2 * orientation.z;
	
	mlFloat twoAB = x2 * orientation.y;
	mlFloat twoAC = x2 * orientation.z;
	mlFloat twoBC = y2 * orientation.z;
	
	mlFloat twoSA = x2 * orientation.w;
	mlFloat twoSB = y2 * orientation.w;
	mlFloat twoSC = z2 * orientation.w;
	
	I.x = 1.0f - twoBsq - twoCsq;
	J.y = 1.0f - twoAsq - twoCsq;
	K.z = 1.0f - twoAsq - twoBsq;
	
	K.y = twoBC - twoSA;
	J.z = twoBC + twoSA;
	
	K.x = twoAC + twoSB;
	I.z = twoAC - twoSB;
	
	J.x = twoAB - twoSC;
	I.y = twoAB + twoSC;
}

void mlMatrix3x4::ApplyScale(const mlVector3D & scale)
{
    I *= scale.x;
    J *= scale.y;
    K *= scale.z;
}

void mlMatrix3x4::SetScale(const mlVector3D & scale)
{
    ApplyScale(InvertScaleVector(GetScale()));
    ApplyScale(scale);
}


mlVector3D mlMatrix3x4::GetScale() const
{
    return mlVector3D(I.Magnitude(),
                      J.Magnitude(),
                      K.Magnitude());
}

mlVector4D	mlMatrix4x4::TransformVector(const mlVector4D &v) const
{
	return mlVector4D(
				I.x * v.x + J.x * v.y + K.x * v.z + T.x * v.w,
				I.y * v.x + J.y * v.y + K.y * v.z + T.y * v.w,
				I.z * v.x + J.z * v.y + K.z * v.z + T.z * v.w,
				I.w * v.x + J.w * v.y + K.w * v.z + T.w * v.w
				);
}

// Up is along the positive Y axis (vUp in global coordinates)
// Forward is along the NEGATIVE Z axis (negative, remember! Its the opengl standard, its what the camera uses, and so it can be what objects use)
// Right is along the positive X axis.
void mlMatrixUtility::MatrixFromDirection(mlMatrix3x3 & result, const mlVector3D & vForward, const mlVector3D & vUp)
{
	// This produces an identity matrix when vForward is along the Z axis, and vUp is along the Y axis.
	// Alternative - an identity matrix when vForward is along the -ve Z axis, and vUp is along the Y axis.

	// Compute our new look at vector, which will be
	//   the new negative Z axis of our transformed object.
	result.K = vForward;
	result.K.Normalise();

	// Cross product of the new look at vector and the current
	//   up vector will produce a vector which is the new
	//   positive X axis of our transformed object.
	result.I = mlVectorCross(result.K, vUp);
	//result.I = mlVectorCross(vUp, result.K); // new
	result.I.Normalise();

	// Calculate the new up vector, which will be the
	//   positive Y axis of our transformed object. Note
	//   that it will lie in the same plane as the new
	//   look at vector and the old up vector.
	result.J = mlVectorCross(result.I, result.K);
	//result.J = mlVectorCross(result.K, result.I); // new

	// Account for the fact that the geometry will be defined to
	//   point along the negative Z axis.
    result.K *= -1.0f;
}

void mlMatrixUtility::MatrixFromVectorProduct(mlMatrix3x3 & result, const mlVector3D & a, const mlVector3D & b)
{
	result.I.x = a.x * b.x;
	result.I.y = a.x * b.y;
	result.I.z = a.x * b.z;
	
	result.J.x = a.y * b.x;
	result.J.y = a.y * b.y;
	result.J.z = a.y * b.z;
	
	result.K.x = a.z * b.x;
	result.K.y = a.z * b.y;
	result.K.z = a.z * b.z;
}

void mlxMatrixAdd(mlMatrix3x3 & result, const mlMatrix3x3 & matrixToAdd)
{
	result.I += matrixToAdd.I;
	result.J += matrixToAdd.J;
	result.K += matrixToAdd.K;
}

void mlxMatrixSubtract(mlMatrix3x3 & result, const mlMatrix3x3 & matrixToSubtract)
{
	result.I -= matrixToSubtract.I;
	result.J -= matrixToSubtract.J;
	result.K -= matrixToSubtract.K;
}

void mlxMatrixAddScaled(mlMatrix3x3 & result, const mlMatrix3x3 & matrixToSubtract, mlFloat scale)
{
	result.I += matrixToSubtract.I * scale;
	result.J += matrixToSubtract.J * scale;
	result.K += matrixToSubtract.K * scale;
}

void mlxMatrixFromAxisScale(mlMatrix3x3 & result, const mlVector3D & axis, mlFloat scale)
{
	result.SetIdentity();
	
	mlMatrix3x3 outerProduct;
	mlMatrixUtility::MatrixFromVectorProduct(outerProduct, axis, axis);
	
	mlxMatrixAddScaled(result, outerProduct, scale - 1.0f);
}

void mlMatrixUtility::MatrixFromProjection(mlMatrix3x3 & result, const mlVector3D & planeNormal)
{
	mlxMatrixFromAxisScale(result, planeNormal, 0.0f);
}

void mlMatrixUtility::MatrixFromReflection(mlMatrix3x3 & result, const mlVector3D & planeNormal)
{
	mlxMatrixFromAxisScale(result, planeNormal, -1.0f);
}
