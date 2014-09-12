
#ifndef ML_MATRIX_H
#define ML_MATRIX_H

#include "ML_Vector.h"

class mlQuaternion;

class mlMatrix3x3
{
public:
	mlVector3D	I;
	mlVector3D	J;
	mlVector3D	K;
	
	mlMatrix3x3() {}
	mlMatrix3x3(const mlVector3D & initI, const mlVector3D & initJ, const mlVector3D & initK);
	
	explicit mlMatrix3x3(const mlQuaternion &quaternion);
	
	void				SetIdentity();
	void				SetRotation(const mlQuaternion &rotation);
	
	mlVector3D			Transform(const mlVector3D &v) const; 
	
	mlVector3D			TransformByTranspose(const mlVector3D &v) const; 
	
	mlFloat				GetDeterminant() const;
	
	void OrthoNormaliseOnX();
	void OrthoNormaliseOnY();
	void OrthoNormaliseOnZ();
	
	mlVector3D & 		operator[](int j);
	const mlVector3D & 	operator[](int j) const;

};


class mlMatrix3x4
{
public:
	mlVector3D	I;
	mlVector3D	J;
	mlVector3D	K;
	mlVector3D	T;
	
	mlMatrix3x4() {}
	mlMatrix3x4(const mlVector3D & initI, const mlVector3D & initJ, const mlVector3D & initK, const mlVector3D & initT);
	mlMatrix3x4(const mlMatrix3x3& matrixCloneFrom, const mlVector3D& translation);
	
	explicit mlMatrix3x4(const mlQuaternion &quaternion);
	explicit mlMatrix3x4(const mlMatrix3x3 &cloneFrom);
	
	void				SetIdentity();
	void				SetTranslation(const mlVector3D &translation);
	void				SetRotation(const mlQuaternion &rotation);

    void                ApplyScale(const mlVector3D & scale);
    void                SetScale(const mlVector3D & scale);
    mlVector3D          GetScale() const;
	
	mlVector3D			TransformVector(const mlVector3D &v) const;
	mlVector3D			TransformPoint(const mlVector3D &v) const;
	
	mlVector3D			TransformVectorByTranspose(const mlVector3D &v) const; 
	mlVector3D			TransformPointByTranspose(const mlVector3D &v) const; 
	
	void				TransformVectors(mlVector3D *out, const mlVector3D *in, int numVec) const;
	void				TransformPoints(mlVector3D *out, const mlVector3D *in, int numPt) const;
	
	mlFloat				GetDeterminant3x3() const;
	
	mlVector3D&			operator[](int j);
	const mlVector3D& 		operator[](int j) const;

};

class mlMatrix4x4
{
public:
	mlVector4D	I;
	mlVector4D	J;
	mlVector4D	K;
	mlVector4D	T;
	
	mlMatrix4x4() {}
	mlMatrix4x4(const mlVector4D & initI, const mlVector4D & initJ, const mlVector4D & initK, const mlVector4D & initT);
	mlMatrix4x4(const mlMatrix3x3 & matrixCloneFrom, const mlVector3D & translation);
	
	explicit mlMatrix4x4(const mlMatrix3x3 & cloneFrom);
	explicit mlMatrix4x4(const mlMatrix3x4 & cloneFrom);
	
	mlVector4D 			TransformVector(const mlVector4D &v) const;
	void				TransformVectors(mlVector4D *out, const mlVector4D *in, int numVec) const;
	
	mlVector4D& 		operator[](int j);
	const mlVector4D& 	operator[](int j) const;

};

void 					mlMatrixInvert(mlMatrix3x3 & result, const mlMatrix3x3 & source);
void 					mlMatrixInvert(mlMatrix3x4 & result, const mlMatrix3x4 & source);

void 					mlMatrixTranspose(mlMatrix3x3 & result, const mlMatrix3x3 & source);
void 					mlMatrixTranspose(mlMatrix3x4 & result, const mlMatrix3x4 & source);
void 					mlMatrixTranspose(mlMatrix4x4 & result, const mlMatrix4x4 & source);

void					mlMatrixMultiply(mlMatrix3x3 &dest, const mlMatrix3x4 &A, const mlMatrix3x3 &B);
void					mlMatrixMultiply(mlMatrix3x4 &dest, const mlMatrix3x4 &A, const mlMatrix3x4 &B);

void					mlMatrixMultiply(mlMatrix3x3 &dest, const mlMatrix3x3 &A, const mlMatrix3x3 &B);
void					mlMatrixMultiply(mlMatrix4x4 &dest, const mlMatrix4x4 &A, const mlMatrix4x4 &B);

extern const mlMatrix3x3	mlMatrix3x3Identity;
extern const mlMatrix3x4	mlMatrix3x4Identity;
extern const mlMatrix4x4	mlMatrix4x4Identity;

class mlMatrixUtility
{
public:
	// Up is along the positive Y axis (vUp in global coordinates)
	// Forward is along the NEGATIVE Z axis (negative, remember! Its the opengl standard, its what the camera uses, and so it can be what objects use)
	// Right is along the positive X axis.
	static void			MatrixFromDirection(mlMatrix3x3 & result, const mlVector3D & vForward, const mlVector3D & vUp);

	static void			MatrixFromProjection(mlMatrix3x3 & result, const mlVector3D & planeNormalNormalised);
	
	static void			MatrixFromReflection(mlMatrix3x3 & result, const mlVector3D & planeNormalNormalised);

	static void			MatrixFromVectorProduct(mlMatrix3x3 & result, const mlVector3D & a, const mlVector3D & b);	

	static void			MatrixFromVectorCrossProduct(mlMatrix3x3 & result, const mlVector3D & vector);	

	static void			MatrixFromVectorRotation(mlMatrix3x3 & result, const mlVector3D & normalisedVectorA, const mlVector3D & normalisedVectorB);
};

inline mlMatrix3x3::mlMatrix3x3(const mlVector3D & initI, const mlVector3D & initJ, const mlVector3D & initK) :
	I(initI),
	J(initJ),
	K(initK)
{
}

inline mlMatrix3x4::mlMatrix3x4(const mlVector3D & initI, const mlVector3D & initJ, const mlVector3D & initK, const mlVector3D & initT) :
	I(initI),
	J(initJ),
	K(initK),
	T(initT)
{
}

inline mlMatrix3x4::mlMatrix3x4(const mlMatrix3x3& matrixCloneFrom, const mlVector3D& translation) :
	I(matrixCloneFrom.I),
	J(matrixCloneFrom.J),
	K(matrixCloneFrom.K),
	T(translation)
{
}

inline mlMatrix4x4::mlMatrix4x4(const mlVector4D & initI, const mlVector4D & initJ, const mlVector4D & initK, const mlVector4D & initT) :
	I(initI),
	J(initJ),
	K(initK),
	T(initT)
{
}

inline mlMatrix3x3::mlMatrix3x3(const mlQuaternion &quaternion)
{
	SetRotation(quaternion);
}

inline mlMatrix3x4::mlMatrix3x4(const mlMatrix3x3 &cloneFrom) :
	I(cloneFrom.I),
	J(cloneFrom.J),
	K(cloneFrom.K),
	T(mlVector3DZero)
{
}

inline mlVector3D& mlMatrix3x3::operator[](int j) 
{
	return reinterpret_cast<mlVector3D*>(this)[j];
}

inline mlVector3D& mlMatrix3x4::operator[](int j) 
{
	return reinterpret_cast<mlVector3D*>(this)[j];
}

inline mlVector4D& mlMatrix4x4::operator[](int j) 
{
	return reinterpret_cast<mlVector4D*>(this)[j];
}

inline const mlVector3D& mlMatrix3x3::operator[](int j) const
{
	return reinterpret_cast<const mlVector3D*>(this)[j];
}

inline const mlVector3D& mlMatrix3x4::operator[](int j) const
{
	return reinterpret_cast<const mlVector3D*>(this)[j];
}

inline const mlVector4D& mlMatrix4x4::operator[](int j) const
{
	return reinterpret_cast<const mlVector4D*>(this)[j];
}

#endif // ML_MATRIX_H

