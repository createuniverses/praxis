
#ifndef ML_TRANSFORM_H
#define ML_TRANSFORM_H

#include 	"ML_Maths.h"
#include	"ML_Vector.h"
#include	"ML_Quaternion.h"
#include 	"ML_Matrix.h"

class mlTransform 
{
public:
	mlTransform();
	
	mlTransform(const mlQuaternion & rotation, const mlVector3D & translation, mlFloat scale = 1.0f);
	
	const mlTransform& operator=(const mlTransform& cloneFrom);
	mlTransform(const mlTransform& cloneFrom);
	
	const mlTransform& operator=(const mlMatrix3x4 & cloneFrom);
	explicit mlTransform(const mlMatrix3x4& cloneFrom);
	
    bool			operator==(const mlTransform & other) const;
	bool			IsIdentity() const;

    void            SetIdentity();
	
	void			TransformSelf(const mlTransform &transform);
	
	void			Normalise();
	
	void			SetTranslation(const mlVector3D & translation);
	void			SetRotation(const mlQuaternion & rotation);
    void			SetScale(const mlVector3D & scale);
    void			SetScale(float scale);
    void			SetMatrix(const mlMatrix3x4 & matrix);

	void			ApplyTranslation(const mlVector3D & translation);
	void			ApplyRotation(const mlQuaternion & rotation);
    void			ApplyScale(const mlVector3D & scale);
    void			ApplyScale(mlFloat scale);

	void			Invert();
	
	void			Interpolate(mlFloat alpha, const mlTransform &from, const mlTransform &to);
	
	mlVector3D		TransformPoint(const mlVector3D &in) const;
	mlVector3D		TransformPointInverse(const mlVector3D &in) const;
	mlVector3D		TransformVector(const mlVector3D &in) const;
	mlVector3D		TransformVectorInverse(const mlVector3D &in) const;
	
	void			TransformPoints			(mlVector3D outArray[], const mlVector3D inArray[], int pointCount) const;
	void			TransformPointsInverse	(mlVector3D outArray[], const mlVector3D inArray[], int pointCount) const;
	void			TransformVectors		(mlVector3D outArray[], const mlVector3D inArray[], int vectorCount) const;
	void			TransformVectorsInverse	(mlVector3D outArray[], const mlVector3D inArray[], int vectorCount) const;
	
    mlVector3D		GetScale() const;
    mlQuaternion	GetRotation() const;
	mlVector3D		GetTranslation() const;
	
	const mlMatrix3x4 &	GetMatrix() const;
	
private:
    mlQuaternion			m_rotation;
	mlVector3D				m_translation;
	
    mutable mlMatrix3x4		m_matrix;
    mlVector3D              m_scale;
	
    mutable bool			m_matrix_valid;
	
	void					UpdateMatrixAlways() const;
	void					UpdateMatrixIfNecessary() const;
};

extern const mlTransform 		mlTransformIdentity;

void		mlCombineTransform		 (mlTransform &AtoC, const mlTransform &AtoB, const mlTransform &BtoC);
void		mlCombineTransformInverse(mlTransform &AtoC, const mlTransform &AtoB, const mlTransform &CtoB);

class mlTransformUtility
{
public:
	static mlTransform			TransformFromReflection(const mlVector3D & pointOnPlane, const mlVector3D & planeNormalNormalised);
	static void					TransformFromPointForwardUp(mlTransform & trnTransform, const mlVector3D & vPosition, const mlVector3D & vForward, const mlVector3D & vUp);
};

inline const mlTransform & mlTransform::operator=(const mlTransform & cloneFrom)
{
	m_matrix_valid = false;
	m_rotation = cloneFrom.m_rotation;
	m_translation = cloneFrom.m_translation;
	m_scale = cloneFrom.m_scale;

	return *this;
}

inline mlTransform::mlTransform(const mlTransform& cloneFrom)
{
	*this = cloneFrom;
}

inline const mlTransform & mlTransform::operator=(const mlMatrix3x4 & cloneFrom)
{
	SetMatrix(cloneFrom);
	return *this;
}

inline mlTransform::mlTransform(const mlMatrix3x4& cloneFrom)
{
	*this = cloneFrom;
}

inline void	mlTransform::UpdateMatrixIfNecessary() const
{
	if (!m_matrix_valid)
		UpdateMatrixAlways();
}

inline const mlMatrix3x4 & mlTransform::GetMatrix() const
{
	UpdateMatrixIfNecessary();
		
	return m_matrix;
}

inline bool mlTransform::operator==(const mlTransform & other) const
{
	return	(m_translation == other.m_translation) &&
			(m_scale == other.m_scale) &&
			(m_rotation == other.m_rotation);
}

inline bool	mlTransform::IsIdentity() const
{
	return	(m_translation.x == 0.0f && m_translation.y == 0.0f && m_translation.z == 0.0f) &&
            (m_scale == mlVector3D(1.0f, 1.0f, 1.0f)) &&
			(m_rotation.w == 1.0f && m_rotation.x == 0.0f && m_rotation.y == 0.0f && m_rotation.z == 0.0f);
}

#endif // ML_TRANSFORM_H
