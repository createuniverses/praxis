
#include "ML_Transform.h"

const mlTransform 		mlTransformIdentity;

mlTransform::
mlTransform()
:	m_rotation(1.0f, 0.0f, 0.0f, 0.0f),
	m_translation(0.0f, 0.0f, 0.0f),
	m_matrix(	mlVector3D(1.0f, 0.0f, 0.0f),
				mlVector3D(0.0f, 1.0f, 0.0f),
				mlVector3D(0.0f, 0.0f, 1.0f),
				mlVector3D(0.0f, 0.0f, 0.0f)	),
	m_scale(1.0f, 1.0f, 1.0f),
	m_matrix_valid(true)
{
}

mlTransform::
mlTransform(const mlQuaternion &rotation, const mlVector3D &translation, mlFloat scale)
:	m_rotation(rotation),
	m_translation(translation),
    m_scale(scale, scale, scale),
	m_matrix_valid(false)
{
}

void mlTransform::SetIdentity()
{
    m_rotation               = mlQuaternion(1.0f, 0.0f, 0.0f, 0.0f);
    m_translation            = mlVector3D(0.0f, 0.0f, 0.0f);
    m_scale                  = mlVector3D(1.0f, 1.0f, 1.0f);

    m_matrix                 = mlMatrix3x4(mlVector3D(1.0f, 0.0f, 0.0f),
                                           mlVector3D(0.0f, 1.0f, 0.0f),
                                           mlVector3D(0.0f, 0.0f, 1.0f),
                                           mlVector3D(0.0f, 0.0f, 0.0f));

    m_matrix_valid           = true;
}

void mlTransform::
TransformSelf(const mlTransform &t)
{
	m_rotation = t.m_rotation * m_rotation;

    m_translation = ApplyScaleVector(t.m_scale, t.m_rotation.TransformVector(m_translation)) + t.m_translation;

    m_scale = ApplyScaleVector(m_scale, t.m_scale);

	m_matrix_valid = false;
}

void mlTransform::
Interpolate(mlFloat alpha, const mlTransform &from, const mlTransform &to)
{
	m_rotation = mlQuaternionInterpolate(alpha, from.m_rotation, to.m_rotation);

	m_translation = mlInterpolate(alpha, from.m_translation, to.m_translation);
    m_scale       = mlInterpolate(alpha, from.m_scale, to.m_scale);
	m_matrix_valid = false;
}

void mlTransform::
Invert()
{
	m_rotation.Invert();

    m_scale = InvertScaleVector(m_scale);

    m_translation = ApplyScaleVector(m_rotation.TransformVector(m_translation), -m_scale);

	m_matrix_valid = false;
}


mlVector3D mlTransform::
TransformPoint(const mlVector3D &in) const
{
    return m_rotation.TransformVector(ApplyScaleVector(in, m_scale)) + m_translation;
}

mlVector3D mlTransform::
TransformPointInverse(const mlVector3D &in) const
{
	UpdateMatrixIfNecessary();

    mlVector3D inverseScaleSquared = InvertScaleVector(ApplyScaleVector(m_scale, m_scale));
    return ApplyScaleVector(m_matrix.TransformPointByTranspose(in), inverseScaleSquared);
}

mlVector3D mlTransform::
TransformVector(const mlVector3D &in) const
{
    return m_rotation.TransformVector(ApplyScaleVector(in, m_scale));
}

mlVector3D mlTransform::
TransformVectorInverse(const mlVector3D &in) const
{
	UpdateMatrixIfNecessary();

    mlVector3D inverseScaleSquared = InvertScaleVector(ApplyScaleVector(m_scale, m_scale));
    return ApplyScaleVector(m_matrix.TransformVectorByTranspose(in), inverseScaleSquared);
}

void mlTransform::
TransformVectors(mlVector3D outArray[], const mlVector3D inArray[], int vectorCount) const
{
	UpdateMatrixIfNecessary();
	m_matrix.TransformVectors(outArray, inArray, vectorCount);
}

void mlTransform::
TransformVectorsInverse(mlVector3D outArray[], const mlVector3D inArray[], int vectorCount) const
{
	UpdateMatrixIfNecessary();
	
	mlMatrix3x4 inverseMatrix;
	mlMatrixTranspose(inverseMatrix, m_matrix);
	
    mlVector3D inverseScaleSquared = InvertScaleVector(ApplyScaleVector(m_scale, m_scale));
    inverseMatrix.I *= inverseScaleSquared.x;
    inverseMatrix.J *= inverseScaleSquared.y;
    inverseMatrix.K *= inverseScaleSquared.z;

	inverseMatrix.TransformVectors(outArray, inArray, vectorCount);
}

void mlTransform::
TransformPoints(mlVector3D outArray[], const mlVector3D inArray[], int pointCount) const
{
	UpdateMatrixIfNecessary();
	m_matrix.TransformPoints(outArray, inArray, pointCount);
}

void mlTransform::
TransformPointsInverse(mlVector3D outArray[], const mlVector3D inArray[], int pointCount) const
{
	UpdateMatrixIfNecessary();
	
	mlMatrix3x4 inverseMatrix;
	mlMatrixTranspose(inverseMatrix, m_matrix);
	
    mlVector3D inverseScaleSquared = InvertScaleVector(ApplyScaleVector(m_scale, m_scale));
    inverseMatrix.I *= inverseScaleSquared.x;
    inverseMatrix.J *= inverseScaleSquared.y;
    inverseMatrix.K *= inverseScaleSquared.z;
    inverseMatrix.T = ApplyScaleVector(inverseMatrix.T, inverseScaleSquared);
	
	inverseMatrix.TransformPoints(outArray, inArray, pointCount);
}

void mlTransform::UpdateMatrixAlways() const
{
	m_matrix.SetRotation(m_rotation);
	m_matrix.SetTranslation(m_translation);
    m_matrix.ApplyScale(m_scale);

	m_matrix_valid = true;
}

mlVector3D mlTransform::GetTranslation() const
{
	return m_translation;
}

mlQuaternion mlTransform::GetRotation() const
{
	return m_rotation;
}

mlVector3D mlTransform::GetScale() const
{
	return m_scale;
}

void mlTransform::SetTranslation(const mlVector3D & translation)
{
	m_translation = translation;
	m_matrix_valid = false;
}

void mlTransform::SetRotation(const mlQuaternion & rotation)
{
	m_rotation = rotation;
	m_matrix_valid = false;
}

void mlTransform::SetScale(const mlVector3D &scale)
{
	m_scale = scale;
    m_matrix_valid = false;
}

void mlTransform::SetScale(float scale)
{
    m_scale.Set(scale,scale,scale);
    m_matrix_valid = false;
}

void mlTransform::ApplyTranslation(const mlVector3D & translation)
{
	m_translation += translation;
	m_matrix_valid = false;
}

void mlTransform::ApplyRotation(const mlQuaternion & rotation)
{
	m_rotation = rotation * m_rotation;
	m_matrix_valid = false;
}

void mlTransform::ApplyScale(mlFloat scale)
{
	m_scale *= scale;
	m_matrix_valid = false;
}

void mlTransform::ApplyScale(const mlVector3D & scale)
{
    m_scale = ApplyScaleVector(m_scale, scale);
    m_matrix_valid = false;
}

void mlTransform::SetMatrix(const mlMatrix3x4 & matrix)
{	
	mlMatrix3x3 rotationMatrix(matrix.I, matrix.J, matrix.K);
	
	rotationMatrix.I.Normalise();
	rotationMatrix.J.Normalise();
	rotationMatrix.K.Normalise();
	 
	m_rotation = mlQuaternionFromRotationMatrix(rotationMatrix);
	m_translation = matrix.T;
    m_scale = matrix.GetScale();

	m_matrix_valid = false;
}

void mlTransform::Normalise()
{
	m_rotation.Normalise();
}

void mlCombineTransform(mlTransform &AtoC, const mlTransform &AtoB, const mlTransform &BtoC)
{
	if (&BtoC==&AtoC) {
		mlTransform result(AtoB);
		result.TransformSelf(BtoC);
		AtoC = result;
	} else {

		if (&AtoB!=&AtoC)
			AtoC = AtoB;
			
		AtoC.TransformSelf(BtoC);
	}
}

void mlCombineTransformInverse(mlTransform &AtoC, const mlTransform &AtoB, const mlTransform &CtoB)
{
	mlTransform BtoC = CtoB;
	BtoC.Invert();
	
	mlCombineTransform(AtoC, AtoB, BtoC);
}

mlTransform	mlTransformUtility::TransformFromReflection(const mlVector3D & pointOnPlane, const mlVector3D & planeNormalNormalised)
{
	mlTransform pointToOrigin;
	mlTransform originToPoint;
	originToPoint.SetTranslation(pointOnPlane);
	pointToOrigin.SetTranslation(-pointOnPlane);
	
	mlQuaternion rotation;
	rotation.x = planeNormalNormalised.x;
	rotation.y = planeNormalNormalised.y;
	rotation.z = planeNormalNormalised.z;
	rotation.w = 0.0f;
	
	mlTransform rotationScale(rotation, mlVector3DZero, -1.0f);
	
	mlTransform result = pointToOrigin;
	result.TransformSelf(rotationScale);
	result.TransformSelf(originToPoint);
	
	return result;
}

void mlTransformUtility::TransformFromPointForwardUp(mlTransform & trnTransform, const mlVector3D & vPosition, const mlVector3D & vForward, const mlVector3D & vUp)
{
	// Translation

	trnTransform.SetTranslation(vPosition);

	// Rotation

	mlVector3D vForwardNormalised = vForward;
	vForwardNormalised.Normalise();
	mlVector3D vUpNormalised = vUp;
	vUpNormalised.Normalise();

	mlQuaternion rotLookAt = mlQuaternionFromDirection(vForwardNormalised, vUpNormalised);
	trnTransform.SetRotation(rotLookAt);
}

