
#include "ML_Vector.h"

#include "ML_Maths.h"

extern const mlVector2D mlVector2DX(1.0f, 0.0f);
extern const mlVector2D mlVector2DY(0.0f, 1.0f);

extern const mlVector2D mlVector2DZero(0.0f, 0.0f);

void		mlVector2D::SetZero()						{ x = y = 0.0f; }
void		mlVector2D::Set(mlFloat xv, mlFloat yv)		{ x = xv; y = yv; }

mlFloat		mlVector2D::Magnitude()	const				{ return mlSqrt(x * x + y * y); }
mlFloat		mlVector2D::MagnitudeSquared()	const		{ return x * x + y * y; }

void		mlVector2D::Normalise()
{ 
	mlFloat mag = Magnitude();
	
	if(mag != 0.0f)
	{
		mlFloat invMag = 1.0f / mag;
		x *= invMag;
		y *= invMag;
	}
}

mlVector2D	mlVector2D::Normalised()
{
	mlVector2D normalisedVector(x,y);
	
	mlFloat mag = normalisedVector.Magnitude();
	
	if(mag != 0.0f)
	{
		mlFloat invMag = 1.0f / mag;
		normalisedVector.x *= invMag;
		normalisedVector.y *= invMag;
	}
	
	return normalisedVector;
}

mlVector2D
mlVector2D::OrthogonalA()
{
	return mlVector2D(y, -x);
}

mlVector2D
mlVector2D::OrthogonalB()
{
	return mlVector2D(-y, x);
}

mlVector2D	mlVector2D::operator +  (const mlVector2D &v) const	{ return mlVector2D(x + v.x, y + v.y); }
mlVector2D	mlVector2D::operator -  (const mlVector2D &v) const	{ return mlVector2D(x - v.x, y - v.y); }
void		mlVector2D::operator += (const mlVector2D &v)		{ x += v.x; y += v.y; }
void		mlVector2D::operator -= (const mlVector2D &v)		{ x -= v.x; y -= v.y; }
void		mlVector2D::operator *= (mlFloat s)			{ x *= s; y *= s; }

mlVector2D	operator * (const mlVector2D &v, mlFloat s)		{ return mlVector2D(v.x * s, v.y * s); }
mlVector2D	operator * (mlFloat s, const mlVector2D &v)		{ return mlVector2D(v.x * s, v.y * s); }

mlFloat		operator * (const mlVector2D &a, const mlVector2D &b)
{
	return a.x * b.x + a.y * b.y;
}

mlFloat		mlVectorCross2D(const mlVector2D &a, const mlVector2D &b)
{
	return a.x * b.y - a.y * b.x;
}

mlVector2D	mlInterpolate(mlFloat t, const mlVector2D &a, const mlVector2D &b)
{
	return mlVector2D(a.x * (1.0f - t) + b.x * t, a.y * (1.0f - t) + b.y * t);
}

extern const mlVector3D mlVector3DX(1.0f, 0.0f, 0.0f);
extern const mlVector3D mlVector3DY(0.0f, 1.0f, 0.0f);
extern const mlVector3D mlVector3DZ(0.0f, 0.0f, 1.0f);

extern const mlVector3D mlVector3DZero(0.0f, 0.0f, 0.0f);

void		mlVector3D::SetZero()					{ x = y = z = 0.0f; }
void		mlVector3D::Set(mlFloat xv, mlFloat yv, mlFloat zv)	{ x = xv; y = yv; z = zv; }

mlFloat		mlVector3D::Magnitude()	const				{ return mlSqrt(x * x + y * y + z * z); }
mlFloat		mlVector3D::MagnitudeSquared()	const			{ return x * x + y * y + z * z; }

void		mlVector3D::Normalise()
{
	mlFloat maxValue = 0.0f;

	if(mlFabs(x) > maxValue) maxValue = mlFabs(x);
	if(mlFabs(y) > maxValue) maxValue = mlFabs(y);
	if(mlFabs(z) > maxValue) maxValue = mlFabs(z);
	
	if(maxValue == 0.0f)
		return;
	
	x = x / maxValue;
	y = y / maxValue;
	z = z / maxValue;

	mlFloat mag = Magnitude();
	
	if(mag == 0.0f)
		return;

	mlFloat invMag = 1.0f / mag;
	x *= invMag;
	y *= invMag;
	z *= invMag;
}

mlVector3D	mlVector3D::Normalised() const
{
	mlVector3D normalisedVector(x,y,z);
	
	mlFloat maxValue = 0.0f;
	
	if(mlFabs(normalisedVector.x) > maxValue) maxValue = mlFabs(normalisedVector.x);
	if(mlFabs(normalisedVector.y) > maxValue) maxValue = mlFabs(normalisedVector.y);
	if(mlFabs(normalisedVector.z) > maxValue) maxValue = mlFabs(normalisedVector.z);
	
	if(maxValue == 0.0f)
		return mlVector3D(x,y,z);
		
	normalisedVector.x = normalisedVector.x / maxValue;
	normalisedVector.y = normalisedVector.y / maxValue;
	normalisedVector.z = normalisedVector.z / maxValue;
	
	mlFloat mag = normalisedVector.Magnitude();
	
	if(mag == 0.0f)
		return mlVector3D(x,y,z);
	
	mlFloat invMag = 1.0f / mag;
	normalisedVector.x *= invMag;
	normalisedVector.y *= invMag;
	normalisedVector.z *= invMag;
	
	return normalisedVector;
}

mlFloat mlVector3D::AngleToVector(const mlVector3D &v) const
{
    mlVector3D normalized = Normalised();
    mlVector3D vNormalized = v.Normalised();
    mlFloat fDot = normalized * vNormalized;
    mlFloat fAngle = mlArcCos(fDot);

    return fAngle;
}


mlVector3D	mlVector3D::operator +  (const mlVector3D &v) const	{ return mlVector3D(x + v.x, y + v.y, z + v.z); }
mlVector3D	mlVector3D::operator -  (const mlVector3D &v) const	{ return mlVector3D(x - v.x, y - v.y, z - v.z); }
void		mlVector3D::operator += (const mlVector3D &v)		{ x += v.x; y += v.y; z += v.z; }
void		mlVector3D::operator -= (const mlVector3D &v)		{ x -= v.x; y -= v.y; z -= v.z; }
void		mlVector3D::operator *= (mlFloat s)			{ x *= s; y *= s; z *= s; }
mlVector3D	mlVector3D::operator -	() const			{ return mlVector3D(-x, -y, -z);}

mlFloat		operator * (const mlVector3D &a, const mlVector3D &b)
{
	return a.x * b.x + a.y * b.y + a.z * b.z;
}

mlFloat mlVectorDot(const mlVector3D &a, const mlVector3D &b)
{
	return a.x * b.x + a.y * b.y + a.z * b.z;
}

mlVector3D	operator * (const mlVector3D &v, mlFloat s)			{ return mlVector3D(v.x * s, v.y * s, v.z * s); }
mlVector3D	operator * (mlFloat s, const mlVector3D &v)			{ return mlVector3D(v.x * s, v.y * s, v.z * s); }

mlVector3D	mlVectorCross(const mlVector3D &a, const mlVector3D &b)
{
	return mlVector3D(
				a.y * b.z - a.z * b.y,
				a.z * b.x - a.x * b.z,
				a.x * b.y - a.y * b.x
			);
}

mlVector3D mlVectorScale(const mlVector3D &a, const mlVector3D &b)
{
	return mlVector3D(
		a.x * b.x,
		a.y * b.y,
		a.z * b.z);
}

mlVector3D	mlInterpolate(mlFloat t, const mlVector3D &a, const mlVector3D &b)
{
	return a * (1.0f - t) + b * t;
}

mlFloat mlDistanceBetween(const mlVector3D & a, const mlVector3D & b)
{
	mlVector3D difference = a - b;
	return difference.Magnitude();
}

extern const mlVector4D mlVector4DZero(0.0f, 0.0f, 0.0f, 0.0f);

mlVector4D	mlVector4D::operator -  (const mlVector4D &v) const	{ return mlVector4D(x - v.x, y - v.y, z - v.z, w - v.w); }
mlVector4D	mlVector4D::operator +  (const mlVector4D &v) const	{ return mlVector4D(x + v.x, y + v.y, z + v.z, w + v.w); }
void		mlVector4D::operator += (const mlVector4D &v)		{ x += v.x; y += v.y; z += v.z; w += v.w; }
void		mlVector4D::operator *= (mlFloat s)			{ x *= s; y *= s; z *= s; w *= s; }
void		mlVector4D::SetZero()					{ x = y = z = w = 0.0f; }

mlFloat		mlVector4D::Magnitude()	const				{ return mlSqrt(x * x + y * y + z * z + w * w); }
mlFloat		mlVector4D::MagnitudeSquared()	const			{ return x * x + y * y + z * z + w * w; }

mlVector4D	operator * (const mlVector4D &v, mlFloat s)		{ return mlVector4D(v.x * s, v.y * s, v.z * s, v.w * s); }
mlVector4D	operator * (mlFloat s, const mlVector4D &v)		{ return mlVector4D(v.x * s, v.y * s, v.z * s, v.w * s); }

mlFloat		operator * (const mlVector4D &a, const mlVector4D &b)
{
	return a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w;
}

void mlVector4D::Normalise()
{
	mlFloat invMag = mlInvSqrt(x*x + y*y + z*z + w*w);
	x *= invMag;
	y *= invMag;
	z *= invMag;
	w *= invMag;
}

mlVector4D	mlInterpolate(mlFloat t, const mlVector4D &a, const mlVector4D &b)
{
	return a * (1.0f - t) + b * t;
}

mlVector3D mlVector3D::Rotate(const mlVector3D &p1, const mlVector3D &p2, mlFloat theta) const
{
    mlVector3D p = (*this);

    mlVector3D q,r;
    mlFloat costheta,sintheta;

    r.x = p2.x - p1.x;
    r.y = p2.y - p1.y;
    r.z = p2.z - p1.z;
    p.x -= p1.x;
    p.y -= p1.y;
    p.z -= p1.z;
    r = r.Normalised();

#ifdef VECTOR_USING_DOUBLE
    costheta = cos(theta);
    sintheta = sin(theta);
#else
    costheta = cosf(theta);
    sintheta = sinf(theta);
#endif

    q.x += (costheta + (1 - costheta) * r.x * r.x) * p.x;
    q.x += ((1 - costheta) * r.x * r.y - r.z * sintheta) * p.y;
    q.x += ((1 - costheta) * r.x * r.z + r.y * sintheta) * p.z;

    q.y += ((1 - costheta) * r.x * r.y + r.z * sintheta) * p.x;
    q.y += (costheta + (1 - costheta) * r.y * r.y) * p.y;
    q.y += ((1 - costheta) * r.y * r.z - r.x * sintheta) * p.z;

    q.z += ((1 - costheta) * r.x * r.z - r.y * sintheta) * p.x;
    q.z += ((1 - costheta) * r.y * r.z + r.x * sintheta) * p.y;
    q.z += (costheta + (1 - costheta) * r.z * r.z) * p.z;

    q.x += p1.x;
    q.y += p1.y;
    q.z += p1.z;

    return q;
}
