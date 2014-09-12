
#ifndef ML_VECTOR_H
#define ML_VECTOR_H

#include "ML_Types.h"
#include "ML_Maths.h"

class mlVector3D;

class mlVector2D
{
public:
	mlFloat		x;
	mlFloat		y;

	mlVector2D	() { x = 0.0f; y = 0.0f; }
	mlVector2D	(mlFloat xv, mlFloat yv);

	mlVector2D  (const mlVector3D & vectorIn);

	void		SetZero();
	void		Set(mlFloat xv, mlFloat yv);
	void		Normalise();

	mlVector2D	Normalised();

	mlFloat		Magnitude()	const;
	mlFloat		MagnitudeSquared() const;

	mlVector2D	OrthogonalA();
	mlVector2D	OrthogonalB();

	mlVector2D	operator -  (const mlVector2D &v) const;
	mlVector2D	operator +  (const mlVector2D &v) const;
	void		operator += (const mlVector2D &v);
	void		operator -= (const mlVector2D &v);
	void		operator *= (mlFloat s);

	bool		operator == (const mlVector2D &other) const;
	bool		operator != (const mlVector2D &other) const;

	mlFloat&	operator[](int index);
	const mlFloat&	operator[](int index) const;
};

inline mlVector2D::mlVector2D(mlFloat xv, mlFloat yv) : x(xv), y(yv)	{}

inline bool mlVector2D::operator==(const mlVector2D &other) const
{
	return (x == other.x) && (y == other.y);
}

inline bool mlVector2D::operator!=(const mlVector2D &other) const
{
	return (x != other.x) || (y != other.y);
}

inline mlFloat&	mlVector2D::operator[](int index)
{
	return reinterpret_cast<mlFloat*>(this)[index];
}

inline const mlFloat& mlVector2D::operator[](int index) const
{
	return reinterpret_cast<const mlFloat*>(this)[index];
}

mlFloat		operator * (const mlVector2D &a, const mlVector2D &b);
mlVector2D	operator * (const mlVector2D &v, mlFloat s);
mlVector2D	operator * (mlFloat s, const mlVector2D &v);

mlFloat		mlVectorCross2D(const mlVector2D &a, const mlVector2D &b);
mlVector2D	mlInterpolate(mlFloat t, const mlVector2D &a, const mlVector2D &b);

extern const mlVector2D mlVector2DX;
extern const mlVector2D mlVector2DY;

extern const mlVector2D mlVector2DZero;

class mlVector3D
{
public:

	mlFloat		x;
	mlFloat 	y;
	mlFloat		z;

	mlVector3D	() { x = 0.0f; y = 0.0f; z = 0.0f; }
	mlVector3D	(mlFloat xv, mlFloat yv, mlFloat zv);

    mlVector3D	(mlFloat xv, mlFloat yv);
    mlVector3D  (const mlVector2D & vectorIn);

	void		SetZero();
	void		Set(mlFloat xv, mlFloat yv, mlFloat zv);
	void		Normalise();

	mlVector3D	Normalised() const;
	
	mlVector3D	MaskX(mlFloat vX) const;
	mlVector3D	MaskY(mlFloat vY) const;
	mlVector3D	MaskZ(mlFloat vZ) const;

	mlFloat		Magnitude()	const;
	mlFloat		MagnitudeSquared() const;

    mlFloat     AngleToVector (const mlVector3D & v) const;

    mlVector3D  Rotate(const mlVector3D &p1, const mlVector3D &p2, mlFloat theta) const;

	mlVector3D	operator -	() const;
	mlVector3D	operator -  (const mlVector3D &v) const;
	mlVector3D	operator +  (const mlVector3D &v) const;
	void		operator += (const mlVector3D &v);
	void		operator -= (const mlVector3D &v);
	void		operator *= (mlFloat s);

	bool		operator == (const mlVector3D &other) const;
	bool		operator != (const mlVector3D &other) const;

	mlFloat &		operator[](int index);
	const mlFloat &	operator[](int index) const;

};

inline mlVector3D	mlVector3D::MaskX(mlFloat vX) const
{
	return mlVector3D(vX, y, z);
}

inline mlVector3D	mlVector3D::MaskY(mlFloat vY) const
{
	return mlVector3D(x, vY, z);
}

inline mlVector3D	mlVector3D::MaskZ(mlFloat vZ) const
{
	return mlVector3D(x, y, vZ);
}

inline mlVector3D::mlVector3D(mlFloat xv, mlFloat yv) : x(xv), y(yv), z(0.0f)
{
}

inline mlVector3D::mlVector3D(mlFloat xv, mlFloat yv, mlFloat zv) : x(xv), y(yv), z(zv)
{
}

inline bool	mlVector3D::operator==(const mlVector3D &other) const
{
	return (x == other.x) && (y == other.y) && (z == other.z);
}

inline bool	mlVector3D::operator!=(const mlVector3D &other) const
{
	return (x != other.x) || (y != other.y) || (z != other.z);
}

inline mlFloat&	mlVector3D::operator[](int index)
{
	return reinterpret_cast<mlFloat*>(this)[index];
}

inline const mlFloat&	mlVector3D::operator[](int index) const
{
	return reinterpret_cast<const mlFloat*>(this)[index];
}

mlFloat		operator * (const mlVector3D &a, const mlVector3D &b);
mlVector3D	operator * (const mlVector3D &v, mlFloat s);
mlVector3D	operator * (mlFloat s, const mlVector3D &v);

mlVector3D mlVectorScale(const mlVector3D &a, const mlVector3D &b);

mlFloat mlVectorDot(const mlVector3D &a, const mlVector3D &b);

mlVector3D	mlVectorCross(const mlVector3D &a, const mlVector3D &b);

mlVector3D	mlInterpolate(mlFloat t, const mlVector3D &a, const mlVector3D &b);

mlFloat		mlDistanceBetween(const mlVector3D & a, const mlVector3D & b);

extern const mlVector3D mlVector3DX;
extern const mlVector3D mlVector3DY;
extern const mlVector3D mlVector3DZ;

extern const mlVector3D mlVector3DZero;

inline mlVector2D::mlVector2D  (const mlVector3D & vectorIn) : x(vectorIn.x), y(vectorIn.y) {}
inline mlVector3D::mlVector3D  (const mlVector2D & vectorIn) : x(vectorIn.x), y(vectorIn.y), z(0.0f) {}

class mlVector4D
{
public:
	mlFloat	x;
	mlFloat y;
	mlFloat	z;
	mlFloat	w;
	
	mlVector4D	() { x = 0.0f; y = 0.0f; z = 0.0f; w = 0.0f; }
	mlVector4D	(mlFloat xv, mlFloat yv, mlFloat zv, mlFloat wv);
	mlVector4D	(const mlVector3D & v, mlFloat w);
	
	void			SetZero();
	void			Normalise();
	
	mlFloat			Magnitude()	const;
	mlFloat			MagnitudeSquared() const;
	
	void			operator *= (mlFloat s);
	mlVector4D		operator -  (const mlVector4D &v) const;
	mlVector4D		operator +  (const mlVector4D &v) const;
	void			operator += (const mlVector4D &v);
	
	mlFloat &		operator[](int index);
	const mlFloat &	operator[](int index) const;

};

inline mlVector4D::mlVector4D(mlFloat xv, mlFloat yv, mlFloat zv, mlFloat wv) : x(xv), y(yv), z(zv), w(wv)	{}
inline mlVector4D::mlVector4D(const mlVector3D & v, mlFloat w) : x(v.x), y(v.y), z(v.z), w(w) 			{}

inline mlFloat&	mlVector4D::operator[](int index)
{
	return reinterpret_cast<mlFloat*>(this)[index];
}

inline const mlFloat&	mlVector4D::operator[](int index) const
{
	return reinterpret_cast<const mlFloat*>(this)[index];
}

mlFloat		operator * (const mlVector4D &a, const mlVector4D &b);
mlVector4D	operator * (const mlVector4D &v, mlFloat s);
mlVector4D	operator * (mlFloat s, const mlVector4D &v);

mlVector4D	mlInterpolate(mlFloat t, const mlVector4D &a, const mlVector4D &b);

extern const mlVector4D mlVector4DZero;

inline bool							mlEquivalent(const mlVector2D & a, const mlVector2D & b, mlFloat tolerance)
{
	mlFloat deltax = a.x - b.x;
	mlFloat deltay = a.y - b.y;

	return	mlFabs(deltax) < tolerance &&
			mlFabs(deltay) < tolerance;
}

inline bool							mlEquivalent(const mlVector3D & a, const mlVector3D & b, mlFloat tolerance)
{
	mlFloat deltax = a.x - b.x;
	mlFloat deltay = a.y - b.y;
	mlFloat deltaz = a.z - b.z;

	return	mlFabs(deltax) < tolerance &&
			mlFabs(deltay) < tolerance &&
			mlFabs(deltaz) < tolerance;
}

inline bool							mlEquivalent(const mlVector4D & a, const mlVector4D & b, mlFloat tolerance)
{
	mlFloat deltax = a.x - b.x;
	mlFloat deltay = a.y - b.y;
	mlFloat deltaz = a.z - b.z;
	mlFloat deltaw = a.w - b.w;

	return	mlFabs(deltax) < tolerance &&
			mlFabs(deltay) < tolerance &&
			mlFabs(deltaz) < tolerance &&
			mlFabs(deltaw) < tolerance;
}

inline bool mlIsValid(const mlVector2D & v)
{
	return	mlIsValid(v.x) &&
			mlIsValid(v.y);
}

inline bool mlIsValid(const mlVector3D & v)
{
	return	mlIsValid(v.x) &&
			mlIsValid(v.y) &&
			mlIsValid(v.z);
}

inline bool mlIsValid(const mlVector4D & v)
{
	return	mlIsValid(v.x) &&
			mlIsValid(v.y) &&
			mlIsValid(v.z) &&
			mlIsValid(v.w);
}

inline mlVector3D ApplyScaleVector(const mlVector3D & a, const mlVector3D & b)
{
    return mlVector3D(a.x * b.x,
                      a.y * b.y,
                      a.z * b.z);
}

inline mlVector3D InvertScaleVector(const mlVector3D & scale)
{
    return mlVector3D(1.0f / scale.x,
                      1.0f / scale.y,
                      1.0f / scale.z);
}

#endif // ML_VECTOR_H
