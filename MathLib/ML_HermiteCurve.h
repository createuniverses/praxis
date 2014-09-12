// UT_HermiteCurve.h
// Author: Greg Santucci

#ifndef UT_HERMITECURVE_H
#define UT_HERMITECURVE_H

#include "ML_Vector.h"
#include "ML_Quaternion.h"

#include <list>
#include <vector>
#include <map>
#include <string>

#include <iostream>
using namespace std;

class utHermiteCurvePoint
{
public:

	utHermiteCurvePoint()
		: m_point(mlVector3DZero), m_tangent(mlVector3DZero) {}

	utHermiteCurvePoint(const mlVector3D & point)
		: m_point(point), m_tangent(mlVector3DZero) {}

	utHermiteCurvePoint(const mlVector3D & point, const mlVector3D & tangent)
		: m_point(point), m_tangent(tangent) {}

	utHermiteCurvePoint(utHermiteCurvePoint * curvePoint)
		: m_point(curvePoint->m_point), m_tangent(curvePoint->m_tangent) {}

	virtual ~utHermiteCurvePoint() {}

	void operator=(const utHermiteCurvePoint & rightCurvePoint)
	{
		m_point = rightCurvePoint.m_point;
		m_tangent = rightCurvePoint.m_tangent;
	}

	mlVector3D		RealTangentPosition(void) { return m_point + m_tangent; }

	mlVector3D		m_point;
	mlVector3D		m_tangent;
};

class utHermiteCurve
{
public:
	utHermiteCurve();
	virtual ~utHermiteCurve();

	utHermiteCurve *		Clone();
	void					Clear();

    void					Render(int segments);
	void					Serialise(istream & stream);
	void					Serialise(ostream & stream);

	mlVector3D				CalculatePoint(mlFloat t);
	mlVector3D				CalculateVelocity(mlFloat t);
	mlVector3D				CalculateAcceleration(mlFloat t);

	mlQuaternion			CalculateOrientation(mlFloat t);

	mlFloat					GetSplineTimeLength();

	mlVector3D				GetStartPoint();
	mlVector3D				GetEndPoint();

	mlFloat					CalculateSplineTime(const mlVector3D & point, int numSegments);
	mlFloat					CalculateSplineTime(const mlVector3D & point, mlFloat tPrecision, mlFloat startFrom);

	// ************************************************

	typedef std::vector<utHermiteCurvePoint *> OrderedCurvePointSet;

	OrderedCurvePointSet	m_curvePointVector;
};

#endif // #define UT_HERMITECURVE_H
