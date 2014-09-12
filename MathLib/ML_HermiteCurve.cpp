// UT_HermiteCurve.cpp
// Author: Greg Santucci

#include "ML_HermiteCurve.h"

#include "ML_Maths.h"

#include "GL/openglut.h"

// *****************************************************************************

utHermiteCurve::utHermiteCurve()
{
}

utHermiteCurve::~utHermiteCurve()
{
	Clear();
}

void utHermiteCurve::Render(int segments)
{
	if(!(segments > 0))
	{
        //TRACE("utHermiteCurve::Render() - Bad segments argument\n");
		return;
	}

	mlFloat stepSize = 1.0f / mlFloat(segments);

	mlFloat splineTime = 0.0f;
	mlVector3D currentPoint = CalculatePoint(splineTime);
	splineTime += stepSize;

    glBegin(GL_LINES);

	for(; splineTime <= GetSplineTimeLength(); splineTime += stepSize)
	{
		mlVector3D start = currentPoint;
		mlVector3D end = CalculatePoint(splineTime);

		glColor4ub(
            0,
            255,
            0,
            255);

		glVertex3f(
			start.x,
			start.y,
			start.z);
		glVertex3f(
			end.x,
			end.y,
			end.z);

		currentPoint = end;
	}

	glEnd();

	glBegin(GL_LINES);

	for(OrderedCurvePointSet::iterator curvePoint_i = m_curvePointVector.begin();
		curvePoint_i != m_curvePointVector.end();
		++curvePoint_i)
		{
			utHermiteCurvePoint * curvePoint = (*curvePoint_i);

			glColor4ub(
                255,
                0,
                0,
                255);

			glVertex3f(
				curvePoint->m_point.x,
				curvePoint->m_point.y,
				curvePoint->m_point.z);
			glVertex3f(
				curvePoint->RealTangentPosition().x,
				curvePoint->RealTangentPosition().y,
				curvePoint->RealTangentPosition().z);
		}

	glEnd();
}

mlVector3D utHermiteCurve::CalculatePoint(mlFloat t)
{
	if(GetSplineTimeLength() <= 0.0f)
		return mlVector3DZero;

	mlFloat fixT = static_cast<mlFloat>(mlFabs(t));

	unsigned int index = int(fixT);
	mlFloat p = fixT - int(fixT);

	index = index % int(GetSplineTimeLength());

	utHermiteCurvePoint * point1 = m_curvePointVector[index];
	utHermiteCurvePoint * point2 = m_curvePointVector[index + 1];

	mlVector3D point =
		(2 * p * p * p - 3 * p * p + 1) * point1->m_point +
		(-2 * p * p * p + 3 * p * p) * point2->m_point +
		(p * p * p - 2 * p * p + p) * point1->m_tangent +
		(p * p * p - p * p) * point2->m_tangent;

	return point;
}

mlVector3D utHermiteCurve::CalculateVelocity(mlFloat t)
{
	mlFloat delta = 0.01f;

	mlVector3D point1 = CalculatePoint(t);
	mlVector3D point2 = CalculatePoint(t + delta);

	mlVector3D velocity = point2 - point1;

	return velocity;
}

mlVector3D utHermiteCurve::CalculateAcceleration(mlFloat t)
{
	mlFloat delta = 0.01f;

	mlVector3D point1 = CalculateVelocity(t);
	mlVector3D point2 = CalculateVelocity(t + delta);

	mlVector3D acceleration = point2 - point1;

	return acceleration;
}

mlQuaternion utHermiteCurve::CalculateOrientation(mlFloat t)
{
	mlVector3D forwardVector = CalculateVelocity(t);

	forwardVector.Normalise();

    mlQuaternion quaternion = mlQuaternionFromDirection(forwardVector, mlVector3D(0.0f, 1.0f, 0.0f));

	return quaternion;
}

mlFloat utHermiteCurve::CalculateSplineTime(const mlVector3D & point, int numSegments)
{
	mlFloat precision = 1.0f / mlFloat(numSegments);

	mlFloat bestDistance	= 999999.9f;
	mlFloat bestSplineTime	= 0.0f;

	for(mlFloat t = 0.0f; t <= GetSplineTimeLength(); t += precision)
	{
		mlVector3D splinePoint	= CalculatePoint(t);
		mlVector3D line			= splinePoint - point;
		mlFloat distance = line.Magnitude();

		if(distance < bestDistance)
		{
			bestDistance	= distance;
			bestSplineTime	= t;
		}
	}

	return bestSplineTime;
}

mlFloat utHermiteCurve::CalculateSplineTime(const mlVector3D & point, mlFloat tPrecision, mlFloat startFrom)
{
	mlFloat bestDistance	= 999999.9f;
	mlFloat bestSplineTime	= 0.0f;

	{
		mlFloat t = startFrom;
		mlFloat previousDistance = (CalculatePoint(t) - point).Magnitude();

		while(t <= GetSplineTimeLength())
		{
			t += tPrecision;

			mlVector3D	newSplinePoint	= CalculatePoint(t);
			mlVector3D	newline			= newSplinePoint - point;
			mlFloat		newDistance		= newline.Magnitude();

			if(previousDistance < bestDistance)
			{
				bestDistance	= newDistance;
				bestSplineTime	= t;
			}

			if(newDistance > previousDistance)
				break;

			previousDistance = newDistance;
		}
	}

	{
		mlFloat t = startFrom;
		mlFloat previousDistance = (CalculatePoint(t) - point).Magnitude();

		while(t >= 0.0f)
		{
			t -= tPrecision;

			mlVector3D	newSplinePoint	= CalculatePoint(t);
			mlVector3D	newline			= newSplinePoint - point;
			mlFloat		newDistance		= newline.Magnitude();

			if(newDistance < bestDistance)
			{
				bestDistance	= newDistance;
				bestSplineTime	= t;
			}

			if(newDistance > previousDistance)
				break;

			previousDistance = newDistance;
		}
	}

	return bestSplineTime;
}


mlFloat utHermiteCurve::GetSplineTimeLength()
{
	return mlFloat(m_curvePointVector.size() - 1.0f);
}

utHermiteCurve * utHermiteCurve::Clone(void)
{
	utHermiteCurve * newCurve = new utHermiteCurve;

	for(OrderedCurvePointSet::iterator curvePoint_i = m_curvePointVector.begin();
		curvePoint_i != m_curvePointVector.end();
		++curvePoint_i)
		{
			utHermiteCurvePoint * curvePoint = (*curvePoint_i);

			utHermiteCurvePoint * newCurvePoint = new utHermiteCurvePoint(curvePoint);

			newCurve->m_curvePointVector.push_back(newCurvePoint);
		}

	return newCurve;
}

void utHermiteCurve::Clear()
{
	for(OrderedCurvePointSet::iterator curvePoint_i = m_curvePointVector.begin();
		curvePoint_i != m_curvePointVector.end();
		++curvePoint_i)
		{
			utHermiteCurvePoint * curvePoint = (*curvePoint_i);

			delete curvePoint; curvePoint = NULL;
		}

	m_curvePointVector.clear();
}

mlVector3D utHermiteCurve::GetStartPoint()
{
	return CalculatePoint(0.0f);
}

mlVector3D utHermiteCurve::GetEndPoint()
{
	mlFloat endLength = GetSplineTimeLength();
	return CalculatePoint(endLength - 0.001f);
}

void	utHermiteCurve::Serialise(istream & stream)
{
    //utVerifyTokenInStream(stream, '\n', "Curve");
    //utVerifyTokenInStream(stream, ' ', "NumberOfNodes");

	int nNodes; stream >> nNodes;

	{for(int iNode = 0; iNode < nNodes; iNode++)
	{
		utHermiteCurvePoint * node = new utHermiteCurvePoint;

		m_curvePointVector.push_back(node);

        //utVerifyTokenInStream(stream, '\n', "Node");

        //utVerifyTokenInStream(stream, ' ', "Point");
		stream >> node->m_point.x;
		stream >> node->m_point.y;
		stream >> node->m_point.z;

        //utVerifyTokenInStream(stream, ' ', "Tangent");
		stream >> node->m_tangent.x;
		stream >> node->m_tangent.y;
		stream >> node->m_tangent.z;

        //utVerifyTokenInStream(stream, '\n', "NodeEnd");
	}}
    //utVerifyTokenInStream(stream, '\n', "CurveEnd");
}

void	utHermiteCurve::Serialise(ostream & stream)
{
	stream << "Curve\n";
	stream << "NumberOfNodes " << m_curvePointVector.size() << "\n";

	for(int iNode = 0; iNode < m_curvePointVector.size(); iNode++)
	{
		utHermiteCurvePoint * node = m_curvePointVector[iNode];

		stream << "Node\n";

		stream << "Point " << node->m_point.x << " " << node->m_point.y << " " << node->m_point.z << "\n";
		stream << "Tangent " << node->m_tangent.x << " " << node->m_tangent.y << " " << node->m_tangent.z << "\n";

		stream << "NodeEnd\n";
	}
	stream << "CurveEnd\n";
}
