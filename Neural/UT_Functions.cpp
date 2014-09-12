#include "UT_Functions.h"

#include <vector>
#include <algorithm>

mlFloat utSigmoid(mlFloat x, mlFloat k, mlFloat bias)
{
	return 1 / (1 + mlExp(-1 * k * (x - bias)));
}

mlFloat utSigmoidDerivative(mlFloat x, mlFloat k, mlFloat bias)
{
	float e_kx = mlExp(-1 * k * (x - bias));
	return (k * e_kx)/((e_kx + 1) * (e_kx + 1));
}

mlFloat utLatticeMagnitude(const mlVector2D & line)
{
	mlVector2D distanceVector = line;

	distanceVector.x = mlFabs(distanceVector.x);
	distanceVector.y = mlFabs(distanceVector.y);

	mlFloat latticeDistance;

	if(distanceVector.x < distanceVector.y)
		latticeDistance = distanceVector.x * mlSqrtTwo + (distanceVector.y - distanceVector.x);
	else
		latticeDistance = distanceVector.y * mlSqrtTwo + (distanceVector.x - distanceVector.y);

	return latticeDistance;
}

mlFloat utLatticeMagnitude(const mlVector3D & line)
{
	mlVector2D distanceVector = line;

	distanceVector.x = mlFabs(distanceVector.x);
	distanceVector.y = mlFabs(distanceVector.y);

	mlFloat latticeDistance2D;

	if(distanceVector.x < distanceVector.y)
		latticeDistance2D = distanceVector.x * mlSqrtTwo + (distanceVector.y - distanceVector.x);
	else
		latticeDistance2D = distanceVector.y * mlSqrtTwo + (distanceVector.x - distanceVector.y);

	std::vector<mlFloat> dimensions;

	dimensions.push_back(mlFabs(line.x));
	dimensions.push_back(mlFabs(line.y));
	dimensions.push_back(mlFabs(line.z));

	std::sort(dimensions.begin(), dimensions.end());

	mlFloat d0 = dimensions[0];
	mlFloat d1 = dimensions[1];
	mlFloat d2 = dimensions[2];

	mlFloat latticeDistance3D = 
		(dimensions[0] * mlSqrt(3.0f)) +
		((dimensions[1] - dimensions[0]) * mlSqrt(2.0f)) +
		(dimensions[2] - dimensions[1]);

	return latticeDistance3D;
}

mlFloat utRand(mlFloat min, mlFloat max, int granularity)
{
	int randomInt = rand() % granularity;

	mlFloat range = max - min;

	return ((range / mlFloat(granularity)) * mlFloat(randomInt)) + min;
}
