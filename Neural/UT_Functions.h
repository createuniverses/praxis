#ifndef UT_FUNCTIONS_H
#define UT_FUNCTIONS_H

#include "ML_Maths.h"
#include "ML_Vector.h"

mlFloat utSigmoid(mlFloat x, mlFloat k, mlFloat bias);
mlFloat utSigmoidDerivative(mlFloat x, mlFloat k, mlFloat bias);

mlFloat utRand(mlFloat min, mlFloat max, int granularity);

mlFloat utLatticeMagnitude(const mlVector2D & line);
mlFloat utLatticeMagnitude(const mlVector3D & line);

#endif // UT_FUNCTIONS_H
