/*!
    \file  og_geometry.c
    \brief Algorithmic geometric objects.
*/
/*
 * OpenGLUT geometry rendering methods.
 *
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
 * OpenGLUT branched from freeglut in February, 2004.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Fri Dec 3 1999
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * PAWEL W. OLSZTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <float.h>

#include <GL/openglut.h>
#include "og_internal.h"

/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Draw a wireframe cube centered at the origin.
    \ingroup  geometry
    \author   Code contributed by Andreas Umbach <marvin@dataway.ch>
    \param    width       The width, height and depth of the cube.

              The glutWireCube() function draws an axis-aligned wireframe cube
              with a specified width, height and depth. The vertices of
              the cube are at
              (+/- \a width/2, +/- \a width/2, +/- \a width/2),
              so that the cube is centered at the origin.

    \see      glutSolidCube()
*/
void OGAPIENTRY glutWireCube( GLdouble width )
{
    double size = width * 0.5;

#   define V(a,b,c) glVertex3d( a size, b size, c size );
#   define N(a,b,c) glNormal3d( a, b, c );
    glBegin( GL_LINE_LOOP );
        N( 0, 0, +1 ); V( +, +, + ); V( -, +, + ); V( -, -, + ); V( +, -, + );
    glEnd( );
    glBegin(GL_LINE_LOOP);
        N( 0, 0, -1 ); V( +, +, - ); V( -, +, - ); V( -, -, - ); V( +, -, - );
    glEnd( );

    glBegin( GL_LINES );
        N( +1,  0, 0 ); V( +, +, + ); V( +, +, - );
        N(  0, +1, 0 ); V( -, +, + ); V( -, +, - );
        N( -1,  0, 0 ); V( -, -, + ); V( -, -, - );
        N(  0, -1, 0 ); V( +, -, + ); V( +, -, - );
    glEnd( );
#   undef V
#   undef N
}

/*!
    \fn
    \brief    Draw a solid cube centered at the origin.
    \ingroup  geometry
    \param    width       The width, height and depth of the cube.

              The glutSolidCube() function draws a solid-shaded cube
              with side-length given by \a width.  The vertices of
              the cube are at
              (+/- \a width/2, +/- \a width/2, +/- \a width/2),
              so that the cube is centered at the origin.

    \author   Code contributed by Andreas Umbach <marvin@dataway.ch>
    \see      glutWireCube()
*/
void OGAPIENTRY glutSolidCube( GLdouble width )
{
    double size = width * 0.5;

#   define V(a,b,c) glVertex3d( a size, b size, c size );
#   define N(a,b,c) glNormal3d( a, b, c );
    /* PWO: Again, I dared to convert the code to use macros... */
    glBegin( GL_QUADS );
        N( 1, 0, 0 ); V( +, -, + ); V( +, -, - ); V( +, +, - ); V( +, +, + );
        N( 0, 1, 0 ); V( +, +, + ); V( +, +, - ); V( -, +, - ); V( -, +, + );
        N( 0, 0, 1 ); V( +, +, + ); V( -, +, + ); V( -, -, + ); V( +, -, + );
        N( -1, 0, 0 ); V( -, -, + ); V( -, +, + ); V( -, +, - ); V( -, -, - );
        N( 0, -1, 0 ); V( -, -, + ); V( -, -, - ); V( +, -, - ); V( +, -, + );
        N( 0, 0, -1 ); V( -, -, - ); V( -, +, - ); V( +, +, - ); V( +, -, - );
    glEnd( );
#   undef V
#   undef N
}

/*!
    \brief Compute a lookup table of cos and sin values forming a cirle.

    It is the responsibility of the caller to free these tables

    The size of the table is (n+1) to form a connected loop

    The last entry is exactly the same as the first

    The sign of n can be flipped to get the reverse loop
*/
static void ogCircleTable( double **sint, double **cost, const int n )
{
    int i;
    const int size = abs( n );
    double angle;

    assert( n );
    angle = 2 * M_PI / ( double )n;

    *sint = ( double * )calloc( sizeof( double ), size + 1 );
    *cost = ( double * )calloc( sizeof( double ), size + 1 );

    if( !( *sint ) || !( *cost ) )
    {
        free( *sint );
        free( *cost );
        ogError( "Failed to allocate memory in ogCircleTable" );
    }

    for( i = 0; i < size; i++ )
    {
        ( *sint )[ i ] = sin( angle * i );
        ( *cost )[ i ] = cos( angle * i );
    }

    /* Last sample is duplicate of the first */
    ( *sint )[ size ] = ( *sint )[ 0 ];
    ( *cost )[ size ] = ( *cost )[ 0 ];
}

/*!
    \fn
    \ingroup  geometry
    \brief    Draw a solid sphere centered at the origin.
    \param    radius        Sphere radius.
    \param    slices        The number of divisions around the z axis.
                            (latitudal)
    \param    stacks        The number of divisions along the z axis.
                            (longitudal)

              The glutSolidSphere() function draws a shaded sphere centered at
              the origin.  The surface is created from quadrangles
              (except for triangles as degenerate quads at the poles) in a
              longitude/latitude pattern.  The equatorial great circle lies
              in the xy-plane and is centered on the origin.

    \note     The number of polygons representing the spherical surface is
              proportional to (slices*stacks).

    \see glutWireSphere()
*/
void OGAPIENTRY glutSolidSphere( GLdouble radius, GLint slices, GLint stacks )
{
    int i, j;

    /* Adjust z and radius as stacks are drawn. */
    double z0, z1;
    double r0, r1;

    /* Pre-computed circle */
    double *sint1 = NULL, *cost1 = NULL;
    double *sint2 = NULL, *cost2 = NULL;

    if( DBL_EPSILON > radius )
        ogWarning( "Small radius in glutSolidSphere" );

    if( 1 > slices )
        ogWarning( "Invalid slices in glutSolidSphere" );
    else
        ogCircleTable( &sint1, &cost1, slices );

    if( 1 > stacks )
        ogWarning( "Invalid stacks in glutSolidSphere" );
    else
        ogCircleTable( &sint2, &cost2, stacks * 2 );

    slices = abs( slices );
    stacks = abs( stacks );

    if( sint1 && cost1 && sint2 && cost2 )
    {
        /* The top stack is covered with a triangle fan */
        z0 = 1.0;
        z1 = cost2[ 1 ];
        r0 = 0.0;
        r1 = sint2[ 1 ];

        glBegin( GL_TRIANGLE_FAN );
        glNormal3d( 0, 0, 1 );
        glVertex3d( 0, 0, radius );

        for( j = slices; j >= 0; j-- )
        {
            glNormal3d( cost1[ j ] * r1, sint1[ j ] * r1, z1 );
            glVertex3d(
                cost1[ j ] * r1 * radius, sint1[ j ] * r1 * radius, z1 * radius
            );
        }
        glEnd( );

        /* Cover each stack with a quad strip except the top/bottom stacks */
        for( i = 1; i < stacks - 1; i++ )
        {
            z0 = z1;
            z1 = cost2[ i + 1 ];
            r0 = r1;
            r1 = sint2[ i + 1];

            glBegin( GL_QUAD_STRIP );
            for( j = 0; j <= slices; j++ )
            {
                glNormal3d( cost1[ j ] * r1, sint1[ j ] * r1, z1 );
                glVertex3d(
                    cost1[ j ] * r1 * radius,
                    sint1[ j ] * r1 * radius,
                    z1 * radius
                );
                glNormal3d( cost1[ j ] * r0, sint1[ j ] * r0, z0 );
                glVertex3d(
                    cost1[ j ] * r0 * radius,
                    sint1[ j ] * r0 * radius,
                    z0 * radius
                );
            }
            glEnd( );
        }

        /* The bottom stack is covered with a triangle fan */
        z0 = z1;
        r0 = r1;

        glBegin( GL_TRIANGLE_FAN );
        glNormal3d( 0, 0, -1 );
        glVertex3d( 0, 0, -radius );

        for(j = 0; j <= slices; j++ )
        {
            glNormal3d( cost1[ j ] * r0, sint1[ j ] * r0, z0 );
            glVertex3d(
                cost1[ j ] * r0 * radius, sint1[ j ] * r0 * radius, z0 * radius
            );
        }
        glEnd( );
    }

    free( sint1 );
    free( cost1 );
    free( sint2 );
    free( cost2 );
}

/*!
    \fn
    \ingroup  geometry
    \brief    Draw a wireframe sphere centered at the origin.
    \param    radius        Sphere radius.
    \param    slices        The number of divisions around the z axis.
                            (latitudal)
    \param    stacks        The number of divisions along the z axis.
                            (longitudal)

              The glutWireSphere() function draws a wireframe sphere centered
              at the origin.
              The "equatorial" great circle lies in the xy-plane.

    \note     The number of line segments representing the spherical surface is
              proportional to (slices*stacks).

    \see      glutSolidSphere().
*/
void OGAPIENTRY glutWireSphere( GLdouble radius, GLint slices, GLint stacks )
{
    int i, j;

    /* Adjust z and radius as stacks and slices are drawn. */
    double r;
    double x, y, z;

    /* Pre-computed circle */
    double *sint1 = NULL, *cost1 = NULL;
    double *sint2 = NULL, *cost2 = NULL;

    if( DBL_EPSILON > radius )
        ogWarning( "Small radius in glutWireSphere" );

    if( 1 > slices )
        ogWarning( "Invalid slices in glutWireSphere" );
    else
        ogCircleTable( &sint1, &cost1, slices );

    if( 1 > stacks )
        ogWarning( "Invalid stacks in glutWireSphere" );
    else
        ogCircleTable( &sint2, &cost2,  stacks * 2 );

    slices = abs( slices );
    stacks = abs( stacks );

    if( sint1 && cost1 && sint2 && cost2 )
    {
        /* Draw a line loop for each stack */
        for( i = 1; i < stacks; i++ )
        {
            z = cost2[ i ];
            r = sint2[ i ];

            glBegin( GL_LINE_LOOP );
            for( j = 0; j <= slices; j++ )
            {
                x = cost1[ j ];
                y = sint1[ j ];

                glNormal3d( x, y, z );
                glVertex3d( x * r * radius, y * r * radius, z * radius );
            }
            glEnd( );
        }

        /* Draw a line loop for each slice */
        for( i = 0; i < slices; i++ )
        {
            glBegin( GL_LINE_STRIP );
            for( j = 0; j <= stacks; j++ )
            {
                x = cost1[ i ] * sint2[ j ];
                y = sint1[ i ] * sint2[ j ];
                z = cost2[ j ];

                glNormal3d( x, y, z );
                glVertex3d( x * radius, y * radius, z * radius );
            }
            glEnd( );
        }
    }

    free( sint1 );
    free( cost1 );
    free( sint2 );
    free( cost2 );
}

/*!
    \fn
    \brief    Draw a solid cone.
    \ingroup  geometry
    \param    base       Cone radius at the base in the xy plane.
    \param    height     Height of cone in positive z direction.
    \param    slices     The number of divisions around the z axis. (latitudal)
    \param    stacks     The number of divisions along the z axis. (longitudal)

              The glutSolidCone() function draws a shaded cone
              with a base in the xy-plane, oriented in the positive z
              direction.

    \note     The number of polygons representing the conical surface is
              proportional to (slices*stacks).

    \see      glutWireCone()
*/
void OGAPIENTRY glutSolidCone( GLdouble base, GLdouble height,
                               GLint slices, GLint stacks )
{
    int i, j;

    /* Step in z and radius as stacks are drawn. */
    double z0, z1;
    double r0, r1;

    /* Used for computing scaling factors for vertex normals */
    const double side_length = sqrt( height*height + base*base );

    /* Pre-computed circle */
    double *sint = NULL, *cost = NULL;

    if( DBL_EPSILON > base )
        ogWarning( "Small base in glutSolidCone" );
    if( DBL_EPSILON > height )
        ogWarning( "Small height in glutSolidCone" );

    if( 1 > slices )
        ogWarning( "Invalid slices in glutSolidCone" );
    else if( 1 > stacks )
        ogWarning( "Invalid stacks in glutSolidCone" );
    else
        ogCircleTable( &sint, &cost, -slices );

    slices = abs( slices );
    stacks = abs( stacks );

    if( sint && cost && side_length>0.0 )
    {
        const double zStep = height/stacks;
        const double rStep = base/stacks;

        /* Scaling factors for vertex normals */
        const double cosn = height / side_length;
        const double sinn = base   / side_length;

        /* Cover the circular base with a triangle fan... */
        z0 = 0.0;
        z1 = zStep;

        r0 = base;
        r1 = r0 - rStep;

        glBegin( GL_TRIANGLE_FAN );
        glNormal3d( 0.0, 0.0, -1.0 );
        glVertex3d( 0.0, 0.0, z0 );

        for( j = 0; j <= slices; j++ )
            glVertex3d( cost[ j ] * r0, sint[ j ] * r0, z0 );
        glEnd( );

        /* Cover each stack with a quad strip, except the top stack */
        for( i = 0; i < stacks - 1; i++ )
        {
            glBegin( GL_QUAD_STRIP );

            for( j = 0; j <= slices; j++ )
            {
                glNormal3d( cost[ j ] * sinn, sint[ j ] * sinn, cosn );
                glVertex3d( cost[ j ] * r0,   sint[ j ] * r0,   z0   );
                glVertex3d( cost[ j ] * r1,   sint[ j ] * r1,   z1   );
            }

            z0 = z1;
            z1 += zStep;
            r0 = r1;
            r1 -= rStep;
            glEnd( );
        }

        /* The top stack is covered with individual triangles */
        glBegin( GL_TRIANGLES );
        glNormal3d( cost[ 0 ] * sinn, sint[ 0 ] * sinn, cosn );

        for( j = 0; j < slices; j++ )
        {
            glVertex3d( cost[ j + 0 ] * r0,   sint[ j + 0 ] * r0,   z0     );
            glVertex3d( 0,                    0,                    height );
            glNormal3d( cost[ j + 1 ] * sinn, sint[ j + 1 ] * sinn, cosn   );
            glVertex3d( cost[ j + 1 ] * r0,   sint[ j + 1 ] * r0,   z0     );
        }
        glEnd( );
    }

    free( sint );
    free( cost );
}

/*!
    \fn
    \brief    Draw a wireframe cone.
    \ingroup  geometry
    \param    base       Cone radius at the base in the xy plane.
    \param    height     Height of cone in positive z direction.
    \param    slices     The number of divisions around the z axis. (latitudal)
    \param    stacks     The number of divisions along the z axis. (longitudal)

              The glutWireCone() function draws a wireframe cone
              with a base in the xy plane oriented in positive z.

    \note     The number of line segments representing the conical surface is
              proportional to (slices*stacks).

    \see      glutSolidCone()
*/
void OGAPIENTRY glutWireCone( GLdouble base, GLdouble height,
                              GLint slices, GLint stacks)
{
    int i, j;

    /* Step in z and radius as stacks are drawn. */
    double z = 0.0;
    double r = base;

    double side_length = sqrt( height*height + base*base );

    /* Pre-computed circle */
    double *sint = NULL, *cost = NULL;

    if( DBL_EPSILON > base )
        ogWarning( "Small base in glutWireCone" );
    if( DBL_EPSILON > height )
        ogWarning( "Small height in glutWireCone" );

    if( 1 > slices )
        ogWarning( "Invalid slices in glutWireCone" );
    else if( 1 > stacks )
        ogWarning( "Invalid stacks in glutWireCone" );
    else
        ogCircleTable( &sint, &cost, slices );

    slices = abs( slices );
    stacks = abs( stacks );

    if( sint && cost && (DBL_EPSILON < side_length ) )
    {
        const double zStep = height / stacks;
        const double rStep = base   / stacks;

        /* Scaling factors for vertex normals */
        const double cosn = height / side_length;
        const double sinn = base   / side_length;

        /* Draw the stacks... */
        for( i = 0; i < stacks; i++ )
        {
            glBegin( GL_LINE_LOOP );
            for( j = 0; j < slices; j++ )
            {
                glNormal3d( cost[ j ] * sinn, sint[ j ]* sinn, cosn );
                glVertex3d( cost[ j ] * r,    sint[ j ] * r,   z    );
            }
            glEnd( );

            z += zStep;
            r -= rStep;
        }

        /* Draw the slices */
        r = base;

        glBegin( GL_LINES );
        for( j = 0; j < slices; j++ )
        {
            glNormal3d( cost[ j ] * sinn, sint[ j ] * sinn, cosn   );
            glVertex3d( cost[ j ] * r,    sint[ j ] * r,    0.0    );
            glVertex3d( 0.0,              0.0,              height );
        }
        glEnd( );
    }

    free( sint );
    free( cost );
}


/*!
    \fn
    \brief    Draw a solid cylinder.
    \ingroup  geometry
    \param    radius     Radius of the cylinder.
    \param    height     Z height.
    \param    slices     Divisions around z axis.
    \param    stacks     Divisions along z axis.

              glutSolidCylinder() draws a shaded cylinder,
              the center of whose base is at the origin and
              whose axis is along the positive z axis.

    \see      glutWireCylinder()
*/
void OGAPIENTRY glutSolidCylinder( GLdouble radius, GLdouble height,
                                   GLint slices, GLint stacks )
{
    int i, j;

    /* Pre-computed circle */
    double *sint = NULL, *cost = NULL;

    if( DBL_EPSILON > radius )
        ogWarning( "Small radius in glutSolidCylinder" );
    if( DBL_EPSILON > height )
        ogWarning( "Small height in glutSolidCylinder" );

    if( 1 > slices )
        ogWarning( "Invalid slices in glutSolidCylinder" );
    else if( 1 > stacks )
        ogWarning( "Invalid stacks in glutSolidCylinder" );
    else
        ogCircleTable( &sint, &cost, -slices );

    slices = abs( slices );
    stacks = abs( stacks );

    if( sint && cost )
    {
        /* Step in z and radius as stacks are drawn. */
        double z0, z1;
        const double zStep = height / stacks;

        /* Cover the base and top */
        glBegin( GL_TRIANGLE_FAN );
        glNormal3d( 0.0, 0.0, -1.0 );
        glVertex3d( 0.0, 0.0,  0.0 );
        for (j = 0; j <= slices; j++ )
            glVertex3d( cost[ j ] * radius, sint[ j ] * radius, 0.0 );
        glEnd();

        glBegin( GL_TRIANGLE_FAN );
        glNormal3d( 0.0, 0.0, 1.0    );
        glVertex3d( 0.0, 0.0, height );
        for ( j = slices; j >= 0; j-- )
            glVertex3d( cost[ j ] * radius, sint[ j ] * radius, height );
        glEnd( );

        /* Do the stacks */
        z0 = 0.0;
        z1 = zStep;

        for( i = 1; i <= stacks; i++ )
        {
            if( i == stacks )
                z1 = height;

            glBegin( GL_QUAD_STRIP );
            for( j = 0; j <= slices; j++ )
            {
                glNormal3d( cost[ j ],          sint[ j ],          0.0 );
                glVertex3d( cost[ j ] * radius, sint[ j ] * radius, z0  );
                glVertex3d( cost[ j ] * radius, sint[ j ] * radius, z1  );
            }
            glEnd( );

            z0 = z1;
            z1 += zStep;
        }
    }

    free( sint );
    free( cost );
}

/*!
    \fn
    \brief    Draw a wireframe cylinder.
    \ingroup  geometry
    \param    radius     Radius of cylinder.
    \param    height     Z height.
    \param    slices     Number of divisions around the z axis.
    \param    stacks     Number of divisions along the z axis.

              glutWireCylinder() draws a wireframe of a cylinder,
              the center of whose base is at the origin, and
              whose axis parallels the z axis.

    \see      glutSolidCylinder()
*/
void OGAPIENTRY glutWireCylinder(
    GLdouble radius, GLdouble height, GLint slices, GLint stacks
)
{
    int i, j;

    /* Pre-computed circle */
    double *sint = NULL, *cost = NULL;

    if( DBL_EPSILON > radius )
        ogWarning( "Small radius in glutWireCylinder" );

    if( DBL_EPSILON > height )
        ogWarning( "Small height in glutWireCylinder" );

    if( 1 > slices )
        ogWarning( "Invalid slices in glutWireCylinder" );
    else if( 1 > stacks )
        ogWarning( "Invalid stacks in glutWireCylinder" );
    else
        ogCircleTable( &sint, &cost, -slices );

    slices = abs(slices);
    stacks = abs(stacks);

    if( sint && cost )
    {
        /* Step in z and radius as stacks are drawn. */
        double z = 0.0;
        const double zStep = height / stacks;

        /* Draw the stacks... */
        for( i = 0; i <= stacks; i++ )
        {
            if( i == stacks )
                z = height;

            glBegin( GL_LINE_LOOP );
            for( j = 0; j < slices; j++ )
            {
                glNormal3d( cost[ j ],          sint[ j ],          0.0 );
                glVertex3d( cost[ j ] * radius, sint[ j ] * radius, z   );
            }
            glEnd( );

            z += zStep;
        }

        /* Draw the slices */
        glBegin( GL_LINES );
        for( j = 0; j < slices; j++ )
        {
            glNormal3d( cost[ j ],          sint[ j ],          0.0    );
            glVertex3d( cost[ j ] * radius, sint[ j ] * radius, 0.0    );
            glVertex3d( cost[ j ] * radius, sint[ j ] * radius, height );
        }
        glEnd( );
    }

    free( sint );
    free( cost );
}

/*!
    \fn
    \brief    Draw a wireframe torus.
    \ingroup  geometry
    \param    dInnerRadius    Radius of ``tube''
    \param    dOuterRadius    Radius of ``path''
    \param    nSides          Facets around ``tube''
    \param    nRings          Joints along ``path''

              This function effectively wraps a cylinder with \a nSides slats
              and bends it at \a nRings facets around a circular
              path, forming a torus, or ``donut''.  The center is
              at the origin and the ``path'' rings around the
              z axis.

              The torus parameters can be explored interactively
              with the OpenGLUT shapes demo.

    \note     \a dInnerRadius and \a dOuterRadius are <b>not</b>
              analogous to similar measurements of an anulus.

    \see      glutSolidTorus()

*/
void OGAPIENTRY glutWireTorus( GLdouble dInnerRadius, GLdouble dOuterRadius,
                               GLint nSides, GLint nRings )
{
    double iradius = dInnerRadius;
    double oradius = dOuterRadius;
    double phi, psi, dpsi, dphi;
    double *vertex = NULL, *normal = NULL;
    double spsi, cpsi, sphi, cphi;
    int i, j;

    /*
     * XXX Probably should not print any of these.  If the
     * XXX radii are too small, that's a client decision and
     * XXX not worth a comment.  If the sides/rings are less than
     * XXX 1, then we can with some correctness simply do nothing.
     * XXX Less than 0 makes no sense.
     */
    if( DBL_EPSILON > dInnerRadius )
        ogWarning( "Small tube radius in glutWireTorus" );
    if( DBL_EPSILON > dOuterRadius )
        ogWarning( "Small path radius in glutWireTorus" );

    if( 0 > nSides )
        ogWarning( "Invalid sides in glutWireTorus" );
    else if ( 0 > nRings )
        ogWarning( "Invalid rings in glutWireTorus" );
    else
    {
        /*
         * Increment the number of sides and rings to allow for one more point
         * than surface
         */
        nSides = abs( nSides ) + 1;
        nRings = abs( nRings ) + 1;

        vertex = ( double * )calloc( sizeof( double ), 3 * nSides * nRings );
        normal = ( double * )calloc( sizeof( double ), 3 * nSides * nRings );
    }

    if( vertex && normal )
    {
        dpsi =  2.0 * M_PI / ( double )nRings;
        dphi = -2.0 * M_PI / ( double )nSides;
        psi  = 0.0;

        for( j = 0; j < nRings; j++ )
        {
            cpsi = cos( psi );
            spsi = sin( psi );
            phi = 0.0;

            for( i = 0; i < nSides; i++ )
            {
                int offset = 3 * ( j * nSides + i );
                cphi = cos( phi );
                sphi = sin( phi );
                *( vertex + offset + 0 ) = cpsi * ( oradius + cphi * iradius );
                *( vertex + offset + 1 ) = spsi * ( oradius + cphi * iradius );
                *( vertex + offset + 2 ) =                    sphi * iradius;
                *( normal + offset + 0 ) = cpsi * cphi;
                *( normal + offset + 1 ) = spsi * cphi;
                *( normal + offset + 2 ) =        sphi;
                phi += dphi;
            }

            psi += dpsi;
        }

        for( i = 0; i < nSides; i++ )
        {
            glBegin( GL_LINE_LOOP );
            for( j = 0; j < nRings; j++ )
            {
                int offset = 3 * ( j * nSides + i );
                glNormal3dv( normal + offset );
                glVertex3dv( vertex + offset );
            }
            glEnd( );
        }

        for( j = 0; j < nRings; j++ )
        {
            glBegin( GL_LINE_LOOP );
            for( i = 0; i < nSides; i++ )
            {
                int offset = 3 * ( j * nSides + i );
                glNormal3dv( normal + offset );
                glVertex3dv( vertex + offset );
            }
            glEnd( );
        }
    }

    free( vertex );
    free( normal );
}

/*!
    \fn
    \brief    Draw a solid torus.
    \ingroup  geometry
    \param    dInnerRadius    Radius of ``tube''
    \param    dOuterRadius    Radius of ``path''
    \param    nSides          Facets around ``tube''
    \param    nRings          Joints along ``path''

              This function effectively wraps a cylinder with \a nSides slats
              and bends it at \a nRings facets around a circular
              path, forming a torus, or ``donut''.  The center is
              at the origin and the ``path'' rings around the
              z axis.

              The torus parameters can be explored interactively
              with the OpenGLUT shapes demo.

    \note     \a dInnerRadius and \a dOuterRadius are <b>not</b>
              analogous to similar measurements of an anulus.

    \see      glutWireTorus()

*/
void OGAPIENTRY glutSolidTorus( GLdouble dInnerRadius, GLdouble dOuterRadius,
                                GLint nSides, GLint nRings )
{
    double iradius = dInnerRadius;
    double oradius = dOuterRadius;
    double phi, psi, dpsi, dphi;
    double *vertex = NULL, *normal = NULL;
    double spsi, cpsi, sphi, cphi;
    int i, j;

    if( DBL_EPSILON > dInnerRadius )
        ogWarning( "Small tube radius in glutSolidTorus" );
    if( DBL_EPSILON > dOuterRadius )
        ogWarning( "Small path radius in glutSolidTorus" );

    if( 1 > nSides )
        ogWarning( "Invalid sides in glutSolidTorus" );
    else if( 1 > nRings )
        ogWarning( "Invalid rings in glutSolidTorus" );
    else
    {

        /*
         * Increment the number of sides and rings to allow for one more point
         * than surface
         */

        nSides = abs(nSides)+1;
        nRings = abs(nRings)+1;

        vertex = ( double * )calloc( sizeof( double ), 3 * nSides * nRings );
        normal = ( double * )calloc( sizeof( double ), 3 * nSides * nRings );
    }

    if( vertex && normal )
    {
        dpsi =  2.0 * M_PI / ( double )( nRings - 1 );
        dphi = -2.0 * M_PI / ( double )( nSides - 1 );
        psi  = 0.0;

        for( j = 0; j < nRings; j++ )
        {
            cpsi = cos( psi );
            spsi = sin( psi );
            phi = 0.0;

            for( i = 0; i < nSides; i++ )
            {
                int offset = 3 * ( j * nSides + i );
                cphi = cos( phi );
                sphi = sin( phi );
                *( vertex + offset + 0 ) = cpsi * ( oradius + cphi * iradius );
                *( vertex + offset + 1 ) = spsi * ( oradius + cphi * iradius );
                *( vertex + offset + 2 ) =                    sphi * iradius;
                *( normal + offset + 0 ) = cpsi * cphi;
                *( normal + offset + 1 ) = spsi * cphi;
                *( normal + offset + 2 ) =        sphi;
                phi += dphi;
            }

            psi += dpsi;
        }

        glBegin( GL_QUADS );
        for( i = 0; i < nSides - 1; i++ )
            for( j = 0; j < nRings - 1; j++ )
            {
                int offset = 3 * ( j * nSides + i );
                glNormal3dv( normal + offset );
                glVertex3dv( vertex + offset );
                glNormal3dv( normal + offset + 3 );
                glVertex3dv( vertex + offset + 3 );
                glNormal3dv( normal + offset + 3 * nSides + 3 );
                glVertex3dv( vertex + offset + 3 * nSides + 3 );
                glNormal3dv( normal + offset + 3 * nSides );
                glVertex3dv( vertex + offset + 3 * nSides );
            }
        glEnd( );
    }

    free( vertex );
    free( normal );
}

/*!
    \fn
    \brief      Draw a wireframe dodecahedron.
    \ingroup    geometry

                This function draws a regular, wireframe 12-sided polyhedron
                centered at the origin.
                The distance from the origin to the vertices is
                sqrt(3).
                The facets are pentagons.
                No facet is normal any of the \a x, \a y, or \a z
                axes.

    \see        glutSolidDodecahedron(), glutWireRhombicDodecahedron(),
                glutSolidRhombicDodecahedron()

*/
void OGAPIENTRY glutWireDodecahedron( void )
{
    /*
     * Magic Numbers:  It is possible to create a dodecahedron by attaching
     * two pentagons to each face of of a cube.  The coordinates of the points
     * are:
     *   {(+/- x, 0, z), (+/- 1, 1, 1), (0, z, x )}
     * where x = (-1 + sqrt(5))/2 and z = (1 + sqrt(5))/2 or, approximately
     *       x = 0.61803398875    and z = 1.61803398875.
     */
    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.0,  0.525731112119,  0.850650808354 );
        glVertex3d( 0.0,  1.61803398875,  0.61803398875 );
        glVertex3d( -1.0,  1.0,  1.0 );
        glVertex3d( -0.61803398875, 0.0,  1.61803398875 );
        glVertex3d( 0.61803398875, 0.0,  1.61803398875 );
        glVertex3d( 1.0,  1.0,  1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.0,  0.525731112119, -0.850650808354 );
        glVertex3d( 0.0,  1.61803398875, -0.61803398875 );
        glVertex3d( 1.0,  1.0, -1.0 );
        glVertex3d( 0.61803398875, 0.0, -1.61803398875 );
        glVertex3d( -0.61803398875, 0.0, -1.61803398875 );
        glVertex3d( -1.0,  1.0, -1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.0, -0.525731112119,  0.850650808354 );
        glVertex3d( 0.0, -1.61803398875,  0.61803398875 );
        glVertex3d( 1.0, -1.0,  1.0 );
        glVertex3d( 0.61803398875, 0.0,  1.61803398875 );
        glVertex3d( -0.61803398875, 0.0,  1.61803398875 );
        glVertex3d( -1.0, -1.0,  1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.0, -0.525731112119, -0.850650808354 );
        glVertex3d( 0.0, -1.61803398875, -0.61803398875 );
        glVertex3d( -1.0, -1.0, -1.0 );
        glVertex3d( -0.61803398875, 0.0, -1.61803398875 );
        glVertex3d( 0.61803398875, 0.0, -1.61803398875 );
        glVertex3d( 1.0, -1.0, -1.0 );
    glEnd( );

    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.850650808354,  0.0,  0.525731112119 );
        glVertex3d( 0.61803398875,  0.0,  1.61803398875 );
        glVertex3d( 1.0, -1.0,  1.0 );
        glVertex3d( 1.61803398875, -0.61803398875, 0.0 );
        glVertex3d( 1.61803398875,  0.61803398875, 0.0 );
        glVertex3d( 1.0,  1.0,  1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( -0.850650808354,  0.0,  0.525731112119 );
        glVertex3d( -0.61803398875,  0.0,  1.61803398875 );
        glVertex3d( -1.0,  1.0,  1.0 );
        glVertex3d( -1.61803398875,  0.61803398875, 0.0 );
        glVertex3d( -1.61803398875, -0.61803398875, 0.0 );
        glVertex3d( -1.0, -1.0,  1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.850650808354,  0.0, -0.525731112119 );
        glVertex3d( 0.61803398875,  0.0, -1.61803398875 );
        glVertex3d( 1.0,  1.0, -1.0 );
        glVertex3d( 1.61803398875,  0.61803398875, 0.0 );
        glVertex3d( 1.61803398875, -0.61803398875, 0.0 );
        glVertex3d( 1.0, -1.0, -1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( -0.850650808354,  0.0, -0.525731112119 );
        glVertex3d( -0.61803398875,  0.0, -1.61803398875 );
        glVertex3d( -1.0, -1.0, -1.0 );
        glVertex3d( -1.61803398875, -0.61803398875, 0.0 );
        glVertex3d( -1.61803398875,  0.61803398875, 0.0 );
        glVertex3d( -1.0,  1.0, -1.0 );
    glEnd( );

    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.525731112119,  0.850650808354,  0.0 );
        glVertex3d( 1.61803398875,  0.61803398875,  0.0 );
        glVertex3d( 1.0,  1.0, -1.0 );
        glVertex3d( 0.0,  1.61803398875, -0.61803398875 );
        glVertex3d( 0.0,  1.61803398875,  0.61803398875 );
        glVertex3d( 1.0,  1.0,  1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.525731112119, -0.850650808354,  0.0 );
        glVertex3d( 1.61803398875, -0.61803398875,  0.0 );
        glVertex3d( 1.0, -1.0,  1.0 );
        glVertex3d( 0.0, -1.61803398875,  0.61803398875 );
        glVertex3d( 0.0, -1.61803398875, -0.61803398875 );
        glVertex3d( 1.0, -1.0, -1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( -0.525731112119,  0.850650808354,  0.0 );
        glVertex3d( -1.61803398875,  0.61803398875,  0.0 );
        glVertex3d( -1.0,  1.0,  1.0 );
        glVertex3d( 0.0,  1.61803398875,  0.61803398875 );
        glVertex3d( 0.0,  1.61803398875, -0.61803398875 );
        glVertex3d( -1.0,  1.0, -1.0 );
    glEnd( );
    glBegin( GL_LINE_LOOP );
        glNormal3d( -0.525731112119, -0.850650808354,  0.0 );
        glVertex3d( -1.61803398875, -0.61803398875,  0.0 );
        glVertex3d( -1.0, -1.0, -1.0 );
        glVertex3d( 0.0, -1.61803398875, -0.61803398875 );
        glVertex3d( 0.0, -1.61803398875,  0.61803398875 );
        glVertex3d( -1.0, -1.0,  1.0 );
    glEnd( );
}

/*!
    \fn
    \brief      Draw a solid dodecahedron.
    \ingroup    geometry

                This function draws a regular, solid, 12-sided polyhedron
                centered at the origin.
                The distance from the origin to the vertices is
                sqrt(3).
                The facets are pentagons.

    \see        glutWireDodecahedron(), glutSolidRhombicDodecahedron(),
                glutWireRhombicDodecahedron()

*/
void OGAPIENTRY glutSolidDodecahedron( void )
{
    /* See glutWireDodecahedron() for info about the Magic Numbers. */
    glBegin( GL_POLYGON );
        glNormal3d( 0.0,  0.525731112119,  0.850650808354 );
        glVertex3d( 0.0,  1.61803398875,  0.61803398875 );
        glVertex3d( -1.0,  1.0,  1.0 );
        glVertex3d( -0.61803398875, 0.0,  1.61803398875 );
        glVertex3d( 0.61803398875, 0.0,  1.61803398875 );
        glVertex3d( 1.0,  1.0,  1.0 );
    glEnd( );
    glBegin( GL_POLYGON );
        glNormal3d( 0.0,  0.525731112119, -0.850650808354 );
        glVertex3d( 0.0,  1.61803398875, -0.61803398875 );
        glVertex3d( 1.0,  1.0, -1.0 );
        glVertex3d( 0.61803398875, 0.0, -1.61803398875 );
        glVertex3d( -0.61803398875, 0.0, -1.61803398875 );
        glVertex3d( -1.0,  1.0, -1.0 );
    glEnd( );
    glBegin( GL_POLYGON );
        glNormal3d( 0.0, -0.525731112119,  0.850650808354 );
        glVertex3d( 0.0, -1.61803398875,  0.61803398875 );
        glVertex3d( 1.0, -1.0,  1.0 );
        glVertex3d( 0.61803398875, 0.0,  1.61803398875 );
        glVertex3d( -0.61803398875, 0.0,  1.61803398875 );
        glVertex3d( -1.0, -1.0,  1.0 );
    glEnd( );
    glBegin( GL_POLYGON );
        glNormal3d( 0.0, -0.525731112119, -0.850650808354 );
        glVertex3d( 0.0, -1.61803398875, -0.61803398875 );
        glVertex3d( -1.0, -1.0, -1.0 );
        glVertex3d( -0.61803398875, 0.0, -1.61803398875 );
        glVertex3d( 0.61803398875, 0.0, -1.61803398875 );
        glVertex3d( 1.0, -1.0, -1.0 );
     glEnd( );

    glBegin( GL_POLYGON );
        glNormal3d( 0.850650808354,  0.0,  0.525731112119 );
        glVertex3d( 0.61803398875,  0.0,  1.61803398875 );
        glVertex3d( 1.0, -1.0,  1.0 );
        glVertex3d( 1.61803398875, -0.61803398875, 0.0 );
        glVertex3d( 1.61803398875,  0.61803398875, 0.0 );
        glVertex3d( 1.0,  1.0,  1.0 );
    glEnd( );
    glBegin ( GL_POLYGON );
        glNormal3d( -0.850650808354,  0.0,  0.525731112119 );
        glVertex3d( -0.61803398875,  0.0,  1.61803398875 );
        glVertex3d( -1.0,  1.0,  1.0 );
        glVertex3d( -1.61803398875,  0.61803398875, 0.0 );
        glVertex3d( -1.61803398875, -0.61803398875, 0.0 );
        glVertex3d( -1.0, -1.0,  1.0 );
    glEnd( );
    glBegin( GL_POLYGON );
        glNormal3d( 0.850650808354,  0.0, -0.525731112119 );
        glVertex3d( 0.61803398875,  0.0, -1.61803398875 );
        glVertex3d( 1.0,  1.0, -1.0 );
        glVertex3d( 1.61803398875,  0.61803398875, 0.0 );
        glVertex3d( 1.61803398875, -0.61803398875, 0.0 );
        glVertex3d( 1.0, -1.0, -1.0 );
    glEnd( );
    glBegin( GL_POLYGON );
        glNormal3d( -0.850650808354,  0.0, -0.525731112119 );
        glVertex3d( -0.61803398875,  0.0, -1.61803398875 );
        glVertex3d( -1.0, -1.0, -1.0 );
        glVertex3d( -1.61803398875, -0.61803398875, 0.0 );
        glVertex3d( -1.61803398875,  0.61803398875, 0.0 );
        glVertex3d( -1.0,  1.0, -1.0 );
    glEnd( );

    glBegin( GL_POLYGON );
        glNormal3d( 0.525731112119,  0.850650808354,  0.0 );
        glVertex3d( 1.61803398875,  0.61803398875,  0.0 );
        glVertex3d( 1.0,  1.0, -1.0 );
        glVertex3d( 0.0,  1.61803398875, -0.61803398875 );
        glVertex3d( 0.0,  1.61803398875,  0.61803398875 );
        glVertex3d( 1.0,  1.0,  1.0 );
    glEnd( );
    glBegin( GL_POLYGON );
        glNormal3d( 0.525731112119, -0.850650808354,  0.0 );
        glVertex3d( 1.61803398875, -0.61803398875,  0.0 );
        glVertex3d( 1.0, -1.0,  1.0 );
        glVertex3d( 0.0, -1.61803398875,  0.61803398875 );
        glVertex3d( 0.0, -1.61803398875, -0.61803398875 );
        glVertex3d( 1.0, -1.0, -1.0 );
    glEnd( );
    glBegin( GL_POLYGON );
        glNormal3d( -0.525731112119,  0.850650808354,  0.0 );
        glVertex3d( -1.61803398875,  0.61803398875,  0.0 );
        glVertex3d( -1.0,  1.0,  1.0 );
        glVertex3d( 0.0,  1.61803398875,  0.61803398875 );
        glVertex3d( 0.0,  1.61803398875, -0.61803398875 );
        glVertex3d( -1.0,  1.0, -1.0 );
    glEnd( );
    glBegin( GL_POLYGON );
        glNormal3d( -0.525731112119, -0.850650808354,  0.0 );
        glVertex3d( -1.61803398875, -0.61803398875,  0.0 );
        glVertex3d( -1.0, -1.0, -1.0 );
        glVertex3d( 0.0, -1.61803398875, -0.61803398875 );
        glVertex3d( 0.0, -1.61803398875,  0.61803398875 );
        glVertex3d( -1.0, -1.0,  1.0 );
    glEnd( );
}



/*
 * Octahedron VERTices.  Cleans up the code a bit.  (^&
 */
static double overt[ 6 ][ 3 ] =
{
    {  1,  0,  0 },
    {  0,  1,  0 },
    {  0,  0,  1 },
    { -1,  0,  0 },
    {  0, -1,  0 },
    {  0,  0, -1 },
};


/*!
    \fn
    \brief      Draw a wireframe octahedron.
    \ingroup    geometry

                This function draws a regular wireframe 8-sided polyhedron
                centered at the origin.
                The vertices are at
                 (+/-1, 0, 0),
                 (0, +/-1, 0),
                 (0, 0, +/-1).

    \note       We visit the same vertices the same number of times
                as for the solid octahedron, but the order is different.
    \note       Draws every edge twice.
    \note       The lines have normals, but the normals are from the
                facets, rather than upon the edge.  If you squint too
                hard, the lighting on a wireframe octahedron does
                not look quite right.
    \todo       It may be faster (and look better) to draw each edge
                once, setting the Normal at each edge.  (I don't
                think that this matters all that much, but a lineloop
                was proposed for the wire Cube for speed.)
    \see        glutSolidOctahedron()

*/
void OGAPIENTRY glutWireOctahedron( void )
{
    glBegin( GL_LINE_LOOP );
        glNormal3d( 0.577350269189, 0.577350269189, 0.577350269189 );
        glVertex3dv( overt[ 0 ] );
        glVertex3dv( overt[ 1 ] );
        glVertex3dv( overt[ 2 ] );
        glNormal3d( 0.577350269189, 0.577350269189,-0.577350269189 );
        glVertex3dv( overt[ 0 ] );
        glVertex3dv( overt[ 1 ] );
        glVertex3dv( overt[ 5 ] );
        glNormal3d( 0.577350269189,-0.577350269189, 0.577350269189 );
        glVertex3dv( overt[ 0 ] );
        glVertex3dv( overt[ 2 ] );
        glVertex3dv( overt[ 4 ] );
        glNormal3d( 0.577350269189,-0.577350269189,-0.577350269189 );
        glVertex3dv( overt[ 0 ] );
        glVertex3dv( overt[ 4 ] );
        glVertex3dv( overt[ 5 ] );
        glNormal3d(-0.577350269189, 0.577350269189, 0.577350269189 );
        glVertex3dv( overt[ 1 ] );
        glVertex3dv( overt[ 2 ] );
        glVertex3dv( overt[ 3 ] );
        glNormal3d(-0.577350269189, 0.577350269189,-0.577350269189 );
        glVertex3dv( overt[ 3 ] );
        glVertex3dv( overt[ 1 ] );
        glVertex3dv( overt[ 5 ] );
        glNormal3d(-0.577350269189,-0.577350269189, 0.577350269189 );
        glVertex3dv( overt[ 3 ] );
        glVertex3dv( overt[ 4 ] );
        glVertex3dv( overt[ 2 ] );
        glNormal3d(-0.577350269189,-0.577350269189,-0.577350269189 );
        glVertex3dv( overt[ 3 ] );
        glVertex3dv( overt[ 4 ] );
        glVertex3dv( overt[ 5 ] );
    glEnd( );
}

/*!
    \fn
    \brief      Draw a solid octahedron.
    \ingroup    geometry

                This function draws a regular, solid 8-sided polyhedron
                centered at the origin.
                The vertices are at
                 (+/-1, 0, 0),
                 (0, +/-1, 0),
                 (0, 0, +/-1).

    \note       We visit the same vertices the same number of times
                as in the wire octahedron, but the order is different.

    \see        glutWireOctahedron()

*/
void OGAPIENTRY glutSolidOctahedron( void )
{
    glBegin( GL_TRIANGLES );
        glNormal3d( 0.577350269189, 0.577350269189, 0.577350269189 );
        glVertex3dv( overt[ 0 ] );
        glVertex3dv( overt[ 1 ] );
        glVertex3dv( overt[ 2 ] );
        glNormal3d( 0.577350269189, 0.577350269189,-0.577350269189 );
        glVertex3dv( overt[ 5 ] );
        glVertex3dv( overt[ 1 ] );
        glVertex3dv( overt[ 0 ] );
        glNormal3d( 0.577350269189,-0.577350269189, 0.577350269189 );
        glVertex3dv( overt[ 2 ] );
        glVertex3dv( overt[ 4 ] );
        glVertex3dv( overt[ 0 ] );
        glNormal3d( 0.577350269189,-0.577350269189,-0.577350269189 );
        glVertex3dv( overt[ 0 ] );
        glVertex3dv( overt[ 4 ] );
        glVertex3dv( overt[ 5 ] );
        glNormal3d(-0.577350269189, 0.577350269189, 0.577350269189 );
        glVertex3dv( overt[ 2 ] );
        glVertex3dv( overt[ 1 ] );
        glVertex3dv( overt[ 3 ] );
        glNormal3d(-0.577350269189, 0.577350269189,-0.577350269189 );
        glVertex3dv( overt[ 3 ] );
        glVertex3dv( overt[ 1 ] );
        glVertex3dv( overt[ 5 ] );
        glNormal3d(-0.577350269189,-0.577350269189, 0.577350269189 );
        glVertex3dv( overt[ 3 ] );
        glVertex3dv( overt[ 4 ] );
        glVertex3dv( overt[ 2 ] );
        glNormal3d(-0.577350269189,-0.577350269189,-0.577350269189 );
        glVertex3dv( overt[ 5 ] );
        glVertex3dv( overt[ 4 ] );
        glVertex3dv( overt[ 3 ] );
    glEnd();
}


/*!
    \fn
    \brief      Draw a wireframe tetrahedron.
    \ingroup    geometry

                This function draws a regular, wireframe 4-sided polyhedron
                centered at the origin.
                The distance from the origin to the vertices is 1.

    \todo       Merge \a r0 \a r1 \a r2 and \a r3 into one array.
    \todo       Put the normals into the (or an) array.
    \todo       Make the array static const, with file scope, and share
                with glutSolidTetrahedron().
    \todo       Maybe consolidate with the SierpinskySponge?
    \see        glutSolidTetrahedron()

*/
void OGAPIENTRY glutWireTetrahedron( void )
{
    /*
     * Magic Numbers:  r0 = ( 1, 0, 0 )
     *                 r1 = ( -1/3, 2 sqrt(2) / 3, 0 )
     *                 r2 = ( -1/3, -sqrt(2) / 3, sqrt(6) / 3 )
     *                 r3 = ( -1/3, -sqrt(2) / 3, -sqrt(6) / 3 )
     * |r0| = |r1| = |r2| = |r3| = 1
     * Distance between any two points is 2 sqrt(6) / 3
     *
     * Normals:  The unit normals are simply the negative of the coordinates
     * of the point not on the surface.
     */

    double r0[3] = {             1.0,             0.0,             0.0 };
    double r1[3] = { -0.333333333333,  0.942809041582,             0.0 };
    double r2[3] = { -0.333333333333, -0.471404520791,  0.816496580928 };
    double r3[3] = { -0.333333333333, -0.471404520791, -0.816496580928 };

    glBegin( GL_LINE_LOOP );
        glNormal3d(           -1.0,             0.0,             0.0 );
        glVertex3dv( r1 );
        glVertex3dv( r3 );
        glVertex3dv( r2 );
        glNormal3d( 0.333333333333, -0.942809041582,             0.0 );
        glVertex3dv( r0 );
        glVertex3dv( r2 );
        glVertex3dv( r3 );
        glNormal3d( 0.333333333333,  0.471404520791, -0.816496580928 );
        glVertex3dv( r0 );
        glVertex3dv( r3 );
        glVertex3dv( r1 );
        glNormal3d( 0.333333333333,  0.471404520791,  0.816496580928 );
        glVertex3dv( r0 );
        glVertex3dv( r1 );
        glVertex3dv( r2 );
    glEnd();
}


/*!
    \fn
    \brief      Draw a solid tetrahedron.
    \ingroup    geometry

                This function draws a regular, solid 4-sided polyhedron
                centered at the origin.
                The distance from the origin to the vertices is 1.

    \todo       See todo-list on glutWireTetrahedron().
    \see        glutWireTetrahedron()

*/
void OGAPIENTRY glutSolidTetrahedron( void )
{
    /*
     * Magic Numbers:  r0 = ( 1, 0, 0 )
     *                 r1 = ( -1/3, 2 sqrt(2) / 3, 0 )
     *                 r2 = ( -1/3, -sqrt(2) / 3, sqrt(6) / 3 )
     *                 r3 = ( -1/3, -sqrt(2) / 3, -sqrt(6) / 3 )
     * |r0| = |r1| = |r2| = |r3| = 1
     * Distance between any two points is 2 sqrt(6) / 3
     *
     * Normals:  The unit normals are simply the negative of the coordinates
     * of the point not on the surface.
     */

    double r0[3] = {             1.0,             0.0,             0.0 };
    double r1[3] = { -0.333333333333,  0.942809041582,             0.0 };
    double r2[3] = { -0.333333333333, -0.471404520791,  0.816496580928 };
    double r3[3] = { -0.333333333333, -0.471404520791, -0.816496580928 };

    glBegin( GL_TRIANGLES );
        glNormal3d(           -1.0,             0.0,             0.0 );
        glVertex3dv( r1 );
        glVertex3dv( r3 );
        glVertex3dv( r2 );
        glNormal3d( 0.333333333333, -0.942809041582,             0.0 );
        glVertex3dv( r0 );
        glVertex3dv( r2 );
        glVertex3dv( r3 );
        glNormal3d( 0.333333333333,  0.471404520791, -0.816496580928 );
        glVertex3dv( r0 );
        glVertex3dv( r3 );
        glVertex3dv( r1 );
        glNormal3d( 0.333333333333,  0.471404520791,  0.816496580928 );
        glVertex3dv( r0 );
        glVertex3dv( r1 );
        glVertex3dv( r2 );
    glEnd();
}

/*
 *
 */
double icos_r[ 12 ][ 3 ] =
{
    { 1.0, 0.0, 0.0 },
    {  0.447213595500,  0.894427191000, 0.0 },
    {  0.447213595500,  0.276393202252, 0.850650808354 },
    {  0.447213595500, -0.723606797748, 0.525731112119 },
    {  0.447213595500, -0.723606797748, -0.525731112119 },
    {  0.447213595500,  0.276393202252, -0.850650808354 },
    { -0.447213595500, -0.894427191000, 0.0 },
    { -0.447213595500, -0.276393202252, 0.850650808354 },
    { -0.447213595500,  0.723606797748, 0.525731112119 },
    { -0.447213595500,  0.723606797748, -0.525731112119 },
    { -0.447213595500, -0.276393202252, -0.850650808354 },
    { -1.0, 0.0, 0.0 }
};
int icos_v[ 20 ][ 3 ] =
{
    { 0, 1, 2 }, { 0, 2, 3 }, { 0, 3, 4 }, { 0, 4, 5 }, { 0, 5, 1 },
    { 1, 8, 2 }, { 2, 7, 3 }, { 3, 6, 4 }, { 4, 10, 5 }, { 5, 9, 1 },
    { 1, 9, 8 }, { 2, 8, 7 }, { 3, 7, 6 }, { 4, 6, 10 }, { 5, 10, 9 },
    { 11, 9, 10 }, { 11, 8, 9 }, { 11, 7, 8 }, { 11, 6, 7 }, { 11, 10, 6 }
};


/*!
    \fn
    \brief      Draw a wireframe icosahedron.
    \ingroup    geometry

                This function draws a regular, solid 20-sided polyhedron
                centered at the origin.
                The distance from the origin to the vertices is 1.
                No facet is normal to any of the \a x, \a y, or \a z
                axes.

    \see        glutSolidIcosahedron()
*/
void OGAPIENTRY glutWireIcosahedron( void )
{
    int i;
    for( i = 0; i < 20; i++ )
    {
        double normal[ 3 ];
        normal[ 0 ] =
            ( icos_r[ icos_v[ i ][ 1 ] ][ 1 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 1 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 2 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 2] ) -
            ( icos_r[ icos_v[ i ][ 1 ] ][ 2 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 2 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 1 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 1 ] );
        normal[ 1 ] =
            ( icos_r[ icos_v[ i ][ 1 ] ][ 2 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 2 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 0 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 0 ] ) -
            ( icos_r[ icos_v[ i ][ 1 ] ][ 0 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 0 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 2 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 2 ] );
        normal[ 2 ] =
            ( icos_r[ icos_v[ i ][ 1 ] ][ 0 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 0 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 1 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 1 ] ) -
            ( icos_r[ icos_v[ i ][ 1 ] ][ 1 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 1 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 0 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 0 ] );
        glBegin( GL_LINE_LOOP );
            glNormal3dv( normal );
            glVertex3dv( icos_r[ icos_v[ i ][ 0 ] ] );
            glVertex3dv( icos_r[ icos_v[ i ][ 1 ] ] );
            glVertex3dv( icos_r[ icos_v[ i ][ 2 ] ] );
        glEnd( );
    }
}


/*!
    \fn
    \brief      Draw a solid icosahedron.
    \ingroup    geometry

                This function draws a regular, solid 20-sided polyhedron
                centered at the origin.
                The distance from the origin to the vertices is 1.

    \see        glutWireIcosahedron()
*/
void OGAPIENTRY glutSolidIcosahedron( void )
{
    int i;

    glBegin( GL_TRIANGLES );
    for( i = 0; i < 20; i++ )
    {
        double normal[ 3 ];
        normal[ 0 ] =
            ( icos_r[ icos_v[ i ][ 1 ] ][ 1 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 1 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 2 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 2 ] ) -
            ( icos_r[ icos_v[ i ][ 1 ] ][ 2 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 2 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 1 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 1 ] );
        normal[ 1 ] =
            ( icos_r[ icos_v[ i ][ 1 ] ][ 2 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 2 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 0 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 0 ] ) -
            ( icos_r[ icos_v[ i ][ 1 ] ][ 0 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 0 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 2 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 2 ] );
        normal[ 2 ] =
            ( icos_r[ icos_v[ i ][ 1 ] ][ 0 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 0 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 1 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 1 ] ) -
            ( icos_r[ icos_v[ i ][ 1 ] ][ 1 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 1 ] ) *
            ( icos_r[ icos_v[ i ][ 2 ] ][ 0 ] -
              icos_r[ icos_v[ i ][ 0 ] ][ 0 ] );
        glNormal3dv( normal );
        glVertex3dv( icos_r[ icos_v[ i ][ 0 ] ] );
        glVertex3dv( icos_r[ icos_v[ i ][ 1 ] ] );
        glVertex3dv( icos_r[ icos_v[ i ][ 2 ] ] );
    }
    glEnd( );
}

/*
 *
 */
double rdod_r[ 14 ][ 3 ] =
{
    { 0.0, 0.0, 1.0 },
    {  0.707106781187,  0.000000000000,  0.5 },
    {  0.000000000000,  0.707106781187,  0.5 },
    { -0.707106781187,  0.000000000000,  0.5 },
    {  0.000000000000, -0.707106781187,  0.5 },
    {  0.707106781187,  0.707106781187,  0.0 },
    { -0.707106781187,  0.707106781187,  0.0 },
    { -0.707106781187, -0.707106781187,  0.0 },
    {  0.707106781187, -0.707106781187,  0.0 },
    {  0.707106781187,  0.000000000000, -0.5 },
    {  0.000000000000,  0.707106781187, -0.5 },
    { -0.707106781187,  0.000000000000, -0.5 },
    {  0.000000000000, -0.707106781187, -0.5 },
    {  0.0, 0.0, -1.0 }
};
int rdod_v[ 12 ][ 4 ] =
{
    { 0,  1,  5,  2 },
    { 0,  2,  6,  3 },
    { 0,  3,  7,  4 },
    { 0,  4,  8,  1 },
    { 5, 10,  6,  2 },
    { 6, 11,  7,  3 },
    { 7, 12,  8,  4 },
    { 8,  9,  5,  1 },
    { 5,  9, 13, 10 },
    { 6, 10, 13, 11 },
    { 7, 11, 13, 12 },
    { 8, 12, 13,  9 }
};
double rdod_n[ 12 ][ 3 ] =
{
  {  0.353553390594,  0.353553390594,  0.5 },
  { -0.353553390594,  0.353553390594,  0.5 },
  { -0.353553390594, -0.353553390594,  0.5 },
  {  0.353553390594, -0.353553390594,  0.5 },
  {  0.000000000000,  1.000000000000,  0.0 },
  { -1.000000000000,  0.000000000000,  0.0 },
  {  0.000000000000, -1.000000000000,  0.0 },
  {  1.000000000000,  0.000000000000,  0.0 },
  {  0.353553390594,  0.353553390594, -0.5 },
  { -0.353553390594,  0.353553390594, -0.5 },
  { -0.353553390594, -0.353553390594, -0.5 },
  {  0.353553390594, -0.353553390594, -0.5 }
};

/*!
    \fn
    \brief    Draw a wireframe rhombic dodecahedron
    \ingroup  geometry

              This function draws a wireframe dodecahedron whose
              facets are rhombic and
              whose vertices are at unit radius.
              No facet lies normal to any coordinate axes.
              The polyhedron is centered at the origin.

    \see      glutSolidRhombicDodecahedron(), glutWireDodecahedron(),
              glutSolidDodecahedron()
*/
void OGAPIENTRY glutWireRhombicDodecahedron( void )
{
    int i;
    for( i = 0; i < 12; i++ )
    {
        glBegin( GL_LINE_LOOP );
            glNormal3dv( rdod_n[ i ] );
            glVertex3dv( rdod_r[ rdod_v[ i ][ 0 ] ] );
            glVertex3dv( rdod_r[ rdod_v[ i ][ 1 ] ] );
            glVertex3dv( rdod_r[ rdod_v[ i ][ 2 ] ] );
            glVertex3dv( rdod_r[ rdod_v[ i ][ 3 ] ] );
        glEnd( );
    }
}

/*!
    \fn
    \brief    Draw a solid rhombic dodecahedron
    \ingroup  geometry

              This function draws a solid-shaded dodecahedron
              whose facets are rhombic and
              whose vertices are at unit radius.
              No facet lies normal to any coordinate axes.
              The polyhedron is centered at the origin.

    \see      glutWireRhombicDodecahedron(), glutWireDodecahedron(),
              glutSolidDodecahedron()
*/
void OGAPIENTRY glutSolidRhombicDodecahedron( void )
{
    int i;

    glBegin( GL_QUADS );
    for( i = 0; i < 12; i++ )
    {
        glNormal3dv( rdod_n[ i ] );
        glVertex3dv( rdod_r[ rdod_v[ i ][ 0 ] ] );
        glVertex3dv( rdod_r[ rdod_v[ i ][ 1 ] ] );
        glVertex3dv( rdod_r[ rdod_v[ i ][ 2 ] ] );
        glVertex3dv( rdod_r[ rdod_v[ i ][ 3 ] ] );
    }
    glEnd( );
}



#define NUM_FACES     4

static GLdouble tetrahedron_v[ 4 ][ 3 ] =  /* Vertices */
{
    { -0.5, -0.288675134595, -0.144337567297 },
    {  0.5, -0.288675134595, -0.144337567297 },
    {  0.0,  0.577350269189, -0.144337567297 },
    {  0.0,  0.0,             0.672159013631 }
};

static GLint tetrahedron_i[ 4 ][ 3 ] =  /* Vertex indices */
{
    { 1, 0, 2 }, { 2, 0, 3 }, { 0, 1, 3 }, { 1, 2, 3 }
};

static GLdouble tetrahedron_n[ 4 ][ 3 ] =  /* Normals */
{
    {  0.0,             0.0,            -1.0 },
    { -0.816496580928,  0.471404520791,  0.333333333333 },
    {  0.0,            -0.942809041582,  0.333333333333 },
    {  0.816496580928,  0.471404520791,  0.333333333333 }
};

/*
 * The edges for the wireframe tetrahedron at the base of the Sponge object.
 *
 * This array is structured as: 6 edges.  Each edge has
 * the two vertices that define it, and the normal which
 * we determine as the 50/50 blend of the two faces that the
 * edge bounds.
 *
 * The array has been constructed with some care so that we can define
 * a GL_LINE_STRIP as:
 *
 *   edges[ 0 ].v1, edges[ 0 ].v2 == edges[ 1 ].v1,
 *                  edges[ 1 ].v2 == edges[ 2 ].v1,
 *                  edges[ 2 ].v2 == edges[ 3 ].v1,
 *   ...
 *
 * However, because an Euler Circuit can't be made on the vertices of a
 * tetrahedron, we either need to walk one edge twice or stop and
 * start at least once.  I chose to walk one edge twice, and use a secondary
 * array to tell me in what order to walk which edges.  (See below.)
 */
typedef struct tetrahedron_edge
{
    int v1;
    int v2;
    double normal[ 3 ];
} tetrahedron_edge;
static const tetrahedron_edge tetrahedron_edges[ 6 ] =
{
    { 0, 1, { -0.707106781186674,  0.408248290463772, -0.577350269189535 } },
    { 1, 2, { -0.707106781186674, -0.408248290463772,  0.577350269189535 } },
    { 2, 0, {  0.000000000000000, -0.816496580927708, -0.577350269189652 } },
    { 0, 3, {  0.707106781186674,  0.408248290463772, -0.577350269189535 } },
    { 3, 1, {  0.000000000000000,  0.816496580927708,  0.577350269189652 } },
    { 2, 3, {  0.707106781186674, -0.408248290463772,  0.577350269189535 } },
};

/*
 * This is the list of edges for a GL_LINE_STRIP to build the tetrahedron.
 * NOTE that this is not for a GL_LINE_LOOP.
 */
static const int tetrahedron_edge_list[ 7 ] = { 0, 1, 2, 3, 4, 1, 5 };

/*!
    \fn
    \brief    Draw a wireframe Spierspinski's sponge.
    \ingroup  geometry
    \param    num_levels    Recursive depth.
    \param    offset        Location vector.
    \param    scale         Relative size.

              This function recursively draws a few levels of
              Sierpinski's Sponge
              in wireframe.
              If \a num_levels is 0, draws 1 tetrahedron.
              The \a offset is a translation.
              The \a z axis is normal to the base.
              The sponge is centered at the origin.

    \note     Runtime is exponential in \a num_levels .

    \see      glutSolidSierpinskiSponge()
*/
void OGAPIENTRY glutWireSierpinskiSponge(
    int num_levels, const GLdouble offset[ 3 ], GLdouble scale
)
{
    int i;

    if( !num_levels )
    {
        int edge = tetrahedron_edge_list[ 0 ];
        int vert = tetrahedron_edges[ edge ].v1;
        double x = offset[ 0 ] + scale * tetrahedron_v[ vert ][ 0 ];
        double y = offset[ 1 ] + scale * tetrahedron_v[ vert ][ 1 ];
        double z = offset[ 2 ] + scale * tetrahedron_v[ vert ][ 2 ];

        glBegin( GL_LINE_STRIP );
        glVertex3d( x, y, z );
        for( i = 0; i < 5; ++i )
        {
            edge = tetrahedron_edge_list[ i ];
            vert = tetrahedron_edges[ edge ].v2;
            glNormal3dv( tetrahedron_edges[ edge ].normal );
            x = offset[ 0 ] + scale * tetrahedron_v[ vert ][ 0 ];
            y = offset[ 1 ] + scale * tetrahedron_v[ vert ][ 1 ];
            z = offset[ 2 ] + scale * tetrahedron_v[ vert ][ 2 ];
            glVertex3d( x, y, z );
        }
        glEnd( );
        glBegin( GL_LINES );
        for( i = 5; i < 7; ++i )
        {
            edge = tetrahedron_edge_list[ i ];
            vert = tetrahedron_edges[ edge ].v2;
            glNormal3dv( tetrahedron_edges[ edge ].normal );
            x = offset[ 0 ] + scale * tetrahedron_v[ vert ][ 0 ];
            y = offset[ 1 ] + scale * tetrahedron_v[ vert ][ 1 ];
            z = offset[ 2 ] + scale * tetrahedron_v[ vert ][ 2 ];
            glVertex3d( x, y, z );
        }
        glEnd( );
    }
    else
    {
        /* Use a local variable to avoid buildup of roundoff errors */
        GLdouble local_offset[ 3 ];
        num_levels--;
        scale /= 2.0;
        local_offset[ 0 ] = offset[ 0 ] + scale * tetrahedron_v[ 0 ][ 0 ];
        local_offset[ 1 ] = offset[ 1 ] + scale * tetrahedron_v[ 0 ][ 1 ];
        local_offset[ 2 ] = offset[ 2 ] + scale * tetrahedron_v[ 0 ][ 2 ];
        glutWireSierpinskiSponge( num_levels, local_offset, scale );
        local_offset[ 0 ] += scale;
        glutWireSierpinskiSponge( num_levels, local_offset, scale );
        local_offset[ 0 ] -= 0.5            * scale;
        local_offset[ 1 ] += 0.866025403784 * scale;
        glutWireSierpinskiSponge( num_levels, local_offset, scale );
        local_offset[ 1 ] -= 0.577350269189 * scale;
        local_offset[ 2 ] += 0.816496580928 * scale;
        glutWireSierpinskiSponge( num_levels, local_offset, scale );
    }
}

/*!
    \fn
    \brief    Draw a solid Spierspinski's sponge.
    \ingroup  geometry
    \param    num_levels    Recursive depth.
    \param    offset        Location vector.
    \param    scale         Relative size.

              This function recursively draws a few levels of
              a solid-shaded Sierpinski's Sponge.
              If \a num_levels is 0, draws 1 tetrahedron.
              The \a offset is a translation.
              The \a z axis is normal to the base.
              The sponge is centered at the origin.

    \note     Runtime is exponential in \a num_levels .

    \todo     Consider removing the \a offset parameter from the
              API (use a helper function).

    \see      glutWireSierpinskiSponge()
*/
void OGAPIENTRY glutSolidSierpinskiSponge(
    int num_levels, const GLdouble offset[ 3 ], GLdouble scale
)
{
    int i, j;

    if( !num_levels )
    {
        /* Maybe the base-case should be a glutSolidTetrahedron() call? */
        glBegin( GL_TRIANGLES );
        for( i = 0; i < NUM_FACES; i++ )
        {
            glNormal3dv( tetrahedron_n[ i ] );
            for( j = 0; j < 3; j++ )
            {
                double x = offset[ 0 ] +
                    scale * tetrahedron_v[ tetrahedron_i[ i ][ j ] ][ 0 ];
                double y = offset[ 1 ] +
                    scale * tetrahedron_v[ tetrahedron_i[ i ][ j ] ][ 1 ];
                double z = offset[ 2 ] +
                    scale * tetrahedron_v[ tetrahedron_i[ i ][ j ] ][ 2 ];
                glVertex3d( x, y, z );
            }
        }
        glEnd( );
    }
    else
    {
        /* Use a local variable to avoid buildup of roundoff errors */
        GLdouble local_offset[ 3 ];
        num_levels--;
        scale /= 2.0;
        local_offset[ 0 ] = offset[ 0 ] + scale * tetrahedron_v[ 0 ][ 0 ];
        local_offset[ 1 ] = offset[ 1 ] + scale * tetrahedron_v[ 0 ][ 1 ];
        local_offset[ 2 ] = offset[ 2 ] + scale * tetrahedron_v[ 0 ][ 2 ];
        glutSolidSierpinskiSponge( num_levels, local_offset, scale );
        local_offset[ 0 ] += scale;
        glutSolidSierpinskiSponge( num_levels, local_offset, scale );
        local_offset[ 0 ] -= 0.5            * scale;
        local_offset[ 1 ] += 0.866025403784 * scale;
        glutSolidSierpinskiSponge( num_levels, local_offset, scale );
        local_offset[ 1 ] -= 0.577350269189 * scale;
        local_offset[ 2 ] += 0.816496580928 * scale;
        glutSolidSierpinskiSponge( num_levels, local_offset, scale );
    }
}

#undef NUM_FACES
