#ifndef  __OPENGLUT_EXP_H__
#define  __OPENGLUT_EXP_H__

/*
 * openglut_exp.h
 *
 * The experimental interfaces to the OpenGLUT library.
 *
 * You should only include this file if you are using experimental
 * OpenGLUT features.  The OpenGLUT project may change or remove
 * any feature in this file, without published warning.  There may
 * not be a corresponding library major version bump to indicate
 * this change, as these are not strictly part of the API.  We
 * appreciate feedback on the utility, desirability, problems, or
 * even pointlessness of features herein, of course.  And, though
 * we discourage depending upon these features in your own releases,
 * we certainly encourage you to try these out to judge their merit.
 *
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
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
 * THE OPENGLUT PROJECT BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */


#ifdef __cplusplus
extern "C" 
{
#endif


/*
 * GLUT API Extension macro definitions -- display mode
 */
#define GLUT_BORDERLESS                     0x0800

/*
 * OpenGLUT experimental functions.
 */
OGAPI int glutCreateMenuWindow( int parent, int x, int y, int w, int h );

OGAPI void glutSetWindowStayOnTop( int enable );

/*
 * Allow for conditional compilation of experimental features.
 */

#define OPENGLUT_MENUWINDOW
#define OPENGLUT_STAYONTOP

#ifdef __cplusplus
}
#endif

#endif
