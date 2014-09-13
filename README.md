praxis
======

A live coding environment based on Lua, Lisp and Forth.

Compiling and running praxis in Linux
-------------------------------------

Prerequisites:

 - Qt (strictly speaking, only qmake is required, praxis is not dependent on Qt libraries)
 - The sdl library (at the time of writing I'm using version 1.2.15)
 - The freetype library (at the time of writing I'm using version 17.1.11)

Once you have these, you can compile praxis the usual way you would for a qmake based project, either from the command line with qmake then make, or by loading the project in Qt Creator.

When running praxis, praxis expects to see the following files in the current folder:

 - Bitstream-Vera-Sans-Mono.ttf
 - prod.lua

See the prods folder for runnable examples.

Compiling and running praxis in Windows
---------------------------------------

Prerequisites:

 - Qt (strictly speaking, only qmake is required, praxis is not dependent on Qt libraries)

Praxis links against the following static libraries:

 - fmodvc.lib (found in fmodapi375win/lib)
 - freetype235.lib (found in freetype-2.3.5/objs)

These files need to be copied to a folder called "libs" under the parent of the build folder. For example, if your build folder is in C:\builds\build-praxis-Qt\_5\_3-Release, the path of the libs folder will be C:\builds\libs.

You can compile praxis the usual way you would for a qmake based project, either from the command line with qmake then make, or by loading the project in Qt Creator.

When running praxis, praxis expects to see the following files in the current folder:

 - Bitstream-Vera-Sans-Mono.ttf
 - fmod.dll
 - music.mp3
 - prod.lua

See the prods folder for runnable examples.

Features
--------

Praxis features a mish-mash of features I've added over a long period of time.

 - OpenGL
 - Real-time audio generation
 - Midi
 - A voxel engine
 - A programmable text editor
 - lots more

See the end of luaCallbacks.cpp for available lua commands, and the folders in /prods for examples.
