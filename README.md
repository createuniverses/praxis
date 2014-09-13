praxis
======

A live coding environment based on Lua, Lisp and Forth

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

