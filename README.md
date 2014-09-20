praxis
======

A live coding environment based on Lua, Lisp and Forth.

Although mainly Lua at the moment.

Videos
------

Introduction: https://www.youtube.com/watch?v=1VRtRazMYSA

Running the examples: https://www.youtube.com/watch?v=6rB39AXPmQQ

Camera control: https://www.youtube.com/watch?v=ezWaNu-FMPk

Lua and audio demo: https://www.youtube.com/watch?v=0uEBs98PKQE

Lisp demo: https://www.youtube.com/watch?v=PGNEuq3XL7c

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

To use the midi commands you need a midi server running. I've been using timidity for this by invoking the following command in a terminal:

timidity -iA -B2,8 -Os1l -s 44100


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

Usage guide
-----------

 - Ctrl-Enter runs the Lua block which the cursor is in. If the cursor is in a multi-line block, this will be indicated by faint highlighting. If there is no highlighting, the Lua block is the code on the current line.
 - The Lua block will be the function or do-end block surrounding the cursor
 - Shift-Enter runs the Lua block, but first moves to the end of the block and inserts a line. This is useful for code which inserts text into the buffer.
 - F1 runs the entire buffer
 - The function print writes text to the trace output. This appears on the right and in 3D in the world.
 - When an error message appears, you can use clearError() to clear it
 - The function print2 writes text to the buffer at the current cursor position. Use Shift-Enter when issuing this command, or else the text that gets printed will be inserted wherever your cursor is, which may be inside your code.
 - To load a file into the edit buffer, the command is loadBuffer(<filename>)
 - Every frame, the Lua functions render() and update() are called
 - To bring up a function definition, use print2(getFunction(<function name as a string>)) then Shift-Enter
 - For example, to bring up the definition of getFunction, type print2(getFunction("getFunction"))
 - getFunction returns 2 items - the function text and the debug info.
 - Inspect the 2nd parameter like this: s,t = getFunction("update") print2(inspect(t))
 - The function keys can be redefined. f1Pressed() to f12Pressed() are the functions that get called when you press a function key. These can be freely inspected (using the tip above) and redefined.

Blog
----

http://createuniverses.wordpress.com/
