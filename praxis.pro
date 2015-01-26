TEMPLATE = app
#CONFIG += console
CONFIG -= qt

INCLUDEPATH += ./MathLib/
INCLUDEPATH += ./World/
INCLUDEPATH += ./OpenGlut/include/
INCLUDEPATH += ./lua-5.1.4/src/
INCLUDEPATH += ./Voxel/
INCLUDEPATH += ./Neural/

win32 {
    DEFINES += __PRAXIS_WINDOWS__
    DEFINES += __WINDOWS_MM__
    DEFINES += FREETYPE2_STATIC
    INCLUDEPATH += ./freetype-2.3.5/include/
    INCLUDEPATH += ./fmodapi375win/inc/
    LIBS += ..\\libs\\fmodvc.lib
    LIBS += ..\\libs\\freetype235.lib
    LIBS += wsock32.lib
}

unix {
    DEFINES += __PRAXIS_LINUX__
    DEFINES += __LINUX_ALSASEQ__
    QMAKE_CXXFLAGS += $$system(freetype-config --cflags)
    QMAKE_CXXFLAGS += $$system(sdl-config --cflags)
    QMAKE_CXXFLAGS += -std=c++11
    message(output from freetype-config --cflags added to QMAKE_CXXFLAGS= $$QMAKE_CXXFLAGS)
    LIBS += $$system(freetype-config --libs)
    LIBS += $$system(sdl-config --libs)
    LIBS += -lGL -lGLU -lm -lX11 -ldl -lpthread
    #LIBS += -lsocket -lnsl
    CONFIG += link_pkgconfig
    PKGCONFIG += alsa
}

DEFINES += _CRT_SECURE_NO_WARNINGS

DEFINES -= UNICODE

SOURCES += main.cpp \
    OpenGlut/src/og_window.c \
    OpenGlut/src/og_videoresize.c \
    OpenGlut/src/og_teapot.c \
    OpenGlut/src/og_structure.c \
    OpenGlut/src/og_stroke_roman.c \
    OpenGlut/src/og_stroke_mono_roman.c \
    OpenGlut/src/og_state.c \
    OpenGlut/src/og_overlay.c \
    OpenGlut/src/og_misc.c \
    OpenGlut/src/og_menu.c \
    OpenGlut/src/og_main.c \
    OpenGlut/src/og_joystick.c \
    OpenGlut/src/og_init.c \
    OpenGlut/src/og_geometry.c \
    OpenGlut/src/og_gamemode.c \
    OpenGlut/src/og_font_data.c \
    OpenGlut/src/og_font.c \
    OpenGlut/src/og_ext.c \
    OpenGlut/src/og_display.c \
    OpenGlut/src/og_cursor.c \
    OpenGlut/src/og_callbacks.c \
    MathLib/ML_Vector.cpp \
    MathLib/ML_Transform.cpp \
    MathLib/ML_Quaternion.cpp \
    MathLib/ML_Matrix.cpp \
    MathLib/ML_Maths.cpp \
    MathLib/ML_HermiteCurve.cpp \
    Voxel/VoxelBlock.cpp \
    Voxel/Voxel.cpp \
    World.cpp \
    SingleWorldConfiguration.cpp \
    PolyGlyph.cpp \
    GLEditor.cpp \
    luaInterface.cpp \
    luaCallbacks.cpp \
    Neural/AI_Synapse.cpp \
    Neural/AI_Neuron.cpp \
    Neural/AI_NeuralNetworkSystem.cpp \
    Neural/AI_NeuralNetworkPatternTrainer.cpp \
    Neural/AI_NeuralNetwork.cpp \
    Neural/UT_String.cpp \
    Neural/UT_Functions.cpp \
    lpeg-0.12/lpcap.c \
    lpeg-0.12/lpcode.c \
    lpeg-0.12/lpprint.c \
    lpeg-0.12/lptree.c \
    lpeg-0.12/lpvm.c \
    lua-5.1.4/src/lapi.c \
    lua-5.1.4/src/lauxlib.c \
    lua-5.1.4/src/lbaselib.c \
    lua-5.1.4/src/lcode.c \
    lua-5.1.4/src/ldblib.c \
    lua-5.1.4/src/ldebug.c \
    lua-5.1.4/src/ldo.c \
    lua-5.1.4/src/ldump.c \
    lua-5.1.4/src/lfunc.c \
    lua-5.1.4/src/lgc.c \
    lua-5.1.4/src/linit.c \
    lua-5.1.4/src/liolib.c \
    lua-5.1.4/src/llex.c \
    lua-5.1.4/src/lmathlib.c \
    lua-5.1.4/src/lmem.c \
    lua-5.1.4/src/loadlib.c \
    lua-5.1.4/src/lobject.c \
    lua-5.1.4/src/lopcodes.c \
    lua-5.1.4/src/loslib.c \
    lua-5.1.4/src/lparser.c \
    lua-5.1.4/src/lstate.c \
    lua-5.1.4/src/lstring.c \
    lua-5.1.4/src/lstrlib.c \
    lua-5.1.4/src/ltable.c \
    lua-5.1.4/src/ltablib.c \
    lua-5.1.4/src/ltm.c \
    lua-5.1.4/src/lundump.c \
    lua-5.1.4/src/lvm.c \
    lua-5.1.4/src/lzio.c \
    s7/s7.c \
    lispCallbacks.cpp \
    lispInterface.cpp \
    audioWave.cpp \
    RtMidi.cpp \
    forthCallbacks.cpp \
    forthInterface.cpp \
    ficl-4.1.0/bit.c \
    ficl-4.1.0/callback.c \
    ficl-4.1.0/compatibility.c \
    ficl-4.1.0/dictionary.c \
    ficl-4.1.0/double.c \
    ficl-4.1.0/extras.c \
    ficl-4.1.0/fileaccess.c \
    ficl-4.1.0/float.c \
    ficl-4.1.0/hash.c \
    ficl-4.1.0/lzcompress.c \
    ficl-4.1.0/lzuncompress.c \
    ficl-4.1.0/prefix.c \
    ficl-4.1.0/primitives.c \
    ficl-4.1.0/search.c \
    ficl-4.1.0/softcore.c \
    ficl-4.1.0/stack.c \
    ficl-4.1.0/system.c \
    ficl-4.1.0/tools.c \
    ficl-4.1.0/utility.c \
    ficl-4.1.0/vm.c \
    ficl-4.1.0/word.c

unix {
SOURCES += \
    ficl-4.1.0/ficlplatform/unix.c
}

win32 {
SOURCES += \
    audioMidi.cpp \
    ficl-4.1.0/ficlplatform/win32.c
}

HEADERS += \
    OpenGlut/include/GL/openglut_std.h \
    OpenGlut/include/GL/openglut_ext.h \
    OpenGlut/include/GL/openglut_exp.h \
    OpenGlut/include/GL/openglut.h \
    MathLib/ML_Vector.h \
    MathLib/ML_Types.h \
    MathLib/ML_Transform.h \
    MathLib/ML_Quaternion.h \
    MathLib/ML_Matrix.h \
    MathLib/ML_Maths.h \
    MathLib/ML_HermiteCurve.h \
    Voxel/VoxelBlock.h \
    Voxel/Voxel.h \
    World.h \
    SingleWorldConfiguration.h \
    PolyGlyph.h \
    GLEditor.h \
    luaInterface.h \
    luaCallbacks.h \
    Neural/AI_Synapse.h \
    Neural/AI_Neuron.h \
    Neural/AI_NeuralNetworkSystem.h \
    Neural/AI_NeuralNetworkPatternTrainer.h \
    Neural/AI_NeuralNetwork.h \
    Neural/UT_String.h \
    Neural/UT_Functions.h \
    lpeg-0.12/lpcap.h \
    lpeg-0.12/lpcode.h \
    lpeg-0.12/lpprint.h \
    lpeg-0.12/lptree.h \
    lpeg-0.12/lptypes.h \
    lpeg-0.12/lpvm.h \
    lua-5.1.4/src/lapi.h \
    lua-5.1.4/src/lauxlib.h \
    lua-5.1.4/src/lcode.h \
    lua-5.1.4/src/ldebug.h \
    lua-5.1.4/src/ldo.h \
    lua-5.1.4/src/lfunc.h \
    lua-5.1.4/src/lgc.h \
    lua-5.1.4/src/llex.h \
    lua-5.1.4/src/llimits.h \
    lua-5.1.4/src/lmem.h \
    lua-5.1.4/src/lobject.h \
    lua-5.1.4/src/lopcodes.h \
    lua-5.1.4/src/lparser.h \
    lua-5.1.4/src/lstate.h \
    lua-5.1.4/src/lstring.h \
    lua-5.1.4/src/ltable.h \
    lua-5.1.4/src/ltm.h \
    lua-5.1.4/src/lua.h \
    lua-5.1.4/src/luaconf.h \
    lua-5.1.4/src/lualib.h \
    lua-5.1.4/src/lundump.h \
    lua-5.1.4/src/lvm.h \
    lua-5.1.4/src/lzio.h \
    s7/s7.h \
    lispCallbacks.h \
    lispInterface.h \
    RtMidi.h \
    RtError.h \
    forthCallbacks.h \
    forthInterface.h \
    ficl-4.1.0/ficl.h \
    ficl-4.1.0/ficlcompatibility.h \
    ficl-4.1.0/ficllocal.h \
    ficl-4.1.0/ficltokens.h

unix {
HEADERS += \
    ficl-4.1.0/ficlplatform/unix.h
}

win32 {
HEADERS += \
    ficl-4.1.0/ficlplatform/win32.h
}
