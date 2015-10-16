TEMPLATE = app
CONFIG += console
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
    DEFINES += _ALLOW_KEYWORD_MACROS
    INCLUDEPATH += ./freetype-2.3.5/include/
    INCLUDEPATH += ./fmodapi375win/inc/

    #message($$_PRO_FILE_PWD_)

    LIBS += $$_PRO_FILE_PWD_/fmodapi375win/lib/fmodvc.lib
    LIBS += $$_PRO_FILE_PWD_/freetype-2.3.5/objs/freetype235.lib
    #LIBS += ..\\libs\\fmodvc.lib
    #LIBS += ..\\libs\\freetype235.lib
    LIBS += wsock32.lib
    LIBS += Shell32.lib
}

unix {
    DEFINES += __PRAXIS_LINUX__
    DEFINES += __LINUX_ALSASEQ__
    QMAKE_CXXFLAGS += $$system(freetype-config --cflags)
    QMAKE_CXXFLAGS += $$system(sdl-config --cflags)
    QMAKE_CXXFLAGS += -std=c++11

    LIBS += $$system(freetype-config --libs)
    LIBS += $$system(sdl-config --libs)
    LIBS += -lGL -lGLU -lm -lX11 -ldl -lpthread
    #LIBS += -lsocket -lnsl
    CONFIG += link_pkgconfig
    PKGCONFIG += alsa

    #QMAKE_CFLAGS_RELEASE -= -O2

    message(output from freetype-config --cflags added to QMAKE_CXXFLAGS= $$QMAKE_CXXFLAGS)
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
    ficl-4.1.0/word.c \
    io/BStream.c \
    io/BStreamTag.c \
    io/cdecode.c \
    io/cencode.c \
    io/CHash.c \
    io/Collector.c \
    io/CollectorMarker.c \
    io/Common.c \
    io/context.c \
    io/Coro.c \
    io/Date.c \
    io/Duration.c \
    io/DynLib.c \
    io/Hash_fnv.c \
    io/Hash_murmur.c \
    io/Hash_superfast.c \
    io/IoBlock.c \
    io/IoCall.c \
    io/IoCFunction.c \
    io/IoCollector.c \
    io/IoCompiler.c \
    io/IoCoroutine.c \
    io/IoDate.c \
    io/IoDebugger.c \
    io/IoDirectory.c \
    io/IoDuration.c \
    io/IoDynLib.c \
    io/IoError.c \
    io/IoFile.c \
    io/IoFile_stat.c \
    io/IoLexer.c \
    io/IoList.c \
    io/IoMap.c \
    io/IoMessage.c \
    io/IoMessage_opShuffle.c \
    io/IoMessage_parser.c \
    io/IoNumber.c \
    io/IoObject.c \
    io/IoObject_flow.c \
    io/IoProfiler.c \
    io/IoSandbox.c \
    io/IoSeq.c \
    io/IoSeq_immutable.c \
    io/IoSeq_mutable.c \
    io/IoSeq_vector.c \
    io/IoState.c \
    io/IoState_callbacks.c \
    io/IoState_coros.c \
    io/IoState_debug.c \
    io/IoState_eval.c \
    io/IoState_exceptions.c \
    io/IoState_symbols.c \
    io/IoSystem.c \
    io/IoTag.c \
    io/IoToken.c \
    io/IoVMInit.c \
    io/IoWeakLink.c \
    io/List.c \
    io/MainArgs.c \
    io/PHash.c \
    io/PointerHash.c \
    io/PortableGettimeofday.c \
    io/PortableSnprintf.c \
    io/PortableSorting.c \
    io/PortableStrlcpy.c \
    io/PortableStrptime.c \
    io/PortableTruncate.c \
    io/PortableUsleep.c \
    io/RandomGen.c \
    io/UArray.c \
    io/UArray_character.c \
    io/UArray_format.c \
    io/UArray_math.c \
    io/UArray_path.c \
    io/UArray_stream.c \
    io/UArray_string.c \
    io/UArray_utf.c \
    io/ucs2.c \
    io/ucs4.c \
    io/utf_convert.c \
    io/utf8.c \
    ioInterface.cpp \
    ioCallbacks.cpp \
    io/io_main.c \
    io/Stack_io.c \
    PraxisTexture.cpp \
    PraxisLog.cpp \
    PraxisServer.cpp \
    pforth/pf_cglue.c \
    pforth/pf_clib.c \
    pforth/pf_core.c \
    pforth/pf_inner.c \
    pforth/pf_io.c \
    pforth/pf_io_none.c \
    pforth/pf_main.c \
    pforth/pf_mem.c \
    pforth/pf_save.c \
    pforth/pf_text.c \
    pforth/pf_words.c \
    pforth/pfcompil.c \
    pforth/pfcustom.c \
    luaCBOpenGL.cpp \
    luaCBEditor.cpp \
    luaCBServer.cpp \
    luaCBTransform.cpp \
    luaCBTexture.cpp \
    luaCBMp3.cpp \
    luaCBWindow.cpp \
    luaCBClipboard.cpp \
    luaCBVoxel.cpp \
    luaCBSynth.cpp \
    luaCBMidi.cpp \
    luaCBWorld.cpp \
    luaCBLang.cpp \
    luaCBJoystick.cpp \
    luaCBSystem.cpp

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
    ficl-4.1.0/ficltokens.h \
    io/386-ucontext.h \
    io/amd64-ucontext.h \
    io/Base.h \
    io/BStream.h \
    io/BStreamTag.h \
    io/cdecode.h \
    io/cencode.h \
    io/CHash.h \
    io/CHash_inline.h \
    io/Collector.h \
    io/Collector_inline.h \
    io/CollectorMarker.h \
    io/CollectorMarker_inline.h \
    io/Common.h \
    io/Common_inline.h \
    io/Coro.h \
    io/Date.h \
    io/Duration.h \
    io/DynLib.h \
    io/Hash_fnv.h \
    io/Hash_murmur.h \
    io/Hash_superfast.h \
    io/IoBlock.h \
    io/IoCall.h \
    io/IoCFunction.h \
    io/IoCollector.h \
    io/IoCompiler.h \
    io/IoConfig.h \
    io/IoContext.h \
    io/IoCoroutine.h \
    io/IoDate.h \
    io/IoDebugger.h \
    io/IoDirectory.h \
    io/IoDuration.h \
    io/IoDynLib.h \
    io/IoError.h \
    io/IoFile.h \
    io/IoFile_stat.h \
    io/IoInstallPrefix.h \
    io/IoLexer.h \
    io/IoList.h \
    io/IoMap.h \
    io/IoMessage.h \
    io/IoMessage_inline.h \
    io/IoMessage_opShuffle.h \
    io/IoMessage_parser.h \
    io/IoNumber.h \
    io/IoObject.h \
    io/IoObject_flow.h \
    io/IoObject_inline.h \
    io/IoObject_struct.h \
    io/IoProfiler.h \
    io/IoSandbox.h \
    io/IoSeq.h \
    io/IoSeq_immutable.h \
    io/IoSeq_inline.h \
    io/IoSeq_mutable.h \
    io/IoSeq_vector.h \
    io/IoState.h \
    io/IoState_callbacks.h \
    io/IoState_coros.h \
    io/IoState_debug.h \
    io/IoState_eval.h \
    io/IoState_exceptions.h \
    io/IoState_inline.h \
    io/IoState_symbols.h \
    io/IoSystem.h \
    io/IoTag.h \
    io/IoTag_inline.h \
    io/IoToken.h \
    io/IoVersion.h \
    io/IoVM.h \
    io/IoVMApi.h \
    io/IoWeakLink.h \
    io/List.h \
    io/List_inline.h \
    io/MainArgs.h \
    io/PHash.h \
    io/PHash_inline.h \
    io/PHash_struct.h \
    io/PointerHash.h \
    io/PointerHash_inline.h \
    io/PointerHash_struct.h \
    io/PortableGettimeofday.h \
    io/PortableSorting.h \
    io/PortableStdint.h \
    io/PortableStrlcpy.h \
    io/PortableStrptime.h \
    io/PortableTruncate.h \
    io/PortableUsleep.h \
    io/power-ucontext.h \
    io/RandomGen.h \
    io/simd_cp.h \
    io/simd_cp_arm-iwmmx.h \
    io/simd_cp_emu.h \
    io/simd_cp_x86.h \
    io/Stack.h \
    io/Stack_inline.h \
    io/taskimpl.h \
    io/UArray.h \
    io/UArray_character.h \
    io/UArray_format.h \
    io/UArray_math.h \
    io/UArray_path.h \
    io/UArray_stream.h \
    io/UArray_string.h \
    io/UArray_utf.h \
    io/utf_convert.h \
    io/utf8.h \
    io/utf8internal.h \
    ioInterface.h \
    ioCallbacks.h \
    PraxisTexture.h \
    PraxisLog.h \
    PraxisServer.h \
    pforth/pf_all.h \
    pforth/pf_cglue.h \
    pforth/pf_clib.h \
    pforth/pf_core.h \
    pforth/pf_float.h \
    pforth/pf_guts.h \
    pforth/pf_host.h \
    pforth/pf_inc1.h \
    pforth/pf_io.h \
    pforth/pf_mem.h \
    pforth/pf_save.h \
    pforth/pf_text.h \
    pforth/pf_types.h \
    pforth/pf_win32.h \
    pforth/pf_words.h \
    pforth/pfcompfp.h \
    pforth/pfcompil.h \
    pforth/pfdicdat_arm.h \
    pforth/pfinnrfp.h \
    pforth/pforth.h \
    luaCB.h

unix {
HEADERS += \
    ficl-4.1.0/ficlplatform/unix.h
}

win32 {
HEADERS += \
    ficl-4.1.0/ficlplatform/win32.h
}
