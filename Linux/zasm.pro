#-------------------------------------------------
#
# Project created by QtCreator 2014-10-06T16:47:38
#
#-------------------------------------------------

TEMPLATE = app

QT       -= core
QT       -= gui

TARGET	= zasm
CONFIG += console
CONFIG -= app_bundle
CONFIG += c++11
CONFIG += precompiled_header


INCLUDEPATH +=          \
	./                  \
	Source              \
	Libraries           \

SOURCES += \
    Source/Error.cpp \
    Source/Label.cpp \
    Source/main.cpp \
    Source/Segment.cpp \
    Source/Source.cpp \
    Source/Z80Assembler.cpp \
	Source/Z80Head.cpp \
    Source/helpers.cpp \
    Source/outputfile.cpp \
    Source/listfile.cpp \
    Libraries/cstrings/cstrings.cpp \
    Libraries/kio/abort.cpp \
    Libraries/kio/errors.cpp \
    Libraries/kio/log.cpp \
    Libraries/unix/FD.cpp \
    Libraries/unix/tempmem.cpp \
    Libraries/unix/files.cpp \

HEADERS += \
    Source/Error.h \
    Source/Label.h \
    Source/Segment.h \
    Source/settings.h \
    Source/Source.h \
    Source/SyntaxError.h \
    Source/Z80Assembler.h \
    Source/settings.h \
	Source/Z80Head.h \
    config.h \
    Libraries/cstrings/base85.h \
    Libraries/cstrings/cstrings.h \
    Libraries/kio/abort.h \
    Libraries/kio/errors.h \
    Libraries/kio/kio.h \
    Libraries/kio/log.h \
    Libraries/unix/FD.h \
    Libraries/unix/tempmem.h \
    Libraries/unix/files.h \
	Libraries/kio/standard_types.h \
	Libraries/kio/peekpoke.h \
    Libraries/Templates/Array.h \
    Libraries/Templates/HashMap.h \
    Source/helpers.h

OTHER_FILES += \
    ../Examples/Jupiter Ace Character Ram.bin \
    ../Examples/Jupiter Ace Character Ram.asm \
    ../Examples/template_ace_sysvars.asm \
    ../Examples/template_ace.asm \
    ../Examples/template_bin.asm \
    ../Examples/template_minimal_rom.asm \
    ../Examples/template_o.asm \
    ../Examples/template_p.asm \
    ../Examples/template_rom.asm \
    ../Examples/template_sna.asm \
    ../Examples/template_tap.asm \
    ../Examples/template_z80.asm \
    ../Examples/template_zx80_sysvars.asm \
    ../Examples/template_zx81_sysvars.asm \
    ../Examples/template_zx82_sysvars.asm \
    ../Examples/zx82_rom.asm

