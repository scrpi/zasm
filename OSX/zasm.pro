#-------------------------------------------------
#
# Project created by QtCreator 2014-10-06T16:47:38
#
#-------------------------------------------------

QT       += core
QT       -= gui

TEMPLATE = app

TARGET	= zasm
CONFIG += console
CONFIG -= app_bundle
CONFIG += c++11
CONFIG += precompiled_header

QMAKE_MAC_SDK = macosx10.9


INCLUDEPATH +=          \
	./                  \
	Source              \
	Libraries           \
	Libraries/kio       \
	Libraries/cstrings  \
	Libraries/sort      \
	Libraries/unix      \
	Libraries/Templates \

SOURCES += \
    Source/Error.cpp \
    Source/Label.cpp \
    Source/main.cpp \
    Source/Segment.cpp \
    Source/Source.cpp \
    Source/Z80Assembler.cpp \
    ../../../Libraries/cstrings/cstrings.cpp \
    ../../../Libraries/kio/abort.cpp \
    ../../../Libraries/kio/errors.cpp \
    ../../../Libraries/kio/log.cpp \
    ../../../Libraries/unix/FD.cpp \
    ../../../Libraries/unix/tempmem.cpp \
    ../../../Libraries/unix/files.cpp

HEADERS

HEADERS += \
    Source/Error.h \
    Source/Label.h \
    Source/Segment.h \
    Source/settings.h \
    Source/Source.h \
    Source/SyntaxError.h \
    Source/Z80Assembler.h \
    Source/settings.h \
    config.h \
    ../../../Libraries/cstrings/base85.h \
    ../../../Libraries/cstrings/cstrings.h \
    ../../../Libraries/kio/abort.h \
    ../../../Libraries/kio/errors.h \
    ../../../Libraries/kio/kio.h \
    ../../../Libraries/kio/log.h \
    ../../../Libraries/unix/FD.h \
    ../../../Libraries/unix/tempmem.h \
    ../../../Libraries/unix/files.h \
	../../../Libraries/kio/standard_types.h \
	../../../Libraries/kio/peekpoke.h







