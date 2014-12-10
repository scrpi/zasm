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
CONFIG(release,debug|release) { DEFINES += NDEBUG }


QMAKE_MAC_SDK = macosx10.9


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
	Source/CharMap.cpp \
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
    Libraries/Z80/Z80_clock_cycles.cpp \

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
	Source/CharMap.h \
    Source/helpers.h \
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
    Libraries/Z80/Z80_clock_cycles.h \

OTHER_FILES += \
    ../.gitignore \
    ../Examples/sdcc/device/lib/___setjmp.s \
    ../Examples/sdcc/device/lib/__divsint.s \
    ../Examples/sdcc/device/lib/__divsuchar.s \
    ../Examples/sdcc/device/lib/__divuint.s \
    ../Examples/sdcc/device/lib/__divuschar.s \
    ../Examples/sdcc/device/lib/__modsint.s \
    ../Examples/sdcc/device/lib/__modsuchar.s \
    ../Examples/sdcc/device/lib/__moduint.s \
    ../Examples/sdcc/device/lib/__mulint.s \
    ../Examples/sdcc/device/lib/__mulschar.s \
    ../Examples/sdcc/device/lib/__sdcc_call_hl.s \
    ../Examples/sdcc/device/lib/_localtime.s \
    ../Examples/sdcc/device/lib/_memmove.s \
    ../Examples/sdcc/device/lib/_putchar.s \
    ../Examples/sdcc/device/lib/_strcpy.s \
    ../Examples/sdcc/device/lib/_strlen.s \
    ../Examples/sdcc/device/lib/crt0 .s \
    ../Examples/sdcc/device/lib/heap .s \
    ../Examples/sdcc/device/lib/___fs2schar.c \
    ../Examples/sdcc/device/lib/___fs2sint.c \
    ../Examples/sdcc/device/lib/___fs2slong.c \
    ../Examples/sdcc/device/lib/___fs2uchar.c \
    ../Examples/sdcc/device/lib/___fs2uint.c \
    ../Examples/sdcc/device/lib/___fs2ulong.c \
    ../Examples/sdcc/device/lib/___fsadd.c \
    ../Examples/sdcc/device/lib/___fsdiv.c \
    ../Examples/sdcc/device/lib/___fseq.c \
    ../Examples/sdcc/device/lib/___fsgt.c \
    ../Examples/sdcc/device/lib/___fslt.c \
    ../Examples/sdcc/device/lib/___fsmul.c \
    ../Examples/sdcc/device/lib/___fsneq.c \
    ../Examples/sdcc/device/lib/___fssub.c \
    ../Examples/sdcc/device/lib/___schar2fs.c \
    ../Examples/sdcc/device/lib/___sint2fs.c \
    ../Examples/sdcc/device/lib/___slong2fs.c \
    ../Examples/sdcc/device/lib/___uchar2fs.c \
    ../Examples/sdcc/device/lib/___uint2fs.c \
    ../Examples/sdcc/device/lib/___ulong2fs.c \
    ../Examples/sdcc/device/lib/__assert.c \
    ../Examples/sdcc/device/lib/__divslong.c \
    ../Examples/sdcc/device/lib/__divslonglong.c \
    ../Examples/sdcc/device/lib/__divulong.c \
    ../Examples/sdcc/device/lib/__divulonglong.c \
    ../Examples/sdcc/device/lib/__itoa.c \
    ../Examples/sdcc/device/lib/__modslong.c \
    ../Examples/sdcc/device/lib/__modulong.c \
    ../Examples/sdcc/device/lib/__mullong.c \
    ../Examples/sdcc/device/lib/__mullonglong.c \
    ../Examples/sdcc/device/lib/__print_format.c \
    ../Examples/sdcc/device/lib/__rlslonglong.c \
    ../Examples/sdcc/device/lib/__rlulonglong.c \
    ../Examples/sdcc/device/lib/__rrslonglong.c \
    ../Examples/sdcc/device/lib/__rrulonglong.c \
    ../Examples/sdcc/device/lib/__uitoa.c \
    ../Examples/sdcc/device/lib/_abs.c \
    ../Examples/sdcc/device/lib/_acosf.c \
    ../Examples/sdcc/device/lib/_asctime.c \
    ../Examples/sdcc/device/lib/_asincosf.c \
    ../Examples/sdcc/device/lib/_asinf.c \
    ../Examples/sdcc/device/lib/_atan2f.c \
    ../Examples/sdcc/device/lib/_atanf.c \
    ../Examples/sdcc/device/lib/_atof.c \
    ../Examples/sdcc/device/lib/_atoi.c \
    ../Examples/sdcc/device/lib/_atol.c \
    ../Examples/sdcc/device/lib/_calloc.c \
    ../Examples/sdcc/device/lib/_ceilf.c \
    ../Examples/sdcc/device/lib/_check_struct_tm.c \
    ../Examples/sdcc/device/lib/_cosf.c \
    ../Examples/sdcc/device/lib/_coshf.c \
    ../Examples/sdcc/device/lib/_cotf.c \
    ../Examples/sdcc/device/lib/_ctime.c \
    ../Examples/sdcc/device/lib/_days_per_month.c \
    ../Examples/sdcc/device/lib/_errno.c \
    ../Examples/sdcc/device/lib/_expf.c \
    ../Examples/sdcc/device/lib/_fabsf.c \
    ../Examples/sdcc/device/lib/_floorf.c \
    ../Examples/sdcc/device/lib/_free.c \
    ../Examples/sdcc/device/lib/_frexpf.c \
    ../Examples/sdcc/device/lib/_gets.c \
    ../Examples/sdcc/device/lib/_gmtime.c \
    ../Examples/sdcc/device/lib/_heap.c \
    ../Examples/sdcc/device/lib/_isalnum.c \
    ../Examples/sdcc/device/lib/_isalpha.c \
    ../Examples/sdcc/device/lib/_isblank.c \
    ../Examples/sdcc/device/lib/_iscntrl.c \
    ../Examples/sdcc/device/lib/_isdigit.c \
    ../Examples/sdcc/device/lib/_isgraph.c \
    ../Examples/sdcc/device/lib/_islower.c \
    ../Examples/sdcc/device/lib/_isprint.c \
    ../Examples/sdcc/device/lib/_ispunct.c \
    ../Examples/sdcc/device/lib/_isspace.c \
    ../Examples/sdcc/device/lib/_isupper.c \
    ../Examples/sdcc/device/lib/_isxdigit.c \
    ../Examples/sdcc/device/lib/_labs.c \
    ../Examples/sdcc/device/lib/_ldexpf.c \
    ../Examples/sdcc/device/lib/_log10f.c \
    ../Examples/sdcc/device/lib/_logf.c \
    ../Examples/sdcc/device/lib/_ltoa.c \
    ../Examples/sdcc/device/lib/_malloc.c \
    ../Examples/sdcc/device/lib/_memchr.c \
    ../Examples/sdcc/device/lib/_memcmp.c \
    ../Examples/sdcc/device/lib/_memcpy.c \
    ../Examples/sdcc/device/lib/_memset.c \
    ../Examples/sdcc/device/lib/_mktime.c \
    ../Examples/sdcc/device/lib/_modff.c \
    ../Examples/sdcc/device/lib/_powf.c \
    ../Examples/sdcc/device/lib/_printf_small.c \
    ../Examples/sdcc/device/lib/_printf.c \
    ../Examples/sdcc/device/lib/_put_char_to_stdout.c \
    ../Examples/sdcc/device/lib/_put_char_to_string.c \
    ../Examples/sdcc/device/lib/_puts.c \
    ../Examples/sdcc/device/lib/_rand.c \
    ../Examples/sdcc/device/lib/_realloc.c \
    ../Examples/sdcc/device/lib/_sincosf.c \
    ../Examples/sdcc/device/lib/_sincoshf.c \
    ../Examples/sdcc/device/lib/_sinf.c \
    ../Examples/sdcc/device/lib/_sinhf.c \
    ../Examples/sdcc/device/lib/_sprintf.c \
    ../Examples/sdcc/device/lib/_sqrtf.c \
    ../Examples/sdcc/device/lib/_strcat.c \
    ../Examples/sdcc/device/lib/_strchr.c \
    ../Examples/sdcc/device/lib/_strcmp.c \
    ../Examples/sdcc/device/lib/_strcspn.c \
    ../Examples/sdcc/device/lib/_strncat.c \
    ../Examples/sdcc/device/lib/_strncmp.c \
    ../Examples/sdcc/device/lib/_strncpy.c \
    ../Examples/sdcc/device/lib/_strpbrk.c \
    ../Examples/sdcc/device/lib/_strrchr.c \
    ../Examples/sdcc/device/lib/_strspn.c \
    ../Examples/sdcc/device/lib/_strstr.c \
    ../Examples/sdcc/device/lib/_strtok.c \
    ../Examples/sdcc/device/lib/_strxfrm.c \
    ../Examples/sdcc/device/lib/_tancotf.c \
    ../Examples/sdcc/device/lib/_tanf.c \
    ../Examples/sdcc/device/lib/_tanhf.c \
    ../Examples/sdcc/device/lib/_time.c \
    ../Examples/sdcc/device/lib/_tolower.c \
    ../Examples/sdcc/device/lib/_toupper.c \
    ../Examples/sdcc/device/lib/_vprintf.c \
    ../Examples/sdcc/device/lib/_vsprintf.c \
    ../Examples/sdcc/device/lib/_log_table.h \
    \
    ../Examples/sdcc/device/include/asm/default/features.h \
    ../Examples/sdcc/device/include/asm/z80/features.h \
    ../Examples/sdcc/device/include/assert.h \
    ../Examples/sdcc/device/include/ctype.h \
    ../Examples/sdcc/device/include/errno.h \
    ../Examples/sdcc/device/include/float.h \
    ../Examples/sdcc/device/include/iso646.h \
    ../Examples/sdcc/device/include/limits.h \
    ../Examples/sdcc/device/include/malloc.h \
    ../Examples/sdcc/device/include/math.h \
    ../Examples/sdcc/device/include/sdcc-lib.h \
    ../Examples/sdcc/device/include/setjmp.h \
    ../Examples/sdcc/device/include/stdalign.h \
    ../Examples/sdcc/device/include/stdarg.h \
    ../Examples/sdcc/device/include/stdbool.h \
    ../Examples/sdcc/device/include/stddef.h \
    ../Examples/sdcc/device/include/stdint.h \
    ../Examples/sdcc/device/include/stdio.h \
    ../Examples/sdcc/device/include/stdlib.h \
    ../Examples/sdcc/device/include/stdnoreturn.h \
    ../Examples/sdcc/device/include/string.h \
    ../Examples/sdcc/device/include/time.h \
    ../Examples/sdcc/device/include/tinibios.h \
    ../Examples/sdcc/device/include/typeof.h \
	\
    ../Examples/main.c \
    ../Examples/globls.s \
    ../Examples/sdcc_info.txt \
    \
    ../Examples/jupiter_ace_character_ram.s \
    ../Examples/jupiter_ace_sysvars.s \
    ../Examples/zx80_sysvars.s \
    ../Examples/zx81_sysvars.s \
    ../Examples/zx_spectrum_basic_tokens.s \
    ../Examples/zx_spectrum_sysvars.s \
    \
    ../Examples/template_bin.asm \
    ../Examples/template_minimal_rom.asm \
    ../Examples/template_rom_with_c_code.asm \
    ../Examples/template_o.asm \
    ../Examples/template_p.asm \
    ../Examples/template_rom.asm \
    ../Examples/template_sna.asm \
    ../Examples/template_tap.asm \
    ../Examples/template_z80.asm \
    ../Examples/template_ace.asm \
    \
    ../Test/main.c \
    ../Test/main.s \
    ../Test/test-cc.asm \
    ../Test/test-cc.lst \
    ../Test/test.asm \
    ../Test/test.lst \
    ../Test/test-opcodes.asm \
    ../Test/test-tap.asm \
    ../Test/test-tap.lst \
    ../Test/zx82_rom.asm \
    ../Test/zx82_rom.lst \
    ../Examples/zx_spectrum_io_rom.s










