




#include <stdlib.h>
#include <stdio.h>

#include <string.h>
#include <sdcc-lib.h>

char bu1[10];
char bu2[10];
char str3[10] ="123456789";
volatile int a_counter = 0;

const char so_many_days_per_month[] = { 31,28,31,30,31,30,31,31,30,31,30,31 };


// #pragma std_sdcc99
// //bool requires std-c99 or std-sdcc99 or better
// #include "stdbool.h"
// bool f;

	void Intr(void) __naked __interrupt 0
	{
		__asm__(" exx\n");
		a_counter++;
		__asm__(" exx\n reti\n");
	}

	void NMI_Intr(void) __critical __interrupt
	{
		a_counter++;
	}

void main()
{
//	int i;

	puts("Hello C World!");
	puts("Say 'Hi' to Kio");
	puts("");

#ifdef __STDC_VERSION__
// __STDC_VERSION__ ist erst ab c99 definiert:
//	printf("__STDC_VERSION__ = %l\n",(long)__STDC_VERSION__);		TODO: DEBUG
	printf("__STDC_VERSION__ = %i%02i\n",(int)(__STDC_VERSION__/100),(int)(__STDC_VERSION__%100));
#else
	puts("__STDC_VERSION__ undef (c89)");
#endif

	if(strcmp("aa","aa")) printf("");		// --> _strcmp.c
	memcpy(bu1,bu2,10);						// --> __builtin_memcpy ((inline))
	strcpy(bu2,str3);						// --> __builtin_strcpy ((inline))

// __SDCC hat auch einen Wert,
// das ist aber ein nicht weiter definierter Text à la SDCC_3_2_1
// und keine Zahl oder String. Schwer irgendwas damit zu machen.
#ifdef __SDCC					// defined by sdcc
	printf("__SDCC isdef\n");
#else    //"12345678901234567890123456789012"
	#error
#endif

#ifdef __SDCC_z80				// --mz80
	printf("__SDCC_z80 isdef\n");
#else    //"12345678901234567890123456789012"
	#error
#endif

#ifdef __SDCC_STACK_AUTO		// --stack-auto
	printf("__SDCC_STACK_AUTO isdef\n");			// default = defined
#else    //"12345678901234567890123456789012"
	printf("__SDCC_STACK_AUTO ISNDEF\n");
#endif

#ifdef __SDCC_CHAR_UNSIGNED		// --funsigned-char
	printf("__SDCC_CHAR_UNSIGNED ISDEF\n");
#else    //"12345678901234567890123456789012"
	printf("__SDCC_CHAR_UNSIGNED isndef\n");		// default = signed char
#endif

#ifdef __SDCC_ALL_CALLEE_SAVES	// --all-callee-saves
	printf("__SDCC_ALL_CALLEE_SAVES ISDEF\n");
#else    //"12345678901234567890123456789012"
	printf("__SDCC_ALL_CALLEE_SAVES isndef\n");		// default = undefined
#endif

#ifdef __SDCC_FLOAT_REENTRANT	// --float-reentrant
	printf("__SDCC_FLOAT_REENTRANT isdef\n");
#else    //"12345678901234567890123456789012"
	printf("__SDCC_FLOAT_REENTRANT ISNDEF\n");		// default = undefined
#endif												// but lib/math.h says: float is ALWAYS reentrant!

#ifdef __SDCC_INT_LONG_REENT	// --int-long-reent
	printf("__SDCC_INT_LONG_REENT isdef\n");		// default = defined
#else    //"12345678901234567890123456789012"
	printf("__SDCC_INT_LONG_REENT ISNDEF\n");
#endif

//#ifndef _REENTRANT	// sdcc-lib.h: define to empty
//	#error
//#endif
//#ifndef _CODE		// sdcc-lib.h: define to empty
//	#error
//#endif
//#ifndef _AUTOMEM	// sdcc-lib.h: define to empty
//	#error
//#endif
//#ifndef _STATMEM	// sdcc-lib.h: define to empty
//	#error
//#endif



//	for(i=0;i<10;i++)
//	{
//		printf("%i * %i = %i\n", i,i,i*i);
//	}
}


/*
	Defines created by SDCC:

	__SDCC					SDCC_3_2_0			how to test this?
	__SDCC_REVISION			SVN revision number

	__SDCC_z80				--mz80				default=defined
	__SDCC_STACK_AUTO		--stack-auto		default=defined
	__SDCC_CHAR_UNSIGNED	--funsigned-char	default=undefined
	__SDCC_ALL_CALLEE_SAVES	--all-callee-saves	default=undefined
	__SDCC_FLOAT_REENTRANT	--float-reentrant	default=undefined
	__SDCC_INT_LONG_REENT	--int-long-reent	default=defined

	__SDCC_MODEL_SMALL		--model-small		8051
	__SDCC_MODEL_MEDIUM		--model-medium		8051
	__SDCC_MODEL_LARGE		--model-large		8051
	__SDCC_MODEL_HUGE		--model-huge		8051
	__SDCC_USE_XSTACK		--xstack			8051
	SDCC_PARMS_IN_BANK1		--parms-in-bank1	undocumented

	--std-sdcc89	Generally follow the ANSI C89 standard, but allow some conflictiong SDCC behaviour. (default)
	--std-89
	--std-sdcc99	Generally follow the ISO C99 standard, but allow some conflictiong SDCC behaviour.
	--std-99
	--std-c11		not recognized by the preprocessor !?!!

	__STDC_VERSION__		--std-c89			not defined
							--std-c99			199901L
							--std-c11			201112L (to be tested)

	--codeseg NAME				// e.g. "--codeseg CODE" for segment _CODE
	--constseg NAME				// NOT FOR Z80: const always in code segment!
	--callee-saves-bc			// all functions save/restore BC on entry/exit
	--reserve-regs-iy			// do not use IY
	--fno-omit-frame-pointer	// never omit frame pointer


	i/o for Z80: (dsccman pg.40)

		__sfr __at 0x78 IoPort;				// define an I/O port with 8 bit address  at 0x78  called IoPort
		__sfr __banked __at 0x123 IoPort;	// define an I/O port with 16 bit address at 0x123 called IoPort

	SDCC named address spaces for bank switching

	void setb0(void);			// The function that sets the currently active memory bank to b0
    void setb1(void);			// The function that sets the currently active memory bank to b1
    __addressmod setb0 spaceb0; // Declare a named address space called spaceb0 that uses setb0()
    __addressmod setb1 spaceb1; // Declare a named address space called spaceb1 that uses setb1()
    spaceb0 int x;				// An int in address space spaceb0
    spaceb1 int *y;				// A pointer to an int in address space spaceb1
    spaceb0 int *spaceb1 z;		// A pointer in address space sapceb1 that points to an int in address space spaceb0
    __addressmod const setb0 spaceb0; // Declare a named address space called spaceb0 that uses setb0()
    __addressmod setb1 spaceb1;	// Declare a named address space called spaceb1 that uses setb1() and resides in ROM
    const spaceb0 int x = 42;	// An int in address space spaceb0
    spaceb1 int *y;				// A pointer to an int in address space spaceb1
    const spaceb0 int *spaceb1 z; // A pointer in address space sapceb1 that points to a const int in address space spaceb0

//	absolute addressing: NOT FOR Z80
//	__xdata __at (0x7ffe) unsigned int chksum;		// in xdata memory
//	__code __at (0x7ff0) char Id[5] = "abcd";		// in code memory

//	NOT FOR Z80:
//	unsigned char foo(char i) __reentrant
//    { ... }

//    unsigned char foo(__xdata int parm)				// for non-reentrant and probably for z80 not at all
//    {
//        __xdata unsigned char i;
//        __data __at (0x31) unsigned char j;
//        ...
//	}

	void Intr(void) __interrupt 0					// INT handler, jump from int entry address must probably added manually in asm file
	{ ... }

	void nmi_isr (void) __critical __interrupt		// NMI handler
    { ... }

	int foo () __critical							// whole function runs with interrupts disabled
    { ... }

	__critical{ i++; }								// code block runs with interrupts disabled



*/










