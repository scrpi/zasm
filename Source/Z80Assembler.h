/*	Copyright  (c)	Günter Woigk 1994 - 2014
					mailto:kio@little-bat.de

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

	Permission to use, copy, modify, distribute, and sell this software and
	its documentation for any purpose is hereby granted without fee, provided
	that the above copyright notice appear in all copies and that both that
	copyright notice and this permission notice appear in supporting
	documentation, and that the name of the copyright holder not be used
	in advertising or publicity pertaining to distribution of the software
	without specific, written prior permission.  The copyright holder makes no
	representations about the suitability of this software for any purpose.
	It is provided "as is" without express or implied warranty.

	THE COPYRIGHT HOLDER DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
	INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
	EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY SPECIAL, INDIRECT OR
	CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
	DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
	TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
	PERFORMANCE OF THIS SOFTWARE.
*/


#ifndef ZASM_H
#define ZASM_H

#include "kio/kio.h"
#include "Templates/Array.h"
#include "Source.h"
#include "Label.h"
#include "Segment.h"
#include "Error.h"
#include "SyntaxError.h"



extern const char DEFAULT_CODE_SEGMENT[];

#define TAE	throw(any_error)
class CharMap;



class Z80Assembler
{
public:
	double		timestamp;			// of assembly

	cstr		source_directory;	// top-level source
	cstr		source_filename;
	cstr		temp_directory;
	cstr		target;				// "BIN, "ROM", "SNA", ...
	cstr		target_filepath;

// source:
	Source		source;						// SourceLine[] accumulating total source
	uint		current_sourceline_index;
	SourceLine&	current_sourceline()		{ return source[current_sourceline_index]; }

// code:
	Segments	segments;					// code and data segments
	Segment*	current_segment_ptr;
	Segment&	current_segment()			{ return *current_segment_ptr; }

// Labels:
	ObjArray<Labels> labels;
	uint		local_labels_index;
	Labels&		global_labels()				{ return labels[0]; }
	Labels&		local_labels()				{ return labels[local_labels_index]; }
	uint		local_blocks_count;
	cstr		reusable_label_basename;	// name of last normal label

// cond. assembly:
	uint32		cond_off;		// effective final on/off state of conditions nested:
								// 0 = assemble; !0 => assembly off
	uint8		cond[32];		// cond. state for up to 32 nested conditional blocks
	enum 	{	no_cond=0, 		// no conditional assembly
				cond_if,		// #if or #elif pending and no path 'on' up to now
				cond_if_dis,	// #if or #elif pending and 'on' path currently or already processed
				cond_else	 	// #else pending
			};

// Errors:
	Errors		errors;
	uint		max_errors;
	uint		pass;
	bool		final;
	bool		end;
	uint		verbose;

// c compiler:
	cstr		c_compiler;		// -c: fqn to sdcc or similar or NULL
	cstr		c_includes;		// -I: fqn to custom include dir or NULL
	cstr		stdlib_dir;		// -L: fqn to custom library dir or NULL (not only c but any .globl)
	cstr		c_tempdir;		// fqn of sub directory in temp_directory acc. to c_flags for .s files
	CstrArray	c_flags;
	int			c_qi;			// index of source file in cc_argv[] or -1
	int			c_zi;			// index of output file in cc_argv[] or -1

// more:
	CharMap*	charset;

// options:
	bool		ixcbr2_enabled;		// enable ixcb illegals: e.g. set b,(ix+d),r2
	bool		ixcbxh_enabled;		// enable ixcb illegals: e.g. bit b,xh
	bool		target_hd64180;		// enable hd64180 opcodes
	bool		target_8080;		// limit instruction set to 8080 opcodes
	bool		syntax_8080;		// use 8080 assembler syntax (TODO)
	bool		registers_8080;		// limit known register names to 8080 registers => allow others for label names

private:
	int32	value			(SourceLine&, int prio, bool& valid) TAE;
	void	skip_expression	(SourceLine&, int prio)		TAE;
	void	asmLabel		(SourceLine&)				TAE;
	void	asmDirect		(SourceLine&)				throw(fatal_error);		// #directives
	void	asmIf			(SourceLine&)				TAE;
	void	asmElif			(SourceLine&)				TAE;
	void	asmElse			(SourceLine&)				TAE;
	void	asmEndif		(SourceLine&)				TAE;
	void	asmTarget		(SourceLine&)				TAE;
	void	asmInclude		(SourceLine&)				TAE;
	void	asmInsert		(SourceLine&)				TAE;
	void	asmSegment		(SourceLine&,bool)			TAE;
	void	asmCFlags		(SourceLine&)				TAE;
	void	asmLocal		(SourceLine&)				TAE;
	void	asmEndLocal		(SourceLine&)				TAE;
	void	asmEnd			(SourceLine&)				TAE;
	void	asmInstr		(SourceLine&)				TAE;
	void	asmAssert		(SourceLine&)				TAE;
	void	asmCharset		(SourceLine&)				TAE;
	cstr	compileFile		(cstr)						TAE;

	void	store			(int n)						TAE { current_segment_ptr->store(n); }
	void	store			(int n, int m)				TAE { current_segment_ptr->store(n,m); }
	void	store			(int n, int m, int u)		TAE { current_segment_ptr->store(n,m,u); }
	void	store			(int a, int b, int c, int d)TAE { current_segment_ptr->store(a,b,c,d); }
//	void	storeCBopcode	(int n)						TAE { store(0xCB,n); }
//	void	storeIXopcode	(int n)						TAE { store(0xDD,n); }
	void	storeEDopcode	(int n)						TAE;
//	void	storeIYopcode	(int n)						TAE { store(0xFD,n); }

//	void	storeOpcode     (int n)						TAE	{ current_segment_ptr->store(n); }
	void 	storeWord		(int n)						TAE	{ current_segment_ptr->storeWord(n); }
	void	storeBlock		(cstr blk, int n)			TAE	{ current_segment_ptr->storeBlock(blk,n); }
	void	storeHexbytes	(cstr hex, int n)			TAE	{ current_segment_ptr->storeHexBytes(hex,n); }

	void	storeByte 		(int n, bool valid)			TAE;
	void	storeOffset 	(int n, bool valid)			TAE;
	void	storeSpace		(int n, bool valid, int c)	TAE	{ current_segment().storeSpace(n,valid,c); }
	void	storeSpace		(int n, bool valid)			TAE	{ current_segment().storeSpace(n,valid); }
	void	store_XYCB_op	(int pfx, int op, int dis, bool valid)	TAE;
	void	store_XY_byte_op(int pfx, int op, int dis, bool valid)	TAE;
	uint8	popLastByte		()							{ return current_segment().popLastByte(); }

	uint32	currentPosition	()							{ return current_segment().currentPosition(); }
	bool	currentPositionValid()						{ return current_segment().currentPositionValid(); }
	int32	currentAddress	()							{ return current_segment().logicalAddress(); }
	bool	currentAddressValid()						{ return current_segment().logicalAddressValid(); }
	int32	realAddress		()							{ return current_segment().physicalAddress(); }
	bool	realAddressValid()							{ return current_segment().physicalAddressValid(); }

	uint	getCondition	(SourceLine&, bool expect_comma)				throw(syntax_error);
	uint	getRegister		(SourceLine&, int32&, bool&)throw(syntax_error);

	void	setError		(any_error&);				// set error for current file, line & column
	void	addError		(cstr text);				// add error without source line
	void	init_c_flags	();
	void	init_c_tempdir	()							THF;

public:
			Z80Assembler	();
			~Z80Assembler	();
	void	assembleFile	(cstr sourcepath,			// source file must exist
							 cstr destpath=NULL,		// dflt = source directory, may be dir or filename
							 cstr listpath=NULL,		// dflt = dest direcory, may be dir or filename
							 cstr temppath=NULL,		// dflt = dest dir, must be dir
							 int  liststyle=1,			// 0=none, 1=plain, 2=w/ocode, 4=w/labels, 8=w/clkcycles
							 int  deststyle='b',		// 0=none, 'b'=bin, 'x'=intel hex, 's'=moto s19
							 bool clean=no)			throw();
	void	assemble		(StrArray& sourcelines)	throw();
	void	assembleLine	(SourceLine&)			TAE;
	uint	assembleSingleLine(uint address, cstr z80_instruction, char buffer[]);

	void	checkTargetfile	()		TAE;
	void	writeListfile	(cstr filepath, int style) TAE;
	void	writeTargetfile	(cstr filepath, int style) TAE;
	void	writeBinFile	(FD&)	TAE;
	void	writeHexFile	(FD&)	TAE;
	void	writeTapFile	(FD&)	TAE;
	void	writeZ80File	(FD&)	TAE;
	void	writeSnaFile	(FD&)	TAE;
	void	writeAceFile	(FD&)	TAE;
	void	writeZX80File	(FD&)	TAE;
	void	writeZX81File	(FD&)	TAE;
	void	checkBinFile	()		TAE;
	void	checkTapFile	()		TAE;
	void	checkZ80File	()		TAE;
	void	checkSnaFile	()		TAE;
	void	checkAceFile	()		TAE;
	void	checkZX80File	()		TAE;
	void	checkZX81File	()		TAE;

	uint	numErrors		()							{ return errors.count(); }
	cstr	targetFilepath	()							{ return target_filepath; }
};





#endif // ZASM_H

























