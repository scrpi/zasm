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



class Z80Assembler
{
public:
	double		timestamp;			// of assembly

	cstr		source_directory;	// top-level source
	cstr		source_filename;
	cstr		temp_directory;
	cstr		target;				// "BIN, "ROM", "SNA", ...

// source:
	Source		source;				// SourceLine[] accumulating total source
	uint		current_sourceline_index;
	SourceLine&	current_sourceline()		{ return source[current_sourceline_index]; }

// code:
	Segments	segments;			// code and data segments
	Segment*	current_segment_ptr;
	Segment&	current_segment()			{ return *current_segment_ptr; }

// Labels:
	ObjArray<Labels>	labels;
	uint				local_labels_index;
	Labels&				global_labels()		{ return labels[0]; }
	Labels&				local_labels()		{ return labels[local_labels_index]; }
	uint				local_blocks_count;
//	bool				temp_label_seen;
//	char				temp_label_suffix[8];
//	cstr				reusable_label_basename;	// name of last normal label

// cond. assembly:
	uint32		cond_off;		// effective final on/off state of conditions nested:
								// 0 = assemble; !0 => assembly off
	char		cond[32];		// cond. state for up to 32 nested conditional blocks
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
	cstr		c_compiler;		// fqn to sdcc or similar
	Array<cstr>	c_flags;
	uint		c_qi;			// index of source file in cc_argv[]
	uint		c_zi;			// index of output file in cc_argv[]

private:
	int32	value			(SourceLine&, int prio, bool& valid) TAE;
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

//	void	store			(int n)						TAE	{ current_segment().store(n); }
//	void	store			(int n,int m)				TAE	{ current_segment().store(n,m); }
//	void	store			(int a,int b,int c)			TAE	{ current_segment().store(a,b,c); }
//	void	store			(int a,int b,int c,int d)	TAE	{ current_segment().store(a,b,c,d); }
	void	storeOpcode     (int n)						TAE	{ current_segment().store(n); }
	void 	storeWord		(int n)						TAE	{ current_segment().storeWord(n); }
	void	storeBlock		(cstr blk, int n)			TAE	{ current_segment().storeBlock(blk,n); }
	void	storeHexbytes	(cstr hex, int n)			TAE	{ current_segment().storeHexBytes(hex,n); }

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

	int		getCondition	(cstr w)					throw(syntax_error);
	int		getRegister		(SourceLine&);

	void	setError		(any_error&);				// set error for current file, line & column
	void	addError		(cstr text);				// add error without source line

public:
			Z80Assembler	();
			~Z80Assembler	();
	void	assembleFile	(cstr sourcepath,			// source file must exist
							 cstr destpath=NULL,		// dflt = source directory, may be dir or filename
							 cstr listpath=NULL,		// dflt = dest direcory, may be dir or filename
							 cstr temppath=NULL,		// dflt = dest dir, must be dir
							 int liststyle='1',			// '0'=none, '1'=plain, '2'=w/ocode, '4'=w/labels
							 int deststyle='b')		throw();// '0'=none, 'b'=bin, 'x'=intel hex, 's'=moto s19
	void	assemble		(StrArray& sourcelines)	throw();
	void	assembleLine	(SourceLine&)			TAE;

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
};





#endif // ZASM_H

























