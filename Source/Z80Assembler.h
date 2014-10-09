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








class Z80Assembler
{
public:
	double		timestamp;			// of assembly
//	int			listfile_flags;
//	cstr		listfilepath;

	cstr		source_directory;	// top-level source
	cstr		source_filename;
	cstr		target;				// "BIN, "ROM", "SNA", ...
//	cstr		targetfilepath;

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
	bool				temp_label_seen;
	char				temp_label_suffix[8];

// cond. assembly:
	uint32		cond_off;		// effective final on/off state of conditions nested: 0 = assemble; !0 => assembly off
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

private:
	int32	value			(SourceLine&, int prio, bool& valid) throw(any_error);
	void	asmLabel		(SourceLine&)				throw(any_error);
	void	asmDirect		(SourceLine&)				throw(fatal_error);		// processor specific #directives
	void	asmIf			(SourceLine&)				throw(any_error);
	void	asmElif			(SourceLine&)				throw(any_error);
	void	asmElse			(SourceLine&)				throw(any_error);
	void	asmEndif		(SourceLine&)				throw(any_error);
	void	asmTarget		(SourceLine&)				throw(any_error);
	void	asmInclude		(SourceLine&)				throw(any_error);
	void	asmInsert		(SourceLine&)				throw(any_error);
	void	asmSegment		(SourceLine&,bool)			throw(any_error);
	void	asmLocal		(SourceLine&)				throw(any_error);
	void	asmEndLocal		(SourceLine&)				throw(any_error);
	void	asmInstr		(SourceLine&)				throw(any_error);

	void	store			(int n)						throw(any_error)	{ current_segment().store(n); }
	void	store			(int n,int m)				throw(any_error)	{ current_segment().store(n,m); }
	void	store			(int a,int b,int c)			throw(any_error)	{ current_segment().store(a,b,c); }
	void	store			(int a,int b,int c,int d)	throw(any_error)	{ current_segment().store(a,b,c,d); }
	void	storeOpcode     (int n)						throw(any_error)	{ current_segment().store(n); }
	void 	storeWord		(int n)						throw(any_error)	{ current_segment().storeWord(n); }
	void	storeBlock		(cstr blk, int n)			throw(any_error)	{ current_segment().storeBlock(blk,n); }
	void	storeHexbytes	(cstr hex, int n)			throw(any_error)	{ current_segment().storeHexBytes(hex,n); }

	void	storeByte 		(int n, bool valid)			throw(any_error);
	void	storeOffset 	(int n, bool valid)			throw(any_error);
	void	storeSpace		(int c, int n, bool valid)	throw(any_error)	{ current_segment().storeSpace(c,n,valid); }
	void	storeSpace		(int n, bool valid)			throw(any_error)	{ current_segment().storeSpace(n,valid); }
	void	store_XYCB_op	(int pfx, int op, int dis, bool valid)	throw(any_error);
	void	store_XY_byte_op(int pfx, int op, int dis, bool valid)	throw(any_error);

	int		getCondition	(cstr w)					throw(syntax_error);
	int		getRegister		(SourceLine&);
//	void	compressPageAce	(Array<uint8>&);
//	void	compressPageZ80	(Array<uint8>&);

public:
			Z80Assembler	();
	void	assembleFile	(cstr sourcepath, cstr destpath, cstr listpath=NULL,
							 bool v=no,					// include opject code in listing
							 bool w=no,					// include label listing in listing
							 char style='b' )			throw();						// target style: 'b'=binary, 'x'=intel hex
	void	assemble		(StrArray& sourcelines)		throw();
	void	assembleLine	(SourceLine&)				throw(any_error);

//	enum { NoListfile=0,ListingWithObjcode=1,ListingWithLabelList=2,BasicListing=4 };	// note: bit masks %11

	void	writeListfile	(cstr listpath, bool v, bool w) throw(any_error);
	void	writeTargetfile	(cstr filename, int style)	throw(any_error);
};





#endif // ZASM_H

























