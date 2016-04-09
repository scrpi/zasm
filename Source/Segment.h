/*	Copyright  (c)	Günter Woigk 2014 - 2016
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


#ifndef ZASMSEGMENT_H
#define ZASMSEGMENT_H


#include "Templates/ObjArray.h"
#include "SyntaxError.h"
typedef Array<uint8> Core;





class Z80Assembler;


class Segment
{
public:
	Core		core;

	cstr		name;
	bool		is_data;			// => no actual code storing allowed
	uint8		fillbyte;			// $FF for ROM else $00
	bool		relocatable;		// address has not been explicitely set => append to prev. segment
	bool		resizable;			// size has not been explicitely set    => shrink to fit
	bool		has_flag;

	uint32		address;			// "physical" segment start address (as set in segment header
									// or calculated when appended to prev. segment)
	uint32		size;				// segment size
	uint32		dpos;				// code deposition index
	int32		org_base_address;	// base address for "logical" address (at dpos==0)
	int			flag;				// flag for .z80 and .tap code segments

	bool		address_valid;
	bool		size_valid;
	bool		dpos_valid;
	bool		org_valid;
	bool		flag_valid;


public:			Segment			(cstr name, bool is_data, uint8 fillbyte, bool relocatable, bool resizable, bool has_flag);

// store object code
	void		store			(int byte)						throw(fatal_error);
	void		store			(int a,int b)					{ store(a); store(b); }
	void		store			(int a,int b,int c)				{ store(a); store(b); store(c); }
	void		store			(int a,int b,int c,int d)		{ store(a); store(b); store(c); store(d); }
	void 		storeWord		(int n);
	void		storeBlock		(cptr, int sz)					throw(syntax_error);
	void		skipExistingData(int sz)						throw(syntax_error);
	void		storeSpace		(int sz, bool sz_valid, int c)	throw(syntax_error);
	void		storeSpace		(int sz, bool sz_valid)			throw(syntax_error);
	void		storeSpaceUpToAddress(int addr, bool addr_valid ) throw(syntax_error);
	void		storeHexBytes	(cptr data, int n)				throw(syntax_error);
	void		setOrigin		(int32 a, bool a_valid)			throw(syntax_error);

	uint8&		operator[]		(uint32 i)						{ return core[i]; }
	uint8		popLastByte		()								{ XXXASSERT(dpos>0); return core[--dpos]; }

	void		rewind			();
	void		setAddress		(int32 a)						throw(syntax_error);
	void		setSize			(uint32 n)						throw(syntax_error);
	void		setFlag			(int32 n)						throw(syntax_error);

	bool		isAtStart		()		{ return dpos_valid && dpos==0; }
	uint32		currentPosition	()		{ return dpos; }						// offset in core
	bool		currentPositionValid()	{ return dpos_valid; }					// … valid?
	uint32		physicalAddress	()		{ return address + dpos; }				// segment_address + dpos
	bool		physicalAddressValid()	{ return address_valid && dpos_valid; }	// … valid?
	int32		logicalAddress	()		{ return org_base_address + dpos; }		// org + dpos
	bool		logicalAddressValid()	{ return org_valid; }					// … valid?
	bool		isData			()		{ return is_data; }
	bool		isCode			()		{ return !is_data; }
	uint8*		getData			()		{ return core.getData(); }
	bool		isEmpty			()		{ bool empty=yes; for(uint i=0; i<size && empty; i++)
										  empty = core[i]==fillbyte; return empty; }
};



class Segments : public ObjArray<Segment>
{
public:
	void		add(Segment* s)					{ ObjArray<Segment>::append(s); }
	Segment*	find(cstr name);
INL	uint32		totalCodeSize();
INL	int			firstCodeSegmentWithValidFlag();
INL	void		checkNoFlagsSet()		 throw(syntax_error);
INL	uint		numCodeSegments();
};



INL uint32 Segments::totalCodeSize()
{
	uint32 sz=0;
	for(uint i=count();i--;)
	{
		Segment* s = data[i];
		if(!s->is_data) sz += s->size;
	}
	return sz;
}

INL int Segments::firstCodeSegmentWithValidFlag()
{
	for(uint i=0; i<count()&&data[i]->isCode(); i++)
	{
		if(data[i]->flag_valid) return i;
	}
	return -1;
}

INL void Segments::checkNoFlagsSet() throw(syntax_error)
{
	for(uint i=0; i<count(); i++)
	{
		if(data[i]->flag_valid)
			throw syntax_error(usingstr("segment %s must not have flag set", data[i]->name));
	}
}

INL uint Segments::numCodeSegments()
{
	uint n=0;
	for(uint i=0;i<count();i++) n += data[i]->isCode();
	return n;
}



#endif // ZASMSEGMENT_H































