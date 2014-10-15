/*	Copyright  (c)	Günter Woigk 2014 - 2014
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


#include "Templates/Array.h"
#include "SyntaxError.h"
typedef Array<uint8> Core;





class Z80Assembler;


class Segment
{
public:
	cstr		name;
	bool		is_data;				// => no actual code storing allowed
	uint8		fillbyte;				// $FF for ROM else $00

	Core		core;

	int32		address;				// segment start address
	uint32		size;					// segment size
	bool		address_valid;			// segment start address valid
	bool		size_valid;				// segment size valid
	bool		relocatable;			// address has not been explicitely set
	bool		resizable;				// size has not been explicitely set

	uint32		dptr;					// deposition pointer (index)
	bool		dptr_valid;				// code deposition position inside segment
	bool		dptr_address_valid;		// mostly address_valid && dptr_valid, but may be valid independently after org instruction

private:
	void		validate_address_and_size() throw(syntax_error);


public:
				Segment			(cstr name, int address, int size, bool is_data, uint8 fillbyte, bool address_valid=yes, bool size_valid=yes) throw(syntax_error);
				Segment			(cstr name, bool is_data, uint8 fillbyte=0);

// store object code
	void		store			(int byte)						throw(fatal_error);
	void		store			(int a,int b)					{ store(a); store(b); }
	void		store			(int a,int b,int c)				{ store(a); store(b); store(c); }
	void		store			(int a,int b,int c,int d)		{ store(a); store(b); store(c); store(d); }
	void 		storeWord		(int n);
	void		storeBlock		(cptr, int sz)					throw(syntax_error);
	void		skipExistingData(int sz)						throw(syntax_error);
	void		storeSpace		(int c, int sz, bool sz_valid)	throw(syntax_error);
	void		storeSpace		(int sz, bool sz_valid)			throw(syntax_error);
	void		storeHexBytes	(cptr data, int n)				throw(syntax_error);
	void		setOrigin		(int32 a, bool a_valid)			throw(syntax_error);

	uint8		popLastByte		()								{ XXXASSERT(dptr>0); return core[--dptr]; }

	void		setAddress		(int32 a)						throw(syntax_error);
	void		setSize			(uint32 n)						throw(syntax_error);

	uint32		currentPosition	()								{ return dptr; }				// write position (offset) in core
	bool		currentPositionValid()							{ return dptr_valid; }			// … valid?
	int32		currentAddress	()								{ return address + dptr; }		// address associated with write position
	bool		currentAddressValid()							{ return dptr_address_valid; }	// … valid?

	void		rewind			();
};



class Segments : public ObjArray<Segment>
{
public:
	void		add(Segment* s)	{ ObjArray<Segment>::append(s); }
	Segment*	find(cstr name);
};




#endif // ZASMSEGMENT_H































