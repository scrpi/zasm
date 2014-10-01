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


#define LOG 1
#define SAFE 3
#include "Segment.h"
#include "Z80Assembler.h"




// -------------------------------------------------------
//			Segments
// -------------------------------------------------------


Segment* Segments::find(cstr name)
{
	for(uint i=0;i<count();i++)
	{
		if(eq(data[i]->name,name)) return data[i];
	}
	return NULL;
}


// -------------------------------------------------------
//			Segment
// -------------------------------------------------------


/*	creator
	core is always set to 0x10000 bytes
	may throw syntax_error if address+size exceeds limit
*/
Segment::Segment(cstr name, int address, int size, bool is_data, uint8 fillbyte, bool addr_valid, bool size_valid) throw(syntax_error)
:	name(name),
	is_data(is_data),
	fillbyte(fillbyte),
	dptr(0),
	address(address),
	size(size),
	address_valid(addr_valid),
	dptr_valid(yes),
	current_address_valid(addr_valid),
	size_valid(size_valid),
	relocatable(no)
{
	core.grow(0x10000);
	validate_address_and_size();
}


/*	creator
	core is always set to 0x10000 bytes
*/
Segment::Segment(cstr name, bool is_data, uint8 fillbyte )
:	name(name),
	is_data(is_data),
	fillbyte(fillbyte),
	dptr(0),
	address(0),
	size(0),
	address_valid(no),
	dptr_valid(yes),
	current_address_valid(no),
	size_valid(no),
	relocatable(yes)
{
	core.grow(0x10000);
}


/*	validate segment base address and size
*/
void Segment::validate_address_and_size() throw(syntax_error)
{
	if(size_valid && (uint32)size>0x10000)
	{
		size_valid = no;		// prevent code deposition
		throw syntax_error(usingstr("segment size out of range: %i",(int)size));
	}

	if(address_valid && address!=(uint16)address && address!=(int16)address)
	{
		address_valid = no;
		current_address_valid = no;
		throw syntax_error(usingstr("segment base address out of range: %i",(int)address));
	}

	if(address_valid && size_valid)
	{
		if(address>=0 && address+size > 0x10000)
			throw syntax_error(usingstr("segment size out of range: %i + %u = %i",
			(int)address, (uint)size, (int)address+(int)size));
		if(address<0 && address+size > 0x8000)
			throw syntax_error(usingstr("segment size out of range: %i + %u = %i",
			(int)address, (uint)size, (uint)address+(int)size));
	}
}


/*	set segment start address
*/
void Segment::setAddress(int a) throw(syntax_error)
{
	if(address_valid && a!=address) throw syntax_error("segment address redefined");

	address = a;
	address_valid = yes;
	if(dptr_valid) current_address_valid = yes;
	relocatable = no;

	validate_address_and_size();
}


/*	set segment size
*/
void Segment::setSize(int n) throw(syntax_error)
{
	if(size_valid && n!=size) throw syntax_error("segment size redefined");

	size = n;
	size_valid = yes;

	validate_address_and_size();
}


/*	move dptr to new current address
	this inserts space
*/
void Segment::storeSpace4Org( int32 addr, bool addr_valid ) throw(syntax_error)
{
	if(addr_valid && current_address_valid)	// => wir können die gap size berechnen:
	{
		int32 sz = addr - currentAddress();
		if(sz>=0x10000 && address<0) sz -= 0x10000;		// be nice
		if(sz<0        && addr<0)	 sz += 0x10000;		// be nice
		storeSpace(fillbyte,sz,yes);
	}
	else if(addr_valid)
	{
		if(addr!=(int16)addr && addr!=(uint16)addr) throw syntax_error("current address out of range");
		current_address_valid = true;
		dptr_valid = no;

		dptr = addr - address;			// current_address = addr = address + dptr  <=>  dptr = current_address - address
	}
	else
	{
		current_address_valid = no;
		dptr_valid = no;
	}
}


/* store byte
*/
void Segment::store( int byte ) throw(fatal_error)
{
	if((uint32)dptr<0x10000) core[dptr] = byte;
	if(++dptr>size && dptr_valid && size_valid) throw fatal_error("segment overflow");
}

/* store 2 bytes (lsb first)
*/
void Segment::storeWord( int n )
{
	store(n);
	store(n>>8);
}

/* store block of raw bytes
*/
void Segment::storeBlock( cptr data, int n ) throw(syntax_error)
{
	if(n<0) throw syntax_error("size < 0");
	if(n>0x10000) throw syntax_error("size > 0x10000");

	if((uint32)dptr<0x10000) memcpy(&core[dptr], data, min(n,0x10000-dptr));
	if((dptr+=n)>size && dptr_valid && size_valid) throw syntax_error("segment overflow");
}

/* skip over existing data in pass≥2:
*/
void Segment::skipExistingData(int n) throw(syntax_error)
{
	if(n<0) throw syntax_error("size < 0");
	if(n>0x10000) throw syntax_error("size > 0x10000");
	if((dptr+=n)>size && dptr_valid && size_valid) throw syntax_error("segment overflow");
}


/*	store block of bytes
	source bytes are stuffed as hex
	n = bytes to stuff   ( => 2*n hex digits )
*/
void Segment::storeHexBytes(cptr data, int n ) throw(syntax_error)
{
	if(n<0) throw syntax_error("size < 0");
	if(n>0x10000) throw syntax_error("size > 0x10000");

	while (n--)
	{
		char c = digit_value(*data++);
		char d = digit_value(*data++);
		store((c<<4)+d);
	}
}

/* store space
*/
void Segment::storeSpace( int c, int sz, bool sz_valid ) throw(syntax_error)
{
	if(sz_valid)
	{
		if(sz<0) throw syntax_error("gap size < 0");
		if(sz>0x10000) throw syntax_error("gap size > 0x10000");

		if((uint32)dptr<0x10000) memset(&core[dptr], c, min(sz,0x10000-dptr));
		if((dptr+=sz)>size && dptr_valid && size_valid) throw syntax_error("segment overflow");
	}
	else
	{
		dptr_valid = no;
		current_address_valid = no;
	}
}

/* store space with default fillbyte
*/
void Segment::storeSpace( int sz, bool sz_valid ) throw(syntax_error)
{
	storeSpace(fillbyte, sz, sz_valid);
}


void Segment::rewind()
{
	dptr = 0;
	dptr_valid = yes;
	current_address_valid = address_valid;
}



































