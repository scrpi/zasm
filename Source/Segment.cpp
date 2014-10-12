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
	core-array is always set to 0x10000 bytes
	may throw syntax_error if address+size exceeds limit
*/
Segment::Segment(cstr name, int address, int size, bool is_data, uint8 fillbyte, bool addr_valid, bool size_valid) throw(syntax_error)
:
	name(name),
	is_data(is_data),
	fillbyte(fillbyte),
	core(0x10000),
	address(address),
	size(size),
	address_valid(addr_valid),
	size_valid(size_valid),
	relocatable(no),
	dptr(0),
	dptr_valid(yes),
	dptr_address_valid(addr_valid)
{
	validate_address_and_size();
}


/*	creator
	core-array is always set to 0x10000 bytes
*/
Segment::Segment(cstr name, bool is_data, uint8 fillbyte )
:
	name(name),
	is_data(is_data),
	fillbyte(fillbyte),
	core(0x10000),
	address(0),
	size(0),
	address_valid(no),
	size_valid(no),
	relocatable(yes),
	dptr(0),
	dptr_valid(yes),
	dptr_address_valid(no)
{}



/*	validate segment base address and size
*/
void Segment::validate_address_and_size() throw(syntax_error)
{
	if(size_valid && size>0x10000)
	{
		size_valid = no;		// prevent code deposition
		throw syntax_error(usingstr("segment size out of range: %i",(int)size));
	}

	if(address_valid && address!=(uint16)address && address!=(int16)address)
	{
		address_valid = no;
		dptr_address_valid = no;
		throw syntax_error(usingstr("segment base address out of range: %i",(int)address));
	}

	if(address_valid && size_valid)
	{
		if( (address & 0x7fff) + size > 0x10000 )
			throw syntax_error(usingstr("segment size out of range: %i + %u = %i",
			(int)address, (uint)size, (int)(address+size)));
	}
}


/*	set segment start address
*/
void Segment::setAddress(int a) throw(syntax_error)
{
	if(address_valid)
	{
		if(a!=address) throw syntax_error("segment address redefined");
		return;
	}

	if(dptr_address_valid)	// e.g. after 'org' instruction
	{
		XXXASSERT(!dptr_valid);
		dptr += address - a;
	}

	address = a;
	address_valid = yes;
	if(dptr_valid) dptr_address_valid = yes;
	relocatable = no;

	validate_address_and_size();
}


/*	set segment size
*/
void Segment::setSize(uint n) throw(syntax_error)
{
	if(size_valid)
	{
		if(n!=size) throw syntax_error("segment size redefined");
		return;
	}

	size = n;
	size_valid = yes;

	validate_address_and_size();
}


/*	set segment start address or move dptr
	if there is not yet stored any code in the segment, then set segment start address,
	else insert space up to the new address
	negative gap size (thus moving back dptr) is not allowed, because total size of segment is calculated from dptr
*/
void Segment::setOrigin( int32 addr, bool addr_valid ) throw(syntax_error)
{
	if(dptr_valid && dptr==0)	// set segment start address
	{
		if(addr_valid) setAddress(addr);
		return;
	}

	// store space up to new address:

	if(addr_valid)
	{
		if(addr!=(int16)addr && addr!=(uint16)addr)
		{
			address_valid = no;
			dptr_address_valid = no;
			throw syntax_error("address out of range");
		}

		if(dptr_address_valid)	// gap size can be calculated:
		{
			int32 old_address = currentAddress();				// this.address + this.dptr

			if(addr<0 && old_address>=0x8000) addr += 0x10000;	// be nice in case of signedness mismatch
			if(addr>=0x8000 && old_address<0) addr -= 0x10000;	// be nice ""

			int32 size = addr - old_address;
			storeSpace(fillbyte,size,yes);
		}
		else // gap size can't be calculated but we know the current address:
		{
			dptr_address_valid = true;
			dptr_valid = no;
			dptr = addr - address;		// current_address = addr = address + dptr  <=>  dptr = current_address - address
		}
	}
	else
	{
		dptr_address_valid = no;
		dptr_valid = no;
	}
}


/* store byte
*/
void Segment::store( int byte ) throw(fatal_error)
{
	if(dptr<0x10000) core[dptr] = byte;
	if(++dptr>size && dptr_valid && size_valid) { dptr_valid=no; throw fatal_error("segment overflow"); }
}

/* store 2 bytes (z80 byte order: lsb first)
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

	if(dptr<0x10000) memcpy(&core[dptr], data, min((uint)n,0x10000-dptr));
	if((dptr+=n)>size && dptr_valid && size_valid) { dptr_valid=no; throw syntax_error("segment overflow"); }
}

/* skip over existing data in pass≥2:
*/
void Segment::skipExistingData(int n) throw(syntax_error)
{
	if(n<0) throw syntax_error("size < 0");
	if(n>0x10000) throw syntax_error("size > 0x10000");
	if((dptr+=n)>size && dptr_valid && size_valid) { dptr_valid=no; throw syntax_error("segment overflow"); }
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
		char c = *data++; if(!is_hex_digit(c)) throw syntax_error(usingstr("only hex characters allowed: '%c'",c));
		char d = *data++; if(!is_hex_digit(d)) throw syntax_error(usingstr("only hex characters allowed: '%c'",d));

		store( (digit_value(c)<<4) + digit_value(d) );
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

		if(dptr<0x10000) memset(&core[dptr], c, min((uint)sz,0x10000-dptr));
		if((dptr+=sz)>size && dptr_valid && size_valid) { dptr_valid=no; throw syntax_error("segment overflow"); }
	}
	else
	{
		dptr_valid = no;
		dptr_address_valid = no;
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
	dptr_address_valid = address_valid;
}



































