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


// -------------------------------------------------------
//			c'tor and stuff
// -------------------------------------------------------


/*	creator
	#code core-array is always set to 0x10000 bytes
	address, size and flag should be set immediately after this call
	or at the end of an assembler pass.
*/
Segment::Segment(cstr name, bool is_data, uint8 fillbyte , bool relocatable, bool resizable, bool has_flag)
:
	core(0x10000),
	name(name),
	is_data(is_data),
	fillbyte(fillbyte),
	relocatable(relocatable),
	resizable(resizable),
	has_flag(has_flag),

	address(0),
	size(0),
	dpos(0),
	org_base_address(0),
	flag(0),

	address_valid(no),
	size_valid(no),
	dpos_valid(yes),
	org_valid(no),
	flag_valid(no)
{}



/*	rewind dpos to offset 0
	used at start of next assembly pass
	=> rewinds dpos
	=> sets logical address to physical address
	=> preserves size
	=> preserves segment address
*/
void Segment::rewind()
{
	XXXASSERT(!resizable || size==dpos || !dpos_valid || !size_valid);

	// rewind dpos:
	dpos = 0;
	dpos_valid = yes;

	// set log. address to phys. address:
	org_base_address = address;
	org_valid = address_valid;
}


/*	set "physical" segment start address
	should be set between assembly passes
	->	concatenate relocatable segments after each pass
	->	set the address of not relocatable segments in the #code directive

	valid address: 0 … 0x10000-size

	does not clear the 'relocatable' flag
	does not set the "logical" address ('org')
	for setting the "logical" address as well, call setOrigin() or rewind() after setAddress().
*/
void Segment::setAddress(int32 a) throw(syntax_error)
{
	if(address_valid && (uint32)a!=address)
		{ address_valid=no; throw syntax_error("segment address redefined"); }
	if(a>0x10000)
		{ address_valid = no; throw syntax_error(usingstr("segment address out of range: %i",(int)a)); }
	if(size_valid && a + size > 0x10000)
			throw syntax_error(usingstr("segment %s: address+size out of range: %u + %u = %u",
			name, (uint)a, (uint)size, (uint)(a+size)));

	address = a;
	address_valid = yes;
}


/*	set segment size
	should be set between assembly passes
	->	set the size of resizable segments after each pass if dpos is valid
	->	set the size of not resizable segments as soon size is valid

	does not clear the 'resizable' flag
*/
void Segment::setSize(uint32 sz) throw(syntax_error)
{
	if(size_valid && sz!=size)
		{ size_valid = no; throw syntax_error("segment size redefined"); }
	if(sz>0x10000)
		{ size_valid = no; throw syntax_error(usingstr("segment size out of range: %i",(int)size)); }
	if(dpos_valid && dpos>sz)
		{ dpos_valid=no; throw syntax_error("segment overflow"); }
	if(address_valid && address + sz > 0x10000 )
		{ throw syntax_error(usingstr("segment %s: address+size out of range: %u + %u = %u",
			name, (uint)address, (uint)sz, (uint)(address+sz))); }

	size = sz;
	size_valid = yes;

}


/*	Set segment flag
	used for .z80 and .tap files
	may be set any time
	should be set from the #code directive
*/
void Segment::setFlag( int32 n ) throw(syntax_error)
{
	if(flag_valid && n!=flag) throw syntax_error("segment flag redefined");

	flag = n;
	flag_valid = true;
}


/*	set "logical" code address
	valid range: -0x8000 … +0xFFFF
	does not move dpos
	does not insert space
	note: zasm v3 never moved the dpos as result to an org instruction!
*/
void Segment::setOrigin( int32 new_address, bool valid ) throw(syntax_error)
{
	// kann wieder invalid werden: nach .phase ... .dephase
	//	XXXASSERT(valid || !org_valid);		// darf nicht wieder invalid werden

	if(valid && new_address!=(int16)new_address && new_address!=(uint16)new_address)
		throw syntax_error("address out of range");

	org_base_address = new_address - dpos;
	org_valid = valid;
}



// -------------------------------------------------------
//			Store Code
// -------------------------------------------------------


/* store byte
*/
void Segment::store( int byte ) throw(fatal_error)
{
	if(dpos<0x10000) core[dpos] = byte;
	if(++dpos>size && dpos_valid && size_valid) { dpos_valid = no; throw fatal_error("segment overflow"); }
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

	if(dpos<0x10000) memcpy(&core[dpos], data, min((uint)n,0x10000-dpos));
	if((dpos+=n)>size && dpos_valid && size_valid) { dpos_valid=no; throw syntax_error("segment overflow"); }
}


/*	skip over existing data in pass 2++:
	in case of an error
*/
void Segment::skipExistingData(int n) throw(syntax_error)
{
	if(n<0) throw syntax_error("size < 0");
	if(n>0x10000) throw syntax_error("size > 0x10000");

	if((dpos+=n)>size && dpos_valid && size_valid) { dpos_valid=no; throw syntax_error("segment overflow"); }
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
		char c = *data++;
		if(!is_hex_digit(c)) throw syntax_error(usingstr("only hex characters allowed: '%c'",c));
		char d = *data++;
		if(!is_hex_digit(d)) throw syntax_error(usingstr("only hex characters allowed: '%c'",d));

		store( (digit_value(c)<<4) + digit_value(d) );
	}
}


/* store space
*/
void Segment::storeSpace( int sz, bool sz_valid, int c ) throw(syntax_error)
{
	if(sz_valid)
	{
		if(sz<0) throw syntax_error("gap size < 0");
		if(sz>0x10000) throw syntax_error("gap size > 0x10000");
//		if((uint8)c!=fillbyte && is_data) throw syntax_error("illegal fillbyte in data segment");		denk…

		if(dpos<0x10000) memset(&core[dpos], c, min((uint)sz,0x10000-dpos));
		if((dpos+=sz)>size && dpos_valid && size_valid)
			{ dpos_valid=no; throw syntax_error("segment overflow"); }
	}
	else
	{
		dpos_valid = no;
		org_valid = no;
	}
}


/* store space with default fillbyte
*/
void Segment::storeSpace( int sz, bool sz_valid ) throw(syntax_error)
{
	storeSpace(sz, sz_valid, fillbyte);
}

void Segment::storeSpaceUpToAddress(int addr, bool addr_valid ) throw(syntax_error)
{
	storeSpace(addr-logicalAddress(), addr_valid && logicalAddressValid(), fillbyte);
	org_valid = addr_valid;
}






































