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


#define SAFE 3
#define LOG 1
#include "helpers.h"
#include "Segment.h"


/*	calculate size of compressed data in .z80 format
*/
uint32 compressed_page_size_z80( uint8 const* q, uint qsize )
{
	XLogIn("compressed_page_size_z80");

	uint8 const* qe = q + qsize;
	uint32 sz = 0;

	while( q<qe )
	{
		uint8 c = *q++;
		if(q==qe || *q!=c)				// single byte
		{
			sz++;
			// special care for compressible sequence after single 0xed:
			if( c==0xed && q+2<=qe && *q==*(q+1) ) { sz++; q++; }
		}
		else							// sequence of same bytes
		{
			int n=1; while( n<255 && q<qe && *q==c ) { n++; q++; }
			if(n>=4 || c==0xed)	sz+=4; 	// compress ?
			else				sz+=n; 	// don't compress
		}
	}

	return sz;
}


/*	write compressed data in .z80 format

		page_id<0:	=> v1.45 format without page header
					compressed or uncompressed data is stored without header.
					compression is indicated by bit head.data&0x20
					note: calculate compressed size with compressed_page_size_z80()
						  to decide whether to compress the page or not!
		page_id≥0:	=> z80 v2.0++ with page header
					all pages are compressed
					pages are preceded by a 3-byte header:
						dw	length of data (without this header; low byte first)
						db	page number of block

		compression scheme:
			dc.b $ed, $ed, count, char
*/
void write_compressed_page_z80 (FD& fd, int page_id , uint8 const* q, uint qsize) throw(file_error,bad_alloc)
{
	XLogIn("write_compressed_page_z80",page_id);

	assert(qsize>=1 kB && qsize<=64 kB);
	assert(page_id>=0 || qsize==0xC000);		// v1.45 must be 48k

	uint8 const* qe = q + qsize;
	uint8 zbu[qsize*5/3+9];					// worst case size: 5/3*qsize
	uint8* z = zbu;

	while( q<qe )
	{
		uint8 c = *q++;
		if(q==qe || *q!=c)					// single byte: next byte is different
		{
			*z++ = c;
			// special care for compressible sequence after single 0xed:
			// prevent triple 0xed
			if(c==0xed && q+2<=qe && *q==*(q+1)) { *z++ = *q++; }
		}
		else								// sequence of same bytes
		{
			int n=1; while( n<255 && q<qe && *q==c ) { n++; q++; }
			if( n>=4 || c==0xed ) { *z++ = 0xed; *z++ = 0xed; *z++ = n; *z++ = c; }	// compress ?
			else				  { while(n--) *z++ = c; }							// don't compress
		}
	}

	uint zsize = z-zbu;

	if(page_id>=0)	// v2.0++
	{
		fd.write_uint16_z(zsize);
		fd.write_char(page_id);
	}
	fd.write_bytes(zbu,zsize);
}











#if 0
/*	compress .z80 block v2.01 or later
	core[]:	data[]    in & out
	size:   data size in & out

	if id<0 then a v1.45 block is created:
		no header with size and id is prepended
		if size increases then the block is not compressed: check size==0xc000 on return!

	block layout:
		dc.w		length of data (without this header; low byte first)
		dc.b		page number of block
		dc.s		compressed data follows
	compression scheme:
		dc.b $ed, $ed, count, char
*/
void compressPageZ80( Segment& segment )
{
	Core&   core = segment.core;
	uint32& size = segment.size;
	int     flag = segment.flag;	// page ID

	XXXASSERT(flag>=0 || size==0xc000);	// id<0  -->  v1.45

	uint8  const* q = core.getData();
	uint8  const* qe = q + size;
	uint8  zbu[size*5/3+9];
	uint8* z = zbu + 3;

	while(q<qe)
	{
		uint8 c = *q++;
		if(q==qe || *q!=c)		// single byte
		{
			*z++ = c;
			if(c==0xed && q+2<=qe && *q==*(q+1)) { *z++ = *q++; }
		}
		else					// sequence of same bytes
		{
			int n=1; while(n<255 && q<qe && *q==c) { n++; q++; }
			if(n>=4 || c==0xed)	{ *z++ = 0xed; *z++ = 0xed; *z++ = n; *z++ = c; }	// compress ?
			else				{ while(n--) *z++ = c; }							// don't compress
		}
	}

	if(flag<0)	// v1.45
	{
		uint32 zsize = z-zbu-3;
		if(zsize>=size) return;		// out: size=0xc000  -->  uncompressed!
		memcpy(zbu+3,core.getData(),zsize);
		size = zsize;
	}
	else		// v2.0++
	{
		uint32 zsize = z-zbu;		// len incl. head
		poke2Z(zbu,zsize-3);		// data len
		zbu[2] = flag;				// block id
		if(core.count()<zsize) { core.purge(); core.grow(zsize); }
		memcpy(zbu,core.getData(),zsize);
		size = zsize;
	}
}
#endif

#if 0
/*	compress ram page for Jupiter Ace
*/
static void compressPageAce( Array<uint8>& zbu )
{
	uint   n  = zbu.count();
	uint8  bu[n]; memcpy(bu,zbu.getData(),n);
	uint8* z  = 0;
	uint8* q  = bu;
	uint8* qe = bu + n;

// Zielgröße berechnen:
	while(q<qe)
	{
		uint8 c = *q++;
		uint  n = 1; while(q<qe && *q==c && n<240) { q++; n++; }
		z += c==0xed || n>3 ? 3 : n;
	}

// Array resizen:
	zbu.purge();
	zbu.grow((size_t)z);

// Daten komprimieren:
	z = zbu.getData();
	q = bu;
	qe = bu+n;

	while(q<qe)
	{
		uint8 c = *q++;

		uint n=1; while(q<qe && *q==c && n<240) { q++; n++; }
		if(c==0xed || n>3)
		{
			*z++ = 0xed;
			*z++ = n;
			*z++ = c;
		}
		else
		{
			while(n--) *z++ = c;
		}
	}
}
#endif

#if 0
static uint8* compress_z80(uint id, uint8 const* q, uint qsize, uint8* z)
{
	uint8 const* qe = q + qsize;
	uint8* za = z;
	z += 3;

	while(q<qe)
	{
		uint8 c = *q++;
		if(q==qe || *q!=c)		// single byte
		{
			*z++ = c;
			if(c==0xed && q+2<=qe && *q==*(q+1)) { *z++ = *q++; }
		}
		else					// sequence of same bytes
		{
			int n=1; while(n<255 && q<qe && *q==c) { n++; q++; }
			if(n>=4 || c==0xed)	{ *z++ = 0xed; *z++ = 0xed; *z++ = n; *z++ = c; }	// compress ?
			else				{ while(n--) *z++ = c; }							// don't compress
		}
	}

	poke2Z(za,z-za-3);			// data len
	za[2] = id;					// block id

	return z;
}
#endif



