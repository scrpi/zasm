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
#include "Z80Assembler.h"



/*	Helper: write one line with address, code and text to log file
	address	= base address of opcode
	bytes	= pointer to start of opcode
	count	= size of opcode (num. bytes)
	offset	= offset in bytes[]
	text	= source line etc.

	returns:  bytes printed. (may be faked to 'oll' for longish fillers)

	if multiple lines must be printed for an opcode (a 'defs' or similar)
	then address, bytes and count must not be incremented by the caller
	instead the caller only increments offset

	format:
	1234: 12345678	sourceline
*/
uint write_line_with_objcode(FD& fd, uint address, uint8* bytes, uint count, uint offset, cstr text)
{
	address += offset;
	bytes   += offset;
	count   -= offset;

	switch(count)
	{
	case 0:	fd.write_fmt("%04X:         \t%s\n",  address&0xffff,		  text); return 0;
	case 1:	fd.write_fmt("%04X: %02X      \t%s\n",address, peek1X(bytes), text); return 1;
	case 2: fd.write_fmt("%04X: %04X    \t%s\n",  address, peek2X(bytes), text); return 2;
	case 3: fd.write_fmt("%04X: %06X  \t%s\n",    address, peek3X(bytes), text); return 3;
	case 4:
	default:
		// wenn zuletzt 4 gleiche Bytes geloggt wurden
		// und noch mehr als 4 Bytes folgen
		// und nur noch diese Bytes folgen
		// dann verkürze die ausgegebenen Datenbytes mit "...":
		if(offset>=4 && count>4)
		{
			uint8* p = bytes-4;
			uint8* e = bytes+count;
			uint8  c = *p++;
			while(p<e && *p==c) ++p;
			if(p==e)
			{
				fd.write_fmt("%04X: %02X...   \t%s\n", address, peek1X(bytes), text);
				return count;
			}
		}

		fd.write_fmt("%04X: %08X\t%s\n", address, peek4X(bytes), text);
		return 4;
	}
}



/* ==============================================================
		Write List File
============================================================== */


void Z80Assembler::writeListfile(cstr listpath, int style) throw(any_error)
{
	XLogLine("writeListfile %s (style=%i)", listpath, style);

	XXXASSERT(listpath && *listpath);
	XXXASSERT(source.count()); 	// da muss zumindest das selbst erzeugte #include in Zeile 0 drin sein

	FD fd(listpath,'w');

	uint si=0,ei=0;	// source[] index, errors[] index
	while( si<source.count() )
	{
		SourceLine& sourceline = source[si++];
		Segment& segment = *sourceline.segment;
		if(&segment==NULL) break;		// after '#end'

		if(style&2)	// include objcode:
		{
			XXXASSERT(!segment.size_valid || sourceline.bytecount<=0x10000);
			XXXASSERT(!segment.size_valid || sourceline.byteptr+sourceline.bytecount <= segment.size);

			uint count   = sourceline.bytecount;			// bytes to print
			uint offset  = sourceline.byteptr;				// offset from segment start
			uint8* bytes = segment.core.getData() + offset;// ptr -> opcode
			uint address = segment.address + offset;		// "physical" address of opcode
			// offset = 0;									// offset in opcode

			// print line with address, up to 4 opcode bytes and source line:
			// note: real z80 opcodes have max. 4 bytes
			offset = write_line_with_objcode(fd, address, bytes, count, 0, sourceline.text);

			// print errors and suppress printing of further opcode bytes:
			while( ei<errors.count() && errors[ei].sourceline == &sourceline )
			{
				fd.write_fmt("***ERROR***   \t%s^ %s\n", sourceline.whitestr(), errors[ei++].text);
				offset = count;
			}

			// print remaining opcode bytes
			// note: real z80 opcodes have max. 4 bytes
			// but some pseudo opcodes like 'defm' or 'defs' may have more:
			while(offset<count)
			{
				offset += write_line_with_objcode(fd, address, bytes, count, offset, "");
			}
		}
		else	// plain listing without object code:
		{
			// print source line
			fd.write_str(sourceline.text); fd.write_char('\n');

			// print errors and suppress printing of further opcode bytes:
			while( ei<errors.count() && errors[ei].sourceline == &sourceline )
			{
				fd.write_fmt("%s^ ***ERROR*** %s\n", sourceline.whitestr(), errors[ei++].text);
			}
		}
	}

	while(ei<errors.count())	// remaining errors (presumably without associated source line)
	{
		fd.write_fmt("***ERROR*** %s\n",errors[ei++].text);
	}

	// TODO: Labelliste
	if(style&4) addError("writeListfile: write label list: TODO");

	fd.close_file();
}




























