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
#include "unix/files.h"
#include "unix/tempmem.h"


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
static
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


/*	compare two labels by name
	for sort()
*/
static
bool gt_by_name(Label*& a, Label*& b)
{
	return gt(a->name,b->name);
//	return gt(lowerstr(a->name),lowerstr(b->name));
}



/* ==============================================================
		Write List File
============================================================== */


void Z80Assembler::writeListfile(cstr listpath, int style) throw(any_error)
{
	XXXASSERT(listpath && *listpath);
	XXXASSERT(source.count()); 	// da muss zumindest das selbst erzeugte #include in Zeile 0 drin sein

	FD fd(listpath,'w');
	TempMemPool tempmempool;	// be nice in case zasm is included in another project
	uint si=0,ei=0;	// source[] index, errors[] index


	// Listing with object code:
	// lines after #end are not included in the list file!
	//
	if(style&2)
	{
		while( si<source.count() )
		{
			SourceLine& sourceline = source[si++];

			Segment& segment = *sourceline.segment;
			if(&segment==NULL) break;		// after '#end'

			XXXASSERT(!segment.size_valid || sourceline.bytecount<=0x10000);
			XXXASSERT(!segment.size_valid || sourceline.byteptr+sourceline.bytecount <= segment.size);

			uint count   = sourceline.bytecount;			// bytes to print
			uint offset  = sourceline.byteptr;				// offset from segment start
			uint8* bytes = segment.core.getData() + offset;	// ptr -> opcode
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
	}

	// Plain listing without object code:
	// all lines are included in the list file
	//
	else
	{
		while( si<source.count() )
		{
			SourceLine& sourceline = source[si++];

			// print source line
			fd.write_str(sourceline.text); fd.write_char('\n');

			// print errors and suppress printing of further opcode bytes:
			while( ei<errors.count() && errors[ei].sourceline == &sourceline )
			{
				fd.write_fmt("%s^ ***ERROR*** %s\n", sourceline.whitestr(), errors[ei++].text);
			}
		}
	}

	// List remaining errors:
	// without associated source line
	//
	while(ei<errors.count())
	{
		fd.write_fmt("***ERROR*** %s\n",errors[ei++].text);
	}

	// List Labels:
	// labels are listed in groups by locality, globals first
	// within each group they are sorted by name
	// TODO: option sort by
	//		 Segment + Adresse
	//		 Segment + Name
	//		 File
	//
	if(style&4)
	{
		{	fd.write_str("\n; +++ global symbols +++\n\n");

			Array<Label*> globals = this->labels[0].getItems();
			Array<Label*> labels(globals.copy());
			labels.sort(&gt_by_name);		// sort by name

			uint maxlnamelen = 0;
			for(uint j=0; j<labels.count(); j++)
				maxlnamelen = max(maxlnamelen,(uint)strlen(labels[j]->name));
			limit(7u,maxlnamelen,19u);
			uint maxsnamelen = 0;
			for(uint j=0; j<segments.count(); j++)
				maxsnamelen = max(maxsnamelen,(uint)strlen(segments[j].name));
			limit(7u,maxsnamelen,19u);
			str lnamefiller = spacestr(maxlnamelen);
			str snamefiller = spacestr(maxsnamelen);
			XXASSERT(labels[0]->name==DEFAULT_CODE_SEGMENT);	// "(none)" should be first => exclude from listing

			for(uint j=0+1; j<labels.count(); j++)
			{
				Label* l = labels[j];
			//	cstr		name = l->name;
			//	Segment*	segment = l->segment;
				int			value = l->value;
			//	bool		is_valid = l->is_valid;
			//	uint		sourcelinenumber = l->sourceline;
				SourceLine&	sourceline = source[l->sourceline];
			//	cstr		text = sourceline.text;
				cstr		sourcefile = filename_from_path(sourceline.sourcefile);
				uint		linenumber = sourceline.sourcelinenumber;

				// name  equ $1234 ; -12345 segment sourcefile:linenumber

				uint lnamelen = strlen(l->name);
				fd.write_str(l->name);
				if(lnamelen<maxlnamelen) fd.write_str(lnamefiller+lnamelen);

				if(l->is_valid) fd.write_fmt(" = $%04X ;%8i  %s", value&0xffff, value, l->segment->name);
				else			fd.write_fmt(" = $0000 ; invalid  %s", l->segment->name);

				uint snamelen = strlen(l->segment->name);
				if(snamelen<maxsnamelen) fd.write_str(snamefiller+snamelen);

				fd.write_fmt(" %s:%u\n", sourcefile, linenumber);
			}
		}

		for(uint i=1; i<labels.count(); i++)
		{
			fd.write_str("\n; +++ local symbols +++\n\n");

			Array<Label*> labels = this->labels[i].getItems();

			uint maxlnamelen = 0;
			for(uint j=0; j<labels.count(); j++)
				maxlnamelen = max(maxlnamelen,(uint)strlen(labels[j]->name));
			limit(7u,maxlnamelen,19u);
			uint maxsnamelen = 0;
			for(uint j=0; j<segments.count(); j++)
				maxsnamelen = max(maxsnamelen,(uint)strlen(segments[j].name));
			limit(7u,maxsnamelen,19u);
			str lnamefiller = spacestr(maxlnamelen);
			str snamefiller = spacestr(maxsnamelen);

			for(uint j=0; j<labels.count(); j++)
			{
				Label* l = labels[j];
				int			value = l->value;
				SourceLine&	sourceline = source[l->sourceline];
				cstr		sourcefile = filename_from_path(sourceline.sourcefile);
				uint		linenumber = sourceline.sourcelinenumber;

				// name  equ $1234 ; -12345 segment sourcefile:linenumber

				uint lnamelen = strlen(l->name);
				fd.write_str(l->name);
				if(lnamelen<maxlnamelen) fd.write_str(lnamefiller+lnamelen);

				if(l->is_valid) fd.write_fmt(" = $%04X ;%8i  %s", value&0xffff, value, l->segment->name);
				else			fd.write_fmt(" = $0000 ; invalid  %s", l->segment->name);

				uint snamelen = strlen(l->segment->name);
				if(snamelen<maxsnamelen) fd.write_str(snamefiller+snamelen);

				fd.write_fmt(" %s:%u\n", sourcefile, linenumber);
			}
		}
	}

	fd.write_fmt("\n%s error%s\n", errors.count()?numstr(errors.count()):"no", errors.count()==1?"":"s");
	fd.close_file();
}




























