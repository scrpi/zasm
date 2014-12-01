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
#include "helpers.h"



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
//	return gt_tolower(a->name,b->name);
}


/*	calculate a padding string for names
	padding string is used to make all names align properly
	if some labels are excessively long, then these may extend beyond the common length
	the returned string can be used like this:

	printf("%s%s",name,padding+strlen(name))
*/
static
cstr calc_padding(Array<uint32>& lens)
{
	if(lens.count()==0) return "";

	lens.sort();
	uint32 maxlen = max(7u,lens.last());
	str padding = spacestr(maxlen);
	if(maxlen<=19) return padding;

	uint32 bestlen = lens[lens.count()*95/100];
	memset(padding+bestlen,0,sizeof(char)*(maxlen-bestlen));
	return padding;
}

// convenience:
static
cstr calc_padding(Segments& segments)
{
	Array<uint32> lens(segments.count());
	for(uint i=0; i<segments.count(); i++) { lens[i] = (uint32) strlen(segments[i].name); }
	return calc_padding(lens);
}

// convenience:
static
cstr calc_padding(Array<Label*>& labels)
{
	Array<uint32> lens(labels.count());
	for(uint j=0; j<labels.count(); j++) { lens[j] = (uint)strlen(labels[j]->name); }
	return calc_padding(lens);
}

// convenience:
inline
cstr calc_padding(Labels& labels)
{
	return calc_padding(labels.getItems());
}

// convenience:
inline
cstr calc_padding(ObjArray<Labels>& labelsAE)
{
	Array<Label*> labels;
	for(uint i=0;i<labelsAE.count();i++) { labels.append(labelsAE[i].getItems()); }
	return calc_padding(labels);
}

cstr u5str(uint n, bool valid)
{
	if(!valid) return "***invalid***";
	str s = spacestr(5);
	sprintf(s,"%u",n&0xffff); if(n<10000) *strchr(s,0)=' ';
	return s;
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

	if(style&6)
	{
		cstr pfx = style&2 ? "              \t" : "";
		fd.write_fmt("%s; --------------------------------------\n",pfx);
		fd.write_fmt("%s; zasm: assemble \"%s\"\n",pfx,source_filename);
		fd.write_fmt("%s; date: %s\n",pfx,datetimestr(timestamp));
		fd.write_fmt("%s; --------------------------------------\n\n\n",pfx);
	}

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

			// if line contains a label which is defined with 'equ' then print label value instead of address
			if(count==0 && sourceline[0]>='A')				// no space, '#' or '.' => label
			{
				sourceline.rewind();
				cstr lname = sourceline.nextWord();
				sourceline.testChar(':'); sourceline.testChar(':');
				if(sourceline.testWord("equ") || sourceline.testWord("defl") || sourceline.testChar('='))
				{
					for(uint i=0;i<labels.count();i++)
					{
						Label& label = labels[i].find(lname);
						if(&label!=NULL && label.sourceline==si-1) { address=label.value; break; }
					}
				}
			}

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
		cstr spadding = calc_padding(segments);

		// list segments
		// "#CODE|#DATA name: start=n, len=n, flag=n"

		fd.write_str("\n\n; +++ segments +++\n\n");
		for(uint i=0; i<segments.count(); i++)
		{
			Segment& s = segments[i];
			if(s.has_flag)
				fd.write_fmt("#%s %s:%s start=%s len=%s flag=%i\n",
					s.isCode()?"CODE":"DATA",
					s.name,spadding+strlen(s.name),
					u5str(s.address,s.address_valid), u5str(s.size,s.size_valid), s.flag);
			else
				fd.write_fmt("#%s %s:%s start=%s len=%s\n",
					s.isCode()?"CODE":"DATA",
					s.name,spadding+strlen(s.name),
					u5str(s.address,s.address_valid), u5str(s.size,s.size_valid));
		}

		// list labels:
		// "name = $1234 = -12345  segment  sourcefile:linenumber (unused)"

		for(uint i=0; i<labels.count(); i++)
		{
			fd.write_str(i?"\n; +++ local symbols +++\n\n":"\n; +++ global symbols +++\n\n");

			Array<Label*> labels = this->labels[i].getItems().copy();
			labels.sort(&gt_by_name);							// sort by name
			XXASSERT(i||labels[0]->name==DEFAULT_CODE_SEGMENT);	// "(DEFAULT)" should be first => exclude from listing
			cstr lpadding = calc_padding(labels);

			for(uint j=i?0:1; j<labels.count(); j++)
			{
				Label* l = labels[j];
				if(i&&l->is_global) continue;		// don't list .globl labels in locals[]
				bool		is_valid = l->is_valid;
				bool		is_used = l->is_used;
				if(!is_valid&&!is_used) continue;
				cstr		name = l->name;
				Segment*	segment = l->segment; if(!segment) segment = &segments[0];
				int			value = l->value;
			//	uint		sourcelinenumber = l->sourceline;
				SourceLine&	sourceline = source[l->sourceline];
			//	cstr		text = sourceline.text;
				cstr		sourcefile = filename_from_path(sourceline.sourcefile);
				uint		linenumber = sourceline.sourcelinenumber;

				fd.write_fmt("%s%s = ", name, lpadding+strlen(name));

				if(!l->is_defined)
					fd.write_str("***undefined***");
				else if(!l->is_valid)
					fd.write_fmt("***invalid***   %s%s %s:%u",
						segment->name, spadding+strlen(segment->name), sourcefile, linenumber+1);
				else
					fd.write_fmt("$%04X = %6i  %s%s %s:%u",
						value&0xffff, value,
						segment->name, spadding+strlen(segment->name), sourcefile, linenumber+1);

				fd.write_str(l->is_used?"\n":" (unused)\n");
			}
		}

		// list unresolved labels:
		if(pass>1 && errors.count())
		{
			Array<Label*> unresolved_labels;

			for(uint i=0;i<labels.count();i++)
			{
				Array<Label*>& labels = this->labels[i].getItems();
				for(uint j=0;j<labels.count();j++)
				{
					Label* l = labels[j];
					if(!l->is_valid && l->is_used) unresolved_labels.append(l);
				}
			}

			if(unresolved_labels.count())
			{
				fd.write_str("\n\n; +++ used but undefined or unresolved labels +++\n\n");

				cstr lpadding = calc_padding(unresolved_labels);

				for(uint i=0;i<unresolved_labels.count(); i++)
				{
					Label* l = unresolved_labels[i];
					fd.write_fmt("%s%s = %s\n",
						l->name,lpadding+strlen(l->name),
						l->is_defined?"***unresolved***":"***undefined***");
				}
			}
		}
	}

	// list elapsed time and errors:
	fd.write_fmt("\n\ntotal time: %3.4f sec.\n",now()-timestamp);
	fd.write_fmt("%s error%s\n", errors.count()?numstr(errors.count()):"no", errors.count()==1?"":"s");
	fd.close_file();
}




























