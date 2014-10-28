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


/* ==============================================================
		Write List File
============================================================== */


void Z80Assembler::writeListfile(cstr listpath, bool v, bool w) throw(any_error)
{
	XLogLine("writeListfile %s, %s%s", listpath, v?"v":"", w?"w":"");

	XXXASSERT(listpath && *listpath);
	XXXASSERT(source.count()); 	// da muss zumindest das selbst erzeugte #include in Zeile 0 drin sein

	FD fd(listpath,'w');

	uint si=0,ei=0;	// source[] index, errors[] index
	while( si<source.count() )
	{
		SourceLine* sourceline = &source[si++];

		if(v)	// include objcode:
		{
			uint ocode_size = sourceline->bytecount;
			if(ocode_size==0)
			{
				fd.write_str("              \t");
			}
			else
			{
				Segment* segment = sourceline->segment;
				uint ocode_index = sourceline->byteptr;

				XXXASSERT(segment);
				XXXASSERT(!segment->size_valid || ocode_size<=0x10000);
				XXXASSERT(!segment->size_valid || ocode_index+ocode_size <= segment->size);

				uint8* core = segment->core.getData();

				while(ocode_size>4)
				{
					uint32 data = peek4X(core+ocode_index);
					if(data != segment->fillbyte*0x01010101u)					// exclude (large) ranges of empty space
					{
						fd.write_fmt("%04X: ",segment->address+ocode_index);	// address
						fd.write_fmt("%08X\n",data);							// 4 data bytes; note: assumes int==int32
					}
					ocode_index += 4;
					ocode_size  -= 4;
				}

				fd.write_fmt("%04X: ",segment->address+ocode_index);							// address
				for(uint n=0; n<ocode_size; n++) { fd.write_fmt("%02X",core[ocode_index++]); }  // 1..4 data bytes
				fd.write_str(&"        \t"[ocode_size*2]);						// padding for bytes less than 4; plus tab
			}
		}

		fd.write_str(sourceline->text); fd.write_char('\n');					// source line

		while( ei<errors.count() && errors[ei].sourceline == sourceline )
		{
			if(v) fd.write_str(usingstr("***ERROR***   \t%s^ %s\n", sourceline->whitestr(), errors[ei++].text));
			else  fd.write_str(usingstr("%s^ ***ERROR*** %s\n", sourceline->whitestr(), errors[ei++].text));
		}
	}

	while(ei<errors.count())	// remaining errors (presumably without associated source line)
	{
		fd.write_fmt("***ERROR*** %s\n",errors[ei++].text);
	}

	// TODO: Labelliste
	if(w) addError("writeListfile: write label list: TODO");

	fd.close_file();
}




























