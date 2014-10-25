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
#include "Z80Head.h"
#include "helpers.h"



/* ==============================================================
		write segments[] to output file
		no error checking
============================================================== */

void Z80Assembler::writeTargetfile(cstr dname, char style) throw(any_error)
{
	XXXASSERT(errors.count()==0);
	XXXASSERT(dname!=NULL);
	XLogLine("writeTargetfile %s, '%c'", dname, style);

	if(target==NULL) target="ROM";
	cstr ext = lowerstr(target);
	if(style=='x' && (eq(ext,"rom") || eq(ext,"bin"))) ext = "hex";

	if(endswith(dname,".$")) dname = catstr(leftstr(dname,strlen(dname)-1),ext);
	FD fd(dname,'w');	// create & open file for writing

	if(eq(ext,"rom") || eq(ext,"bin")) writeBinFile(fd);
	else if(eq(ext,"hex")) writeHexFile(fd);
	else if(eq(ext,"sna")) writeSnaFile(fd);
	else if(eq(ext,"z80")) writeZ80File(fd);
	else if(eq(ext,"ace")) writeAceFile(fd);
	else if(startswith(ext,"tap")) writeTapFile(fd);
	else if(eq(ext,"o")||eq(ext,"80")) writeZX80File(fd);
	else if(eq(ext,"p")||eq(ext,"81")||eq(ext,"p81")) writeZX81File(fd);
	else throw syntax_error("internal error: writeTargetfile: unknown target");
}


void Z80Assembler::writeBinFile(FD& fd) throw(any_error)	// no error checking!
{
	// just concatenate everything:
	for(uint i=0; i<segments.count() && segments[i].isCode(); i++)
	{
		Segment& s = segments[i];
		fd.write_bytes(s.getData(),s.size);
	}
}


void Z80Assembler::writeSnaFile(FD& fd) throw(any_error)	// no error checking!
{
	// just concatenate everything:
	writeBinFile(fd);
}


void Z80Assembler::writeZX80File(FD& fd) throw(any_error)	// no error checking!
{
	// just concatenate everything:
	writeBinFile(fd);
}


void Z80Assembler::writeZX81File(FD& fd) throw(any_error)	// no error checking!
{
	// just concatenate everything:
	writeBinFile(fd);
}


void Z80Assembler::writeHexFile(FD& fd) throw(any_error)	// no error checking!
{
	// store data from segments into hex file starting at address 0 with no gaps
	//	 ignoring segment addresses!
	// trailing fillbytes in each segment are omitted from the file
	//	 but accounted for the address of the following segment
	//	 this is just to save space in the file and may be used
	//	 to skip over areas of not yet erased contents in eproms
	uint32 addr = 0;
	for(uint i=0; i<segments.count() && segments[i].isCode(); i++)
	{
		Segment& s = segments[i];

		uint8  fillbyte = s.fillbyte;
		uint8* data = s.getData();
		uint32 len  = s.size;
		while(len>0 && data[len-1]==fillbyte) { len--; }

		write_intel_hex(fd, addr, data, len);
		addr += s.size;
	}

	// eof marker:
	fd.write_str(":00000001FF\r\n");
}


void Z80Assembler::writeTapFile(FD& fd) throw(any_error)	// no error checking!
{
	// tape data blocks are written like this:
	//		dw	len				; number of bytes that follow
	//		db	blocktype		; not for Jupiter ACE!
	//		ds	data			; from segment
	//		db	checksum		; simple xor of blocktype + data bytes

	// include block type byte in tap block?
	// ZX Spectrum: yes; Jupiter ACE: no
	bool writetypebyte = segments[0].size != 25;	// 25 => Jupiter ACE

	// Jupiter ACE: write block type bytes if they do not alternate as expected:
	for(uint i=0; !writetypebyte && i<segments.count() && segments[i].isCode(); i++)
	{ writetypebyte = segments[i].flag != (i&1 ? 0xff : 0x00); }

	for(uint i=0; i<segments.count() && segments[i].isCode(); i++)
	{
		Segment& s = segments[i];
		uint checksum = s.flag;
		uint8* qa = s.getData();
		for(uint8* q = qa+s.size; q>qa; ) checksum ^= *--q;

		fd.write_uint16_z(writetypebyte+s.size+1);	// length of following data
		if(writetypebyte) fd.write_uint8(s.flag);	// block type
		fd.write_bytes(qa,s.size);					// data
		fd.write_uint8(checksum);					// checksum
	}
}


void Z80Assembler::writeZ80File(FD& fd) throw(any_error)	// no error checking!
{
	// first segment is the z80 file header and written as-is
	// subsequent segments are written as compressed memory pages
	// except if v1.45 is detected and the compressed data bit is not set
	Segment& hs = segments[0];
	fd.write_bytes(hs.getData(),hs.size);

	if(hs.size == z80v1len)		// write v1.45 single page:
	{
		Segment& s = segments[1];
		if(hs.core[12] & 0x20)	// head.data.bit5
			 write_compressed_page_z80( fd, -1, s.getData(), s.size );
		else fd.write_bytes( s.getData(), s.size );
	}
	else // write v2.0++ pages:
	{
		for(uint i=1; i<segments.count() && !segments[i].isData(); i++)
	    {
			Segment& s = segments[i];
			write_compressed_page_z80( fd, s.flag, s.getData(), s.size);
		}
	}
}


void Z80Assembler::writeAceFile(FD& fd) throw(any_error)	// no error checking!
{
	// just compress & concatenate everything:
	for(uint i=0; i<segments.count() && segments[i].isCode(); i++)
	{
		Segment& s = segments[i];
		write_compressed_page_ace(fd, s.getData(), s.size);
	}

	// eof marker:
	fd.write_uint8(0xED);
	fd.write_uint8(0);
}



/* ==============================================================
		check segments[] for #target
============================================================== */

void Z80Assembler::checkTargetfile() throw(any_error)
{
	XLogLine("checkTargetfile");

	// Prevent empty output:
	if(segments.totalSize()==0) throw syntax_error("code size = 0");

	// Move all code segments before all data segments:
	for(uint i=1; i<segments.count(); i++)
	{
		if(segments[i].isData()) continue;
		for(int j=i-1; j>=0 && segments[j].isData(); j--) kio::swap(segments[j],segments[j+1]);
	}

	// remove empty DEFAULT_CODE_SEGMENT:
	XXXASSERT(segments[0].isCode());
	XXXASSERT(segments[0].size>0 || (segments.count()>1 && segments[0].name==DEFAULT_CODE_SEGMENT));
	if(segments[0].size==0) segments.remove((uint)0);

	if(target==NULL) target="ROM";
	if(eq(target,"ROM") || eq(target,"BIN")) checkBinFile();
	else if(eq(target,"SNA")) checkSnaFile();
	else if(eq(target,"Z80")) checkZ80File();
	else if(eq(target,"ACE")) checkAceFile();
	else if(startswith(target,"TAP")) checkTapFile();
	else if(eq(target,"O")||eq(target,"80")) checkZX80File();
	else if(eq(target,"P")||eq(target,"81")||eq(target,"P81")) checkZX81File();
	else throw syntax_error("internal error: checkTargetfile: unknown target");
}



/*	check segments[] for target "TAP":
	verify that all tape blocks have a flag
*/
void Z80Assembler::checkTapFile() throw(any_error)
{
	for(uint i=0; i<segments.count() && segments[i].isCode(); i++)
	{
		Segment& s = segments[i];
		if(!s.flag_valid) throw syntax_error(usingstr("segment %s: flag missing", s.name));
		if(s.size==0)     throw syntax_error(usingstr("segment %s: size = 0", s.name));
		if(s.size>0xfeff) throw syntax_error(usingstr("segment %s: size = %u (max = 0xfeff)", s.name, s.size));
	}
}


void Z80Assembler::checkBinFile() throw(any_error)
{
	throw syntax_error("checkBinFile: TODO");
}

void Z80Assembler::checkSnaFile() throw(any_error)
{
	throw syntax_error("checkSnaFile: TODO");
}

void Z80Assembler::checkAceFile() throw(any_error)
{
	throw syntax_error("checkAceFile: TODO");
}

void Z80Assembler::checkZX80File() throw(any_error)
{
	throw syntax_error("checkZX80File: TODO");
}

void Z80Assembler::checkZX81File() throw(any_error)
{
	throw syntax_error("checkZX81File: TODO");
}





void Z80Assembler::checkZ80File() throw(any_error)
{
	uint seg_cnt = 0; for(uint i=0; i<segments.count() && segments[i].isCode(); i++) { seg_cnt++; }

	// assert header and at least one ram page:
	if(seg_cnt<2) throw syntax_error("no ram pages found");

	// verify that first block is the header:
	Segment& hs = segments[0];
	if(hs.flag_valid) throw syntax_error("first code segment must be the z80 file header (no flag!)");

	Z80Head& head = *(Z80Head*)hs.getData();

	// handle version 1.45:
	if(hs.size == z80v1len)
	{
		// check header:
		if(!head.pch && !head.pcl) addError("header v1.45: PC at offset 6 must not be 0");

		// check segments:
		if(seg_cnt>2) addError("v1.45: only one ram page allowed");
		Segment& s = segments[1];
		if(s.size!=0xc000) addError(usingstr("segment %s: v1.45: page size must be 0xC000",s.name));
		if(s.flag_valid) addError(usingstr("segment %s: v1.45: no page ID allowed",s.name));

		// comfort: clear compression flag if size increases:
		if(head.data&0x20 && compressed_page_size_z80(s.getData(),0xc000)>0xc000) head.data -= 0x20;
		return;
	}

	// v2.0++
	// check header:

	if(hs.size < z80v3len && hs.size!=z80v2len) throw syntax_error("header: length must be 30 (v1.45), 55 (v2.01) or 86++ (v3++)");
	if(head.pch || head.pcl) throw syntax_error("header v2++: PC at offset 6 must be 0");

	uint n = head.h2lenl + 256 * head.h2lenh;	// length of header extension
	if(32+n != hs.size) throw syntax_error(usingstr("header v2++: wrong header extension length at offset 30: %u + 32 != %u", n, hs.size ));

	Model model = head.getZxspModel();
	if(model==void_model) throw syntax_error("header: illegal model");
	if(model>=zxplus3 && model<=zxplus2a_span && hs.size<z80v3len+1) throw syntax_error("header: size must be ≥ 87 for +3/+2A for port_1ffd byte (warajewo/xzx extension)");

//	uint32 cc = head.getCpuCycle(model_info->cpu_cycles_per_frame);
//	if(cc>70000) {}

	bool spectra_used = head.isVersion300() && (head.rldiremu & 0x08);
	if(spectra_used && model>inves) throw syntax_error("header: SPECTRA extension can only be attached to ZX Spectrum 48k models (rldiremu&8)");
	if(spectra_used && hs.size<89) throw syntax_error("header: size must be ≥ 89 bytes for SPECTRA extension (rldiremu&8)");

	// v2.0++
	// check pages:
	// verify that all pages have a flag and proper size

//	bool ay_used	= head.rldiremu & 0x04;
//	bool fuller_ay	= head.rldiremu & 0x40;				// only if ay_used
//	bool if1_used	= head.model==1 || head.model==5;
//	bool mgt_used	= head.model==3 || head.model==6;
	bool paged_mem	= (model>=zx128 && model<=zxplus2a_span) || model==pentagon || model==scorpion || model==samcoupe;
	bool varying_pagesize = model>=zx80;

	static uint sz[num_models] = {16,48,48,48,48,48,128,128,128,128,128,128,128,128,128,48,48,48,48,48,48,128,256,256,1,1,2,16,16,3};
	uint32 ramsize = (varying_pagesize && head.spectator ? head.spectator : sz[model]) * 1024;

	uint32 addr = 0;	// for varying_pagesize
	uint32 loaded = 0;

	for(uint i=1; i<seg_cnt; i++)
    {
		Segment& s = segments[i];
        if(!s.flag_valid) { addError(usingstr("segment %s: page ID missing",s.name)); continue; }
        uint page_id = s.flag;

        switch (page_id)
        {
        case 2:	// rom at address 0x4000
				if(!paged_mem) addError(usingstr("segment %s: invalid page ID: this model does not have 32 kB of rom",s.name));
        case 0:	// rom at address 0x0000
        case 1:	// IF1, Disciple or Plus D Rom
				goto anypage;			// TODO: b&w machines may have different rom size (not yet supported in zxsp)
        case 11: // Multiface Rom or ram page if ram size > 128k
				if(ramsize>128 kB) goto rampage; else goto anypage;
		case 12: // SPECTRA Rom
		case 13: // SPECTRA Ram
		case 14: // SPECTRA Ram
				if(spectra_used) goto anypage; else goto rampage;
		case 8:	// convert page number 48k -> 128k
			    if(!paged_mem && !varying_pagesize) page_id = 3;
			    goto rampage;
		default:
rampage:	page_id -= 3;
			if(varying_pagesize)	// b&w machines:
			{
				if(page_id>7) { addError(usingstr("segment %s: page ID out of range",s.name)); continue; }
				if(loaded & (1<<page_id)) { addError(usingstr("segment %s: page ID occured twice",s.name)); continue; }
				loaded |= 1<<page_id;
				uint32 size = 1024 << page_id;
				if(size!=s.size) { addError(usingstr("segment %s: page size does not match page ID",s.name)); continue; }
				if(addr+size>s.size) { addError(usingstr("segment %s: sum of page sizes exceeds ram size",s.name)); continue; }
				addr += size;
			}
			else	// std. 16k page:
			{
				if(page_id >= ramsize>>14) { addError(usingstr("segment %s: page ID out of range",s.name)); continue; }
				if(loaded & (1<<page_id)) { addError(usingstr("segment %s: page ID occured twice",s.name)); continue; }
				loaded |= 1<<page_id;
				addr += 16 kB;
anypage:		if(s.size!=16 kB) { addError(usingstr("segment %s: page size must be 16 kB",s.name)); continue; }
			}
			continue;
        }
    }

	if(errors.count()) return;
	if(addr<ramsize)
	{
		uint32 needed = varying_pagesize ? (ramsize/0x400) : ~((0xffffffff << (ramsize/0x4000)));
		uint32 missing = needed &= ~loaded;
		for(int i=0; i<32; i++)
		{ if((missing>>i)&1) addError(usingstr("code segment for page ID %i is missing",i+3)); }
	}
}





















