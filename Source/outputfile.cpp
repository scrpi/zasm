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



#define SAFE 3
#define LOG 1
#include "Z80Assembler.h"
#include "Z80Header.h"
#include "helpers.h"
#include "unix/files.h"



/* ==============================================================
		write segments[] to output file
		no error checking
		if name ends with ".$" then the caller's dname is updated
			to the actually used name (unprotected tempstr)
============================================================== */

void Z80Assembler::writeTargetfile(cstr& dname, int style) throw(any_error)
{
	XXXASSERT(errors.count()==0);
	XXXASSERT(dname!=NULL);
//	XLogLine("writeTargetfile %s, '%c'", dname, style);

	if(target==NULL) target="ROM";
	cstr ext = lowerstr(target);
	if(style=='x' && (eq(ext,"rom") || eq(ext,"bin"))) ext = "hex";
	if(style=='s' && (eq(ext,"rom") || eq(ext,"bin"))) ext = "s19";

	if(endswith(dname,".$")) dname = catstr(leftstr(dname,strlen(dname)-1),ext);
	target_filepath = dname;
	FD fd(dname,'w');	// create & open file for writing

	if(eq(ext,"rom") || eq(ext,"bin")) writeBinFile(fd);
	else if(eq(ext,"hex")) writeHexFile(fd);
	else if(eq(ext,"s19")) writeS19File(fd);
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

void Z80Assembler::writeS19File(FD& fd) throw(any_error)	// no error checking!
{
	cstr info = catstr(basename_from_path(source_filename)," ",datestr(timestamp));
	write_srecord(fd,S19_InfoHeader,0,(uchar*)info,min((uint)strlen(info),64u));

	// store data from segments into S-Record file starting at address 0 with no gaps
	//	 ignoring segment addresses!
	// trailing fillbytes in each segment are omitted from the file
	//	 but accounted for the address of the following segment
	//	 this is just to save space in the file and may be used
	//	 to skip over areas of not yet erased contents in eproms
	uint32 address = 0;
	uint   srcount = 0;
	for(uint i=0; i<segments.count() && segments[i].isCode(); i++)
	{
		Segment& s = segments[i];

		uint8  fillbyte = s.fillbyte;
		uint8* data = s.getData();
		uint32 len  = s.size;
		while(len>0 && data[len-1]==fillbyte) { len--; }

		srcount += write_motorola_s19(fd, address, data, len);
		address += s.size;
	}

	// eof marker:
	write_srecord(fd,S19_RecordCount,srcount,NULL,0);
	write_srecord(fd,S19_BlockEnd,0,NULL,0);
}


void Z80Assembler::writeTapFile(FD& fd) throw(any_error)	// no error checking!
{
	// tape data blocks are written like this:
	//		dw	len				; number of bytes that follow
	//		db	blocktype		; not for Jupiter ACE!
	//		ds	data			; from segment(s)
	//		db	checksum		; simple xor of blocktype + data bytes

	uint i0=0; while(i0<segments.count() && segments[i0].size==0 && !segments[i0].has_flag) { i0++; }
	XXXASSERT(segments[i0].has_flag);
	uint i,j;

	// include block type byte in tap block?
	// ZX Spectrum: yes;
	// Jupiter ACE: no, except:
	//		first header block (and segment!) must be 25 bytes long (Jupiter ACE header block size)
	//		block type bytes must alternate  $00 - $ff - $00 - $ff etc.
	bool jupiterace = segments[i0].size==25;
	int h = 0x00;
	for(i=i0; jupiterace && i<segments.count() && segments[i].isCode(); i++)
	{ if(segments[i].has_flag) { jupiterace = segments[i].flag == h; h ^= 0xff; } }
	bool writetypebyte = !jupiterace;

	// write tape blocks
	// each block may consist of multiple segmentes
	// where the first segment has the flag byte defined and
	// following segments without flag byte are appended to this block.
	//
	for( i=i0; i<segments.count() && segments[i].isCode(); )
	{
		Segment* s = &segments[i];
		uint flag = s->flag;
		uint size = s->size;

		// calc block size:
		for(j=i+1; j<segments.count(); j++)
		{
			s = &segments[j];
			if(s->isData()) break;		// end of code
			if(s->has_flag) break;		// next tape block
			size += s->size;			// accumulate size
		}

		// write block size and block type:
		fd.write_uint16_z(writetypebyte+size+1);	// length of following data
		if(writetypebyte) fd.write_uint8(flag);		// block type

		// write data and calc checksum
		uint checksum = jupiterace ? 0 : flag;
		while(i<j)
		{
			s = &segments[i++];
			uint8* qa = s->getData();
			fd.write_bytes(qa,s->size);	// write data
			for(uint8* q = qa+s->size; q>qa; ) checksum ^= *--q;
		};

		//write checksum
		fd.write_uint8(checksum);		// checksum
	}
}


void Z80Assembler::writeZ80File(FD& fd) throw(any_error)	// no error checking!
{
	const uint i0 = 0;

	// first segment is the z80 file header and written as-is
	// subsequent segments are written as compressed memory pages
	// except if v1.45 is detected and the compressed data bit is not set
	Segment& hs = segments[i0];
	fd.write_bytes(hs.getData(),hs.size);

	if(hs.size == z80v1len)		// write v1.45 single page:
	{
		Segment& s = segments[i0+1];
		if(hs.core[12]!=255 && hs.core[12] & 0x20)	// head.data.bit5
			 write_compressed_page_z80( fd, -1, s.getData(), s.size );
		else fd.write_bytes( s.getData(), s.size );
	}
	else // write v2.0++ pages:
	{
		for(uint i=i0+1; i<segments.count() && !segments[i].isData(); i++)
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
//	XLogLine("checkTargetfile");

	// Prevent empty output:
	if(segments.totalCodeSize()==0) throw syntax_error("code size = 0");

	// Move all code segments before all data segments:
	for(uint i=1; i<segments.count(); i++)
	{
		if(segments[i].isData()) continue;
		for(int j=i-1; j>=0 && segments[j].isData(); j--)
			kio::swap(*(segments.getData()+j),*(segments.getData()+(j+1)));
	}

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
	segments are either a tape block on their own and have their tape block flag defined
	or they are a sequence of multiple segments which are to be joined into a single tape block.
	Then only the first segment has a flag defined and the following segments must fit with their
	segment ('physical') address exactly to their predecessor. (prev.addr+prev.len == next.addr)
*/
void Z80Assembler::checkTapFile() throw(any_error)
{
	uint i0=0; while(i0<segments.count() && segments[i0].size==0 && !segments[i0].has_flag) { i0++; }

	for(uint i=i0; i<segments.count() && segments[i].isCode(); )
	{
		Segment* s = &segments[i];
		if(!s->has_flag) throw syntax_error(usingstr("tape block %s: flag missing", s->name));
		if((s->flag>255||s->flag<-128))
			throw syntax_error(usingstr("tape block %s: flag value out of range", s->name));

		uint32 size = s->size;
		uint32 addr = s->address;
		cstr   name = s->name;

		while( ++i<segments.count() )
		{
			s = &segments[i];
			if(s->isData()) break;
			if(s->has_flag) break;
			if(s->address!=addr+size) throw syntax_error(usingstr("tape block %s: flag missing", s->name));
			size += s->size;
		}

		if(size==0)     throw syntax_error(usingstr("tape block %s: size = 0", name));
		if(size>0xfeff) throw syntax_error(usingstr("tape block %s: size = %u (max = 0xfeff)", name, size));
	}
}


void Z80Assembler::checkBinFile() throw(any_error)
{
	segments.checkNoFlagsSet();
}


/*	check target SNA
	48k version only
	(there is also a rarely used 128k variant)
*/
void Z80Assembler::checkSnaFile() throw(any_error)
{
	struct SnaHead
	{
		uint8	i;						// $3f
		uint8	l2,h2,e2,d2,c2,b2,a2,f2;
		uint8	j,h,e,d,c,b,yl,yh,xl,xh;
		uint8	iff2;					// bit 2 = iff2 (iff1 before nmi) 0=di, 1=ei
		uint8	r,f,a;
		uint8	spl,sph;
		uint8	int_mode;				// 1
		uint8	border;					// 7	border color: 0=black ... 7=white
	};
	static_assert(sizeof(SnaHead)==27,"sizeof(SnaHead) wrong!");

	const uint i0 = 0;

	segments.checkNoFlagsSet();

	// verify that first block is the header:
	Segment& hs = segments[i0];
	if(hs.size!=27) throw syntax_error(
		usingstr("target SNA: first code segment must be .sna header and 27 bytes long (size=%u)", (uint)hs.size));
	SnaHead* head = (SnaHead*)hs.getData();

	// verify some values from header:
	if((head->i>>6)==1) addError(
		usingstr("segment %i: i register must not be in range [0x40 .. 0x7F] (i=0x%02X)", hs.name, head->i));
	if(head->iff2&~4) addError(usingstr("segment %i: iff2 byte must be 0 or 4 (iff2=0x%02X)", hs.name, head->iff2));
	uint16 sp = head->spl + 256*head->sph;
	if(sp>0 && sp<0x4002) addError(
		usingstr("segment %i: sp register must not be in range [0x0001 .. 0x4001] (sp=0x%04X)", hs.name, sp));
	if(head->int_mode>2) addError(
		usingstr("segment %i: interrupt mode must be in range 0 .. 2 (im=%u)", hs.name, head->int_mode));
	if(head->border>7) addError(
		usingstr("segment %i: border color byte must not be in range 0 .. 7 (brdr=%u)", hs.name, head->border));
	if(errors.count()) return;

	// verify ram segments:
	uint32 addr = 0x4000;
	for(uint i=i0+1; i<segments.count() && segments[i].isCode(); i++)
	{
		Segment& s = segments[i];
		if(s.address!=addr) addError(
			usingstr("segment %s should start at 0x%04X (size=0x%04X)", s.name, (uint)addr, (uint)s.address));
		addr += s.size;
		if(addr>0x10000) { addError(
			usingstr("segment %s extends beyond ram end (end=0x%05X)", s.name, (uint)addr)); break; }
	}
	if(addr<0x10000) addError(
			usingstr("target SNA: total ram size must be 0xC000 bytes (size=0x%04X)", (uint)addr-0x4000));
//	if(errors.count()) return;
}


/*	check target ACE
	checks for presence of all the empty pages
	checks registers
	checks "physical" segment addresses
*/
void Z80Assembler::checkAceFile() throw(any_error)
{
	struct AceHead
	{
		uint32	flag_8001, z1[0x20-1];
		uint32	ramtop, dda, dba, frame_skip_rate, frames_per_tv_tick, fdfd, time_running, color_mode, z2[0x20-8];
		uint32	af,bc,de,hl,ix,iy,sp,pc,af2,bc2,de2,hl2,im,iff1,iff2,i,r,flag_80, z3[0xC0-18];
	};
	static_assert(sizeof(AceHead)==0x400,"sizeof(AceHead) wrong!");

	const uint i0 = 0;

	segments.checkNoFlagsSet();
	uint32 ramsize = segments.totalCodeSize();
	bool ramsize_valid = ramsize==0x2000 || ramsize==0x6000 || ramsize==0xA000;
	if(!ramsize_valid) addError(
		usingstr("total ram size is not supported: must be 0x2000 (3k), 0x6000 (3+16k) or 0xA000 (3+32k)"));

	uint32 addr = 0x2000;	// current address := ram start
	for(uint i=i0; i<segments.count() && segments[i].isCode(); i++)
	{
		Segment& s = segments[i];
		if(s.size==0) continue;		// skip & don't check address
		if(s.address!=addr) addError(
			usingstr("segment %s should start at 0x%04X (address=0x%04X)", s.name, (uint)addr, (uint)s.address));
		if(addr>=0x3C00) { addr+=s.size; continue; }	// Program Ram: segments may be any size
		if(s.size&0x3ff) throw syntax_error(
			usingstr("segment %s size must be a multiple of 0x400 (size=0x%04X)", s.name, (uint)s.size));

		for(uint32 offs=0; offs<s.size; offs+=0x400, addr+=0x400)
		{
			switch(addr)
			{
			case 0x2000:	// VRAM mirror with Z80 registers
			{
				// check empty:
				uint16 zbu[0x200]; memcpy(zbu,&s[offs],0x400);
				for(uint i=0; i<8;  i++) zbu[0x40+2*i]=0;	// clear settings
				for(uint i=0; i<18; i++) zbu[0x80+2*i]=0;	// clear registers
				bool empty=yes; for(int i=1; i<0x200 && empty; i++) { empty = zbu[i]==0; }
				if(!empty) { addError(
					usingstr("segment %s must be empty except for settings and registers", s.name)); break; }

				// check registers:
				AceHead* head = (AceHead*)&s[offs];
				if(peek2Z(&head->flag_8001) != 0x8001) addError(
					usingstr("segment %s: segment[0].flag must be 0x8001", s.name));
				uint ramtop = peek2Z(&head->ramtop);
				if(ramsize_valid && ramtop!=0x2000+ramsize) addError(
					usingstr("segment %s: settings[0].ramtop != ram end address 0x%04X (ramtop=0x%04X)",
						s.name, 0x2000+(uint)ramsize, ramtop));

				uint sp = peek2Z(&head->sp);
				if(sp<0x2002) addError(
					usingstr("segment %s: z80_regs[6].sp must not be in range [0x0000 .. 0x2001] (sp=0x%04X)",
						s.name, sp));
				if(sp>ramtop) addError(
					usingstr("segment %s: z80_regs[6].sp points behind settings[0].ramtop (sp=0x%04X, ramtop=0x%04X)",
						s.name, sp, ramtop));

				if(peek2Z(&head->im)>2)   addError(
					usingstr("segment %s: z80_regs[12].int_mode must be in range 0 .. 2 (im=%u)", s.name, head->im));
				if(peek2Z(&head->iff1)>1) addError(
					usingstr("segment %s: z80_regs[13].iff1 must be 0 or 1 (iff1=%u)", s.name, head->iff1));
				if(peek2Z(&head->iff2)>1) addError(
					usingstr("segment %s: z80_regs[14].iff2 must be 0 or 1 (iff2=%u)", s.name, head->iff2));
				if(peek2Z(&head->i)>255)  addError(
					usingstr("segment %s: z80_regs[15].reg_i must be in range 0 .. 0xff (i=%u)", s.name, head->i));
				if(peek2Z(&head->r)>255)  addError(
					usingstr("segment %s: z80_regs[16].reg_r must be in range 0 .. 0xff (r=%u)", s.name, head->r));
				if(peek2Z(&head->flag_80) != 0x80) addError(
					usingstr("segment %s: z80_regs[17].flag must be 0x80", s.name));
				break;
			}

			case 0x2400:	// VRAM
			case 0x2C00:	// CRAM
				break;

			case 0x2800:	// CRAM mirror
			case 0x3000:	// Prog RAM 1st mirror
			case 0x3400:	// Prog RAM 2nd mirror
			case 0x3800:	// Prog RAM 3rd mirror
			{
				uint32* bu = (uint32*)&s[offs]; bool empty=yes;
				for(int i=0; i<0x100 && empty; i++) { empty = bu[i]==0; }
				if(!s.isEmpty()) addError(
					usingstr("segment %s: page 0x%04X-0x%04X must be empty", s.name, (uint)addr, (uint)addr+0x3ff));
				else break;
			}
			default:		// Prog RAM
				XXXASSERT(addr==0x3C00);
				throw syntax_error(
					usingstr("segment %s extends into program ram at 0x3C00 (segment end=0x%04X)",
						s.name,(uint)(addr-offs+s.size)));
			}
		}
	}
}


/*	check segments for ZX80 targets: O or 80:
	checks min. and max. size
	checks value of E_LINE
*/
void Z80Assembler::checkZX80File() throw(any_error)
{
	struct ZX80Head
	{
	uint8	ERR_NR;		//	db	$FF		;  1  16384 $4000 IY+$00 One less than report code.
	uint8	FLAGS;		//	db	$04		; X1  16385 $4001 IY+$01 Various Flags to control BASIC System:
	uint16	PPC;		//	dw	$FFFE	;  2  16386 $4002 IY+$02 Line number of current line.
	uint16	P_PTR;		//	dw	$434A	; N2  16388 $4004 IY+$04 Position in RAM of [K] or [L] cursor.
	uint16	E_PPC;		//	dw	0		;  2  16390 $4006 IY+$06 Number of current line with [>] cursor.
	uint16	VARS;		//	dw	$4349	; X2  16392 $4008 IY+$08 Address of start of variables area.
	uint16	E_LINE;		//	dw	$434A	; X2  16394 $400A IY+$0A Address of start of Edit Line.
	uint16	D_FILE;		//	dw	$434C	; X2  16396 $400C IY+$0C Start of Display File.
	uint16	DF_EA;		//	dw	$458C	; X2  16398 $400E IY+$0E Address of the start of lower screen.
	uint16	DF_END;		//	dw	$458F	; X2  16400 $4010 IY+$10 Display File End.
	uint8	DF_SZ;		//	db	2		; X1  16402 $4012 IY+$12 Number of lines in lower screen.
	uint8	S_TOPlo, S_TOPhi;	// dw 0	;  2  16403 $4013 IY+$13 The number of first line on screen.
	uint8	X_PTRlo, X_PTRhi;	// dw 0	;  2  16405 $4015 IY+$15 Address of the character preceding the [S] marker.
	uint8	OLDPPClo, OLDPPChi;	// dw 0	;  2  16407 $4017 IY+$17 Line number to which continue jumps.
	uint8	FLAGX;		//	db	0		; N1  16409 $4019 IY+$19 More flags:
	uint16	T_ADDR;		//	dw	$07A2	; N2  16410 $401A IY+$1A Address of next item in syntax table.
	uint16	SEED;		//	dw	0		; U2  16412 $401C IY+$1C The seed for the random number.
	uint16	FRAMES;		//	dw	$7484	; U2  16414 $401E IY+$1E Count of frames shown since start-up.
	uint16	DEST;		//	dw	$4733	; N2  16416 $4020 IY+$20 Address of variable in statement.
	uint16	RESULT;		//	dw	$3800	; N2  16418 $4022 IY+$22 Value of the last expression.
	uint8	S_POSN_X;	//	db	$21		; X1  16420 $4024 IY+$24 Column number for print position.
	uint8	S_POSN_Y;	//	db	$17		; X1  16421 $4025 IY+$25 Line number for print position.
	uint16	CH_ADD;		//	dw	$FFFF	; X2  16422 $4026 IY+$26 Address of next character to be interpreted.
	};

	static_assert(sizeof(ZX80Head)==0x28,"sizeof(ZX80Head) wrong!");

	const uint i0 = 0;

	segments.checkNoFlagsSet();
	uint32 ramsize = segments.totalCodeSize();
	// valid ram size: 1k, 2k, 3k, 4k, 16k
	bool ramsize_valid = ramsize>=sizeof(ZX80Head)+1 && ramsize<=16 kB;
	if(!ramsize_valid) addError(
		usingstr("total ram size out of range: must be ≥40+1 ($28+1) and ≤16k (size=$%04X",ramsize));
	if(ramsize<sizeof(ZX80Head)) return;

	Segment& hs = segments[i0];
	if(hs.size<sizeof(ZX80Head)) throw syntax_error(
		usingstr("segment %s must be at least 40 ($28) bytes long (size=%u)",hs.name,(uint)hs.size));

	ZX80Head* head = (ZX80Head*)hs.getData();
	uint16 E_LINE = peek2Z(&head->E_LINE);
	if(ramsize_valid && E_LINE != 0x4000+ramsize) addError(
		usingstr("segment %s: E_LINE ($400A) must match ram end address $%04X (E_LINE=$%04X)",
			hs.name, 0x4000+(uint)hs.size, E_LINE));

//	uint16 VARS = peek2Z(&head->VARS);
//	if(VARS<0x4028) addError(usingstr("segment %s: VARS too low",hs.name));
//	if(VARS>=E_LINE) addError(
//		usingstr("segment %s: VARS must be < E_LINE (VARS=$%04X, E_LINE=$%04X)",hs.name,VARS,E_LINE));
//	if(head.S_POSN_X>0x21) 	addError(usingstr("segment %s: S_POSN_X too high",hs.name));
//	if(head.S_POSN_Y>0x17) 	addError(usingstr("segment %s: S_POSN_Y too high",hs.name));

	uint32 addr = 0x4000;
	uint i;
	for(i=i0;i<segments.count()&&segments[i].isCode();i++)
	{
		Segment& s = segments[i];
		if(s.address!=addr) addError(
			usingstr("segment %s should start at $%04X (address=$%04X)", s.name, (uint)addr, (uint)s.address));
		addr += s.size;
	}

	// last byte of a (clean) file must be 0x80 (last byte of VARS):
	while(segments[--i].size==0) {}
	Segment& ls = segments[i];
	if(ls[ls.size-1]!=0x80) addError(usingstr("segment %s: last byte (last byte of VARS) must be $80",ls.name));
}


/*	check segments for ZX81 targets: P, 81 or P81:
	checks min. and max. size
	checks value of E_LINE
	currently only one program per file supported, even for P81
*/
void Z80Assembler::checkZX81File() throw(any_error)
{
	segments.checkNoFlagsSet();

	struct ZX81Head		// SYSVARs $4009++
	{
	uint8 VERSN;			//	0 identifies 8K ZX81 Basic in saved programs.
	uint8 E_PPC,E_PPChi;	//	Number of current line (with program cursor).
	uint8 D_FILE,D_FILEhi;	//	Address of Display File (screen data) in memory.
	uint8 DF_CC,DF_CChi;	//	Address of PRINT position in display file.
	uint8 VARS,VARShi;		//	Address of user program variables in memory.
	uint8 DEST,DESThi;		//	Address of variable in assignment.
	uint8 E_LINE,E_LINEhi;	//	Address of line being editted in memory.
	uint8 CH_ADD,CH_ADDhi;	//	Address of the next character to be interpreted
	uint8 X_PTR,X_PTRhi;	//	Address of the character preceding the [S] marker.
	uint8 STKBOT,STKBOThi;	//	Address of the Calculator stack in memory.
	uint8 STKEND,STKENDhi;	//	End of the Calculator stack.
	uint8 BREG;				//	Calculator’s b register.
	uint8 MEM,MEMhi;		//	Address of area used for calculator’s memory. (Usually MEMBOT but not always.)
	uint8 x1;				//	not used
	uint8 DF_SZ;			//	The number of lines (including one blank line) in the lower part of the screen.
	uint8 S_TOP,S_TOPhi;	//	The number of the top program line in automatic listings.
	uint8 LAST_K,LAST_Khi;	//	Shows which keys pressed
	uint8 x2;				//	Debounce status of keyboard.
	uint8 MARGIN;			//	Number of blank lines above or below picture
	uint8 NXTLIN,NXTLINhi;	//	Address of next program line to be executed.
	uint8 OLDPPC,OLDPPChi;	//	Line number to which CONT jumps.
	uint8 FLAGX;			//	Various flags.
	uint8 STRLEN,STRLENhi;	//	Length of string type designation in assignment.
	uint8 T_ADDR,T_ADDRhi;	//	Address of next item in syntax table (very unlikely to be useful).
	uint8 SEED,SEEHhi;		//	The seed for RND. This is the variable that is set by RAND.
	uint8 FRAMES,FRAMEShi;	//	Counts the frames displayed on the television.
	uint8 CORD_X;			//	x-coordinate of last point PLOTed.
	uint8 CORD_Y;			//	y-coordinate of last point PLOTed.
	uint8 PR_CC;			//	Less significant byte of address of next position for LPRINT to print at.
	uint8 S_POSN_X;			//	Column number for PRINT position.
	uint8 S_POSN_Y;			//	Line number for PRINT position.
	uint8 CDFLAG;			//	Various flags. Bit 7 is on (1) during compute and display (SLOW) mode.
//	PRBUFF	ds	33	; Printer buffer (33rd character is ENTER/NEWLINE).
//	MEMBOT	ds	30	; Calculator’s memory area; used to store numbers that cannot be put on the calculator stack.
//			dw	0	; not used
	};

	static_assert(sizeof(ZX81Head)==125 -9 -65, "sizeof(ZX81Head) wrong!");		// 125 == 0x7D

	const uint i0 = 0;

	segments.checkNoFlagsSet();
	uint32 ramsize = segments.totalCodeSize() +9;
	uint hi = i0;

	if(eq(target,"P81"))
	{
		// first segment must contain the program name only:
		// character set translation must already been done by assembler
		// => prog name: only characters in range 0..63; last char +$80

a:		Segment& s = segments[hi++];
		if(s.size==0) goto a;
		if(s.size>128) throw syntax_error(
			usingstr("segment %s: program name too long: max=128 bytes (size=%u)",s.name,(uint)s.size));

		uint i=0;
		while(i<s.size && s[i]<0x40) i++;
		if(i==s.size) throw syntax_error(usingstr("segment %s: prog name delimiter on last char missing",s.name));
		if(s[i]&0x40) throw syntax_error(usingstr("segment %s: ill. character in prog name: (bit6=1)",s.name));
		ramsize -= i+1;
	}

	// valid ram size: sizeof(sysvars)-9+1 .. 16k-9
	bool ramsize_valid = ramsize >= 125+1 && ramsize <= 16 kB;
	if(!ramsize_valid) addError(
		usingstr("total ram size out of range: must be ≥125+1 ($7D+1) and ≤16k (size=$%04X)",ramsize));

	Segment& hs = segments[hi];
	if(hs.size<125-9) throw syntax_error(
		usingstr("segment %s must be at least 125-9 ($7D-9) bytes long (size=%u)",hs.name,(uint)hs.size));

	ZX81Head* head = (ZX81Head*)hs.getData();
	uint16 E_LINE = peek2Z(&head->E_LINE);
	if(ramsize_valid && E_LINE != 0x4000+ramsize) addError(
		usingstr("segment %s: E_LINE must match ram end address $%04X (E_LINE=$%04X)",
			hs.name, 0x4000+(uint)hs.size, E_LINE));

//	uint16 VARS = peek2Z(&head->VARS);
//	if(VARS<0x4028) addError(usingstr("segment %s: VARS too low",hs.name));
//	if(VARS>=E_LINE) addError(
//		usingstr("segment %s: VARS must be < E_LINE (VARS=$%04X, E_LINE=$%04X)",hs.name,VARS,E_LINE));
//	if(head.S_POSN_X>0x21) 	addError(usingstr("segment %s: S_POSN_X too high",hs.name));
//	if(head.S_POSN_Y>0x17) 	addError(usingstr("segment %s: S_POSN_Y too high",hs.name));

	uint32 addr = 0x4009;
	uint i;
	for(i=hi;i<segments.count()&&segments[i].isCode();i++)
	{
		Segment& s = segments[i];
		if(s.address!=addr) addError(
			usingstr("segment %s should start at $%04X (address=$%04X)", s.name, (uint)addr, (uint)s.address));
		addr += s.size;
	}

	// last byte of a (clean) file must be 0x80 (last byte of VARS):
	while(segments[--i].size==0) {}
	Segment& ls = segments[i];
	if(ls[ls.size-1]!=0x80) addError(usingstr("segment %s: last byte (last byte of VARS) must be $80",ls.name));
}


/*	check segments[] for target Z80
*/
void Z80Assembler::checkZ80File() throw(any_error)
{
	uint seg_cnt = 0; for(uint i=0; i<segments.count() && segments[i].isCode(); i++) { seg_cnt++; }

	// assert header and at least one ram page:
	if(seg_cnt<2) throw syntax_error("no ram pages found");

	const uint i0 = 0;

	// verify that first block is the header:
	Segment& hs = segments[i0];
	if(hs.flag_valid) throw syntax_error("first code segment must be the z80 file header (no flag!)");

	Z80Header& head = *(Z80Header*)hs.getData();

	// handle version 1.45:
	if(hs.size == z80v1len)
	{
		// check header:
		if(!head.pch && !head.pcl) addError("header v1.45: PC at offset 6 must not be 0");

		// check segments:
		if(seg_cnt>2) addError("v1.45: only one ram page allowed");
		Segment& s = segments[i0+1];
		if(s.size!=0xc000) addError(usingstr("segment %s: v1.45: page size must be 0xC000",s.name));
		if(s.flag_valid) addError(usingstr("segment %s: v1.45: no page ID allowed",s.name));

		// comfort: clear compression flag if size increases:
		if(head.data!=255 && head.data&0x20 && compressed_page_size_z80(s.getData(),0xc000)>0xc000) head.data -= 0x20;
		return;
	}

	// v2.0++
	// check header:

	if(hs.size < z80v3len && hs.size!=z80v2len)
		throw syntax_error("header: length must be 30 (v1.45), 55 (v2.01) or 86++ (v3++)");
	if(head.pch || head.pcl) throw syntax_error("header v2++: PC at offset 6 must be 0");

	uint n = head.h2lenl + 256 * head.h2lenh;	// length of header extension
	if(32+n != hs.size) throw syntax_error(
		usingstr("header v2++: wrong header extension length at offset 30: %u + 32 != %u", n, hs.size ));

	Model model = head.getZxspModel();
	if(model==void_model) throw syntax_error("header: illegal model");
	if(model>=zxplus3 && model<=zxplus2a_span && hs.size<z80v3len+1)
		throw syntax_error("header: size must be ≥ 87 for +3/+2A for port_1ffd byte (warajewo/xzx extension)");

//	uint32 cc = head.getCpuCycle(model_info->cpu_cycles_per_frame);
//	if(cc>70000) {}

	bool spectra_used = head.isVersion300() && (head.rldiremu & 0x08);
	if(spectra_used && model>inves)
		throw syntax_error("header: SPECTRA extension can only be attached to ZX Spectrum 16/48k (rldiremu&8)");
	if(spectra_used && hs.size<89)
		throw syntax_error("header: size must be ≥ 89 bytes for SPECTRA extension (rldiremu&8)");

	// v2.0++
	// check pages:
	// verify that all pages have a flag and proper size

//	bool ay_used	= head.rldiremu & 0x04;
//	bool fuller_ay	= head.rldiremu & 0x40;				// only if ay_used
//	bool if1_used	= head.model==1 || head.model==5;
//	bool mgt_used	= head.model==3 || head.model==6;
	bool paged_mem	= (model>=zx128 && model<=zxplus2a_span) || (model>=pentagon && model<=samcoupe);
	bool varying_pagesize = model>=zx80;

	static uint sz[num_models] = {16,48,48,48,48,48,128,128,128,128,128,128,128,128,128,
								  48,48,48,48,48,48,128,256,256,1,1,2,16,16,3};
	uint32 ramsize = (varying_pagesize && head.spectator ? head.spectator : sz[model]) * 1024;

	uint32 addr = 0;	// for varying_pagesize
	uint32 loaded = 0;

	for(uint i=i0+1; i<seg_cnt; i++)
    {
		Segment& s = segments[i];
        if(!s.flag_valid) { addError(usingstr("segment %s: page ID missing",s.name)); continue; }
        uint page_id = s.flag;

        switch (page_id)
        {
        case 2:	// rom at address 0x4000
				if(!paged_mem) addError(
					usingstr("segment %s: invalid page ID: this model does not have 32 kB of rom",s.name));
        case 0:	// rom at address 0x0000
        case 1:	// IF1, Disciple or Plus D Rom
				goto anypage;		// TODO: b&w machines may have different rom size (not yet supported in zxsp)
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
				if(page_id>7)
					{ addError(usingstr("segment %s: page ID out of range",s.name)); continue; }
				if(loaded & (1<<page_id))
					{ addError(usingstr("segment %s: page ID occured twice",s.name)); continue; }
				loaded |= 1<<page_id;
				uint32 size = 1024 << page_id;
				if(size!=s.size)
					{ addError(usingstr("segment %s: page size does not match page ID",s.name)); continue; }
				if(addr+size>s.size)
					{ addError(usingstr("segment %s: sum of page sizes exceeds ram size",s.name)); continue; }
				addr += size;
			}
			else	// std. 16k page:
			{
				if(page_id >= ramsize>>14)
					{ addError(usingstr("segment %s: page ID out of range",s.name)); continue; }
				if(loaded & (1<<page_id))
					{ addError(usingstr("segment %s: page ID occured twice",s.name)); continue; }
				loaded |= 1<<page_id;
				addr += 16 kB;
anypage:		if(s.size!=16 kB)
					{ addError(usingstr("segment %s: page size must be 16 kB",s.name)); continue; }
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





















