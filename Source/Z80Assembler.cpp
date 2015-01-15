/*	Copyright  (c)	Günter Woigk 1994 - 2015
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
#include "kio/kio.h"
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include "unix/FD.h"
#include "unix/files.h"
#include "unix/MyFileInfo.h"
#include "Z80Assembler.h"
#include "Segment.h"
#include "Z80/Z80opcodes.h"
#include "Templates/HashMap.h"
#include "helpers.h"
#include "CharMap.h"
#include "Z80/z80_major_opcode.h"
//#include "hash/sdbm_hash.h"
extern char** environ;


// Priorities for Z80Assembler::value(…)
//
enum
{ 	pAny = 0,		// whole expression: up to ')' or ','
	pTriadic,		// ?:
	pBoolean,		// && ||
	pCmp, 			// comparisions:	 lowest priority
	pAdd, 			// add, sub
	pMul, 			// mul, div, rem
	pBits, 			// bool/masks:		 higher than add/mult
	pRot, 			// rot/shift
	pUna			// unary operator:	 highest priority
};


// name for default code segment, if no #target is given:
//
const char DEFAULT_CODE_SEGMENT[] = "";	// (DEFAULT)";


/*	helper:
	compare word w with string literal s
	w may have a dot '.' prepended
*/
static bool doteq(cptr w, cptr s)
{
	XXXASSERT(s&&w);
	if(*w=='.') w++;
	while(*s) if(*s++ != *w++) return false;
	return *w==0;
}


/*	Helper
	just unquote a string
	Z80 assembler did not use the c-style escapes ...
*/
cstr Z80Assembler::unquotedstr( cstr s0 )
{
    if(!s0||!*s0) return emptystr;

    str  s = dupstr(s0);
    int  n = strlen(s);
    char c = s[0];

    if( n>=2 && (c=='"'||c=='\'') && s[n-1]==c )
    {
		s[n-1] = 0;
		s++;
    }

    return s;
}


// --------------------------------------------------
//					Creator
// --------------------------------------------------


Z80Assembler::Z80Assembler()
:	timestamp(now()),
	source_directory(NULL),
	source_filename(NULL),
	temp_directory(NULL),
	target(NULL),
	target_filepath(NULL),
	current_sourceline_index(0),
	current_segment_ptr(NULL),
	local_labels_index(0),
	local_blocks_count(0),
	cond_off(0),
	max_errors(30),
	pass(0),
	final(0),
	end(0),
	verbose(1),
	c_compiler(NULL),
	c_includes(NULL),
	stdlib_dir(NULL),
	c_tempdir(NULL),
	c_qi(-1),
	c_zi(-1),
	charset(NULL),
//	seg0_org_set(no),
	ixcbr2_enabled(no),	// 	e.g. set b,(ix+d),r2
	ixcbxh_enabled(no),	// 	e.g. set b,xh
	target_z180(no),
	target_8080(no),
	target_z80(yes),
	allow_dotnames(no),
	require_colon(yes),	// prog. label defs. require ':'  =>  label defs. and instructions may be indented at will
	casefold_labels(no),
	flat_operators(no),
	compare_to_old(no)
{}


Z80Assembler::~Z80Assembler()
{
	// wg. Doppelreferenzierung auf .globl-Label müssen erst die lokalen Labels[] gelöscht werden:
	while(labels.count()>1) labels.drop();
	delete charset;
}


// --------------------------------------------------
//					Helper
// --------------------------------------------------


// set error for current file, line & column
//
void Z80Assembler::setError( const any_error& e )
{
	errors.append( new Error(e.what(), &current_sourceline()) );
}

// set error without associated source line
//
void Z80Assembler::addError( cstr text )
{
	errors.append( new Error(text, NULL) );
}


/* ==========================================================
				Assemble source
========================================================== */


/*	assemble source file
		sname = fqn sourcefile
		dname = fqn outputfile; end on ".$" => extension fixed by #target; NULL => only pass 1
		lname = fqn listfile or NULL
		v = verbose output: include object code in list file
		w = include label listing in list file
	output will be in
		source[];
		labels[];
		segments[];		// code and data segments
		errors[];
*/
void Z80Assembler::assembleFile(cstr sourcefile, cstr destpath, cstr listpath, cstr temppath,
								 int liststyle, int deststyle , bool clean) throw()
{
	timestamp = now();

	// 8080 assembler syntax => target 8080 and label names are case insensitive
	if(syntax_8080) { target_8080 = casefold_labels = yes; }
	if(target_8080) { ixcbr2_enabled = ixcbxh_enabled = target_z180 = no; }
	if(target_z180) { ixcbr2_enabled = ixcbxh_enabled = no; }
	target_z80 = !target_8080;						// default

	if(deststyle==0 && compare_to_old) deststyle = 'b';

	XXXASSERT(!c_includes || (eq(c_includes,fullpath(c_includes)) && lastchar(c_includes)=='/' && !errno));
	XXXASSERT(!stdlib_dir || (eq(stdlib_dir,fullpath(stdlib_dir)) && lastchar(stdlib_dir)=='/' && !errno));
	XXXASSERT(!c_compiler || (eq(c_compiler,fullpath(c_compiler)) && lastchar(c_compiler)!='/' && !errno));

	sourcefile = fullpath(sourcefile);			  XXASSERT(errno==ok && is_file(sourcefile));
	if(destpath) { destpath = fullpath(destpath); XXASSERT(errno==ok || errno==ENOENT); }
	if(listpath) { listpath = fullpath(listpath); XXASSERT(errno==ok || errno==ENOENT); }
	if(temppath) { temppath = fullpath(temppath); XXASSERT(errno==ok && is_dir(temppath)); }

	XXASSERT(liststyle>=0 && liststyle<=15);
	XXASSERT(deststyle==0 || deststyle=='b' || deststyle=='x' || deststyle=='s');
	if(liststyle&8) liststyle |= 2;			// "mit clock cycles" implies "with opcodes"

	source_directory = directory_from_path(sourcefile);
	source_filename  = filename_from_path(sourcefile);
	cstr basename    = basename_from_path(source_filename);

	cstr dest_directory = destpath ? directory_from_path(destpath) : destpath=source_directory;
	XXXASSERT(is_dir(dest_directory));

	cstr list_directory = listpath ? directory_from_path(listpath) : listpath=dest_directory;
	XXXASSERT(is_dir(list_directory));

	temp_directory = temppath ? temppath : dest_directory;
	XXXASSERT(is_dir(temp_directory));
	if(clean && is_dir(catstr(temp_directory,"s/"))) delete_dir(catstr(temp_directory,"s/"),yes);

	if(!stdlib_dir && c_includes && endswith(c_includes,"/include/"))	// try to guess missing libdir
	{
		cstr libdir = catstr(leftstr(c_includes,strlen(c_includes)-9),"/lib/");
		if(is_dir(libdir)) stdlib_dir = libdir;
	}

	StrArray source;
	source.append( catstr("#include ", quotedstr(sourcefile)) );
	assemble(source);

	if(errors.count()==0)
		try { checkTargetfile(); }
		catch(any_error& e) { addError(e.what()); }

	if(errors.count()==0 && deststyle)
		try
		{
			destpath = endswith(destpath,"/") ? catstr(destpath, basename, ".$") : destpath;
			if(compare_to_old)
			{
				cstr zpath = catstr("/tmp/zasm/test/",basename,".$");
				create_dir("/tmp/zasm/test/",0700,yes);
				writeTargetfile(zpath,deststyle);
				if(endswith(destpath,".$"))
					destpath = catstr( leftstr(destpath,strlen(destpath)-2), extension_from_path(zpath) );
				FD old(destpath);	// may throw if n.ex.
				FD nju(zpath);
				ulong ofsz = old.file_size();
				ulong nfsz = nju.file_size();

				if(ofsz!=nfsz) addError(usingstr("file size mismatch: old=%lu, new=%lu", ofsz, nfsz));
				uint32 bsize = (uint32)min(ofsz,nfsz);
				uint8 obu[bsize]; old.read_data(obu,bsize);
				uint8 nbu[bsize]; nju.read_data(nbu,bsize);
				for(uint32 i=0; i<bsize && errors.count()<max_errors; i++)
				{
					if(obu[i]==nbu[i]) continue;
					addError(usingstr("mismatch at $%04lX: old=$%02X, new=$%02X",(ulong)i,obu[i],nbu[i]));
				}
				if(errors.count()) liststyle |= 2; else liststyle = 0;
			}
			else
			{
				writeTargetfile(destpath,deststyle);
			}
		}
		catch(any_error& e) { addError(e.what()); }

	if(liststyle)
		try
		{
			listpath = endswith(listpath,"/") ? catstr(listpath, basename, ".lst") : listpath;
			writeListfile(listpath, liststyle);
		}
		catch(any_error& e) { addError(e.what()); }
}


/*	assemble source[]
	output will be in
		source[];
		labels[];
		segments[];
		errors[];
*/
void Z80Assembler::assemble(StrArray& sourcelines) throw()
{
//	XXLogLine("assemble: %u lines", sourcelines.count());

	source.purge();
	for(uint i=0;i<sourcelines.count();i++) { source.append(new SourceLine("", i, dupstr(sourcelines[i]))); }
	//current_sourceline_index = 0;

	target = NULL;

	// setup labels:
	labels.purge();
	labels.append(new Labels(Labels::GLOBALS));		// global_labels must exist

	// setup segments:
	segments.purge();

	// add labels for options:
	if(ixcbr2_enabled) global_labels().add(new Label("ixcbr2",NULL,0,1,yes,yes,yes,no));
	if(ixcbxh_enabled) global_labels().add(new Label("ixcbxh",NULL,0,1,yes,yes,yes,no));
	if(target_z180)    global_labels().add(new Label("z180",NULL,0,1,yes,yes,yes,no));
	if(target_8080)    global_labels().add(new Label("i8080",NULL,0,1,yes,yes,yes,no));
	if(syntax_8080)    global_labels().add(new Label("asm8080",NULL,0,1,yes,yes,yes,no));

	// setup errors:
	errors.purge();
	if(max_errors==0) max_errors = 30;

	// setup conditional assembly:
	cond_off = 0x00;
	static_assert(sizeof(cond[0])==1,"type of cond[] must be byte");
	memset(cond,no_cond,sizeof(cond));

	// DOIT:
	for(pass=1,final=no; pass<9 && !final && errors.count()==0; pass++)
	{
		// reset charset conversion:
		delete charset;
		charset = NULL;

		// final: true = this may be the last pass.
		// wird gelöscht, wenn:
		//	 label nicht in den locals gefunden wurde (wohl aber evtl. in den globals)
		//	 gefundenes label noch nicht valid ist
		//	 auf $ zugegriffen wird wenn !dptr_valid
		// we are finished if it is still set after this assembly pass
		final = true;

		// set by #end -> source end before last line
		end = false;

		// init segments:
		current_segment_ptr = NULL;
		for(uint i=0;i<segments.count();i++) { segments[i].rewind(); }

		// init labels:
		local_labels_index = 0;
		local_blocks_count = 1;
		reusable_label_basename = "";//DEFAULT_CODE_SEGMENT;

		// assemble source:
		for(uint i=0; i<source.count() && !end; i++)
		{
			try
			{
				current_sourceline_index = i;	// req. for errors and labels
				assembleLine(source[i]);
				i = current_sourceline_index;	// some pseudo instr. skip some lines
			}
			catch(fatal_error& e)
			{
				setError(e);
				return;
			}
			catch(any_error& e)
			{
				setError(e);
				if(errors.count()>=max_errors) return;
			}
		}

		// stop on errors:
		if(errors.count()) return;
		if(cond[0]!=no_cond) { addError("#endif missing"); return; }	// TODO: set error marker in '#if/#elif/#else' line
		XXXASSERT(!cond_off);
		if(local_labels_index!=0) { addError("#endlocal missing"); return; }	// TODO: set error marker in '#local' line

		// concatenate segments:
		// => set segment address for relocatable segments
		// => set segment size for resizable segments
		try
		{
			uint32 data_address = 0; bool data_address_valid = yes;	// for data segments
			uint32 code_address = 0; bool code_address_valid = yes;	// for code segments

			for(uint i=0; i<segments.count(); i++)
			{
				Segment& seg = segments[i];
				uint32& seg_address		  = seg.is_data ? data_address		 : code_address;
				bool&   seg_address_valid = seg.is_data ? data_address_valid : code_address_valid;

				if(seg.resizable)
				{
					if(seg.dpos_valid) seg.setSize(seg.dpos);
				}
				else
				{
					if(seg.dpos_valid && seg.size_valid) seg.storeSpace(seg.size-seg.dpos, true);
					else seg.dpos_valid = no;
				}

				if(seg.relocatable)
				{
					if(seg_address_valid) seg.setAddress(seg_address);
				}

				if(seg.address_valid)
				{
					Label& l = global_labels().find(seg.name);
					if(l.is_valid && l.value!=(int32)seg.address)
						{ addError(usingstr("label %s redefined",seg.name)); return; }
					l.value = seg.address;
					l.is_valid = yes;
				}

				seg_address_valid = seg.physicalAddressValid();
				seg_address       = seg.physicalAddress();

			//	final = final && seg.logicalAddressValid();	// this is not strictly neccessary, though if not true,
			//	final = final && seg.physicalAddressValid();// … most likely some label references are still not satisfied.
				final = final && seg.size_valid;			// … might happen with unused (empty) segments.
			}
		}
		catch(any_error& e)
		{
			addError(e.what());
			return;
		}
	}

	if(!final) { addError("some labels failed to resolve"); return; }		// TODO: list them
}


/*	Assemble SourceLine
*/
void Z80Assembler::assembleLine(SourceLine& q) throw(any_error)
{
	q.rewind();							// falls Pass 2++
	q.segment = current_segment_ptr;	// Für Temp Label Resolver
	q.byteptr = current_segment_ptr ? currentPosition() : 0; // Für Temp Label Resolver & Logfile & '$'
//	if(pass==1) q.bytecount = 0;		// Für Logfile und skip over error in pass≥2

#if DEBUG
	LogLine("%s",q.text);
#endif

	if(q.test_char('#'))		// #directive ?
	{
		asmDirect(q);
		q.expectEol();			// expect end of line
	}
	else if(cond_off)			// assembling conditionally off ?
	{
		if(q.testWord("endif")) { if(!q.testChar(':')) { asmEndif(q); q.expectEol(); } return; }
		if(q.testWord("if"))    { if(!q.testChar(':')) { asmIf(q);    q.expectEol(); } return; }
//		if(q.testWord("elif"))  { if(!q.testChar(':')) { asmElif(q);  q.expectEol(); } return; }
//		if(q.testWord("else"))  { if(!q.testChar(':')) { asmElse(q);  q.expectEol(); } return; }
		return;
	}
//#ifndef NDEBUG   				// test suite:
	else if(q.test_char('!'))	// test suite: this line must fail:
	{
		try
		{
			if(q[0]!=';')
			{
				if(((uint8)q[0] >= '0' || require_colon) &&
					(q[0]!='.' || allow_dotnames)) asmLabel(q);	// label definition
				if(syntax_8080) asmInstr8080(q); else asmInstr(q);	// opcode or pseudo opcode
				q.expectEol();		// expect end of line
			}
		}
		catch(any_error&)		// we expect to come here:
		{
			XXXASSERT(q.segment==current_segment_ptr);		// zunächst: wir nehmen mal an,
		//	XXXASSERT(currentPosition() == q.byteptr);		// dass dann auch kein Code erzeugt wurde
			return;
		}
		throw syntax_error("instruction did not fail!");	// did not throw!
	}
//#endif
	else						// [label:] + opcode
	{
		try
		{
			if(q[0]!=';')
			{
				// note: wenn wir auf >= '0' statt > ' ' testen, werden einige Sonderfälle autom. nicht in asmLabel() geschickt
				if(((uint8)q[0] > ' ' || require_colon) &&
					(q[0]!='.' || allow_dotnames)) asmLabel(q);	// label definition
				if(syntax_8080) asmInstr8080(q); else asmInstr(q);	// opcode or pseudo opcode
				q.expectEol();										// expect end of line
			}

			if(current_segment_ptr)
			{
				if(q.segment==current_segment_ptr)
					q.bytecount = currentPosition() - q.byteptr;
				else
				{
					q.segment = current_segment_ptr;	// .area instruction
					q.byteptr = currentPosition();		// Für Temp Label Resolver & Logfile
					XXXASSERT(q.bytecount==0);
				}
			}
		}
		catch(syntax_error& e)
		{
//			setError(e);
			if(pass>1 && q.segment) q.segment->skipExistingData(q.byteptr+q.bytecount-currentPosition());
			throw(e);
		}
	}
}


/*	Minimalistic single line assembler for use with zxsp:
	instruction will be prepended with a space
	=> only instructions, no labels, directives, etc.
	returns size of assembled instruction or 0 for error
*/
uint Z80Assembler::assembleSingleLine(uint address, cstr instruction, char buffer[])
{
	StrArray sourcelines;
	sourcelines.append(catstr(" org ",numstr(address)));	// set the destination address (allow use of '$')
	sourcelines.append(catstr(" ",instruction));			// the instruction to assemble
	assemble(sourcelines);
	if(current_segment_ptr->size>4) addError("resulting code size exceeds size of z80 opcodes");	// defs etc.
	if(errors.count()) return 0;
	memcpy(buffer,current_segment_ptr->getData(),current_segment_ptr->size);
	return current_segment_ptr->size;
}


/* skip expression
*/
void Z80Assembler::skip_expression( SourceLine& q, int prio ) TAE
{
	cstr w = q.nextWord();				// get next word
	if(w[0]==0)							// end of line
eol:	throw syntax_error("unexpected end of line");

	if(w[1]==0)							// 1 char word
	{
		switch(w[0])
		{
//		case '#':	SDASZ80: immediate value prefix: only at start of expression
//		case '<':	SDASZ80: low byte of word	SDCC does not generate pruning operators
//		case '>':	SDASZ80: high byte of word	SDCC does not generate pruning operators
		case ';':	goto eol;			// comment  =>  unexpected end of line
		case '+':
		case '-':
		case '~':
		case '!':	skip_expression(q,pUna); goto op;
		case '(':	skip_expression(q,pAny); q.expect(')'); goto op;	// brackets
		case '$':	goto op; // $ = "logical" address at current code position
		}
	}
	else
	{
		// multi-char word:
		if(w[0]=='$' || w[0]=='%' || w[0]=='\'') goto op;	// hex number or $$, binary number or ascii number
	}

	if(is_dec_digit(w[0])) { q.test_char('$'); goto op; }	// decimal number or reusable label
	if(!is_letter(w[0]) && w[0]!='_') throw syntax_error("syntax error");	// last possibility: plain idf

	if(q.testChar('('))			// test for built-in function
	{
		if(eq(w,"defined") || eq(w,"hi") || eq(w,"lo") || eq(w,"min") || eq(w,"max") || eq(w,"opcode"))
		{
			for(uint nkl = 1; nkl; )
			{
				w = q.nextWord();
				if(w[0]==0) throw syntax_error("')' missing");	// EOL
				if(w[0]=='(') { nkl++; continue; }
				if(w[0]==')') { nkl--; continue; }
			}
		}
		else --q;	/* put back '(' */
	}


op:
	if(q.testEol()) return;				// end of line
	if(flat_operators) prio = pAny;

	switch(q.peekChar())				// peek next character
	{
	// TODO: and or xor eq ne gt ge lt le
	case '+':
	case '-':	if(pAdd<=prio) break; skip_expression(++q,pAdd); goto op;

	case '*':
	case '/':
	case '%':
	case '\\':	if(pMul<=prio) break; skip_expression(++q,pMul); goto op;

	case '|':
	case '&':	if(q.p[1]==q.p[0]) { if(pBoolean<=prio) break; skip_expression(q+=2,pBoolean); goto op; }
	case '^':	if(pBits<=prio) break; skip_expression(++q,pBits); goto op;

	case '?':	if(pTriadic<=prio) break;
				skip_expression(++q,pTriadic-1); q.expect(':'); skip_expression(q,pTriadic-1); goto op;

	case '=':	if(pCmp<=prio) break;
				++q; q.skip_char('='); skip_expression(q,pCmp); goto op;			// equal: '=' or '=='

	case '!':	if(q[1]=='=') { if(pCmp<=prio) break; skip_expression(q+=2,pCmp); goto op; }	// !=
				break;// error

	case '<':
	case '>':	if(q.p[1]==q.p[0]) { if(pRot<=prio) break; skip_expression(q+=2,pRot); goto op; }	// >> <<
				// > < >= <= <>
				if(pCmp<=prio)	break;
				if(q.p[1]=='>' || q.p[1]=='=') ++q;
				skip_expression(++q,pCmp); goto op;
	}
}


inline bool utf8_is_null ( char c ) { return c==0; }
inline bool utf8_is_7bit ( char c ) { return c>=0; }			// %0xxxxxxx = ascii
inline bool utf8_no_7bit ( char c ) { return c<0;  }
inline bool utf8_is_fup	 ( char c ) { return c< char(0xc0);  }	// %10xxxxxx = fup
inline bool utf8_no_fup	 ( char c ) { return c>=char(0xc0);  }
inline bool utf8_is_c1	 ( char c ) { return c>=0; }			// == utf8_is_7bit
inline bool utf8_is_c2	 ( char c ) { return (c&0xe0)==0xc0; }	// %110xxxxx
inline bool utf8_is_c3	 ( char c ) { return (c&0xf0)==0xe0; }	// %1110xxxx
inline bool utf8_is_c4	 ( char c ) { return (c&0xf8)==0xf0; }	// %11110xxx
inline bool utf8_is_c5	 ( char c ) { return (c&0xfc)==0xf8; }	// %111110xx
inline bool utf8_is_c6	 ( char c ) { return (uchar)c>=0xfc; }	// %1111110x  2005-06-11: full 32 bit
inline bool utf8_is_ucs4 ( char c ) { return (uchar)c> 0xf0; }	// 2015-01-02 doesn't fit in ucs2?
inline bool utf8_req_c4	 ( char c ) { return (uchar)c>=0xf0; }	// 2015-01-02 requires processing of c4/c5/c6?
#define     RMASK(n)	 (~(0xFFFFFFFF<<(n)))							// mask to select n bits from the right






/* ----	convert UTF-8 char to UCS-2 -------------------------------
		stops at next non-fup
		throws on error
		char(0) is a valid character
		note: only doing UCS2 because class charmap is UCS2 only
*/
static
uint charcode_from_utf8( cptr& s ) throw(syntax_error)
{
	uint n; uint i; char c;

	n = (uchar) *s++;						// char code akku
	if(utf8_is_7bit(n)) return n;			// 7-bit ascii char
	if(utf8_is_fup(n))  goto x1;			// unexpected fup
	if(utf8_is_ucs4(n)) goto x5;			// code exceeds UCS-2

// longish character:
	i = 0;									// UTF-8 character size
	c = n;
//	c = n & ~0x02;							// force stop at i=6
	while( char(c<<(++i)) < 0 )				// loop over fup bytes
	{
		uchar c1 = *s++; if(utf8_no_fup(c1)) goto x3;
		n = (n<<6) + (c1&0x3F);
	}

// simplify error checking for caller:
	if (utf8_is_fup(*s)) goto x1;			// more unexpected fups follows

// now: i = total number of digits
//      n = char code with some of the '1' bits from c0
	n &= RMASK(2+i*5);

// ill. overlong encodings:
//	if ( n < 1u<<(i*5-4) ) goto x4;			// ill. overlong encoding

// ok => return code
	return n;

// error => return replacement char
	x1:	//SetError(unexpectedfup);   return UCS4ReplacementChar;
	x3:	//SetError(truncatedchar);   return UCS4ReplacementChar;
//	x4:	//SetError(illegaloverlong); return UCS4ReplacementChar;
	x5: //is a UCS4 character
	throw syntax_error("broken utf-8 character!");
}



/*	evaluate expression
	stops if end of expression reached or
	stops if operator with priority equal or less is encountered
	any reference to unknown or not-yet-valid label sets argument 'valid' to false
*/
int32 Z80Assembler::value( SourceLine& q, int prio, bool& valid ) throw(any_error)
{
	int32 n = 0;						// value of expression

// ---- expect term ----
w:	cstr w = q.nextWord();				// get next word
	if(w[0]==0) goto syntax_error;		// empty word

	if(w[1]==0)							// 1 char word
	{
		switch(w[0])
		{
		case '#':	if(prio==pAny) goto w; else goto syntax_error;	// SDASZ80: immediate value prefix
		case ';':	throw syntax_error("value expected");	// comment  =>  unexpected end of line
		case '+':	n = +value(q,pUna,valid); goto op;		// plus sign
		case '-':	n = -value(q,pUna,valid); goto op;		// minus sign
		case '~':	n = ~value(q,pUna,valid); goto op;		// complement
		case '!':	n = !value(q,pUna,valid); goto op;		// negation
		case '(':	n =  value(q,pAny,valid); q.expect(')'); goto op;	// brackets
		case '$':	// the following line is equivalent to currentAddress() ((seg.orgbase+dpos))
					// but makes sure that '$' refers to the address at start of line; e.g. for " db N,…,N-$"
					n = current_segment().org_base_address + q.byteptr;
					valid = valid && currentAddressValid();
					if(!valid) final = false; goto op;
		case '<':	q.expect('('); goto lo;		// SDASZ80: low byte of word
		case '>':	q.expect('('); goto hi;		// SDASZ80: high byte of word
		}
	}
	else							// multi-char word:
	{
		char c = 0;

		if (w[0]=='$')				// hex number or $$
		{
			w++;
			if(*w=='$')				// $$ = "physical" address of current code position  (segment.address+dpos)
			{
				w++;
				XXASSERT(*w==0);
				// the following line is equivalent to realAddress() ((seg.addr+dpos))
				// but makes sure that '$' refers to the address at start of line; e.g. for " db N,…,N-$$"
				n = current_segment().address + q.byteptr;
				valid = valid && realAddressValid();
				if(!valid) final = false; goto op;
			}
			else					// hex number
			{
hex_number:		while( is_hex_digit(*w) ) { n = (n<<4)+(*w&0x0f); if(*w>'9') n+=9; w++; }
				if(w[c!=0]==0) goto op; else goto syntax_error;
			}
		}
		else if(w[0]=='%')			// binary number
		{
			w++;
bin_number:	while(is_bin_digit(*w)) { n += n + (*w&1); w++; }
			if(w[c!=0]==0) goto op; else goto syntax_error;
		}
		else if(*w=='\'' || *w=='"')// ascii number: due to ambiguity of num vs. str only ONE CHARACTER ALLOWED!
		{							// uses utf-8 or charset translation
									// also allow "c" as a numeric value (seen in sources!)
			uint slen = strlen(w);
			if(slen<3||w[slen-1]!=w[0]) goto syntax_error;
			w = unquotedstr(w);
			n = charcode_from_utf8(w);
			if(charset) n = charset->get(n);
			if(*w) throw syntax_error("only one character allowed");
			goto op;
		}
		else if(is_dec_digit(w[0]))	// decimal number
		{
			if(w[0]=='0')
			{
				if(tolower(w[1])=='x' && w[2]) { w+=2; goto hex_number; }	// 0xABCD
				if(tolower(w[1])=='b' && w[2] && is_bin_digit(lastchar(w))) // caveat e.g.: 0B0h
												{ w+=2; goto bin_number; }	// 0b0101
			}
			c = tolower(lastchar(w));
			if( c=='h' ) goto hex_number;	// hex number     indicated by suffix
			if( c=='b' ) goto bin_number;	// binary number  indicated by suffix
		}
	}

	if(is_dec_digit(w[0]))			// decimal number or reusable label
	{
		if(q.test_char('$'))		// reusable label (SDASZ80)
		{
			w = catstr(reusable_label_basename,"$",w);
			goto label;
		}
		else						// decimal number
		{
			while(is_dec_digit(*w)) { n = n*10 + *w-'0'; w++; }
			if(*w==0) goto op;
			if((*w|0x20)=='d' && *++w==0) goto op; // decimal number indicated by suffix --> source seen ...
			goto syntax_error;
		}
	}

	if(*w=='_' && eq(w,"__line__"))
	{
		n = q.sourcelinenumber; valid = yes; goto op;
	}

	if(q.test_char('('))		// test for built-in function
	{
		if(eq(w,"defined"))		// defined(NAME)  or  defined(NAME::)
		{						// note: label value is not neccessarily valid
			w = q.nextWord();
			if(!is_letter(*w) && *w!='_') throw fatal_error("label name expected");
			bool global = q.testChar(':')&&q.testChar(':');
			q.expect(')');

			for(uint i=global?0:local_labels_index;;i=labels[i].outer_index)
			{
				Labels& labels = this->labels[i];
				Label& label = labels.find(w);
				if(&label!=NULL && label.is_defined &&			// found && defined?
					label.sourceline<=current_sourceline_index)	// if pass>1: check line of definition. TODO: to be tested
						 { n=1; break; }
				if(i==0) { n=0; break; }						// not found / not defined
			}
			goto op;
		}
		else if(eq(w,"lo"))
		{
lo:			n = uint8(value(q,pAny,valid));
			q.expect(')');
			goto op;
		}
		else if(eq(w,"hi"))
		{
hi:			n = uint8(value(q,pAny,valid)>>8);
			q.expect(')');
			goto op;
		}
		else if(eq(w,"min"))
		{
			n = value(q,pAny,valid);
			q.expectComma();
			n = min(n,value(q,pAny,valid));
			q.expect(')');
			goto op;
		}
		else if(eq(w,"max"))
		{
			n = value(q,pAny,valid);
			q.expectComma();
			n = max(n,value(q,pAny,valid));
			q.expect(')');
			goto op;
		}
		else if(eq(w,"opcode"))		// opcode(ld a,N)  or  opcode(bit 7,(hl))  etc.
		{
			cptr a = q.p;
			uint nkl = 1;
			while(nkl)
			{
				w = q.nextWord();
				if(w[0]==0) throw syntax_error("')' missing");	// EOL
				if(w[0]=='(') { nkl++; continue; }
				if(w[0]==')') { nkl--; continue; }
			}
			n = z80_major_opcode(substr(a,q.p-1));
			valid = yes;
			goto op;
		}
		else --q;	// put back '('
	}

	if(is_name(w))	// name
	{
label:	if(casefold_labels) w = lowerstr(w);

		if(pass==1)	// Pass 1:
		{
		/*	In Pass 1 können auch gefundene, definierte globale Label noch durch lokalere Label,
			die im Source weiter hinten definiert werden, ersetzt werden.

			Label lokal nicht gefunden?
			=> ACTION: Label als referenziert & nicht definiert eintragen
			   dadurch kann das Label im Labellisting ausgegeben werden
			   context==lokal?
			   => wenn das Label bis #endlocal nicht definiert wurde,
				  wird es von #endlocal in den umgebenden Context verschoben
				  (oder evtl. gelöscht, wenn es das dort schon gibt)
				  Dadurch wandern nicht definierte Label in Richtung globaler Context
			   context==global?
			   => dadurch kann das Label von #include library definiert werden

			Label lokal gefunden?
			=> Label definiert?
			   => ACTION: dieses Label nehmen
			   Label noch nicht definiert?
			   => context==lokal?
				  => lokales Label?
				     => Label wurde schon einmal referenziert und dabei eingetragen
				        ACTION: no action
				     globales label?
				     => Label wurde mit .globl deklariert
				        es ist *auch* in globals[] eingetragen.
				        wenn es später mit #include library definiert wird, wird es auch hier definiert sein.
				        ACTION: no action
			   => context==global?
			      => lokales Label?
			         => can't happen (Internal Error)
			         globales Label?
			         => Label wurde schon einmal referenziert und dabei eingetragen
						oder Label wurde mit .globl deklariert
				        ACTION: no action
		*/
			Label* l = &local_labels().find(w);

			if(!l)	// => ACTION: Label als referenziert & nicht definiert eintragen
			{
				local_labels().add(new Label(w,NULL,0,0,no,local_labels_index==0,no,yes));
				valid = no; final = no;
			}
			else if(l->is_defined) // => ACTION: dieses Label nehmen
			{
				n = l->value;
				if(!l->is_valid) { valid = no; final = no; }
				l->is_used = true;
			}
			else	// => ACTION: no action
			{
				valid = no; final = no;
				l->is_used = true;
			}
		}
		else
		{
			// Pass 2++:
			// Für das Label existiert ein Label-Eintrag in this.labels[][] weil in Pass 1 für alle
			// referenzierten Label ein Labeleintrag erzeugt wird. Dieser wird gesucht.
			// Ist er nicht als definiert markiert, wurde in Pass1 die Definition nicht gefunden. => Error

			for( uint i=local_labels_index; ; i=labels[i].outer_index )
			{
				Label* l = &(labels[i].find(w));
				if(!l) continue;
				if(!l->is_defined)
				{
					// Dies muss ein globales Label sein, da alle undeklarierten Label von #endlocal in den
					// umgebenden Kontext geschoben werden.
					// es kann aber evtl. schon in einem lokalen Kontext gefunden werden,
					// wenn es dort mit .globl deklariert wurde:
					XXXASSERT(l->is_global);
					throw syntax_error(usingstr("label \"%s\" not found",w));
				}

				n = l->value;
				if(!l->is_valid) { valid = no; final = no; }
				XXXASSERT(l->is_used);// = yes;
				break;
			}
		}
	}
	else
	{
syntax_error:
		throw syntax_error("syntax error");
	}

// ---- expect operator ----

op:	char c1,c2;
	if(q.testEol()) goto x;
	c1 = q.p[0]; if(is_uppercase(c1)) c1 |= 0x20;
	c2 = q.p[1]; if(is_uppercase(c2)) c2 |= 0x20;
	if(flat_operators) goto any;

	switch(prio+1)
	{
//	case pAny:
	case pTriadic:	// ?:
any:	if(c1=='?')
		{
			q+=1; if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
			if(n) { n = value(q,pTriadic-1,valid); q.expect(':'); skip_expression(q,pTriadic-1); }
			else  { skip_expression(q,pTriadic-1); q.expect(':'); n = value(q,pTriadic-1,valid); }
		}

	case pBoolean:	// && ||
		if(c1==c2)
		{
			if(c1=='&')	// '&&' --> boolean and
			{
				q+=2; if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
				if(n) n = value(q,pBoolean,valid); else skip_expression(q,pBoolean);
				n = n!=0; goto op;
			}
			if(c1=='|')	// '||' --> boolean or
			{
				q+=2; if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
				if(!n) n = value(q,pBoolean,valid); else skip_expression(q,pBoolean);
				n = n!=0; goto op;
			}
		}

	case pCmp:	// > < == >= <= !=
		if(c1>='a')
		{
			if(c1=='n' && c2=='e')  { n = n != value(q+=2,pCmp,valid); goto op; }
			if(c1=='e' && c2=='q')  { n = n == value(q+=2,pCmp,valid); goto op; }
			if(c1=='g' && c2=='e')  { n = n >= value(q+=2,pCmp,valid); goto op; }
			if(c1=='g' && c2=='t')  { n = n >  value(q+=2,pCmp,valid); goto op; }
			if(c1=='l' && c2=='e')  { n = n <= value(q+=2,pCmp,valid); goto op; }
			if(c1=='l' && c2=='t')  { n = n <  value(q+=2,pCmp,valid); goto op; }
		}
		else
		{
			if(c1=='=') { q+=c2-c1?1:2; n = n==value(q,pCmp,valid); goto op; }	// equal: = ==
			if(c1=='!' && c2=='=') { n = n!=value(q+=2,pCmp,valid); goto op; }	// not equal: !=
			if(c1=='<')
			{
				if(c2=='>')	{ n = n!=value(q+=2,pCmp,valid); goto op; }			// not equal:   "<>"
				if(c2=='=')	{ n = n<=value(q+=2,pCmp,valid); goto op; }			// less or equ:	"<="
				if(c2!='<') { n = n< value(q+=1,pCmp,valid); goto op; }			// less than:	"<"
			}
			if(c1=='>')
			{
				if(c2=='=')	{ n = n>=value(q+=2,pCmp,valid); goto op; }			// greater or equ:	">="
				if(c2!='>') { n = n> value(q+=1,pCmp,valid); goto op; }			// greater than:	">"
			}
		}

	case pAdd:	// + -
		if(c1=='+') { n = n + value(++q,pAdd,valid); goto op; }
		if(c1=='-') { n = n - value(++q,pAdd,valid); goto op; }

	case pMul:	// * / %
		if(c1=='*') { n = n * value(++q,pMul,valid); goto op; }
		if(c1=='/')
		{
			int32 m = value(++q,pMul,valid);
			if(m) n = n / m; else if(valid) throw syntax_error("division by zero");
			goto op;
		}
		if(c1=='%' || c1=='\\')
		{
			int32 m = value(++q,pMul,valid);
			if(m) n = n % m; else if(valid) throw syntax_error("division by zero");
			goto op;
		}

	case pBits:	// & | ^
		if(c1=='^')						 { n = n ^ value(++q, pBits,valid); goto op; }
		if(c1=='&' && c2!='&')			 { n = n & value(++q, pBits,valid); goto op; }
		if(c1=='|' && c2!='|')			 { n = n | value(++q, pBits,valid); goto op; }
		if(c1=='a' && q.testWord("and")) { n = n & value(q,   pBits,valid); goto op; }
		if(c1=='o' && c2=='r')			 { n = n | value(q+=2,pBits,valid); goto op; }
		if(c1=='x' && q.testWord("xor")) { n = n ^ value(q,   pBits,valid); goto op; }

	case pRot:	// >> <<
		if(c1==c2)
		{
			if(c1=='<') { n = n << value(q+=2,pRot,valid); goto op; }
			if(c1=='>') { n = n >> value(q+=2,pRot,valid); goto op; }
		}

//	default:	// prio >= pUna
//		break;
	}

// no operator or operator of same or lower priority followed
// =>  return value; caller will check the reason of returning anyway
x:	return valid ? n : 0;



//	switch(q.peekChar())				// peek next character
//	{
//	case '+':	if(pAdd<=prio) break; n = n + value(++q,pAdd,valid); goto op;	// add
//	case '-':	if(pAdd<=prio) break; n = n - value(++q,pAdd,valid); goto op;	// subtract
//	case '*':	if(pMul<=prio) break; n = n * value(++q,pMul,valid); goto op;	// multiply
//	case '^':	if(pBits<=prio) break; n = n ^ value(++q,pBits,valid); goto op;	// boolean xor

//	case '/':	if(pMul<=prio) break;											// divide
//		{
//				int32 m = value(++q,pMul,valid);
//				if(m==0) { if(valid) throw syntax_error("division by zero"); }
//				else n = n / m; goto op;
//		}

//	case '%':																	// remainder (same prio as '*')
//	case '\\':	if(pMul<=prio) break;											// remainder (same prio as '*')
//		{
//				int32 m = value(++q,pMul,valid);
//				if(m==0) { if(valid) throw syntax_error("division by zero"); }
//				else n = n % m; goto op;
//		}

//	case '&':
//		if(q.p[1]=='&') // '&&' --> boolean and
//		{				// pruning is only possible if left-handed term is valid in pass1!
//			if(pBoolean<=prio) break;
//			if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
//			if(n) n = value(q+=2,pBoolean,valid); else skip_expression(q+=2,pBoolean);
//			n = n!=0; goto op;
//		}
//		else { if(pBits<=prio) break; n = n & value(++q,pBits,valid); goto op; }	// bitwise and

//	case '|':
//		if(q.p[1]=='|') // '||' --> boolean or
//		{				// pruning is only possible if left-handed term is valid in pass1!
//			if(pBoolean<=prio) break;
//			if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
//			if(!n) n = value(q+=2,pBoolean,valid); else skip_expression(q+=2,pBoolean);
//			n = n!=0; goto op;
//		}
//		else { if(pBits<=prio) break; n = n | value(++q,pBits,valid); goto op; }	// bitwise or

//	case '?':			// triadic ?:
//						// in general pruning is not possible! (only if left-handed term is valid in pass1!)
//		if(pTriadic<=prio) break;
//		if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
//		if(n) { n = value(++q,pTriadic-1,valid); q.expect(':'); skip_expression(q,pTriadic-1); }
//		else  { skip_expression(++q,pTriadic-1); q.expect(':'); n = value(q,pTriadic-1,valid); }
//		goto op;

//	case '=':
//		if(pCmp<=prio) break;
//		++q; q.skip_char('='); n = n==value(q,pCmp,valid); goto op;				// equal:		'=' or '=='

//	case '!':
//		if (pCmp<=prio) break;
//		++q; if(*q=='=') { n = n!=value(++q,pCmp,valid); goto op; }				// not equal:	'!='
//		--q; break;

//	case '<':
//		if(*++q=='<')		// <<
//		{
//		    if(pRot<=prio) { goto break1; }
//			n = n << value(++q,pRot,valid); goto op;							// shift left:	"<<"
//		}
//		else
//		{
//		    if(pCmp<=prio)	{ goto break1; }
//			else if(*q=='>'){ n = n!=value(++q,pCmp,valid); goto op; }			// not equal:   "<>"
//			else if(*q=='='){ n = n<=value(++q,pCmp,valid); goto op; }			// less or equ:	"<="
//			else			{ n = n< value(  q,pCmp,valid); goto op; }			// less than:	"<"
//		}

//	case '>':
//		if(*++q=='>')		// >>
//		{
//		    if(pRot<=prio)	{ goto break1; }
//			n = n >> value(++q,pRot,valid); goto op;							// shift right:	">>"
//		}
//		else
//		{
//		    if(pCmp<=prio)	{ goto break1; }
//			else if(*q=='='){ n = n>=value(++q,pCmp,valid); goto op; }			// greater or equ.:	">="
//			else			{ n = n> value(  q,pCmp,valid); goto op; }			// greater than:	">"
//		}

//	default:
//		if(q.testWord("and")) { if(pBits<=prio) goto break3; n = n & value(++q,pBits,valid); goto op; }
//		if(q.testWord("or" )) { if(pBits<=prio) goto break2; n = n | value(++q,pBits,valid); goto op; }
//		if(q.testWord("xor")) { if(pBits<=prio) goto break3; n = n ^ value(++q,pBits,valid); goto op; }
//		if(q.testWord("ne"))  { if(pCmp<=prio)  goto break2; n = n != value(++q,pCmp,valid); goto op; }
//		if(q.testWord("ge"))  { if(pCmp<=prio)  goto break2; n = n >= value(++q,pCmp,valid); goto op; }
//		if(q.testWord("le"))  { if(pCmp<=prio)  goto break2; n = n <= value(++q,pCmp,valid); goto op; }
//		if(q.testWord("eq"))  { if(pCmp<=prio)  goto break2; n = n == value(++q,pCmp,valid); goto op; }
//		if(q.testWord("gt"))  { if(pCmp<=prio)  goto break2; n = n >  value(++q,pCmp,valid); goto op; }
//		if(q.testWord("lt"))  { if(pCmp<=prio)  goto break2; n = n <  value(++q,pCmp,valid); goto op; }
//		break;

//break3:	--q;
//break2:	--q;
//break1:	--q;
//		break;
//	}

//// no operator followed  =>  return value; caller will check the reason of returning anyway
//x:	return valid ? n : 0;
}


/*	Handle potential Label Definition
*/
void Z80Assembler::asmLabel(SourceLine& q) throw(any_error)
{
	cptr p = q.p;
	cstr name = q.nextWord();
	if(name[0]==0) return;			// end of line

	bool is_reusable = is_dec_digit(name[0]) && q.test_char('$');	// SDASZ80

	if(!is_reusable && !is_name(name))
	{													// must be a pseudo instruction or broken code
		if(/*!require_colon ||*/ q.testChar(':')) throw syntax_error("illegal label name");
		q.p = p; return;
	}

	if(casefold_labels) name = lowerstr(name);
	if(is_reusable) name = catstr(reusable_label_basename,"$",name);

	bool f = q.test_char(':');
	bool is_global = f && !is_reusable && q.test_char(':');
	bool is_redefinable = no;
	bool is_valid;
	int32 value;

	if(q.testWord("defl"))									// M80: redefinable label; e.g. used this way in CAMEL80
	{
		is_redefinable = yes;
		value = this->value(q,pAny,is_valid=1);				// calc assigned value
	}
	else if(q.testDotWord("equ") || q.test_char('='))
	{														// defined label:
		value = this->value(q,pAny,is_valid=1);				// calc assigned value
	}
	else					// program label, SET or MACRO or no label
	{
		if(!is_reusable)	// SET and MACRO don't require ':'
		{
			// test for SET:								// this is really really bad:
			cptr z = q.p;									// there is a Z80 instruction SET
			if(q.testDotWord("set"))						// and the M80 pseudo instruction SET
			{												// and we'll have to figure out which it is…
				value = this->value(q,pAny,is_valid=1);
				is_redefinable = q.testEol(); if(is_redefinable) goto a;	// heureka! it's the pseudo instruction!
			}
			q.p = z;

			// test for MACRO:
			if(q.testDotWord("macro")) { asmMacro(q,name,'&'); return; }

		}

		if(require_colon && !f) { q.p = p; return; }	// must be a [pseudo] instruction

		if(!current_segment_ptr) throw syntax_error("org not yet set");
		value = currentAddress();
		is_valid = currentAddressValid();

		if(!is_reusable) reusable_label_basename = name;
	}

a:	Labels& labels = is_global ? global_labels() : local_labels();
	Label* l = &labels.find(name);

	if(l)
	{
		if(l->segment==NULL)							// .globl or defined before ORG
		{
			l->segment = current_segment_ptr;			// mit '.globl' deklarierte Label haben noch kein Segment
			l->sourceline = current_sourceline_index;	// und keine Source-Zeilennummer
		}

		if(l->sourceline != current_sourceline_index && !is_redefinable)	// redefined?
		{
			if(l->is_valid && is_valid && l->value==value &&	// allow trivial defs to occur multiple times
				l->is_global==is_global)						// e.g.:	SPACE equ $20 ; somewhere in source
				return;											// later:	SPACE equ $20 ; somewhere else in source
			throw syntax_error("label redefined");
		}

		XXXASSERT(is_valid || !l->is_valid || is_redefinable);
		XXXASSERT(l->segment == current_segment_ptr || is_redefinable);
		XXXASSERT(l->sourceline == current_sourceline_index || is_redefinable);

		if(l->is_valid && l->value!=value && !is_redefinable) throw syntax_error("value redefined");
		l->value    = value;
		l->is_valid = is_valid;
		l->is_defined = true;
	}
	else
	{
		if(pass==1 && !syntax_8080)	// 8080 all names allowed: mnenonic decides which arg is a register and which is a value
		{
			bool ill=no;
			if(name[1]==0)	// strlen(name) == 1
			{
				cstr names = "irbcdehla";
				ill = strchr(names,name[0]) != NULL;
			}
			else if(name[2]==0)	// strlen == 2
			{
				cstr names = "ix iy xh xl yh yl bc de hl sp af";
				ill = findStr(names,name) != NULL;
			}
			if(ill) throw syntax_error(usingstr("'%s' is the name of a register",name));
//			{
//				cstr linenumber = numstr(q.sourcelinenumber+1);		// seen in source used for JR destination
//				fprintf(stderr, "%s: %s\n", linenumber, q.text);	// but this is too dangerous
//				fprintf(stderr, "%s%s^ warning: '%s' is the name of a register\n", spacestr(strlen(linenumber)+2), q.whitestr(), name);
//			}
		}

		l = new Label(name, &current_segment(), current_sourceline_index, value, is_valid, is_global, yes, no);
		labels.add(l);
	}

	if(!is_redefinable)						// SET => value is void when writing the list file => don't list it
		q.label = l;				// this source line defines a label
}


/*	handle #directive
	all errors are fatal
	'#' must already be skipped
	all errors are fatal
*/
void Z80Assembler::asmDirect( SourceLine& q ) throw(fatal_error)
{
	try
	{
		cstr w = lowerstr(q.nextWord());

		if(eq(w,"if"))		  asmIf(q);			else
		if(eq(w,"elif"))	  asmElif(q);		else
		if(eq(w,"else"))	  asmElse(q);		else
		if(eq(w,"endif"))	  asmEndif(q);		else

		if(cond_off) 		  q.skip_to_eol();	else

		if(eq(w,"target"))	  asmTarget(q);		else
		if(eq(w,"code"))	  asmSegment(q,0);	else
		if(eq(w,"data"))	  asmSegment(q,1);	else
		if(eq(w,"include"))	  asmInclude(q);	else
		if(eq(w,"insert"))	  asmInsert(q);		else
		if(eq(w,"cflags"))	  asmCFlags(q);		else
		if(eq(w,"local"))	  asmLocal(q);		else
		if(eq(w,"endlocal"))  asmEndLocal(q);	else
		if(eq(w,"assert"))	  asmAssert(q);		else
		if(eq(w,"charset"))	  asmCharset(q);	else
		if(eq(w,"define"))	  asmDefine(q);		else
		if(eq(w,"end"))		  asmEnd(q);		else	throw fatal_error("unknown assembler directive");
	}
	catch(fatal_error& e) { throw e; }
	catch(any_error& e)   { throw fatal_error(e.what()); }
}

/*	#define <macro> <replacement>
	define some kind of replacement

	#define	NAME,NAME			rename instruction
	#define NAME,EXPRESSION		macro for expression
	#define NAME(ARGS,…) STUFF	macro which may expand to multiple lines by means of a simple '\'
								Aufruf mit NAME(ARGS,…)   (wie im C Preprozessor)
*/
void Z80Assembler::asmDefine( SourceLine& q ) throw(any_error)
{

// Test for: preprocessor function:
//	#define note(l1,l2,r1,r2,time) .dw l1+(l2*256)\.dw r1+(r2*256)\.dw time

	if(q.testChar('(')) throw fatal_error("preprocessor functions are not supported: use macros.");

// Test for: renamed instruction:
//	#define DEFB .BYTE
//	#define DEFW .WORD
//	#define DEFM .TEXT
//	#define ORG  .ORG
//	#define EQU  .EQU
//	#define equ  .EQU

	if(q.testDotWord("equ"))
	{
		if(q.testDotWord("equ")) return;
		else goto unknown_instr;
unknown_instr:
		throw fatal_error("unknown instruction");
	}

	if(q.testDotWord("org"))
	{
		if(q.testDotWord("org")) return;
		else goto unknown_instr;
	}

	if(q.testWord("defw") || q.testWord(".word") || q.testDotWord("dw"))
	{
		if(q.testWord("defw") || q.testWord(".word") || q.testDotWord("dw")) return;
		else goto unknown_instr;
	}

	if(q.testWord("defb") || q.testWord(".byte") || q.testDotWord("db"))
	{
		if(q.testWord("defb") || q.testWord(".byte") || q.testDotWord("db")) return;
		else goto unknown_instr;
	}

	if(q.testWord(".text") || q.testWord(".ascii") || q.testDotWord("dm") || q.testWord("defm"))
	{
		if(q.testWord(".text") || q.testWord(".ascii") || q.testDotWord("dm") || q.testWord("defm")) return;
		else goto unknown_instr;
	}

	if(q.testWord(".block") || q.testDotWord("ds") || q.testWord("defs"))
	{
		if(q.testWord(".block") || q.testDotWord("ds") || q.testWord("defs")) return;
		else goto unknown_instr;
	}

// Test for: const aka label definition:
//	#define strlen 7
//	#define	progStart	06900h
//	#define	LF		0Ah
//	#define	CR		0Dh
//	#define	BDOS	00005h
//	#define	BUFTOP	04000h
//	#define	CALSLT	0001Ch

	cstr w = q.nextWord();
	if(!is_name(w)) throw syntax_error("name expected");
	if(casefold_labels) w = lowerstr(w);

	bool v; int32 n = value(q,pAny,v=1);
	Label* l = &global_labels().find(w);

	if(l)
	{
		if(l->segment==NULL)							// .globl or defined before ORG
		{
			l->segment = current_segment_ptr;			// mit '.globl' deklarierte Label haben noch kein Segment
			l->sourceline = current_sourceline_index;	// und keine Source-Zeilennummer
		}

		if(l->sourceline != current_sourceline_index)	// redefined?
		{
			if(l->is_valid && v && l->value==n)			// allow trivial defs to occur multiple times
				return;									// e.g.:	SPACE equ $20 ; somewhere in source
														// later:	SPACE equ $20 ; somewhere else in source
			else throw syntax_error("label redefined");
		}

		XXXASSERT(v || !l->is_valid);
		XXXASSERT(l->segment == current_segment_ptr);
		XXXASSERT(l->sourceline == current_sourceline_index);

		if(l->is_valid && l->value!=n) throw syntax_error("label redefined");
		l->value	= n;
		l->is_valid	= v;
		l->is_defined = true;
	}
	else
	{
		if(pass==1 && !syntax_8080)	// 8080 all names allowed: mnenonic decides which arg is a register and which is a n
		{
			bool ill=no;
			if(w[1]==0)	// strlen(name) == 1
			{
				cstr names = "irbcdehla";
				ill = strchr(names,w[0]) != NULL;
			}
			else if(w[2]==0)	// strlen == 2
			{
				cstr names = "ix iy xh xl yh yl bc de hl sp af";
				ill = findStr(names,w) != NULL;
			}
			if(ill) throw syntax_error(usingstr("'%s' is the name of a register",w));
		}

		l = new Label(w, current_segment_ptr, current_sourceline_index, n, v, yes, yes, no);
		global_labels().add(l);
	}

	q.label = l;				// this source line defines a label
}

/*		rept	N
	;
	; some instructions
	;
		endm
*/
void Z80Assembler::asmRept( SourceLine& q ) throw(any_error)
{
	uint32& e = current_sourceline_index;
	uint32  a = e;

	// skip over contained instructions:
	// does not check for interleaved macro def or similar.
	for(;;)
	{
		if(++e>=source.count())	throw fatal_error("endm missing");
		SourceLine& s = source[e];
		if(s[0]=='#') throw fatal_error("unexpected assembler directive inside macro");
		s.rewind();
		if(s.testDotWord("endm")) break;
	}

	if(pass>1)	// => just skip the rept macro
	{
		q.skip_to_eol();
		return;
	}

	bool v;
	int32 n = value(q,pAny,v=1);
	if(!v) throw fatal_error("count must be evaluatable in pass 1");
	if(n>0x8000) throw fatal_error("number of repetitions too high");
	if(n<0) throw fatal_error("number of repetitions negative");
	if(source.count() + n*(e-a-1) > 1000000) throw any_error("total source exceeds 1,000,000 lines");

	ObjArray<SourceLine> zsource;
	while(n--)
	{
		for(uint32 i=a+1; i<e; i++)
		{
			zsource.append(new SourceLine(source[i]));
		}
	}
	source.insertat(e+1,zsource);
}

/*	NAME macro
	NAME macro ARG,ARG…
	;
	; some instructions
	;	&ARG may refer to ARG
	;	#ARG may refer to #ARG
	;
		endm
	;
	; invocation:
	;
		NAME ARG,…

	tag = potential tag character, e.g. '&'
	seen syntax:
	NAME macro ARG	; def
		NAME &ARG	; substitution in call
	NAME macro #ARG	; def
		NAME #ARG	; substitution in call
	.macro NAME ARG	; def
		NAME \ARG	; substitution in call

	the good thing is, they all _have_ a tag befor the argument reference…
*/
void Z80Assembler::asmMacro( SourceLine& q, cstr name, char tag ) throw(any_error)
{
	if(pass>1)	// => skip the macro definition
	{
		q.skip_to_eol();
		current_sourceline_index = macros[name].endm;
		source[current_sourceline_index].skip_to_eol();
		return;
	}

	name = lowerstr(name);
	if(macros.contains(name)) throw fatal_error("macro redefined");

	// parse argument list:
	CstrArray args;
	if(!q.testEol())
	{
		if(strchr("!#$%&.:?@\\^_|~",*q)) tag = *q;		// test whether args in def specify some kind of tag
		do												// else use the supplied (if any)
		{
			if(tag) q.testChar(tag);
			cstr w = q.nextWord();
			if(!is_name(w)) throw syntax_error("argument name expected");
			else args.append(w);
		}
		while(q.testChar(','));
		q.expectEol();
	}

	uint32& e = current_sourceline_index;
	uint a = e;

	// skip over contained instructions:
	// does not check for interleaved macro def or similar.
	while(++e<source.count())
	{
		SourceLine& s = source[e];
		s.rewind();
		if(s[0]=='#')
		{
			if(tag=='#' && is_name(++s.p) && args.find(s.nextWord())>=0) continue;
			throw fatal_error("unexpected assembler directive inside macro");
		}
		if(s.testDotWord("endm"))
		{
			s.skip_to_eol();	// problem: eof error would be reported on line with macro definition
			macros.add(name,new Macro(args,a,e,tag));	// note: args[] & name are unprotected cstr in tempmem!
			return;
		}
	}
	throw fatal_error("endm missing");
}

/*	Expand macro in pass1:
*/
void Z80Assembler::asmMacroCall(SourceLine& q, Macro& m) TAE
{
	if(pass>1) { q.skip_to_eol(); return; }

	int32 n;
	cstr w;

	// read arguments in macro call:
	CstrArray rpl;
	if(!q.testEol()) do
	{
		if(q.testChar('<'))		// extended argument: < ... " ... ' ... , ... ; ... > [,;\n]
		{
			cptr aa = q.p;
			cptr ae;
			do
			{
				while(*q && *q!='>') ++q; if(*q==0) throw syntax_error("closing '>' missing");
				ae = q.p;
				++q;			// skip '>'
			}
			while(!q.testEol() && *q!=',');

			// closing '>' found and skipped

			rpl.append(substr(aa,ae));
		}
		else					// simple argument: '"' and ''' must be balanced,
								// ',' and ';' can't occur in argument (except in char/string literal)
		{						// '(' or ')' may be unbalanced
			cptr  aa = q.p;
			char  c;
			while((c=*q.p) && c!=',' && c!=';')
			{
				if(c!='"'&&c!='\'') { q.p++; continue; }
				w = q.nextWord();
				n = strlen(w);
				if(n<2||w[n-1]!=c) throw syntax_error(usingstr("closing '%c' missing",c));
			}
		//	if(aa==q.p) throw syntax_error("empty argument (use <>");		denk…
			rpl.append(substr(aa,q.p));
		}
	}
	while(q.testComma());

	XXXASSERT(q.testEol());

	// get arguments in macro definition:
	CstrArray& args = m.args;
	if(rpl.count()<args.count()) throw syntax_error(usingstr("not enough arguments: required=%i",args.count()));
	if(rpl.count()>args.count()) throw syntax_error(usingstr("too many arguments: required=%i",args.count()));

	// get text of macro definition:
	uint32 i = m.mdef;
	uint32 e = m.endm;
	ObjArray<SourceLine> zsource;
	while(++i < e)
	{
		zsource.append(new SourceLine(source[i]));
		// das übernehmen wir:
		//	text;						// tempmem / shared
		//	sourcefile;					// tempmem / shared between all sourcelines of this file
		//	sourcelinenumber;			// line number in source file; 0-based
		// die sollten alle noch leer sein, da die Zeilen in der mdef selbst nie assembliert werden:
		//	s->segment = NULL;			// of object code
		//	s->byteptr = 0;				// index of object code in segment
		//	s->bytecount = 0;			// of bytes[]
		//	s->label = NULL;			// if a label is defined in this line
		//	s->is_data = 0;				// if generated data is no executable code
		//	s->p = s->text;				// current position of source parser
	}

	// replace arguments:
	for(i=0;i<zsource.count();i++)		// loop over lines
	{
		SourceLine& s = zsource[i];

		for(int32 j=0;;)				// loop over occurance of '&'
		{
			cptr p = strchr(s.text+j,m.tag);	// at next '&'
			if(!p) break;						// no more '&'
			if(!is_name(p+1)) continue;			// not an argument

			s.p = p+1; w = s.nextWord();		// get potential argument name
			if(casefold_labels) w = lowerstr(w);

			int a = args.find(w);				// get index of argument in argument list
			if(a==-1) continue;					// not an argument

			// w is the name of argument #a
			// it was found starting at p+1 in s.text  (p points to the '&')

			j = p+1 + strlen(rpl[a]) - s.text;
			s.text = catstr(substr(s.text,p), rpl[a], s.p);
		}
		s.rewind();	// superflux.
	}

	// insert text of macro definition into source:
	source.insertat(current_sourceline_index+1,zsource);
}


/*	#charset zxspectrum			; zx80, zx81, zxspectrum, jupiterace, ascii
	#charset none				;			 reset to no mapping
	#charset map "ABC" = 65		; or add:	 add mapping(s)
	#charset unmap "£"			; or remove: remove mapping(s)
*/
void Z80Assembler::asmCharset( SourceLine& q ) throw(any_error)
{
	cstr w = lowerstr(q.nextWord());
	bool v;
	int n;

	if(eq(w,"map") || eq(w,"add"))					// add mapping
	{
		w = q.nextWord();
		if(w[0]!='"') throw syntax_error("string with source character(s) expected");
		if(!q.testChar('=') && !q.testChar(',') && !q.testWord("to")) throw syntax_error("keyword 'to' expected");
		n = value(q,pAny,v=1);
		if(n!=(uint8)n&&n!=(int8)n) throw syntax_error("destination char code out of range");
		if(!charset) charset = new CharMap();
		charset->addMappings(unquotedstr(w),n);		// throws on illegal utf-8 chars
	}
	else if(eq(w,"unmap") || eq(w,"remove"))		// remove mapping
	{
		if(!charset) throw syntax_error("no charset in place");
		w = q.nextWord();
		if(w[0]!='"') throw syntax_error("string with source character(s) for removal expected");
		charset->removeMappings(unquotedstr(w));	// throws on illegal utf-8 chars
	}
	else if(eq(w,"none"))							// reset mapping to no mapping at all
	{
		delete charset;
		charset = NULL;
	}
	else											// select charset
	{
		CharMap::CharSet cs = CharMap::charsetFromName(w);
		if(cs==CharMap::NONE) throw syntax_error("map, unmap, none or charset name expected");
		delete charset;
		charset = new CharMap(cs);
	}
}

void Z80Assembler::asmAssert( SourceLine& q ) throw(any_error)
{
	bool v;
	int n = value(q,pAny,v=1);

	if(!v) throw fatal_error("the expression was not evaluatable in pass 1");
	if(!n) throw fatal_error("assertion failed");
}

// helper:
void Z80Assembler::init_c_flags()
{
	XXXASSERT(c_flags.count()==0);

	c_flags.append("-mz80");
	c_flags.append("-S");
	if(c_includes)
	{
		c_flags.append("--nostdinc");
		c_flags.append(catstr("-I",c_includes));	// -Ipath
	}
	c_tempdir = NULL;
}

/*	#CFLAGS -opt1 -opt2 …
	arguments may be quoted
	detects special arguments $SOURCE, $DEST and $CFLAGS
	validates path in -Ipath
	note: argv[0] (the executable's path) is not included in c_flags[].
		  $SOURCE and $DEST may be present or missing: then c_qi or c_zi = -1
		  $CFLAGS adds the old cflags. default: -S -mz80 [ --nostdinc -Ipath ]
		  in #include: default argv[] = { "/…/sdcc", "-S", "-mz80", [ "--nostdinc", "-Ipath", ] "-o", outfile, sourcefile }
*/
void Z80Assembler::asmCFlags( SourceLine& q ) throw(any_error)
{
	if(pass>1) { q.skip_to_eol(); return; }

	XXXASSERT(c_qi<(int)c_flags.count() && c_zi<(int)c_flags.count());

	if(c_flags.count()==0) init_c_flags();	// --> sdcc -mz80 -S
	CstrArray old_cflags = c_flags;		// moves contents
	int old_c_qi = c_qi; c_qi = -1;
	int old_c_zi = c_zi; c_zi = -1;

	while(!q.testEol())
	{
		cptr a = q.p;
		while((uint8)*q>' ') ++q;
		cstr s = substr(a,q.p);
		if(s[0]=='"') s = unquotedstr(s);

		if(s[0]=='$')
		{
			if(eq(s,"$SOURCE"))
			{
				if(c_qi<0) c_qi = c_flags.count();
				else throw fatal_error("$SOURCE redefined");
			}

			if(eq(s,"$DEST"))
			{
				if(c_zi<0) c_flags.count();
				else throw fatal_error("$DEST redefined");
			}

			if(eq(s,"$CFLAGS"))
			{
				if(old_c_qi>=0&&c_qi>=0) throw fatal_error("$SOURCE redefined");
				if(old_c_zi>=0&&c_zi>=0) throw fatal_error("$DEST redefined");
				if(old_c_qi>=0) c_qi = old_c_qi + c_flags.count();
				if(old_c_zi>=0) c_zi = old_c_zi + c_flags.count();
				c_flags.append(old_cflags);	// moves contents
				continue;
			}
		}

		if(s[0]=='-'&&s[1]=='I')	// -I/full/path/to/include/dir
		{							// -Ior/path/rel/to/source/dir	=> path in #cflags is relative to source file!
			cstr path = s+2;
//			if(path[0]=='/') throw fatal_error("hard path not allowed here (use command line option -I instead)");
//			if(contains(path,"..")) throw fatal_error("'..' not allowed here (use command line option -I instead)");
			if(path[0]!='/') path = catstr(source_directory,path);
			path = fullpath(path); if(errno) throw fatal_error(errno);
			if(lastchar(path)!='/') throw fatal_error(ENOTDIR);
			s = catstr("-I",path);
		}
//		else
//		{
//			if(contains(s,"..")) throw fatal_error("'..' not allowed here");
//			if(s[0]=='/') throw fatal_error("hard path not allowed here");
//			if(s[0]=='-' && s[1] && s[2]=='/') throw fatal_error("hard path not allowed here");
//		}

		c_flags.append_if_new(s);
	}

	init_c_tempdir();
}


/*	calculate subdir in tempdir for c compiler .s files
	subdir name is derived from c_flags[]
	name = hash
	first file: name = options
*/
void Z80Assembler::init_c_tempdir() THF
{
	XXXASSERT(lastchar(temp_directory)=='/');

	bool dflt = true;
	c_tempdir = catstr(temp_directory, "s/");
	if(stdlib_dir) { dflt=0; c_tempdir = catstr(c_tempdir, catstr("-L", replacedstr(stdlib_dir,'/',':')), "/"); }
	for(uint i=0; i<c_flags.count(); i++)
	{
		if(eq("-mz80",c_flags[i]))		continue;	// ignore the standard options
		if(eq("-S",c_flags[i]))			continue;	// though now you don't see if you miss them
		if(eq("--nostdinc",c_flags[i]))	continue;
		dflt=0; c_tempdir = catstr(c_tempdir, replacedstr(c_flags[i],'/',':'), "/");
	}
	if(dflt) c_tempdir = catstr(c_tempdir, "default/");

	if(!exists_node(c_tempdir))	create_dir(c_tempdir,0774,yes);
}


/*	#end
	force end of assembler source
	must not be within #if …
*/
void Z80Assembler::asmEnd(SourceLine& q) throw(any_error)
{
	end = true;

	cstr w = q.nextWord();		// seen in some source: "  end <label>"
	if(*w && !&global_labels().find(w)) throw syntax_error("end of line or label name expected");

//	// assign default segment to all remaining source lines
//	// to keep writeListfile() happy:
//	if(pass>1) return;
//	for(uint i=current_sourceline_index+1; i<source.count();i++) { source[i].segment = &segments[0]; }
}


/*	#if <condition>
	start block of source which is only assembled if value==true
	condition must be evaluatable in pass 1
	any number of #elif may follow
	then a single #else may follow
	then final #endif must follow
	while assembling is disabled, only #if, #else, #elif and #endif are recognized
	and #include is also skipped if conditional assembly is off.
*/
void Z80Assembler::asmIf(SourceLine& q) throw(any_error)
{
	if(cond[NELEM(cond)-1] != no_cond) throw fatal_error("too many conditions nested");

	uint lli = local_labels_index;			// force global context: else most labels won't resolve in pass1!
			   local_labels_index=0;		// TODO: denk: Probleme wenn value() throws? (final_error anyway)
	bool v = yes;
	bool f = cond_off || value(q,pAny,v);	// higher nesting level off => ignore; else evaluate value
	local_labels_index = lli;				// restore local_labels_index
	if(!v) throw fatal_error("condition not evaluatable in pass1");
	if(cond_off) q.skip_to_eol();

	memmove( cond+1, cond, sizeof(cond)-sizeof(*cond) );
	cond[0] = cond_if + f;
	cond_off = (cond_off<<1) + !f;
}

/*	#elif <condition>
	condition must be evaluatable in pass 1
*/
void Z80Assembler::asmElif(SourceLine& q) throw(any_error)
{
	switch(cond[0])				// state of innermost condition
	{
	default:			IERR();
	case no_cond:		throw syntax_error("#elif without #if");
	case cond_else:		throw syntax_error("#elif after #else");

	case cond_if_dis:			// we are in an if or elif clause and there was already a true condition
		cond_off |= 1;			// disable #elif clause
		q.skip_to_eol();		// just skip expression
		break;

	case cond_if:				// we are in an if or elif clause and up to now no condition was true
		XXXASSERT(cond_off&1);

		bool v = yes;
		bool f = cond_off>>1;		// outer nesting level
		if(f) q.skip_to_eol();		// outer nesting level off => just skip expression; value is irrelevant
		else
		{
			uint lli = local_labels_index;			// force global context: else most labels won't resolve in pass1!
					   local_labels_index=0;		// TODO: denk: Probleme wenn value() throws? (final_error anyway)
			f = value(q,pAny,v);	// else evaluate value
			local_labels_index = lli;				// restore local_labels_index
		}
		if(!v) throw fatal_error("condition must be evaluatable in pass1");

		cond_off -= f;			// if f==1 then clear bit 0 => enable #elif clause
		cond[0]  += f;			// and switch state to cond_if_dis => disable further elif evaluation
		break;
	}
}


/*	#else
*/
void Z80Assembler::asmElse(SourceLine&) throw(any_error)
{
	switch(cond[0])
	{
	default:			IERR();
	case no_cond:		throw syntax_error("#else without #if");
	case cond_else:		throw syntax_error("multiple #else clause");

	case cond_if_dis:			// we are in an if or elif clause and there was already a true condition
		cond[0] = cond_else;
		cond_off |=  1;			// disable #else clause
		break;

	case cond_if:				// we are in an if or elif clause and up to now no condition was true
		cond[0] = cond_else;
		cond_off &= ~1;			// enable #else clause
		break;
	}
}


/*	#endif
*/
void Z80Assembler::asmEndif(SourceLine&) throw(any_error)
{
	if(cond[0]==no_cond) throw syntax_error("no #if pending");

	memmove(cond, cond+1, sizeof(cond)-sizeof(*cond));
	cond[NELEM(cond)-1] = no_cond;
	cond_off = cond_off>>1;
}


/*	#target <nikname>
	known targets are: 'ROM', 'BIN', 'Z80', 'SNA', 'TAP', 'TAPE', 'O', 'P', '80', '81', 'P81', 'ACE'
*/
void Z80Assembler::asmTarget( SourceLine& q ) throw(any_error)
{
	if(pass>1) { q.skip_to_eol(); return; }
	if(target) throw fatal_error("#target redefined");
	XXXASSERT(!current_segment_ptr);

	target = upperstr(q.nextWord());
	if(!contains(" ROM BIN Z80 SNA TAP TAPE O P 80 81 P81 ACE ",catstr(" ",target," ")))
		throw syntax_error("target name expected");
}


static int find(Array<cstr> a, cstr s)
{
	for(int i=a.count();i--;) if(eq(s,a[i])) return i;
	return -1;
}


/*	#INCLUDE "sourcefile"
	the file is included in pass 1
	filenames ending on ".c" are compiled with sdcc (or the compiler set on the cmd line) into the temp directory

	#INCLUDE LIBRARY "libdir" [ RESOLVE label1, label2 … ]
	#INCLUDE STANDARD LIBRARY [ RESOLVE label1, label2 … ]
	all source files for not-yet-defined labels which were declared with .globl and found in libdir are included
	if keyword RESOLVE is also present,
		then only labels from this list are included.
		labels already defined or not declared with .globl or not yet used are silently ignored
		labels not found in libdir abort assembler
	c source files are compiled into "temp_directory/lib/"
	does not include recursively required definitions!
*/
void Z80Assembler::asmInclude( SourceLine& q ) throw(any_error)
{
	if(pass>1) { q.skip_to_eol(); return; }

	XXXASSERT(lastchar(temp_directory)=='/');
	XXXASSERT(!stdlib_dir || (eq(stdlib_dir,fullpath(stdlib_dir)) && lastchar(stdlib_dir)=='/' && !errno));

	bool is_stdlib = q.testWord("standard") || q.testWord("default") || q.testWord("system");
	bool is_library = q.testWord("library");
	if(is_stdlib && !is_library) throw syntax_error("keyword 'library' expected");

	if(is_library)
	{
		cstr fqn;
		if(is_stdlib)
		{
			if(!stdlib_dir) throw syntax_error("standard library path is not set (use command line option -L)");
			fqn = stdlib_dir;
		}
		else
		{
			fqn = q.nextWord();
			if(fqn[0]!='"') throw syntax_error("quoted directory name expected");
			fqn = unquotedstr(fqn);
			if(fqn[0]!='/') fqn = catstr(directory_from_path(q.sourcefile),fqn);
		}

		if(lastchar(fqn)!='/') fqn = catstr(fqn,"/");

		Array<cstr> names;
		if(q.testWord("resolve") && !q.testChar('*')) do
		{
			cstr w = q.nextWord();
			if(w[0]!='_' && !is_letter(w[0])) throw syntax_error("label name expected");
			names.append(w);
		}
		while(q.testChar(','));
		q.expectEol();

		MyFileInfoArray files;
		read_dir(fqn, files, yes);
		files.sort();					// make loading of library files predictable

		for(uint i=0;i<files.count();i++)
		{
			cstr fname = files[i].fname();
			cstr name  = basename_from_path(fname);

			if(names.count() && !find(names,name)) continue;	// not in explicit list

			Label* l = &global_labels().find(name);
			if(!l) continue;			// never used, defined or declared
			if(l->is_defined) continue;	// already defined
			if(!l->is_used) continue;	// not used: must have been used before position of #include library!

			if(endswith(fname,".c") || endswith(fname,".s") || endswith(fname,".ass") || endswith(fname,".asm"))
			{
				//	#include library "path"			; <-- current_sourceline
				//	#include "path/fname"			; <-- generated
				//	; contents of file will go here	; <-- inserted when #include "path/fname" is assembled
				//	#assert defined(fname::)		; <-- generated: prevent infinite recursion in case of error
				//	#include library "path"			; <-- copy of current_sourceline: include more files from library

				cstr s1 = usingstr("#include \"%s%s\"",fqn,fname);
				cstr s2 = usingstr("#assert defined(%s::)",name);
				cstr s3 = q.text;
				source.insertat(current_sourceline_index+1, new SourceLine(q.sourcefile,q.sourcelinenumber,s1));
				source.insertat(current_sourceline_index+2, new SourceLine(q.sourcefile,q.sourcelinenumber,s2));
				source.insertat(current_sourceline_index+3, new SourceLine(q.sourcefile,q.sourcelinenumber,s3));
				return;
			}
			else continue;			// skip any unknown files: e.g. list files etc.
		}

		// if we come here, not a single label was resolved
		if(names.count()) throw fatal_error(usingstr("source file for label %s not found",names[0]));
		// else we are done.
	}
	else
	{
		cstr fqn = q.nextWord();
		if(fqn[0]!='"') throw syntax_error("quoted filename expected");
		fqn = unquotedstr(fqn);
		if(fqn[0]!='/') fqn = catstr(directory_from_path(q.sourcefile),fqn);

		if(endswith(fqn,".c"))
		{
			//	#include "path/fname"			; <-- current_sourceline
			//	#local							; <-- generated
			//	; contents of file will go here	; <-- inserted by includeFile()
			//	#endlocal						; <-- generated

			fqn = compileFile(fqn);
			source.insertat(current_sourceline_index+1, new SourceLine(q.sourcefile,q.sourcelinenumber,"#local"));
			source.insertat(current_sourceline_index+2, new SourceLine(q.sourcefile,q.sourcelinenumber,"#endlocal"));
			source.includeFile(fqn, current_sourceline_index+2);
		}
		else
		{
			source.includeFile(fqn, current_sourceline_index+1);
		}
	}
}


cstr Z80Assembler::compileFile(cstr fqn) throw(any_error)
{
	if(c_compiler==NULL)
	{
		Array<str> ss;
		split(ss, getenv("PATH"), ':');
		for(uint i=0; i<ss.count(); i++)
		{
			cstr s = catstr(ss[i],"/sdcc");
			if(is_file(s)) { c_compiler = s; break; }
		}
		if(!exists_node(c_compiler))	throw fatal_error("sdcc not found");
		if(!is_file(c_compiler))		throw fatal_error("sdcc is not a regular file");
		if(!is_executable(c_compiler))	throw fatal_error("sdcc is not executable");
	}

	if(c_flags.count()==0) 	// --> sdcc -mz80 -S
	{
		init_c_flags();
		init_c_tempdir();
	}

	cstr fqn_q = fqn;
	cstr fqn_z = catstr(c_tempdir, basename_from_path(fqn), ".s");

	// if the .s file exists and is newer than the .c file, then don't compile again:
	// note: this does not handle modified header files or modified CFLAGS or upgraded SDCC itself!
	if(exists_node(fqn_z) && file_mtime(fqn_z) > file_mtime(fqn_q)) return fqn_z;

	// create pipe:
    const int R=0,W=1;
    int pipout[2];
    if(pipe(pipout)) throw fatal_error(errno);

	// compile source file:

	pid_t child_id = fork();	// fork a child process
	XXXASSERT(child_id!=-1);	// fork failed: can't happen

	if(child_id==0)				// child process:
	{
		close(pipout[R]);		// close unused fd
        close(1);				// close stdout
        close(2);				// close stderr
		int r1 = dup(pipout[W]);			// becomes lowest unused fileid: stdout
	 	int r2 = dup(pipout[W]);			// becomes lowest unused fileid: stderr
        (void)r1; (void)r2;
        close(pipout[W]);		// close unused fd

		int result = chdir(source_directory);	// => partial paths passed to sdcc will start in source dir
		if(result) exit(errno);

		if(c_zi<0) { c_flags.append("-o"); c_flags.append(fqn_z); } else { c_flags[c_zi] = fqn_z; }
		if(c_qi<0) {                       c_flags.append(fqn_q); } else { c_flags[c_qi] = fqn_q; }
		c_flags.insertat(0,c_compiler);
		c_flags.append(NULL);

		execve(c_compiler, (char**)c_flags.getData(), environ);	// exec cmd
		exit(errno);			// exec failed: return errno: will be printed in error msg,
								//				but is ambiguous with cc exit code
	}
	else						// parent process:
	{
		close(pipout[W]);		// close unused fd
		FD fd(pipout[R],"PIPE");

		int status;
		const uint SIZE = 0x7fff;	// if we get more output there is something going very wrong
		char bu[SIZE+1];			// collector
		uint32 size = fd.read_bytes(bu,SIZE,0);
		if(size==SIZE) { fd.close_file(no); bu[SIZE-1]='\n'; }	// there is something wrong => kill child
		bu[size] = 0;

		/*	Output in case of NO ERROR:
			0 bytes

			Sample output in case of ERROR:
		/pub/Develop/Projects/zasm-4.0/Test/main.c:63: warning 112: function 'strcmp' implicit declaration
		/pub/Develop/Projects/zasm-4.0/Test/main.c:64: warning 112: function 'memcpy' implicit declaration
		/pub/Develop/Projects/zasm-4.0/Test/main.c:65: warning 112: function 'strcpy' implicit declaration
		/pub/Develop/Projects/zasm-4.0/Test/main.c:63: error 101: too many parameters
		/pub/Develop/Projects/zasm-4.0/Test/main.c:64: error 101: too many parameters
		/pub/Develop/Projects/zasm-4.0/Test/main.c:65: error 101: too many parameters
		*/

		for(int err; (err = waitpid(child_id,&status,0)) != child_id; )
		{
			XXXASSERT(err==-1);
			if(errno!=EINTR) throw fatal_error(usingstr("waitpid: %s",strerror(errno)));
		}

		if(WIFEXITED(status))				// child process exited normally
		{
			if(WEXITSTATUS(status)!=0)		// child process returned error code
				throw fatal_error(usingstr("\"%s %s\" returned exit code %i\n- - - - - -\n%s- - - - - -\n",
					filename_from_path(c_compiler), filename_from_path(fqn_q), (int)WEXITSTATUS(status), bu));
		}
		else if(WIFSIGNALED(status))		// child process terminated by signal
		{
			throw fatal_error(usingstr("\"%s %s\" terminated by signal %i",
					filename_from_path(c_compiler), filename_from_path(fqn_q), (int)WEXITSTATUS(status)));
		}
		else IERR();
	}

	return fqn_z;
}


/*	#insert <"path/filename">
	insert file's contents into code
*/
void Z80Assembler::asmInsert( SourceLine& q ) throw(any_error)
{
	if(!current_segment_ptr) throw syntax_error("org not yet set");

	q.is_data = yes;	// even if it isn't, but we don't know. else listfile() will bummer

	cstr fqn = q.nextWord();
	if(fqn[0]!='"') throw syntax_error("quoted filename expected");

	fqn = unquotedstr(fqn);
	if(fqn[0]!='/') fqn = catstr(directory_from_path(q.sourcefile),fqn);

	FD fd(fqn,'r');
	off_t sz = fd.file_size();			// file size
	if(sz>0x10000) throw fatal_error("file is larger than $10000 bytes");	// max. possible size in any case

	char bu[sz];
	fd.read_bytes(bu, (uint32)sz);
	storeBlock(bu,(uint32)sz);
}


/*	#code <NAME> [,<start>] [,<size>] [,flags]
	#data <NAME> [,<start>] [,<size>] [,flags]
	on first occurance start and size and, if required, flags may be defined
	<start> may be '*' for relocatable (append to prev. segment)
	<size>  may be '*' for resizable   (shrink to fit)
	on subsequent re-opening of segment no arguments are allowed
*/
void Z80Assembler::asmSegment( SourceLine& q, bool is_data ) throw(any_error)
{
	// wenn #code oder #data benutzt werden, muss #target gesetzt worden sein:
	if(!target) throw fatal_error("#target declaration missing");

	cstr name = q.nextWord();
	if(!is_name(name)) throw fatal_error("segment name expected");
	Segment* segment = segments.find(name);
	XXXASSERT(!segment || eq(segment->name,name));

	if(segment && segment->is_data != is_data) throw fatal_error("#code/#data mismatch");

	int32 address	= 0;
	int32 size		= 0;
	int32 flags		= 0;
	bool  address_is_valid	= no;
	bool  size_is_valid		= no;
	bool  flags_is_valid	= no;
	bool  relocatable		= yes;
	bool  resizable			= yes;
	bool  has_flag			= no;

	if(q.testComma())
	{
		relocatable = q.testChar('*');
		if(!relocatable) address = value(q, pAny, address_is_valid=yes);
	}

	if(q.testComma())
	{
		resizable = q.testChar('*');
		if(!resizable) size = value(q, pAny, size_is_valid=yes);
	}

	if(q.testComma())
	{
		has_flag = yes;
		flags = value(q, pAny, flags_is_valid=yes);
		if(flags_is_valid && flags!=(uint8)flags) throw syntax_error("value out of range");
	}

	if(segment==NULL)	// new segment in pass 1
	{
		XXXASSERT(pass==1);

		uint8 fillbyte = is_data || eq(target,"ROM") ? 0xFF : 0x00;
		segment = new Segment(name,is_data,fillbyte,relocatable,resizable,has_flag);
		segments.append(segment);
		q.label = new Label(name,segment,q.sourcelinenumber,address,address_is_valid,yes,yes,no);
		global_labels().add(q.label);
	}
	else if(address_is_valid)
	{
		Label& l = global_labels().find(name);
		if(l.is_valid && l.value!=(int32)address)
			{ addError(usingstr("label %s redefined",name)); return; }
		l.value    = address;
		l.is_valid = yes;
	}

	if(q.label) reusable_label_basename = name;

	if(address_is_valid) { segment->setAddress(address); segment->setOrigin(address,yes); }	// throws
	if(size_is_valid)    { segment->setSize(size); }			// throws
	if(flags_is_valid)   { segment->setFlag(flags); }			// throws

	current_segment_ptr = segment;
	q.segment = current_segment_ptr;	// Für Temp Label Resolver
	q.byteptr = currentPosition();		// Für Temp Label Resolver & Logfile
	XXXASSERT(q.bytecount==0);
}


/*	Handle FIRST occurance of pseudo instruction ORG
	ORG is handled differently for first occurance or later occurances:
	the first ORG sets the start address of the default code segment
	while later ORGs insert space.
	note: #CODE or #DATA implicitely set an ORG so any ORG thereafter inserts space.

	Source either uses #TARGET and #CODE to set a target and to define code segments
	or source does not use #TARGET/#CODE and simply sets ORG for a single default code segment.

	This is handled here:
		ORG sets the target to ROM,
		creates a default segment
		and sets it's start address.

	Thereafter code can be inserted into this segment.
	Before ORG or #CODE no code can be stored and trying to do so results in an error.
*/
void Z80Assembler::asmFirstOrg(SourceLine& q) throw(any_error)
{
	XXXASSERT(!current_segment_ptr);

	Segment* s;
	Label*   l;

	if(pass==1)
	{
		// ORG after #TARGET and no #CODE:
		if(target) throw fatal_error("#code segment definition expected after #target");

		s = new Segment(DEFAULT_CODE_SEGMENT,no,0xff,no,yes,no);
		l = new Label(DEFAULT_CODE_SEGMENT,s,current_sourceline_index,0,no,yes,yes,no);
		segments.append(s);
		global_labels().add(l);
	}
	else
	{
		s = &segments[0];
		l = &global_labels().find(DEFAULT_CODE_SEGMENT);
	}

	current_segment_ptr = s;			// => from now on code deposition is possible
//	target				= "ROM";		bleibt ungesetzt => #code will bummer
//	reusable_label_basename = DEFAULT_CODE_SEGMENT;

	bool v; int n = value(q,pAny,v=1);

	XXXASSERT(v || !l->is_valid);
	XXXASSERT(l->value==n || !l->is_valid || !v);
	XXXASSERT(s->logicalAddress()==n || !s->logicalAddressValid() || !v);

	q.label = l;
	l->is_defined = yes;
	l->is_valid = yes; l->value = n;
	if(v) { s->setAddress(n); s->setOrigin(n,v); }
	return;
}

/*	#local
	startet einen lokalen Codeblock
	Neue Label, die nicht als global deklariert sind, werden in die aktuellen local_labels gelegt.
*/
void Z80Assembler::asmLocal(SourceLine&) throw(any_error)
{
	// local_labels_index = Index des aktuellen local_labels Blocks in labels[]
	// local_blocks_count = Anzahl local_labels Blocks in labels[] bisher (in pass1: == labels.count)

	if(pass==1)
	{
		XXXASSERT(local_blocks_count == labels.count());

		labels.append(new Labels(local_labels_index));	// neuen Block mit Rückbezug auf aktuellen (umgebenden) Block
	}
	else
	{
		XXXASSERT(labels[local_blocks_count].outer_index==local_labels_index);
	}

	local_labels_index = local_blocks_count++;
}


/*	#endlocal
	beendet lokalen Codeblock
*/
void Z80Assembler::asmEndLocal(SourceLine&) throw(any_error)
{
	if(local_labels_index==0) throw syntax_error("#endlocal without #local");

	if(pass==1)	// Pass 1: verschiebe undefinierte lokale Label in den umgebenden Kontext
	{
		Labels& local_labels = this->local_labels();
		Array<Label*>& local_labels_array = local_labels.getItems();
		Array<Label*> undef_labels_array;
		uint outer_index = local_labels.outer_index;
		Labels& outer_labels = labels[outer_index];
		bool is_global = outer_labels.is_global;

		// Suche lokal undefinierte Labels,
		// die nicht mit .globl als global deklariert wurden:
		for(uint lli = local_labels_array.count(); lli--; )
		{
			Label* label = local_labels_array[lli];
			if( !label->is_defined && !label->is_global )
				undef_labels_array.append(label);
		}

		// Verschiebe diese Labels in den umgebenden Kontext:
		for(uint uli = undef_labels_array.count(); uli--; )
		{
			Label* ql = undef_labels_array[uli];		XXXASSERT(!ql->is_global);
			Label* zl = &outer_labels.find(ql->name);

			XXLogLine("pushing %s",ql->name);

			if(zl==NULL) { zl = new Label(*ql); outer_labels.add(zl); zl->is_global = is_global; }
			else zl->is_used = yes;
			local_labels.remove(ql->name);
		}
	}

	local_labels_index = local_labels().outer_index;
}




// --------------------------------------------------
//				Assemble Opcode
// --------------------------------------------------


/*	store signed or unsigned byte
	validates byte
*/
void Z80Assembler::storeByte(int byte) throw(any_error)
{
	if(byte>255||byte<-128) throw syntax_error( "byte value out of range" );
	store(byte);
}

/*	store offset byte and check range
	validates offset if valid=true
*/
void Z80Assembler::storeOffset(int offset, bool valid) throw(any_error)
{
	if(valid && (offset!=(signed char)offset)) throw syntax_error("offset out of range");
	store(offset);
}

void Z80Assembler::storeEDopcode( int n ) TAE
{
	if(target_8080) throw syntax_error("no i8080 opcode (option --8080)");
	store(0xED,n);
}


// enumeration of Z80 identifiers:
//
enum
{
	NIX,	// end of line

	// conditions:
	NZ,		Z,		NC,		CY,		PO,		PE,		P,		M,	// <-- DO NOT REORDER!

	// 8-bit registers:
	RB,		RC,		RD,		RE,		RH,		RL,		XHL,	RA,	// <-- DO NOT REORDER!
	XH,		XL,		YH,		YL,									// <-- DO NOT REORDER!
	RI,		RR,

	// 16-bit registers:
	BC,		DE,		HL,		SP,			// <-- DO NOT REORDER!
	IX,		IY,		AF,

	// others:
	XBC,	XDE,	XC,		XSP,	XIX,	XIY,
	XNN,	NN,
	XMMBC, XMMDE, XMMHL, XBCPP, XDEPP, XHLPP,	// (hl++) etc. for compound opcodes
};


/*	test and skip over condition
	returns NIX or enum [Z, NZ .. P]
	expect_comma
		must be set if cond must be followed by a comma --> jr, jp and call
		and must be cleared for --> ret
*/
uint Z80Assembler::getCondition( SourceLine& q, bool expect_comma ) throw(syntax_error)
{
	cptr p = q.p;
	cstr w = q.nextWord();	if(w[0]==0) return NIX;
	if(expect_comma && !q.testComma()) { q.p = p; return NIX; }

	char c1 = *w++ | 0x20;
	char c2 = *w++ | 0x20;

	if(c2==0x20)	// strlen = 1
	{
		if(c1=='z') return Z;	if(c1=='c') return CY;
		if(c1=='p') return P;	if(c1=='m') return M;
		if(c1=='s') return M;	// source seen ...
	}
	else if(*w==0)	// strlen = 2
	{
		if(c1=='n') { if(c2=='z') return NZ; if(c2=='c') return NC; }
		if(c1=='p') { if(c2=='o') return PO; if(c2=='e') return PE; }
	}
	throw syntax_error("illegal condition");
}


/*	test and skip over register or value
	returns register enum:
		normal register:     n and v are void (not modified)
		NN, XNN, XIX or XIY: n and v are set
		does not return i, r, (c), ix, iy or related if target_8080
	throws on error
	throws at end of line
*/
uint Z80Assembler::getRegister(SourceLine& q, int32& n, bool& v) throw(syntax_error)
{
	cptr p = q.p;
	cstr w = q.nextWord();

	char c1 = *w++ | 0x20;	if(c1==0x20) throw syntax_error("unexpected end of line");
	char c2 = *w++ | 0x20;

	if(c2==0x20)	// strlen=1
	{
		switch(c1)
		{
		case 'a':	return RA;
		case 'b':	return RB;
		case 'c':	return RC;
		case 'd':	return RD;
		case 'e':	return RE;
		case 'h':	return RH;
		case 'l':	return RL;
		case 'i':	if(target_z80) return RI;
		case 'r':	if(target_z80) return RR;

no_8080:			throw syntax_error("no 8080 register");

		case '(':
			{
				int r;
				if(q.testWord("hl")) { r=XHL; if(*q=='+'&&*(q.p+1)=='+'){ q+=2; r=XHLPP; } q.expect(')'); return r; }
				if(q.testWord("de")) { r=XDE; if(*q=='+'&&*(q.p+1)=='+'){ q+=2; r=XDEPP; } q.expect(')'); return r; }
				if(q.testWord("bc")) { r=XBC; if(*q=='+'&&*(q.p+1)=='+'){ q+=2; r=XBCPP; } q.expect(')'); return r; }
				if(q.testWord("sp")) { q.expect(')'); return XSP; }

				if(*q=='-'&&*(q.p+1)=='-')
				{
					p = q.p;
					q.p += 2;
					if(q.testWord("hl")) { q.expect(')'); return XMMHL; }
					if(q.testWord("de")) { q.expect(')'); return XMMDE; }
					if(q.testWord("bc")) { q.expect(')'); return XMMBC; }
					q.p = p;
				}

				r = XNN; n=0; v=1;

				if(q.testWord("ix")) { if(target_8080) goto no_8080; r = XIX; if(q.testChar(')')) return r; }
				if(q.testWord("iy")) { if(target_8080) goto no_8080; r = XIY; if(q.testChar(')')) return r; }
				if(q.testWord("c"))  { if(target_8080) goto no_8080; q.expect(')'); return XC; }

				n = value(q,pAny,v);
				q.expect(')');
				return r;
			}
		}
	}
	else if(*w==0)	// strlen=2
	{
		switch(c1)
		{
		case 'a':	if(c2=='f') return AF; else break;
		case 'b':	if(c2=='c') return BC; else break;
		case 'd':	if(c2=='e') return DE; else break;
		case 'h':	if(c2=='l') return HL; else break;
		case 's':	if(c2=='p') return SP; else break;
		case 'i':	if(c2=='x') { if(target_z80) return IX; else goto no_8080; }
					if(c2=='y') { if(target_z80) return IY; else goto no_8080; } else break;
		case 'x':	if(c2=='h') { if(target_z180) goto no_z180; if(target_z80) return XH; goto no_8080; }
					if(c2=='l') { if(target_z180) goto no_z180; if(target_z80) return XL; goto no_8080; } else break;
		case 'y':	if(c2=='h') { if(target_z180) goto no_z180; if(target_z80) return YH; goto no_8080; }
					if(c2=='l') { if(target_z180) goto no_z180; if(target_z80) return YL; goto no_8080; } else break;
		}
	}

	// not a register: evaluate expression:
	q.p = p;
	n = value(q,pAny,v=1);
	if(target_8080 || !q.testChar('(')) return NN;

	// SDASZ80 syntax: n(IX)
	if(q.testWord("ix")) { q.expectClose(); return XIX; }
	if(q.testWord("iy")) { q.expectClose(); return XIY; }
	throw syntax_error("syntax error");
no_z180:
	throw syntax_error("illegal register: the Z180 traps illegal instructions");
}


/*	assemble opcode
*/
void Z80Assembler::asmInstr(SourceLine& q) throw(any_error)
{
	int   r,r2;
	int32 n,n2;
	bool  v,v2;
	uint  instr;
	cptr  depp=0;						// dest error position ptr: for instructions where
										// source is parsed before dest can be checked
	cstr w = q.nextWord();

// strlen-Verteiler:

	if(current_segment_ptr)
	{
		switch(strlen(w))
		{
		case 0:		return;				// end of line
		case 2:		n = peek2X(w); break;
		case 3:		n = peek3X(w); break;
		case 4:		n = peek4X(w); break;
		default:	goto wlenXL;
		}
	}
	else	// #CODE or ORG not yet set:
	{
		// allowed: ORG, misc. ignored proprietary words and list options
		// allowed: EQU label definitions (handled in asmLabel())
		// allowed: #directives, except #insert (handled there)

		if(*w==0) return;	// end of line
		w = lowerstr(w);
		if(doteq(w,"org"))	{ asmFirstOrg(q); return; }
		goto valid_without_segment;
	}

	switch(n|0x20202020)
	{
	default:		w=lowerstr(w); goto misc;
	case '  ei':	store(EI); return;
	case '  di':	store(DI); return;

	case '  jp':
		// jp NN
		// jp rr	hl ix iy
		// jp (rr)	hl ix iy
		r2 = getCondition(q,yes);
		r  = getRegister(q,n,v);
		if(r==NN) { store(r2==NIX ? JP : JP_NZ+(r2-NZ)*8); storeWord(n); return; }
		if(r==HL||r==XHL)			 { store(JP_HL); return; }
		if(r==IX||(r==XIX&&v&&n==0)) { store(PFX_IX,JP_HL); return; }
		if(r==IY||(r==XIY&&v&&n==0)) { store(PFX_IY,JP_HL); return; }
		goto ill_dest;

	case '  im':
		// im n		0 1 2
		r = getRegister(q,n,v);
		if(r==NN && ((uint)n<=2 || !v)) { if(n) n++; storeEDopcode(IM_0 + n*8); return; }
		throw syntax_error("illegal interrupt mode");

	case '  in':
		// in a,(N)
		// in a,N		(seen in sources)
		// in r,(c)		a f b c d e h l
		// in r,(bc)	a f b c d e h l
		if(q.testWord("f")) r = XHL;
		else { r = getRegister(q,n,v); if(r==XHL) goto ill_dest; }
		depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);
		if(r2==XC || r2==XBC)
		{
			if(r>RA) goto ill_dest;
			storeEDopcode(IN_B_xC+(r-RB)*8); return;
		}
		if(r2==XNN || r2==NN)
		{
			if(r!=RA) goto ill_dest;
			store(INA); storeByte(n2); return;
		}
		goto ill_source;

	case '  rl':
		instr = RL_B;
		goto rr;

	case '  rr':
		instr = RR_B;
		goto rr;

		// bit n,r			0..7  a b c d e h l (hl)
		// bit n,(ix+d)
		// bit n,xh			ILLEGAL ***NOT ALL Z80 CPUs!***
		// bit n,(ix+d),r	ILLEGAL ***NOT ALL Z80 CPUs!***

		// common handler for:
		// bit, set, res

		// rr r			a b c d e h l (hl)
		// rr xh		xh xl yh yl		// ILLEGAL: ***NOT ALL Z80 CPUs!***
		// rr (ix+n)	ix iy
		// rr (ix+n),r	a b c d e h l	// ILLEGAL: ***NOT ALL Z80 CPUs!***

		// common handler for:
		// rr rrc rl rlc sla sra sll srl

rr:		if(target_8080) goto ill_8080;
		r2 = getRegister(q,n2,v2);

		if(r2<=RA) { store(PFX_CB, instr + r2-RB); return; }

		if(r2<=YL)
		{
			if(!ixcbxh_enabled) throw syntax_error("command line option --ixcb=xh not enabled");
			if(r2>=YH) store(PFX_IY, PFX_CB, 0, instr+r2+RH-YH-RB);
			else	   store(PFX_IX, PFX_CB, 0, instr+r2+RH-XH-RB);
			return;
		}

		if(r2==XIX || r2==XIY)
		{
			r = XHL;
			if(q.testComma())
			{
				if(!ixcbr2_enabled) throw syntax_error("command line option --ixcb=r2 not enabled");
				r = getRegister(q,n,v);
				if(r>RA || r==XHL) throw syntax_error("illegal secondary destination");
			}
			store(r2==XIX?PFX_IX:PFX_IY, PFX_CB);
			storeOffset(n2,v2);
			store(instr+r-RB);
			return;
		}

		if(r2==XHLPP) { store(PFX_CB, instr + XHL-RB, INC_HL); return; }
		if(r2==XMMHL) { store(DEC_HL, PFX_CB, instr + XHL-RB); return; }

		if((instr&0xc0)==BIT0_B) goto ill_source; else goto ill_target;

	case '  ex':
		// ex af,af'
		// ex hl,de		(or vice versa)
		// ex hl,(sp)
		// ex ix,(sp)	valid illegal. 2006-09-13 kio
		// ex ix,de		does not work: swaps de and hl only. 2006-09-13 kio
		r = getRegister(q,n,v);
		depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);

		if(r==AF) { if(target_8080) goto ill_8080;
					if(r2==AF) store(EX_AF_AF); else goto ill_source; q.testChar('\''); return; }
		if(r==HL) { if(r2==DE) store(EX_DE_HL); else if(r2==XSP) store(EX_HL_xSP); else goto ill_source; return; }
		if(r==DE) { if(r2==HL) store(EX_DE_HL); else goto ill_source; return; }
		if(r==IX) { if(r2==XSP) store(PFX_IX, EX_HL_xSP); else goto ill_source; return; }
		if(r==IY) { if(r2==XSP) store(PFX_IY, EX_HL_xSP); else goto ill_source; return; }
		if(r==XSP){ if(r2==HL) store(EX_HL_xSP); else if(r2==IX) store(PFX_IX, EX_HL_xSP);
					else if(r2==IY) store(PFX_IY, EX_HL_xSP); else goto ill_source; return; }
		goto ill_target;

	case '  jr':
		// jr nn
		// jr cc,nn
		if(target_8080) goto ill_8080;
		r2 = getCondition(q,yes); if(r2>CY) throw syntax_error("illegal condition");
		r  = getRegister(q,n,v); if(r!=NN) goto ill_dest;	// before store opcode JR for correct value of "$"

		store(r2==NIX ? JR : JR_NZ+(r2-NZ)*8);
		storeOffset(n - (currentAddress()+1), v && currentAddressValid());
		return;

	case '  or':
		instr = OR_B;
		goto cp;

	case '  cp':
		// cp a,N			first argument (the 'a' register) may be omitted
		// cp a,r			a b c d e h l (hl)
		// cp a,xh
		// cp a,(ix+dis)

		// common handler for
		// add adc sub sbc and or xor cp

		instr = CP_B;

cp:		r = getRegister(q,n,v);
cp_a:	depp=q.p; if(q.testComma()) { if(r!=RA) goto ill_target; else r = getRegister(q,n,v); }

		if(r<=RA) { store(instr+r-RB); return; }
		if(r<=XL) { store(PFX_IX, instr+r+RH-XH-RB); return; }
		if(r<=YL) { store(PFX_IY, instr+r+RH-YH-RB); return; }
		if(r==NN) { store(instr+CP_N-CP_B); storeByte(n); return; }

		if(r==XHLPP) { store(instr+XHL-RB, INC_HL); return; }
		if(r==XMMHL) { store(DEC_HL, instr+XHL-RB); return; }

		if(r==XIX) store(PFX_IX); else if(r==XIY) store(PFX_IY); else goto ill_source;
		store(instr+XHL-RB);
		storeOffset(n,v);
		return;

	case '  ld':
		r = getRegister(q,n,v);
		depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);
		XXXASSERT(r>=RB);
		XXXASSERT(r2>=RB);

		switch(r)
		{
		case RI:
			// ld i,a
			if(r2==RA) { storeEDopcode(LD_I_A); return; }
			goto ill_source;

		case RR:
			// ld r,a
			if(r2==RA) { storeEDopcode(LD_R_A); return; }
			goto ill_source;

		case IX:
			// ld ix,rr		bc de			Goodie
			// ld ix,(NN)
			// ld ix,NN
			instr = PFX_IX;
			goto ld_iy;

		case IY:
			// ld iy,rr		bc de			Goodie		TODO: not for Z180!
			// ld iy,(NN)
			// ld iy,NN
			instr = PFX_IY;
ld_iy:		store(instr);
			if(r2==BC)  { store(LD_H_B, instr, LD_L_C); return; }
			if(r2==DE)  { store(LD_H_D, instr, LD_L_E); return; }
			goto ld_hl;

		case HL:
			// ld hl,rr		bc de			Goodie
			// ld hl,(ix+d)					Goodie
			// ld hl,(NN)
			// ld hl,NN
			if(r2==BC)  { store(LD_H_B, LD_L_C); return; }
			if(r2==DE)  { store(LD_H_D, LD_L_E); return; }
			if(r2==XIX)	{ store(PFX_IX, LD_L_xHL); storeOffset(n2,v2); store(PFX_IX, LD_H_xHL); storeOffset(n2+1,v2); return; }
			if(r2==XIY)	{ store(PFX_IY, LD_L_xHL); storeOffset(n2,v2); store(PFX_IY, LD_H_xHL); storeOffset(n2+1,v2); return; }
ld_hl:		if(r2==XNN) { store(LD_HL_xNN); storeWord(n2); return; }
			if(r2==NN)  { store(LD_HL_NN);  storeWord(n2); return; }
			goto ill_source;

		case BC:
			// ld bc,NN
			// ld bc,(NN)
			// ld bc,rr		de hl ix iy		Goodie
			// ld bc,(hl)					Goodie
			// ld bc,(ix+d)					Goodie
			// ld bc,(hl++)					Goodie
			if(r2==NN)  { store(LD_BC_NN); storeWord(n2); return; }
			if(r2==XNN) { storeEDopcode(LD_BC_xNN); storeWord(n2); return; }
			if(r2==DE)  { store(LD_B_D, LD_C_E); return; }
			if(r2==HL)  { store(LD_B_H, LD_C_L); return; }
			if(r2==IX)  { store(PFX_IX, LD_B_H, PFX_IX, LD_C_L); return; }
			if(r2==IY)  { store(PFX_IY, LD_B_H, PFX_IY, LD_C_L); return; }
			if(r2==XHL)	{ store(LD_C_xHL, INC_HL, LD_B_xHL, DEC_HL); return; }
			if(r2==XIX)	{ store(PFX_IX, LD_C_xHL); storeOffset(n2,v2); store(PFX_IX, LD_B_xHL); storeOffset(n2+1,v2); return; }
			if(r2==XIY)	{ store(PFX_IY, LD_C_xHL); storeOffset(n2,v2); store(PFX_IY, LD_B_xHL); storeOffset(n2+1,v2); return; }
			if(r2==XHLPP) { store(LD_C_xHL, INC_HL, LD_B_xHL, INC_HL); return; }
			if(r2==XMMHL) { store(DEC_HL, LD_C_xHL, DEC_HL, LD_B_xHL); return; }
			goto ill_source;

		case DE:
			// ld de,NN
			// ld de,(NN)
			// ld de,rr		bc hl ix iy		Goodie
			// ld de,(hl)					Goodie
			// ld de,(ix+d)					Goodie
			// ld de,(hl++)					Goodie
			if(r2==NN)  { store(LD_DE_NN); storeWord(n2); return; }
			if(r2==XNN) { storeEDopcode(LD_DE_xNN); storeWord(n2); return; }
			if(r2==BC)  { store(LD_D_B, LD_E_C); return; }
			if(r2==HL)  { store(LD_D_H, LD_E_L); return; }
			if(r2==IX)  { store(PFX_IX, LD_D_H, PFX_IX, LD_E_L); return; }
			if(r2==IY)  { store(PFX_IY, LD_D_H, PFX_IY, LD_E_L); return; }
			if(r2==XHL)	{ store(LD_E_xHL, INC_HL, LD_D_xHL, DEC_HL); return; }
			if(r2==XIX)	{ store(PFX_IX, LD_E_xHL); storeOffset(n2,v2); store(PFX_IX, LD_D_xHL); storeOffset(n2+1,v2); return; }
			if(r2==XIY)	{ store(PFX_IY, LD_E_xHL); storeOffset(n2,v2); store(PFX_IY, LD_D_xHL); storeOffset(n2+1,v2); return; }
			if(r2==XHLPP) { store(LD_E_xHL,INC_HL,LD_D_xHL,INC_HL); return; }
			if(r2==XMMHL) { store(DEC_HL, LD_E_xHL, DEC_HL, LD_D_xHL); return; }
			goto ill_source;

		case SP:
			// ld sp,rr		hl ix iy
			// ld sp,NN
			// ld sp,(NN)
			if(r2==HL) { store(LD_SP_HL); return; }
			if(r2==IX) { store(PFX_IX, LD_SP_HL); return; }
			if(r2==IY) { store(PFX_IY, LD_SP_HL); return; }
			if(r2==NN) { store(LD_SP_NN);  storeWord(n2); return; }
			if(r2==XNN){ storeEDopcode(LD_SP_xNN); storeWord(n2); return; }
			goto ill_source;

		case XIX:
			// ld (ix+d),r		a b c d e h l a
			// ld (ix+d),n
			// ld (ix+d),rr		bc de hl		Goodie
			instr = PFX_IX;
			goto ld_xiy;

		case XIY:
			// ld (iy+d),r		a b c d e h l a
			// ld (iy+d),n
			// ld (iy+d),rr		bc de hl		Goodie
			instr = PFX_IY;
ld_xiy:		if(r2<=RA && r2!=XHL) { store(instr,LD_xHL_B+r2-RB); storeOffset(n,v); return; }
			if(r2==NN) { store(instr,LD_xHL_N); storeOffset(n,v); storeByte(n2); return; }
			if(r2==HL) { store(instr,LD_xHL_L); storeOffset(n,v); store(instr, LD_xHL_H); storeOffset(n+1,v); return; }
			if(r2==DE) { store(instr,LD_xHL_E); storeOffset(n,v); store(instr, LD_xHL_D); storeOffset(n+1,v); return; }
			if(r2==BC) { store(instr,LD_xHL_C); storeOffset(n,v); store(instr, LD_xHL_B); storeOffset(n+1,v); return; }
			goto ill_source;

		case XHL:
			// ld (hl),r		a b c d e h l a
			// ld (hl),n
			// ld (hl),rr		bc de			Goodie
			if(r2<=RA && r2!=XHL) { store(LD_xHL_B+r2-RB); return; }
			if(r2==NN) { store(LD_xHL_N); storeByte(n2); return; }
			if(r2==BC) { store(LD_xHL_C, INC_HL, LD_xHL_B, DEC_HL); return; }
			if(r2==DE) { store(LD_xHL_E, INC_HL, LD_xHL_D, DEC_HL); return; }
			goto ill_source;

		case XNN:
			// ld (NN),a
			// ld (NN),hl	hl ix iy
			// ld (NN),rr	bc de sp
			if(r2==RA) { store(		   LD_xNN_A ); storeWord(n); return; }
			if(r2==HL) { store(		   LD_xNN_HL); storeWord(n); return; }
			if(r2==IX) { store(PFX_IX, LD_xNN_HL); storeWord(n); return; }
			if(r2==IY) { store(PFX_IY, LD_xNN_HL); storeWord(n); return; }
			if(r2==BC) { storeEDopcode(LD_xNN_BC); storeWord(n); return; }
			if(r2==DE) { storeEDopcode(LD_xNN_DE); storeWord(n); return; }
			if(r2==SP) { storeEDopcode(LD_xNN_SP); storeWord(n); return; }
			goto ill_source;

		case XBC:
			// ld (bc),a
			if(r2==RA) { store(LD_xBC_A); return; }
			goto ill_source;

		case XDE:
			// ld (de),a
			if(r2==RA) { store(LD_xDE_A); return; }
			goto ill_source;

		case XMMBC:
			// ld (--bc),a
			if(r2==RA) { store(DEC_BC, LD_xBC_A); return; }
			goto ill_source;

		case XMMDE:
			// ld (--de),a
			if(r2==RA) { store(DEC_DE, LD_xDE_A); return; }
			goto ill_source;

		case XMMHL:
			// ld (--hl),r
			// ld (--hl),rr
			if(r2<=RA && r2!=XHL) { store(DEC_HL, LD_xHL_B + (r2-RB)); return; }
			if(r2==BC) { store(DEC_HL,LD_xHL_B,DEC_HL,LD_xHL_C); return; }
			if(r2==DE) { store(DEC_HL,LD_xHL_D,DEC_HL,LD_xHL_E); return; }
			goto ill_source;

		case XBCPP:
			// ld (bc++),a
			if(r2==RA) { store(LD_xBC_A, INC_BC); return; }
			goto ill_source;

		case XDEPP:
			// ld (de++),a
			if(r2==RA) { store(LD_xDE_A, INC_DE); return; }
			goto ill_source;

		case XHLPP:
			// ld (hl++),r
			// ld (hl++),rr
			if(r2<=RA && r2!=XHL) { store(LD_xHL_B + (r2-RB), INC_HL); return; }
			if(r2==BC) { store(LD_xHL_B,INC_HL,LD_xHL_C,INC_HL); return; }
			if(r2==DE) { store(LD_xHL_D,INC_HL,LD_xHL_E,INC_HL); return; }
			goto ill_source;

		case XH:
		case XL:
			// ld xh,r		a b c d e xh xl N
			// ld xl,r		a b c d e xh xl N
			store(PFX_IX);
			r += RH-XH;
			if(r2<=RE || r2==RA || r2==NN) goto ld_r;
			if(r2==XH || r2==XL) { r2 += RH-XH; goto ld_r; }
			goto ill_source;

		case YH:
		case YL:
			// ld yh,r		a b c d e yh yl N
			// ld yl,r		a b c d e yh yl N
			store(PFX_IY);
			r += RH-YH;
			if(r2<=RE || r2==RA || r2==NN) goto ld_r;
			if(r2==YH || r2==YL) { r2 += RH-YH; goto ld_r; }
			goto ill_source;

		case RA:
			// ld a,i
			// ld i,a
			// ld a,(rr)	bc de
			// ld a,(NN)
			if(r2==RI)  { storeEDopcode(LD_A_I); return; }
			if(r2==RR)  { storeEDopcode(LD_A_R); return; }
			if(r2==XBC) { store(LD_A_xBC); return; }
			if(r2==XDE) { store(LD_A_xDE); return; }
			if(r2==XBCPP) { store(LD_A_xBC, INC_BC); return; }
			if(r2==XDEPP) { store(LD_A_xDE, INC_DE); return; }
			if(r2==XMMBC) { store(DEC_BC, LD_A_xBC); return; }
			if(r2==XMMDE) { store(DEC_DE, LD_A_xDE); return; }
			if(r2==XNN) { store(LD_A_xNN); storeWord(n2); return; }
			goto ld_r;

		case RH:
		case RL:
			if(r2>=XH && r2<=YL) goto ill_source;

		case RB:
		case RC:
		case RD:
		case RE:
			// ld r,r		a b c d e h l (hl)
			// ld r,(ix+d)
			// ld r,N
			// ld r,xh		a b c d e xh xl
			// ld r,yh		a b c d e yh yl
ld_r:		XXXASSERT(r<=RA && r!=XHL);
			if(r2<=RA)  { store(LD_B_B + (r-RB)*8 + (r2-RB)); return; }
			if(r2==XIX) { store(PFX_IX, LD_B_xHL+(r-RB)*8); storeOffset(n2,v2); return; }
			if(r2==XIY) { store(PFX_IY, LD_B_xHL+(r-RB)*8); storeOffset(n2,v2); return; }
			if(r2==NN)  { store(LD_B_N+(r-RB)*8); storeByte(n2); return; }
			if(r2==XH||r2==XL) { store(PFX_IX,LD_B_H+(r2-XH)+(r-RB)*8); return; }
			if(r2==YH||r2==YL) { store(PFX_IY,LD_B_H+(r2-YH)+(r-RB)*8); return; }
			if(r2==XHLPP)	   { store(LD_B_xHL + (r-RB)*8, INC_HL); return; }
			if(r2==XMMHL)	   { store(DEC_HL, LD_B_xHL + (r-RB)*8); return; }
			goto ill_source;

		case NN:
			goto ill_dest;

		default:
			//IERR();
			goto ill_dest;
		}

	case ' mov':	throw syntax_error("if this is 8080 assembler source then use option --asm8080");
	case ' scf':	store(SCF); return;
	case ' ccf':	store(CCF); return;
	case ' cpl':	store(CPL); return;
	case ' daa':	store(DAA); return;
	case ' rra':	store(RRA); return;
	case ' rla':	store(RLA); return;
	case ' nop':	store(NOP); return;
	case ' exx':	if(target_8080) goto ill_8080; store(EXX); return;
	case ' neg':	storeEDopcode(NEG); return;
	case ' rrd':	storeEDopcode(RRD); return;
	case ' rld':	storeEDopcode(RLD); return;
	case ' ldi':	storeEDopcode(LDI); return;
	case ' cpi':	storeEDopcode(CPI); return;
	case ' ini':	storeEDopcode(INI); return;
	case ' ldd':	storeEDopcode(LDD); return;
	case ' cpd':	storeEDopcode(CPD); return;
	case ' ind':	storeEDopcode(IND); return;
	case ' and':	instr = AND_B; goto cp;
	case ' xor':	instr = XOR_B; goto cp;
	case ' sub':	instr = SUB_B; goto cp;
	case ' rlc':	instr = RLC_B; goto rr;
	case ' rrc':	instr = RRC_B; goto rr;
	case ' sla':	instr = SLA_B; goto rr;
	case ' sra':	instr = SRA_B; goto rr;
	case ' sls':	// alias for sll found in some docs
	case ' sll':	if(target_z180) throw syntax_error("illegal instruction: the Z180 traps illegal instructions");
					instr = SLL_B; goto rr;
	case ' srl':	instr = SRL_B; goto rr;

	case ' slp':
		if(!target_z180) goto ill_z180;
		storeEDopcode(0x76); return;

	case ' in0':
		// in0 r,(n)		a b c d e h l f
		if(!target_z180) goto ill_z180;
		if(q.testWord("f")) r = XHL;
		else { r = getRegister(q,n,v); if(r>RA||r==XHL) goto ill_dest; }
		q.expectComma();
		r2 = getRegister(q,n2,v2);
		if(r2==XNN) { storeEDopcode(0x00+8*(r-RB)); storeByte(n2); return; }
		goto ill_source;

	case ' tst':
		// tst r		b c d e h l (hl) a
		// tst n
		if(!target_z180) goto ill_z180;
		r = getRegister(q,n,v);
		if(r==NN) { storeEDopcode(0x64); storeByte(n); return; }
		if(r<=RA) { storeEDopcode(0x04+8*(r-RB)); return; }
		goto ill_source;

//	case ' end':
//		asmEnd(q);
//		return;

//	case ' org':
//		// org <value>	; add space up to address
//org:	n = value(q, pAny, v=1);
//		current_segment().storeSpaceUpToAddress(n,v);
//		return;

	case ' rst':
		// rst n		0 .. 7  or  0*8 .. 7*8
		n = value(q, pAny, v=1);
		if(n%8==0) n>>=3;
		if((n>>3) && v) throw syntax_error( "illegal vector" );
		store(RST00+n*8);
		return;

	case ' add':
		//	add	a,xxx
		//	add hl,rr	bc de hl sp
		//	add ix,rr	bc de ix sp
		r = getRegister(q,n,v); if(r==RA || q.testEol()) { instr = ADD_B; goto cp_a; }
		depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);

		if(r==IX) { if(r2==HL) goto ill_source; store(PFX_IX); if(r2==IX) r2=HL; r=HL; }
		if(r==IY) { if(r2==HL) goto ill_source; store(PFX_IY); if(r2==IY) r2=HL; r=HL; }
		if(r==HL) { if(r2<BC || r2>SP) goto ill_source; store(ADD_HL_BC+(r2-BC)*16); return; }
		goto ill_target;

	case ' sbc':
		//	sbc	a,xxx
		//	sbc hl,rr	bc de hl sp
		r = getRegister(q,n,v); if(r==RA || q.testEol()) { instr = SBC_B; goto cp_a; }
		instr = SBC_HL_BC;
		goto adc;

	case ' adc':
		//	adc	a,xxx
		//	adc hl,rr	bc de hl sp
		r = getRegister(q,n,v); if(r==RA || q.testEol()) { instr = ADC_B; goto cp_a; }
		instr = ADC_HL_BC;
adc:	depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);

		if(r==HL) { if(r2<BC || r2>SP) goto ill_source; storeEDopcode(instr+(r2-BC)*16); return; }
		goto ill_target;

	case ' res':
		instr = RES0_B;
		goto bit;

	case ' set':			// note: M80 pseudo instruction SET is handled by asmLabel()
		instr = SET0_B;
		goto bit;

	case ' bit':
		instr = BIT0_B;
bit:	n = value(q,pAny,v=1);
		if((uint)n>7 && v) throw syntax_error("illegal bit number");
		instr += 8*n;
		q.expectComma();
		goto rr;

	case ' dec':
		n2 = 1;
		goto inc;

	case ' inc':
		// inc r	a b c d e h l (hl) (ix+d)
		// inc xh
		// inc rr	bc de hl sp ix iy
		n2 = 0;
inc:	r = getRegister(q,n,v);
		instr = INC_xHL + n2;	// inc (hl)  or  dec (hl)
		if(r<=RA) { store(        instr+(r-XHL)*8); return; }
		if(r==XIX){ store(PFX_IX, instr); storeOffset(n,v); return; }
		if(r==XIY){ store(PFX_IY, instr); storeOffset(n,v); return; }
		if(r<=XL) { store(PFX_IX, instr+(r+RH-XH-XHL)*8); return; }
		if(r<=YL) { store(PFX_IY, instr+(r+RH-YH-XHL)*8); return; }
		instr = INC_HL + n2*8;	// inc hl or dec hl
		if(r>=BC && r<=SP) { store(instr+(r-HL)*16); return; }
		if(r==IX) { store(PFX_IX, instr); return; }
		if(r==IY) { store(PFX_IY, instr); return; }
		goto ill_target;

	case ' out':
		// out (c),r	a b c d e h l
		// out (c),0	*** NOT ON ALL SYSTEMS / NOT FOR ALL Z80 CPUs! (NMOS vs. CMOS?) ***
		// out (bc),r	--> out (c),r
		// out (bc),0	--> out (c),0
		// out (n),a	--> outa n
		// out n,a      --> outa n		(seen in sources)

		r = getRegister(q,n,v);
		depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);

		if(r==XC || r==XBC)
		{
			if(r2<=RA && r2!=XHL) { storeEDopcode(OUT_xC_B+(r2-RB)*8); return; }
			if(r2==NN) { if(v2&&n2!=0) goto ill_source; storeEDopcode(OUT_xC_0); return; }
			goto ill_source;
		}
		if(r==XNN || r==NN)
		{
			if(r2!=RA) goto ill_source;
			if(v && (n<-128||n>255)) q.p=depp;	// storeByte() will throw
			store(OUTA); storeByte(n); return;
		}
		goto ill_dest;

	case ' ret':
		// ret
		// ret cc
		r = getCondition(q,no);
		store(r==NIX ? RET : RET_NZ+(r-NZ)*8);
		return;

	case ' pop':
		// pop rr		bc de hl af ix iy
		instr = POP_HL;

pop:	r = getRegister(q,n,v);
		if(r>=BC && r<=HL) { store(instr+(r-HL)*16); return; }
		if(r==AF) { store(instr+16); return; }
		if(r==IX) { store(PFX_IX,instr); return; }
		if(r==IY) { store(PFX_IY,instr); return; }
		if(instr==POP_HL) goto ill_target; else goto ill_source;


	case 'rlca':	store(RLCA); return;
	case 'rrca':	store(RRCA); return;
	case 'halt':	store(HALT); return;
	case 'outi':	storeEDopcode(OUTI); return;
	case 'outd':	storeEDopcode(OUTD); return;
	case 'ldir':	storeEDopcode(LDIR); return;
	case 'cpir':	storeEDopcode(CPIR); return;
	case 'inir':	storeEDopcode(INIR); return;
	case 'otir':	storeEDopcode(OTIR); return;
	case 'lddr':	storeEDopcode(LDDR); return;
	case 'cpdr':	storeEDopcode(CPDR); return;
	case 'indr':	storeEDopcode(INDR); return;
	case 'otdr':	storeEDopcode(OTDR); return;
	case 'reti':	storeEDopcode(RETI); return;
	case 'retn':	storeEDopcode(RETN); return;
	case 'push':	instr = PUSH_HL; goto pop;

	case 'call':
		// call nn
		// call cc,nn
		r2 = getCondition(q,yes);
		r  = getRegister(q,n,v);
		if(r!=NN) goto ill_dest;
		store(r2==NIX ? CALL : CALL_NZ+(r2-NZ)*8);
		storeWord(n);
		return;

	case 'djnz':
		// djnz nn
		if(target_8080) goto ill_8080;
		r = getRegister(q,n,v);		// vor store opcode DJNZ wg. Bezug eines evtl. genutzen $
		if(r!=NN) goto ill_dest;
		store(DJNZ);
		storeOffset( n - (currentAddress()+1), v && currentAddressValid() );
		return;

	case 'mult':
		// mult rr		bc de hl sp
		if(!target_z180) goto ill_z180;
		r = getRegister(q,n,v);
		if(r>=BC && r<=SP) { store(PFX_ED,0x4c+16*(r-BC)); return; }
		goto ill_source;

	case 'otim':
		if(!target_z180) goto ill_z180;
		store(PFX_ED,0x83); return;

	case 'otdm':
		if(!target_z180) goto ill_z180;
		store(PFX_ED,0x8b); return;

	case 'out0':
		// out0 (n),r		b c d e h l a
		if(!target_z180) goto ill_z180;
		r = getRegister(q,n,v); if(r!=XNN) goto ill_dest;
		depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);
		if(r2<=RA && r2!=XHL)
		{
			if(v && (n<-128||n>255)) q.p=depp;	// storeByte() will throw
			store(PFX_ED, 0x01+8*(r2-RB)); storeByte(n); return;
		}
		goto ill_source;

	case '.org':
	case ' org':
		// org <value>	; add space up to address
		n = value(q, pAny, v=1);
		current_segment().storeSpaceUpToAddress(n,v);
		return;

	case 'data':
		if(current_segment_ptr->isData()) goto ds;
		else throw syntax_error("only allowed in data segments (use defs)");

	case '  ds':
	case ' .ds':
	case 'defs':
		// store space: (gap)
		// defs cnt
		// defs cnt, fillbyte

ds:		q.is_data = yes;
		n = value(q,pAny,v=1);
		if(q.testComma()) storeSpace(n,v,value(q,pAny,v2=1)); else storeSpace(n,v);
		return;

	case '  dw':
	case ' .dw':
	case 'defw':
		// store words:
		// defw nn [,nn ..]
dw:		q.is_data = yes;
		do { storeWord(value(q,pAny,v=1)); } while(q.testComma());
		return;

	case '  db':
	case ' .db':	// SDASZ80: truncates value to byte (not implemented, done if used this way by SDCC)
	case 'defb':
	case '  dm':
	case ' .dm':
	case 'defm':
		// store bytes:
		// due to wide use of DB for strings DB and DM are handled the same
		// => 'xy' and "xy" are both understood as  "string"!
		// erlaubt jede Mixtur von literal, label, "text", 'c' Char, $abcdef stuffed hex, usw.
		// ACHTUNG: '…' wird als String behandelt! Das wird z.B. im Source  des ZXSP-Roms so verwendet.
		// defb expression, "…", "…"+n, '…', '…'+n, 0xABCDEF…, __date__, __time__, __file__, …
db:dm:	q.is_data = yes;
		w = q.nextWord();
		if(w[0]==0) throw syntax_error("value expected");

	// Text string:
		if(w[0]=='"' || w[0]=='\'')
		{
			n = strlen(w);
			if(n<3 || w[n-1]!=w[0]) throw syntax_error("closing quotes expected");
			w = unquotedstr(w);
			if(*w==0) throw syntax_error("closing quotes expected");	// broken '\' etc.

			depp = w;
			charcode_from_utf8(depp);	// skip over 1 char; throws on ill. utf8

			if(*depp==0)				// single char => numeric expression
			{
				q -= n;
				storeByte(value(q,pAny,v=1));
			}
			else						// multi-char string
			{
cb:				if(charset) while(*w) store(charset->get(charcode_from_utf8(w)));
				else		while(*w) store(charcode_from_utf8(w));

				// test for operation on the final char:
				if(q.testChar ('+'))	{ n=value(q,pAny,v=1); if(v) storeByte(popLastByte() + n); } else
				if(q.test_char('-'))	{ n=value(q,pAny,v=1); if(v) storeByte(popLastByte() - n); } else
				if(q.test_char('|'))	{ n=value(q,pAny,v=1); if(v) storeByte(popLastByte() | n); } else
				if(q.test_char('&'))	{ n=value(q,pAny,v=1); if(v) storeByte(popLastByte() & n); } else
				if(q.test_char('^'))	{ n=value(q,pAny,v=1); if(v) storeByte(popLastByte() ^ n); }
			}
			if(q.testComma()) goto dm; else return;
		}

	// Stuffed Hex:
	// bytes are stored in order of occurance: in $ABCD byte $AB is stored first!
		n = strlen(w);
		if(n>3 && w[0]=='$')
		{
sx:			w = midstr(w,1); n-=1;
sh:			if(n&1) throw syntax_error("even number of hex characters expected");
			storeHexbytes(w,n/2);
			if(q.testComma()) goto dm; else return;
		}

		if(n>4 && is_dec_digit(w[0]) && tolower(w[n-1])=='h')
		{
			w = leftstr(w,n-1); n-=1;
			if(n&1 && w[0]=='0') goto sx; else goto sh;
		}

	// pre-defined special words:
		if(w[0]=='_')
		{
			if(eq(w,"__date__")) { w = datestr(timestamp); w += *w==' ';  goto cb; }
			if(eq(w,"__time__")) { w = timestr(timestamp); w += *w==' ';  goto cb; }
			if(eq(w,"__file__")) { w = q.sourcefile; goto cb; }
			if(eq(w,"__line__")) { w = numstr(q.sourcelinenumber); goto cb; }
		}

	// anything else:
		q -= strlen(w);	// put back opcode
		n = value(q,pAny,v=1); storeByte(n);
		if(q.testComma()) goto dm; else return;
	}

wlenXL:
	w = lowerstr(w);
	if(eq(w,"otimr"))
	{
		if(!target_z180) goto ill_z180;
		store(PFX_ED,0x93); return;
	}
	else if(eq(w,"otdmr"))
	{
		if(!target_z180) goto ill_z180;
		store(PFX_ED,0x9b); return;
	}
	else if(eq(w,"tstio"))
	{
		// tstio n
		if(!target_z180) goto ill_z180;
		r = getRegister(q,n,v);
		if(r==NN) { store(PFX_ED,0x74); storeByte(n); return; }
		goto ill_source;
	}

misc:
	if(doteq(w,"align"))	// align <value> [,<filler>]
	{							// note: current address is evaluated as uint
		q.is_data = yes;
		n = value(q,pAny,v=1);
		if(v&&n<1) throw syntax_error("alignment value must be ≥ 1");
		if(v&&n>0x4000) throw syntax_error("alignment value must be ≤ $4000");

		int32 a = current_segment_ptr->logicalAddress();
		v = v && current_segment_ptr->logicalAddressValid();
		if(v && a<0 && (1<<(msbit(n)))!=n) throw syntax_error("alignment value must be 2^N if $ < 0");

		n = n-1 - ((uint16)a+n-1) % n;

		if(q.testComma()) { bool u=1; storeSpace(n,v,value(q,pAny,u)); } else storeSpace(n,v);
		return;
	}
	if(eq(w,".asciz"))			// store 0-terminated string:
	{
		if(charset && charset->get(' ',' ')==0)	// ZX80/81: the only conversion i know where 0x00 is a printable char
			throw syntax_error("this won't work because in the target charset 0x00 is a printable char");

		q.is_data = yes;
		w = q.nextWord();
		if(w[0]!='"' && w[0]!='\'') throw syntax_error("quoted string expected");

		n = strlen(w);
		if(n<3 || w[n-1]!=w[0]) throw syntax_error("closing quotes expected");
		w = unquotedstr(w);
		if(*w==0) throw syntax_error("closing quotes expected");	// broken '\' etc.

		depp = w;
		charcode_from_utf8(depp);	// skip over 1 char; throws on ill. utf8

		if(charset) while(*w) store(charset->get(charcode_from_utf8(w)));
		else		while(*w) store(charcode_from_utf8(w));
		store(0);
		return;
	}
	if(eq(w,".globl"))				// declare global label for linker: mark label for #include library "libdir"
	{								// das Label wird in mehrere Labels[] eingehängt! => special d'tor!
		w = q.nextWord();
		if(!is_letter(*w) && *w!='_') throw syntax_error("label name expected");

		if(local_labels_index)		// local context?
		{
			Label* g = &global_labels().find(w);
			Label* l = &local_labels().find(w);
			if(l && !l->is_global) throw syntax_error("label already defined local");
			XXXASSERT(!g||!l||g==l);

			Label* label = l ? l : g ? g : new Label(w,NULL,current_sourceline_index,0,no,yes,no,no);
			if(!l) local_labels().add(label);
			if(!g) global_labels().add(label);
		}
		else						// global context
		{
			Label* g = &global_labels().find(w);
			Label* label = g ? g : new Label(w,NULL,current_sourceline_index,0,no,yes,no,no);
			if(!g) global_labels().add(label);
		}
		return;
	}
	if(eq(w,".byte"))	goto db;	// TASM
	if(eq(w,".word"))	goto dw;	// TASM
	if(eq(w,".ascii"))	goto dm;
	if(eq(w,".text"))	goto dm;	// TASM
	if(eq(w,".block"))	goto ds;	// TASM

	if(eq(w,".long"))
	{
		// store long words:
		// .long nn [,nn ..]
		q.is_data = yes;
		do { n = value(q,pAny,v=1); storeWord(n); storeWord(n>>16); } while(q.testComma());
		return;
	}

//	if(eq(w,"float5"))	// floating point number in ZX Spectrum format
//	{
//		//	0 .. 65535:		00, 00, LO, HI, 00
//		//	-65535 .. -1:	00, FF, LO, HI, 00	with LOHI = 0 - N
//		//	other:			EE, HI, .., .., LO
//		//					EE=80 => 0.5 ≤ N < 1
//		//					HI.bit7 = VZ
//		//					range: ±1e38 .. 4e-39
//		//
//		// TODO: we need float value() here…
//	}

//	if(eq(w,"float4"))	// floating point number in sdcc format
//	{
//	}


// instructions below are also valid without #target / org:

valid_without_segment:

	if(eq(w,".area"))			// .area NAME  or  .area NAME (ABS)    => (ABS) is ignored
	{
		// select segment for following code
		// hinter valid_without_segment verschoben, um eine eigene Fehlermeldung auszugeben

		w = upperstr(q.nextWord());	// name
		if(!is_letter(*w) && *w!='_'  && !(allow_dotnames&&*w=='.')) throw fatal_error("segment name expected");
		Segment* segment = segments.find(w);
		if(!segment) throw fatal_error(current_segment_ptr?"segment not found":"no #code or #data segment defined");

		current_segment_ptr = segment;
		q.segment = current_segment_ptr;
		q.byteptr = currentPosition();
		XXXASSERT(q.bytecount==0);

//		if((eq(w,"_CABS")||eq(w,"_DABS")||eq(w,"_RSEG"))	// SDCC generates: " .area _CABS (ABS)"
//			&& q.testChar('('))								// KCC  generates: " .area _RSEG (ABS)"
		if(q.testChar('('))									// SDCC generates: " .area _CABS (ABS)"
															// KCC  generates: " .area _RSEG (ABS)"
															// pacalpaca.asm:  " .area .CODE (ABS)"  => any name
		{
			if(!q.testWord("ABS")) throw syntax_error("'ABS' expected");
			q.expect(')');
		}
		return;
	}

	if(doteq(w,"macro"))		// define macro:	".macro NAME ARG"		"binutils style macros"
	{								//					"	instr \ARG"			seen in: OpenSE
		w = q.nextWord();
		if(!is_name(w)) throw syntax_error("name expected");
		asmMacro(q,w,'\\');
		return;
	}

	if(eq(w,".module"))			// for listing
	{
		q.skip_to_eol();
		return;
	}

	if(eq(w,".optsdcc"))		// .optsdcc -mz80
	{
		if(!q.testChar('-') )		throw syntax_error("-mz80 expected");
		if(ne(q.nextWord(),"mz80"))	throw syntax_error("-mz80 expected");
		return;
	}

	if(eq(w,".phase"))			// M80: set logical code position
	{
		n = value(q,pAny,v=1);
		current_segment().setOrigin(n,v);
		return;
	}

	if(eq(w,".dephase"))		// M80: restore logical code position to real address
	{
		current_segment().setOrigin(current_segment().physicalAddress(),current_segment().physicalAddressValid());
		return;
	}

	if( doteq(w,"title") || doteq(w,"list") || eq(w,".xlist") || eq(w,".nolist") ||
		eq(w,"subttl") || eq(w,".sdsctag") || doteq(w,"section") )
	{
ignore:	q.skip_to_eol();		// ignore
		if(pass>1 || verbose<=1) return;
		goto warn;
	}

	if(eq(w,".z80"))		// does not unset the Z180 option
	{
		if(target_z80) return;
		if(current_segment_ptr) throw fatal_error("this statement must occur before ORG, #CODE or #DATA");
		throw fatal_error("either this statement or your command line option --8080 is wrong");
		return;
	}

	if(eq(w,".z180"))
	{
		if(target_z180) return;
		if(current_segment_ptr) throw fatal_error("this statement must occur before ORG, #CODE or #DATA");
		if(!target_z80) throw fatal_error("either this statement or your command line option --8080 is wrong");
		target_z180 = yes;
		if(ixcbr2_enabled) throw fatal_error("incompatible option --ixcbr2 is set: the Z180 traps all illegals");
		if(ixcbxh_enabled) throw fatal_error("incompatible option --ixcbxh is set: the Z180 traps all illegals");
		global_labels().add(new Label("z180",NULL,current_sourceline_index,1,yes,yes,yes,no));
		return;
	}

	if(eq(w,".8080"))
	{
		syntax_8080 = true;
		casefold_labels = true;
		if(target_8080) return;
		if(current_segment_ptr) throw fatal_error("this statement must occur before ORG, #CODE or #DATA");
		target_8080 = true;
		ixcbr2_enabled = ixcbxh_enabled = no;
		target_z80 = target_z180 = no;
		global_labels().add(new Label("i8080",NULL,current_sourceline_index,1,yes,yes,yes,no));
		global_labels().add(new Label("asm8080",NULL,current_sourceline_index,1,yes,yes,yes,no));
		return;
	}

	if(eq(w,".memorymap"))		// skip up to ".endme" and print warning
	{							// TODO: testen, ob das Verstellen der aktuellen Zeile Probleme bereitet
		uint n = current_sourceline_index;
		uint e = min(n+20u,source.count());
		while(++n<e)
		{
			SourceLine& z = source[n]; z.rewind();
			if(z.testWord(".endme")) { z.skip_to_eol(); current_sourceline_index=n; goto warn; }
		}
		throw syntax_error("'.endme' missing");
	}
	if(eq(w,".rombankmap"))		// skip up to ".endro" and print warning
	{
		uint n = current_sourceline_index;
		uint e = min(n+20u,source.count());
		while(++n<e)
		{
			SourceLine& z = source[n]; z.rewind();
			if(z.testWord(".endro")) { z.skip_to_eol(); current_sourceline_index=n; goto warn; }
		}
		throw syntax_error("'.endro' missing");
	}

	if(eq(w,".bank") || eq(w,".section") || eq(w,"globals") || eq(w,"aseg"))
	{
warn:	q.skip_to_eol();		// print warning & ignore
		if(pass>1 || verbose==0) return;
		cstr linenumber = numstr(q.sourcelinenumber+1);
		fprintf(stderr, "%s: %s\n", linenumber, q.text);
		fprintf(stderr, "%s%s^ warning: instruction '%s' ignored\n", spacestr(strlen(linenumber)+2), q.whitestr(), w);
		return;
	}

	if(doteq(w,"rept")) { asmRept(q); return; }
	if(doteq(w,"if"))	{ asmIf(q); return; }
	if(doteq(w,"endif")){ asmEndif(q); return; }
	if(doteq(w,"end"))	{ asmEnd(q); return; }
	if(eq(w,"include")) { asmInclude(q); return; }
	if(eq(w,"incbin"))	{ asmInsert(q); return; }
	if(doteq(w,"endm")) throw syntax_error("no rept or macro definition pending");
	if(eq(w,".endme"))  throw syntax_error("'.endme' without '.memorymap'");
	if(eq(w,".endro"))  throw syntax_error("'.endro' without '.rombankmap'");

	if(eq(w,"*"))
	{
		if(q.testWord("list")) goto ignore;	// "*LIST ON"  or  "*LIST OFF"
		if(q.testWord("include")) { asmInclude(q); return; }	// CAMEL80: fname follows without '"'
	}															// this must be fixed by the user

	else	// try macro expansion:
	{
		Macro& m = macros.get(w);
		if(&m) { asmMacroCall(q,m); return; }
		else goto ill_opcode;
	}

// generate error
ill_opcode:		if(!is_letter(*w)&&*w!='_'&&*w!='.') throw syntax_error("instruction expected"); // no identifier
				if(!current_segment_ptr) throw fatal_error("org not yet set");
				else throw syntax_error("unknown instruction");
ill_target:		if(depp) q.p=depp; throw syntax_error("illegal target");		// 1st arg
ill_source:		throw syntax_error("illegal source");							// 2nd arg
ill_dest:		if(depp) q.p=depp; throw syntax_error("illegal destination");	// jp etc., ld, in, out: destination
ill_z180:		throw syntax_error("z180 opcode (use option --z180)");
ill_8080:		throw syntax_error("no 8080 opcode (option --8080)");
}


/*	test and skip over register or value
	returns register offset 0…7: b,c,d,e,h,l,m,a
	throws on error
	throws at end of line
*/
uint Z80Assembler::get8080Register(SourceLine& q) throw(syntax_error)
{
	cstr w = q.nextWord();
	char c = *w; if(c==0) throw syntax_error("unexpected end of line");

	if(*++w==0)	// strlen=1
	{
		switch(c|0x20)
		{
		case 'b':	return 0;
		case 'c':	return 1;
		case 'd':	return 2;
		case 'e':	return 3;
		case 'h':	return 4;
		case 'l':	return 5;
		case 'm':	return 6; // XHL
		case 'a':	return 7;
		}
	}
	throw syntax_error("register A to L or memory M expected");
}

enum { BD, BDHSP,BDHAF };

uint Z80Assembler::get8080WordRegister(SourceLine& q, uint what) throw(syntax_error)
{
	cstr w = q.nextWord();

	char c1 = *w++ | 0x20;	if(c1==0) throw syntax_error("unexpected end of line");
	char c2 = *w++ | 0x20;

	if(c2==0x20)	// strlen=1
	{
		if(c1=='b') return 0;				// BC
		if(c1=='d') return 16;				// DE
		if(c1=='h' && what>BD) return 32;	// HL
	}
	else if(what==BDHSP && c1=='s' && c2=='p' && *w==0) return 48;
	else if(what==BDHAF && c1=='p' && c2=='s' && (*w++|0x20)=='w' && *w==0) return 48;

	throw syntax_error( usingstr("word register %s expected",
						what==0?"B or D":what==1?"B, D, H or SP":"B, D, H or PSW") );
}


/*	assemble opcode
*/
void Z80Assembler::asmInstr8080(SourceLine& q) throw(any_error)
{
	int32 n;
	bool  v;
	uint  instr;
	cstr  w;

	if(current_segment_ptr)
	{
		w = q.nextWord();

		switch(strlen(w))
		{
		case 0:		return;			// end of line
		case 2:		n = peek2X(w); break;
		case 3:		n = peek3X(w); break;
		case 4:		n = peek4X(w); break;
		default:	w = lowerstr(w);
					if(eq(w,"endif")) { asmEndif(q); return; }
					if(eq(w,".8080")) { return; }
		//			if(doteq(w,"macro")) {}			detected by asmLabel()
					goto unknown_opcode;				// error
		}
	}
	else  // ORG not yet set:
	{	 // da noch kein Code erzeugt werden kann, ist der Unterschied zw. Z80 und 8080 irrelevant
		// --> Instructions sind entweder für beide möglich, unmöglich oder werden ignoriert
		asmInstr(q); return;
	}

// opcode len = 2, 3, or 4:

	switch(n|0x20202020)
	{
	case '  rz': store(RET_Z);	  return;	// 8080: rz => ret z
	case '  rc': store(RET_C);	  return;	// 8080: rc => ret c
	case '  rp': store(RET_P);	  return;	// 8080: rp => ret p
	case '  rm': store(RET_M);	  return;	// 8080: rm => ret m
	case ' ret': store(RET);	  return; 	// 8080: ret => ret  ; no cc
	case ' rnz': store(RET_NZ);   return;	// 8080: rnz => ret nz
	case ' rnc': store(RET_NC);   return;	// 8080: rnc => ret nc
	case ' rpo': store(RET_PO);   return;	// 8080: rpo => ret po
	case ' rpe': store(RET_PE);   return;	// 8080: rpe => ret pe
	case ' stc': store(SCF);	  return;	// 8080: stc => scf
	case ' cmc': store(CCF);	  return;	// 8080: cmc => ccf
	case ' cma': store(CPL);	  return;	// 8080: cma => cpl
	case ' rar': store(RRA);	  return;	// 8080: rar => rra
	case ' ral': store(RLA);	  return;	// 8080: ral => rla
	case ' rlc': store(RLCA);	  return;	// 8080: rlc => rlca
	case ' rrc': store(RRCA);	  return;	// 8080: rrc => rrca
	case ' hlt': store(HALT);	  return;	// 8080: hlt => halt
	case 'pchl': store(JP_HL);	  return;	// 8080: pchl => jp (hl)
	case 'xthl': store(EX_HL_xSP); return;	// 8080: xthl => ex (sp),hl
	case 'sphl': store(LD_SP_HL); return;	// 8080: sphl => ld sp,hl
	case 'xchg': store(EX_DE_HL); return;	// 8080: xchg => ex de,hl
	case ' daa': store(DAA);	  return;	// same as z80
	case ' nop': store(NOP);	  return;	// same as z80
	case '  ei': store(EI); 	  return;	// same as z80
	case '  di': store(DI); 	  return;	// same as z80

	case 'call': instr = CALL;	  goto iw; 	// 8080: call NN => call NN  ; no cc
	case '  cz': instr = CALL_Z;  goto iw;	// 8080: cz NN => call z,NN
	case '  cc': instr = CALL_C;  goto iw;	// 8080: cc NN => call c,NN
	case '  cp': instr = CALL_P;  goto iw;  // 8080: cp NN => call p,NN
	case '  cm': instr = CALL_M;  goto iw;	// 8080: cm NN => call m,NN
	case ' cnz': instr = CALL_NZ; goto iw;	// 8080: cnz NN => call nz,NN
	case ' cnc': instr = CALL_NC; goto iw;	// 8080: cnc NN => call nc,NN
	case ' cpo': instr = CALL_PO; goto iw;	// 8080: cpo NN => call po,NN
	case ' cpe': instr = CALL_PE; goto iw;	// 8080: cpe NN => call pe,NN
	case '  jz': instr = JP_Z;    goto iw;	// 8080: jz NN => jp z,NN
	case '  jc': instr = JP_C;    goto iw;	// 8080: jc NN => jp c,NN
	case '  jm': instr = JP_M;    goto iw;	// 8080: jm NN => jp m,NN
	case '  jp': instr = JP_P;	  goto iw; 	// 8080: jp NN => jp p,NN
	case ' jnz': instr = JP_NZ;	  goto iw;	// 8080: jnz NN => jp nz,NN
	case ' jnc': instr = JP_NC;	  goto iw;	// 8080: jnc NN => jp nc,NN
	case ' jpo': instr = JP_PO;	  goto iw;	// 8080: jpo NN => jp po,NN
	case ' jpe': instr = JP_PE;	  goto iw;	// 8080: jpe NN => jp pe,NN
	case ' jmp': instr = JP;	  goto iw;	// 8080: jmp NN => jp NN

	case ' inr': instr = INC_B; goto dcr;	// 8080: inr r => inc r				b c d e h l m a
	case ' dcr': instr = DEC_B; goto dcr;	// 8080: dcr r => dec r				b c d e h l m a

dcr:	store(instr + get8080Register(q)*8); return;

	case ' mov':	// 8080: mov r,r => ld r,r			b c d e h l m a

		instr = LD_B_B + get8080Register(q)*8;
		q.expectComma();
		instr += get8080Register(q);
		if(instr!=HALT) { store(instr); return; }
		throw syntax_error("illegal source");

	case ' out': instr = OUTA;  goto ib;	// 8080: out N => out (N),a
	case '  in': instr = INA;   goto ib;	// 8080: in N  => in a,(N)
	case ' aci': instr = ADC_N; goto ib;	// 8080: aci N => adc a,N
	case ' adi': instr = ADD_N; goto ib;	// 8080: adi N => add a,N
	case ' sui': instr = SUB_N; goto ib;	// 8080: sui N => sub a,N
	case ' sbi': instr = SBC_N; goto ib;	// 8080: sbi N => sbc a,N
	case ' ani': instr = AND_N; goto ib;	// 8080: ani N => and a,N
	case ' ori': instr = OR_N;  goto ib;	// 8080: ori N => or a,N
	case ' xri': instr = XOR_N; goto ib;	// 8080: xri N => xor a,N
	case ' cpi': instr = CP_N;  goto ib; 	// 8080: cpi N => cp a,N

	case ' mvi':	// 8080: mvi r,N => ld r,N			b c d e h l m a

		instr = LD_B_N + get8080Register(q)*8;
		q.expectComma(); goto ib;
ib:		store(instr); storeByte(value(q,pAny,v=1)); return;

	case ' add': instr = ADD_B; goto cmp; 	// 8080: add r => add a,r  with r = a b c d e h m a
	case ' adc': instr = ADC_B; goto cmp; 	// 8080: adc r => adc a,r  with r = a b c d e h m a
	case ' sub': instr = SUB_B; goto cmp; 	// 8080: sub r => sub a,r
	case ' sbb': instr = SBC_B; goto cmp;	// 8080: sbb r => sbc a,r			b c d e h l m a
	case ' ana': instr = AND_B; goto cmp;	// 8080: ana r => and a,r			b c d e h l m a
	case ' ora': instr = OR_B;  goto cmp;	// 8080: ora r => or a,r			b c d e h l m a
	case ' xra': instr = XOR_B; goto cmp;	// 8080: xra r => xor a,r			b c d e h l m a
	case ' cmp': instr = CP_B;  goto cmp;	// 8080: cmp r => cp a,r			b c d e h l m a

cmp:	store(instr + get8080Register(q)); return;

	case 'lhld': instr = LD_HL_xNN; goto iw;	// 8080: lhld NN => ld hl,(NN)
	case ' lda': instr = LD_A_xNN;  goto iw; 	// 8080: lda NN  => ld a,(NN)
	case 'shld': instr = LD_xNN_HL; goto iw;	// 8080: shld NN => ld (NN),hl
	case ' sta': instr = LD_xNN_A;  goto iw;	// 8080: sta NN  => ld (NN),a

	case ' lxi':								// 8080: lxi r,NN => ld rr,NN		b, d, h, sp => bc de hl sp

		instr = LD_BC_NN + get8080WordRegister(q,BDHSP);
		q.expectComma(); goto iw;
iw:		store(instr); storeWord(value(q,pAny,v=1)); return;

	case ' dad':	// 8080: dad r => add hl,rr			b, d, h, sp => bc de hl sp

		store(ADD_HL_BC + get8080WordRegister(q,BDHSP)); return;

	case ' dcx':	// 8080: dcx r => dec rr			b, d, h, sp => bc de hl sp

		store(DEC_BC + get8080WordRegister(q,BDHSP)); return;

	case ' inx':	// 8080: inx r => inc rr			b, d, h, sp => bc de hl sp

		store(INC_BC + get8080WordRegister(q,BDHSP)); return;

	case 'ldax':	// 8080: ldax r => ld a,(rr)		b=bc d=de

		store(LD_A_xBC + get8080WordRegister(q,BD)); return;

	case 'stax':	// 8080: stax r => ld (rr),a		b=bc d=de

		store(LD_xBC_A + get8080WordRegister(q,BD)); return;

	case 'push':	// push r => push rr		b d h psw = bc de hl af

		store(PUSH_BC + get8080WordRegister(q,BDHAF)); return;

	case ' pop':	// pop  r => pop  rr		b d h psw = bc de hl af

		store(POP_BC + get8080WordRegister(q, BDHAF)); return;

	case ' end':	asmEnd(q); return;
	case '  if':	asmIf(q); return;
	case 'rept':	asmRept(q); return;

	// handle with asmInstr():
	case ' rst':
	case ' equ':
	case ' org':
	case '  db':
	case '  dw':
	case '  ds':
	case 'endm':	// -> error
	case '.z80':	// -> error
//	case ' set':		label definition is detected by asmLabel()
		q -= strlen(w); asmInstr(q); return;
	} // switch

unknown_opcode:

	// try macro expansion:
	Macro& m = macros.get(w);
	if(&m) { asmMacroCall(q,m); return; }

	// throw error:
	if(!is_letter(*w)&&*w!='_'&&*w!='.') throw syntax_error("instruction expected"); // no identifier
	else throw syntax_error("unknown instruction");
}



























