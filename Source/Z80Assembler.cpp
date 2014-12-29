/*	Copyright  (c)	Günter Woigk 1994 - 2014
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
#define LOG 2
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
const char DEFAULT_CODE_SEGMENT[] = "(DEFAULT)";



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
	ixcbr2_enabled(no),	// 	e.g. set b,(ix+d),r2
	ixcbxh_enabled(no),	// 	e.g. set b,xh
	target_hd64180(no),
	target_8080(no),
	registers_8080(no)
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
void Z80Assembler::setError( any_error& e )
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
	if(syntax_8080) registers_8080 = yes;	// -> limit known register names to 8080 registers
								// target_8080 -> register names are *not* automatically limited to 8080 registers!

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

	if(liststyle)
		try
		{
			listpath = endswith(listpath,"/") ? catstr(listpath, basename, ".lst") : listpath;
			writeListfile(listpath, liststyle);
		}
		catch(any_error& e) { addError(e.what()); }

	if(errors.count()==0 && deststyle)
		try
		{
			destpath = endswith(destpath,"/") ? catstr(destpath, basename, ".$") : destpath;
			writeTargetfile(destpath,deststyle);
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
	XLogLine("assemble: %u lines", sourcelines.count());

	source.purge();
	for(uint i=0;i<sourcelines.count();i++) { source.append(new SourceLine("", i, dupstr(sourcelines[i]))); }
	//current_sourceline_index = 0;

	target = NULL;

	// setup labels:
	labels.purge();
	labels.append(new Labels(Labels::GLOBALS));		// global_labels must exist

	// setup segments:
	segments.purge();
	segments.append(new Segment(DEFAULT_CODE_SEGMENT,no,0xff,no,yes,no));	// current_segment must exist
	segments[0].address_valid = yes;		// "physical" address = $0000 is valid
	segments[0].org_valid = yes;			// "logical"  address = $0000 is valid too
	global_labels().add(new Label(DEFAULT_CODE_SEGMENT,&segments[0],0,0,yes,yes,yes,no));

	if(ixcbr2_enabled) global_labels().add(new Label("ixcbr2",&segments[0],0,1,yes,yes,yes,no));
	if(ixcbxh_enabled) global_labels().add(new Label("ixcbxh",&segments[0],0,1,yes,yes,yes,no));
	if(target_hd64180) global_labels().add(new Label("hd64180",&segments[0],0,1,yes,yes,yes,no));
	if(target_8080)    global_labels().add(new Label("i8080",&segments[0],0,1,yes,yes,yes,no));
	if(syntax_8080)    global_labels().add(new Label("i8080asm",&segments[0],0,1,yes,yes,yes,no));
	if(registers_8080) global_labels().add(new Label("i8080regs",&segments[0],0,1,yes,yes,yes,no));

	// setup errors:
	errors.purge();

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
		current_segment_ptr = &segments[0];
		for(uint i=0;i<segments.count();i++) { segments[i].rewind(); }

		// init labels:
		local_labels_index = 0;
		local_blocks_count = 1;
		reusable_label_basename = DEFAULT_CODE_SEGMENT;

		// assemble source:
		for(uint i=0; i<source.count() && !end; i++)
		{
			try
			{
				current_sourceline_index = i;	// req. for errors and labels
				assembleLine(source[i]);
			}
			catch(fatal_error& e)
			{
				setError(e);
				return;
			}
			catch(any_error& e)
			{
				setError(e);
				if(errors.count()>max_errors) return;
				if(pass>1) source[i].segment->skipExistingData(source[i].bytecount);
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
	q.byteptr = currentPosition();		// Für Temp Label Resolver & Logfile
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
		return;
	}
//#ifndef NDEBUG   				// test suite:
	else if(q.test_char('!'))	// test suite: this line must fail:
	{
		try
		{
			if((uint8)q[1] > ' ' && q[1]!=';') asmLabel(q);	// label definition
			asmInstr(q);		// opcode or pseudo opcode
			q.expectEol();		// expect end of line
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
		if((uint8)q[0] > ' ' && q[0]!=';' && q[0]!='.') asmLabel(q);	// label definition
		asmInstr(q);			// opcode or pseudo opcode
		q.expectEol();			// expect end of line

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

	switch(q.peekChar())				// peek next character
	{
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
		case '$':	n = currentAddress();					// $ = "logical" address at current code position
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
				n = realAddress();	// pc: real address = segment.address + dpos
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
		else if(w[0]=='\'')			// ascii number
		{							// uses utf-8 or charset translation
			uint slen = strlen(w); if(slen<3||w[slen-1]!='\'') goto syntax_error;
			w = unquotedstr(w);		// unquote & unescape
			if(charset) { w = (cstr) charset->translate(w); slen = (uint8) *w++; }
			else		{ errno=0; w = fromutf8str(w); if(errno) throw syntax_error(errno); slen = strlen(w); }
		//	if(slen!=1) throw syntax_error("only one character expected");
			while(slen--) n = (n<<8) + (uint8) *w++;
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
			if( c=='h' ) goto hex_number;	// hex number    indicated by suffix
			if( c=='b' ) goto bin_number;	// binary number indicated by suffix
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
			if(*w!=0) goto syntax_error;
			goto op;
		}
	}

	if(q.testChar('('))			// test for built-in function
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

	if(is_letter(w[0]) || w[0]=='_')			// name
	{
label:	if(pass==1)	// Pass 1:
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
op:
	if(q.testEol()) return n;			// end of line

	switch(q.peekChar())				// peek next character
	{
	case '+':	if(pAdd<=prio) break; n = n + value(++q,pAdd,valid); goto op;	// add
	case '-':	if(pAdd<=prio) break; n = n - value(++q,pAdd,valid); goto op;	// subtract
	case '*':	if(pMul<=prio) break; n = n * value(++q,pMul,valid); goto op;	// multiply
	case '/':	if(pMul<=prio) break; n = n / value(++q,pMul,valid); goto op;	// divide
	case '%':																	// remainder (same prio as '*')
	case '\\':	if(pMul<=prio) break; n = n % value(++q,pMul,valid); goto op;	// remainder (same prio as '*')
//	case '&':	if(pBits<=prio) break; n = n & value(++q,pBits,valid); goto op;	// boolean and
//	case '|':	if(pBits<=prio) break; n = n | value(++q,pBits,valid); goto op;	// boolean or
	case '^':	if(pBits<=prio) break; n = n ^ value(++q,pBits,valid); goto op;	// boolean xor

	case '&':
		if(q.p[1]=='&') // '&&' --> boolean and
		{				// pruning is only possible if left-handed term is valid in pass1!
			if(pBoolean<=prio) break;
			if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
			if(n) n = value(q+=2,pBoolean,valid); else skip_expression(q+=2,pBoolean);
			n = n!=0; goto op;
		}
		else { if(pBits<=prio) break; n = n & value(++q,pBits,valid); goto op; }	// bitwise and

	case '|':
		if(q.p[1]=='|') // '||' --> boolean or
		{				// pruning is only possible if left-handed term is valid in pass1!
			if(pBoolean<=prio) break;
			if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
			if(!n) n = value(q+=2,pBoolean,valid); else skip_expression(q+=2,pBoolean);
			n = n!=0; goto op;
		}
		else { if(pBits<=prio) break; n = n | value(++q,pBits,valid); goto op; }	// bitwise or

	case '?':			// triadic ?:
						// in general pruning is not possible! (only if left-handed term is valid in pass1!)
		if(pTriadic<=prio) break;
		if(!valid) throw syntax_error("1st arg of pruning operator must be valid in pass 1");
		if(n) { n = value(++q,pTriadic-1,valid); q.expect(':'); skip_expression(q,pTriadic-1); }
		else  { skip_expression(++q,pTriadic-1); q.expect(':'); n = value(q,pTriadic-1,valid); }
		goto op;

	case '=':
		if(pCmp<=prio) break;
		++q; q.skip_char('='); n = n==value(q,pCmp,valid); goto op;				// equal:		'=' or '=='

	case '!':
		if (pCmp<=prio) break;
		++q; if(*q=='=') { n = n!=value(++q,pCmp,valid); goto op; }				// not equal:	'!='
		--q; break;

	case '<':
		if(*++q=='<')		// <<
		{
		    if(pRot<=prio) { --q; break; }
			n = n << value(++q,pRot,valid); goto op;							// shift left:	"<<"
		}
		else
		{
		    if(pCmp<=prio)	{ --q; break; }
			else if(*q=='>'){ n = n!=value(++q,pCmp,valid); goto op; }			// not equal:   "<>"
			else if(*q=='='){ n = n<=value(++q,pCmp,valid); goto op; }			// less or equ:	"<="
			else			{ n = n< value(  q,pCmp,valid); goto op; }			// less than:	"<"
		}

	case '>':
		if(*++q=='>')		// >>
		{
		    if(pRot<=prio) { --q; break; }
			n = n >> value(++q,pRot,valid); goto op;							// shift right:	">>"
		}
		else
		{
		    if(pCmp<=prio)	{ --q; break; }
			else if(*q=='='){ n = n>=value(++q,pCmp,valid); goto op; }			// greater or equ.:	">="
			else			{ n = n> value(  q,pCmp,valid); goto op; }			// greater than:	">"
		}
	}

// no operator followed  =>  return value; caller will check the reason of returning anyway
	return n;
}


/*	Handle Label Definition
	Unterscheidet global - lokal - reusable
*/
void Z80Assembler::asmLabel(SourceLine& q) throw(any_error)
{
	XXXASSERT(q.p==q.text+(q[0]=='!'));

	q.is_label = yes;		// this source line defines a label

	cstr name = q.nextWord(); XXXASSERT(name[0]);
	bool is_reusable = is_dec_digit(name[0]) && q.test_char('$');				// SDASZ80
	bool is_global   = q.test_char(':') && !is_reusable && q.test_char(':');
	if(is_reusable) name = catstr(reusable_label_basename,"$",name);

	bool is_valid;
	int32 value;

	if(q.testWord("equ") || q.testWord("defl") || q.testChar('='))
	{
		value = this->value(q,pAny,is_valid=true);	// calc assigned value
	}
	else	// label: <opcode>
	{
		value = currentAddress();
		is_valid = currentAddressValid();

		if(!is_reusable) reusable_label_basename = name;
	}

	Labels& labels = is_global ? global_labels() : local_labels();
	Label* l = &labels.find(name);

	if(l)
	{
		if(l->segment==NULL)
		{
			l->segment = current_segment_ptr;			// mit '.globl' deklarierte Label haben noch kein Segment
			l->sourceline = current_sourceline_index;	// und keine Source-Zeilennummer
		}

		if(l->sourceline != current_sourceline_index)
		{
			if(l->is_valid && is_valid && l->value==value &&	// allow trivial defs to occur multiple times
				l->is_global==is_global)						// e.g.:	SPACE equ $20 ; somewhere in source
				return;											// later:	SPACE equ $20 ; somewhere else in source
			throw syntax_error("label redefined");
		}

		XXXASSERT(is_valid || !l->is_valid);
		XXXASSERT(l->segment == current_segment_ptr);
		XXXASSERT(l->sourceline == current_sourceline_index);

		if(l->is_valid && l->value!=value) throw syntax_error("value redefined");
		l->value    = value;
		l->is_valid = is_valid;
		l->is_defined = true;
	}
	else
	{
		if(name[1]==0)	// strlen(name) == 1
		{
			cstr names = registers_8080 ? "bcdehla" : "irbcdehla";
			if(strchr(names,name[0])) throw syntax_error(usingstr("'%s' is the name of a register",name));
		}
		else if(name[2]==0)	// strlen == 2
		{
			cstr names = registers_8080 ? "bc de hl sp af" : "ix iy xh xl yh yl bc de hl sp af";
			if(findStr(names,name)) throw syntax_error(usingstr("'%s' is the name of a register",name));
		}

		l = new Label(name, &current_segment(), current_sourceline_index, value, is_valid, is_global, yes, no);
		labels.add(l);
	}
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
		if(eq(w,"end"))		  asmEnd(q);		else throw fatal_error("unknown assembler directive");
	}
	catch(fatal_error& e) { throw e; }
	catch(any_error& e)   { throw fatal_error(e.what()); }
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
void Z80Assembler::asmEnd(SourceLine&) throw(any_error)
{
	end = true;

	// assign default segment to all remaining source lines
	// to keep writeListfile() happy:
	if(pass>1) return;
	for(uint i=current_sourceline_index+1; i<source.count();i++) { source[i].segment = &segments[0]; }
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
	note: if #target is defined then the DEFAULT_CODE_SEGMENT
		  will be deleted when the first #code or #data segment is defined
*/
void Z80Assembler::asmTarget( SourceLine& q ) throw(any_error)
{
	if(pass>1) { q.skip_to_eol(); return; }
	if(target) throw fatal_error("#target redefined");

	if(!current_segment_ptr->isAtStart())				// already some code defined
		// || current_segment_ptr->org_valid)	// org defined
		throw fatal_error("#target must defined before first opcode");

	target = upperstr(q.nextWord());
	if(!contains(" ROM BIN Z80 SNA TAP TAPE O P 80 81 P81 ACE ",catstr(" ",target," ")))
		throw syntax_error("target name expected");

	// fix fillbyte in case no segments are defined: (not recommended!)
	current_segment_ptr->fillbyte = eq(target,"ROM") ? 0xff : 0x00;
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
		dup(pipout[W]);			// becomes lowest unused fileid: stdout
		dup(pipout[W]);			// becomes lowest unused fileid: stderr
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
	if(!is_letter(*name) && *name!='_') throw fatal_error("segment name expected");
	Segment* segment = segments.find(name);
	XXXASSERT(!segment || eq(segment->name,name));

	if(segment && segment->is_data != is_data) throw fatal_error("#code/#data mismatch");

//	if(pass==1 ? segment!=NULL : q.peekChar()!=',')
//	{}		// --> expect eol
//	else	// --> parse arguments
//	{
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

		uint8 fillbyte = is_data || ne(target,"ROM") ? 0x00 : 0xFF;
		segment = new Segment(name,is_data,fillbyte,relocatable,resizable,has_flag);
		segments.append(segment);
		global_labels().add(new Label(name,segment,q.sourcelinenumber,address,address_is_valid,yes,yes,no));
		reusable_label_basename = name;
	}

	if(address_is_valid) { segment->setAddress(address); segment->setOrigin(address,yes); }	// throws
	if(size_is_valid)    { segment->setSize(size); }			// throws
	if(flags_is_valid)   { segment->setFlag(flags); }			// throws
//	}

	current_segment_ptr = segment;
	q.segment = current_segment_ptr;	// Für Temp Label Resolver
	q.byteptr = currentPosition();		// Für Temp Label Resolver & Logfile
	XXXASSERT(q.bytecount==0);
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


#if 0
// --------------------------------------------------
//				Assemble Opcode
// --------------------------------------------------


/*	store signed or unsigned byte
	validates byte if valid=true
*/
void Z80Assembler::storeByte(int byte, bool valid) throw(any_error)
{
	if(valid && (byte>255||byte<-128)) throw syntax_error( "byte value out of range" );
	current_segment().store(byte);
}

/*	store offset byte and check range
	validates offset if valid=true
*/
void Z80Assembler::storeOffset(int offset, bool valid) throw(any_error)
{
	if(valid && (offset!=(signed char)offset)) throw syntax_error("offset out of range");
	current_segment().store(offset);
}

/*	store XY CB dis opcode
	validates displacement if valid=true
*/
void Z80Assembler::store_XYCB_op( int pfx, int opcode, int dis, bool valid ) throw(any_error)
{
	if (valid && dis!=(signed char)dis) throw syntax_error("displacement out of range");
	current_segment().store(pfx,PFX_CB,dis,opcode);
}

#define	store_IXCB_opcode(OP,DIS,VALID)	store_XYCB_op(PFX_IX,OP,DIS,VALID)
#define	store_IYCB_opcode(OP,DIS,VALID)	store_XYCB_op(PFX_IY,OP,DIS,VALID)


/*	store pfx + opcode
*/
#define	store_ED_opcode(OP)	current_segment().store(PFX_ED,OP)
#define	store_CB_opcode(OP)	current_segment().store(PFX_CB,OP)
#define	store_IX_opcode(OP)	current_segment().store(PFX_IX,OP)
#define	store_IY_opcode(OP)	current_segment().store(PFX_IY,OP)


/*	store XY opcode dis
	validates displacement if valid=true
*/
void Z80Assembler::store_XY_byte_op( int pfx, int opcode, int dis, bool valid ) throw(any_error)
{
	if (valid && dis!=(signed char)dis) throw syntax_error("displacement out of range");
	current_segment().store(pfx,opcode,dis);
}

#define	store_IX_byte_opcode(OP,DIS,VALID)	store_XY_byte_op(PFX_IX,OP,DIS,VALID)
#define	store_IY_byte_opcode(OP,DIS,VALID)	store_XY_byte_op(PFX_IY,OP,DIS,VALID)


/*	expect and evaluate condition
*/
enum { NZ=0,Z=8,NC=16,C=24,PO=32,PE=40,P=48,M=56 };

int Z80Assembler::getCondition( cstr w ) throw(syntax_error)
{
	switch(strlen(w))
	{
	case 1:
		switch(peek1(w)|0x20)
		{
		case 'z': return Z;
		case 'c': return C;
		case 'p': return P;
		case 'm': return M;
		}
		break;
	case 2:
		switch(peek2X(w)|0x20202020)
		{
		case '  nz':	return NZ;
		case '  nc':	return NC;
		case '  po':	return PO;
		case '  pe':	return PE;
		}
		break;
	}
	throw syntax_error ( "condition expected" );
}


/*	test for and evaluate register
*/
enum
{	RB,RC,RD,RE,RH,RL,OPEN,RA,
	XH,XL,YH,YL,
	RF, RI, RR,   BC,DE,HL,SP,
	IX,IY,AF,PC,  AF2,BC2,DE2,HL2
};

int Z80Assembler::getRegister(SourceLine& q)
{
	cstr w = q.nextWord();

	switch(strlen(w))
	{
	case 1:
		switch(*w | 0x20)
		{
		case 'a':	return RA;
		case 'f':	return RF;
		case 'b':	return RB;
		case 'c':	return RC;
		case 'd':	return RD;
		case 'e':	return RE;
		case 'h':	return RH;
		case 'l':	return RL;
		case '(':	return OPEN;
		case 'i':	return RI;
		case 'r':	return RR;
		}
		break;
	case 2:
		switch(peek2X(w) | 0x20202020)
		{
		case '  af':	return AF;
		case '  bc':	return BC;
		case '  de':	return DE;
		case '  hl':	return HL;
		case '  ix':	return IX;
		case '  iy':	return IY;
		case '  sp':	return SP;
		case '  pc':	return PC;
		case '  xh':	return XH;
		case '  xl':	return XL;
		case '  yh':	return YH;
		case '  yl':	return YL;
		}
		break;
	}
	return -1;
}

/*	assemble opcode
*/
void Z80Assembler::asmInstr(SourceLine& q) throw(any_error)
{
	int32 i,j,n=0,m=0;
	cstr s;
	cstr w = lowerstr(q.nextWord());
	bool v;

// strlen-Verteiler:

	switch(strlen(w))
	{
	case 0:		return;					// end of line
	case 1:		goto wlen1;
	case 2:		goto wlen2;
	case 3:		goto wlen3;
	case 4:		goto wlen4;
	default:	goto wlenXL;
	}


// opcode len = 1:

wlen1:


	if(*w=='.')							// SDASZ80 opcodes
	{
		w = lowerstr(q.nextWord());

		if(eq(w,"module"))				// for listing
		{
			q.skip_to_eol();
			return;
		}
		if(eq(w,"optsdcc"))				// .optsdcc -mz80
		{
			if(!q.testChar('-') )		throw syntax_error("-mz80 expected");
			if(ne(q.nextWord(),"mz80"))	throw syntax_error("-mz80 expected");
			return;
		}
		if(eq(w,"area"))				// select segment for following code
		{
			cstr name = upperstr(q.nextWord());
			if(!is_letter(*name) && *name!='_') throw fatal_error("segment name expected");
			Segment* segment = segments.find(name);
			if(!segment) throw fatal_error("segment not found");

			current_segment_ptr = segment;
			q.segment = current_segment_ptr;
			q.byteptr = currentPosition();
			XXXASSERT(q.bytecount==0);

			if((eq(name,"_CABS")||eq(name,"_DABS")||eq(name,"_RSEG"))	// SDCC generates: " .area _CABS (ABS)"
				&& q.testChar('('))										// KCC  generates: " .area _RSEG (ABS)"
			{
				if(!q.testWord("ABS")) throw syntax_error("'ABS' expected");
				q.expect(')');
			}
			return;
		}
		if(eq(w,"globl"))				// declare global label for linker: mark label for #include library "libdir"
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
		if(eq(w,"ds"))	 goto ds;
		if(eq(w,"dw"))	 goto dw;
		if(eq(w,"db"))	 goto db; // SDASZ80: truncates value to byte (not implemented, done if used this way by SDCC)
		if(eq(w,"byte")) goto db; // SDASZ80: truncates value to byte (not implemented, done if used this way by SDCC)
		if(eq(w,"org"))	 goto org;
		if(eq(w,"ascii"))goto dm;

/*		if(eq(opcode,"title"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"sbttl"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"list"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"nlist"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"page"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"if"))		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"iif"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"else"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// after .if
		if(eq(opcode,"endif"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// after .if
		if(eq(opcode,"byte"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"fcb"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"word"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"fcw"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"3byte"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"triple"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"4byte"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"quad"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"blkb"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"rmb"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"rs"))		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"blkw"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"blk3"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"blk4"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"str"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"fcc"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"ascis"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"strs"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"asciz"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"strz"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"radix"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"even"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"odd"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"bndry"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"arg"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"local"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"equ"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"gblequ"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"lclequ"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"include"))throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"define"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"undefine"))throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"setdp"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"16bit"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"24bit"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"32bit"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"end"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"macro"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"endm"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"mexit"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"narg"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"nchr"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"ntyp"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"nval"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"irp"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"irpc"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"rept"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"mdelete"))throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"mlib"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(eq(opcode,"mcall"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
*/
		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
	}
	goto unknown_opcode;	// error






// opcode len = 2:

wlen2:
	switch(peek2X(w)|0x20202020)
	{
	default:		goto unknown_opcode;	// error
	case '  db':	goto db;
	case '  dw':	goto dw;
	case '  ds':	goto ds;
	case '  dm':	goto dm;
	case '  ei':	storeOpcode(EI); return;
	case '  di':	storeOpcode(DI); return;
	case '  jp':
	{
		if(q.testChar('('))					// jp (hl), (ix), (iy)
		{
			switch(getRegister(q))
			{
			case HL:	storeOpcode	   (JP_HL); break;
			case IX:	store_IX_opcode(JP_HL); break;
			case IY:	store_IY_opcode(JP_HL); break;
			default:	goto ill_reg;
			}
			q.expectClose();
			return;
		}

		s = q.p;

		switch(getRegister(q))				// jp hl, ix, iy	alternate notation as used by disass   kio 2012-01-02
		{
		case HL:	storeOpcode	   (JP_HL); return;
		case IX:	store_IX_opcode(JP_HL); return;
		case IY:	store_IY_opcode(JP_HL); return;
		default:	break;
		}

		// not jp(reg)  =>  jp address

		if(q.testComma())		// conditional jump
		{
			q.p = s;
			i = JP_NZ + getCondition(q.nextWord());
			q.expectComma();	// skip comma
		}
		else					// unconditional jump
		{
			q.p = s;
			i = JP;
		}

		n = value(q,pAny,v=1);
		storeOpcode(i);
		storeWord(n);
		return;
	}
	case '  im':
	{
		static char im[]={IM_0,IM_1,IM_2};
		n=value(q,pAny,v=1); if(n<0||n>2) { if(v) throw syntax_error("illegal interrupt mode"); else n=0; }
		store_ED_opcode(im[n]);
		return;
	}
	case '  in':
	{
		n=getRegister(q); if (n==OPEN) goto ill_dest;
		q.expectComma();
		s=q.p;
		q.expect('(');
		m = getRegister(q);
		q.expectClose();
		if (m==RC)
		{	if (n==RF) n=OPEN;
			if (n>=RB&&n<=RA) store_ED_opcode(IN_B_xC+8*n); else { q.p=s; goto ill_dest; }
			return;
		}
		q.p=s;
		if(n!=RA) goto ill_dest;
		storeOpcode(INA);
		n=value(q,pAny,v=1); storeByte(n,v);
		return;
	}
	case '  rl':
	{
		i = RL_B;
		goto rr;
	}
	case '  rr':
	{
		i = RR_B;
rr:		s = q.p;
		n = getRegister(q);
		int r;

		if(n<0)			// rr n(XY)  oder  illegal  rr n(XY),r
		{
			q.p=s;
			n=value(q,pAny,v=1);
			q.expect('(');
			r = getRegister(q);
			if(r!=IX && r!=IY) goto ix_iy_expected;
			goto rrxy;
//			q.expectClose();
//			int r2 = OPEN;
//			if(q.testComma())		// illegal  rr d(ix),r
//			{
//				r2 = getRegister(q);
//				if(r2<RB || r2>RA || r2==OPEN) goto ill_target;
//			}
//			store_XYCB_op(r==IX?PFX_IX:PFX_IY, i+r2, n, v);
		}
		else if(n==OPEN) 	// hl/ix/iy register indirect ?
		{
			r = getRegister(q);
			if(r==HL)
			{
				store_CB_opcode(i+OPEN);
				q.expectClose();
			}
			else if(r==IX||r==IY)
			{
				n = value(q,pAny,v=1);
rrxy:			q.expectClose();
				int r2 = OPEN;
				if(enable_illegal_ixcb_r2_instructions && q.testComma()) // illegal: rr (ix+d),r ***NOT ALL Z80 CPUs!***
				{
					r2 = getRegister(q);
					if(r2<RB || r2>RA || r2==OPEN) goto ill_target;
				}
				store_XYCB_op(r==IX?PFX_IX:PFX_IY, i+r2, n, v);
			}
			else if(enable_illegal_ixcb_xh_instructions && n>=XH && n<=YL) // illegal: rr xh ***NOT ALL Z80 CPUs!***
			{
				int pfx = n>=YH ? PFX_IY : PFX_IX;
				n -= n>=YH ? YH-RH : XH-RH;
				store_XYCB_op(pfx, i+n, 0, true);
			}
			else goto ill_reg;
		}
		else if(n>=RB && n<=RA)
			store_CB_opcode(i+n);
		else
			goto ill_target;
		return;
	}
	case '  ex':
	{
		i = getRegister(q);
		if(i==OPEN) { if(getRegister(q)!=SP) goto ill_reg; q.expectClose(); }
		q.expectComma();
		n = getRegister(q);
		if(n==OPEN) { if (getRegister(q)!=SP) goto ill_reg; q.expectClose(); }
		if(n==AF) q.expect('\'');
		if(i==DE) { i=n; n=DE; }
		if(i==OPEN) { i=n; n=OPEN; }
		if(i==AF&&n==AF) storeOpcode(EX_AF_AF);
		else if(i==HL&&n==DE) storeOpcode(EX_DE_HL);
		else if(i==HL&&n==OPEN) storeOpcode(EX_HL_xSP);
//		else if(i==IX&&n==DE) store_IX_opcode(EX_DE_HL); 	// ex de,hl always changes only de and hl. 2006-09-13 kio
		else if(i==IX&&n==OPEN) store_IX_opcode(EX_HL_xSP); // valid illegal. 2006-09-13 kio
//		else if(i==IY&&n==DE) store_IY_opcode(EX_DE_HL); 	// ex de,hl always changes only de and hl. 2006-09-13 kio
		else if(i==IY&&n==OPEN) store_IY_opcode(EX_HL_xSP);	// valid illegal. 2006-09-13 kio
		else goto ill_reg;
		return;
	}
	case '  jr':
	{
		s=q.p;
		i=JR;
		w=q.nextWord();
		if(q.testComma())
		{
			i = JR_NZ + getCondition(w);
			if(i>JR_C) goto ill_cond;
		}
		else q.p=s;

		n = value(q,pAny,v=1);	// kio 2014-02-09: vorgezogen wg. möglicher Referenz auf $
		storeOpcode(i);
		storeOffset(n - (currentAddress()+1), v && currentAddressValid());
		return;
	}
	case '  cp':
	{
		i=CP_B;
cp:		s=q.p;

		int r=getRegister(q);
		if(q.testComma())
		{
			if(r!=RA) goto ill_target;
			s=q.p;
			r=getRegister(q);
		}
		switch(r)
		{
		case -1:		// immediate value
		{
			q.p=s;
			n = value(q,pAny,v=1);

			if(q.testChar('('))		// n(XY)
			{
				r = getRegister(q);
				if(r!=IX&&r!=IY) goto ix_iy_expected;
				q.expectClose();
				store_XY_byte_op(r==IX?PFX_IX:PFX_IY,i+OPEN,n,v);
			}
			else
			{
				storeOpcode(i+CP_N-CP_B);
				storeByte(n,v);
			}
			return;
		}
		case OPEN:		// hl/ix/iy register indirect ?
		{
			switch(getRegister(q))
			{
			case IX:
				n=0; v=1; if(q.peekChar()!=')') n = value(q,pAny,v);
				store_IX_byte_opcode(i+r,n,v);
				break;
			case IY:
				n=0; v=1; if(q.peekChar()!=')') n = value(q,pAny,v);
				store_IY_byte_opcode(i+r,n,v);
				break;
			case HL:
				storeOpcode(i+r);
				break;
			default:
				goto ill_reg;
			}
			q.expectClose();
			return;
		}
		case XL:										// 2006-09-08 kio: for ill. IX/IY opcodes
		case XH:										// 2006-09-08 kio: for ill. IX/IY opcodes
			store_IX_opcode(i+r+(RL-XL)); return;		// 2006-09-08 kio: for ill. IX/IY opcodes
		case YL:										// 2006-09-08 kio: for ill. IX/IY opcodes
		case YH:										// 2006-09-08 kio: for ill. IX/IY opcodes
			store_IY_opcode(i+r+(RL-YL)); return;		// 2006-09-08 kio: for ill. IX/IY opcodes
		default:
			if(r>RA) goto ill_source;
			storeOpcode(i+r);
			return;
		}
	}
	case '  or':
	{
		i = OR_B; goto cp;
	}
	case '  ld':
	{
		bool u=1;	// u == m_valid, v == n_valid
			 v=1;

		// target:	i = register;  if no register: -1;  if indirect: 100 added
		// 			n = value (100-1) or offset (100+IX/100+IY)
		// source:	j = register;  if no register: -1;  if indirect: 100 added
		// 			m = value (100-1/-1) or offset (100+IX/100+IY)
		s=q.p;
		i = getRegister(q);
		if(i<0)			// n(XY)
		{
			q.p=s; n = value(q,pAny,v);
			if(q.testChar('('))
			{
				i = getRegister(q);
				if(i!=IX && i!=IY) goto ix_iy_expected;
				i += 100;
				q.expectClose();
			}
			else goto ill_dest;
		}
		if (i==OPEN)
		{
			s=q.p;
			i = getRegister(q);
			if(i==OPEN) i=-1;		// sdcc:  ld ((nnnn)),hl
			if (i>=0)	// (reg)
			{
				if(i==IX||i==IY) n = q.peekChar()==')' ? 0 : value(q,pAny,v);
			}
			else		// (nn)
			{
				q.p=s; n=value(q,pAny,v);
			};
			i+=100;
			q.expectClose();
		};
		q.expectComma();

		s=q.p;
		j = getRegister(q);
		if (j<0) 		// nn
		{
			q.p=s; m = value(q,pAny,u);
			if(q.testChar('('))
			{
				j = getRegister(q);
				if(j!=IX && j!=IY) goto ix_iy_expected;
				j += 100;
				q.expectClose();
			}
		}
		else if (j==OPEN)
		{
			s=q.p;
			j = getRegister(q);
			if(j==OPEN) j=-1;
			if(j>=0)	// (reg)
			{
				if(j==IX||j==IY) m = q.peekChar()==')' ? 0 : value(q,pAny,u);
			}
			else		// (nn)
			{
				q.p=s; m = value(q,pAny,u);
			};
			j+=100;
			q.expectClose();
		};

		if(i>=99&&j>=99) throw syntax_error( "no z80 instruction can access memory twice!" );

		if( (i<BC||i>=99/*>IY*/) && (j<BC||j>=99/*>IY*/) )	// byte operations
		{													// korr.: nach IY noch Doppelregister bis HL2 (kio)
			if (i==100+HL) i=OPEN;
			if (j==100+HL) j=OPEN;

			if (i>=RB&&i<=RA)
			{
				switch(j)
				{
				case -1:	 storeOpcode(LD_B_N+i*8); storeByte(m,u); return;
				case 100+IX: store_IX_byte_opcode(LD_B_xHL+i*8,m,u); return;
				case 100+IY: store_IY_byte_opcode(LD_B_xHL+i*8,m,u); return;
				case XH:													// for ill. IX/IY opcodes
				case XL:	if (i==RH||i==RL||i==OPEN) break;				// for ill. IX/IY opcodes
							store_IX_opcode(LD_B_B+i*8+(j-XH+RH)); return;	// for ill. IX/IY opcodes
				case YH:													// for ill. IX/IY opcodes
				case YL:	if (i==RH||i==RL||i==OPEN) break;				// for ill. IX/IY opcodes
							store_IY_opcode(LD_B_B+i*8+(j-YH+RH)); return;	// for ill. IX/IY opcodes
				case RB:
				case RC:
				case RD:
				case RE:
				case RH:
				case RL:
				case OPEN:
				case RA:	storeOpcode(LD_B_B+i*8+j); return;
				}
			}

			if(i>=XH&&i<=YL)		// 2006-09-08 kio: for ill. IX/IY opcodes
			{
				if(i<=XL)	// XH or XL
				{
					switch(j)
					{
					case -1:	store_IX_opcode(LD_B_N+(i-XH+RH)*8); storeByte(m,u); return;
					case XH:
					case XL:	store_IX_opcode(LD_B_B+(i-XH+RH)*8+(j-XH+RH)); return;
					case RB:
					case RC:
					case RD:
					case RE:
					case RA:	store_IX_opcode(LD_B_B+(i-XH+RH)*8+j); return;
					}
				}
				else		// YH or YL
				{
					switch(j)
					{
					case -1:	store_IY_opcode(LD_B_N+(i-YH+RH)*8); storeByte(m,u); return;
					case YH:
					case YL:	store_IY_opcode(LD_B_B+(i-YH+RH)*8+(j-YH+RH)); return;
					case RB:
					case RC:
					case RD:
					case RE:
					case RA:	store_IY_opcode(LD_B_B+(i-YH+RH)*8+j); return;
					}
				}
			}

			if (i==RA)
			{	switch (j)
				{
				case 100-1:		storeOpcode(LD_A_xNN); storeWord(m); return;
				case 100+BC:	storeOpcode(LD_A_xBC); return;
				case 100+DE:	storeOpcode(LD_A_xDE); return;
				case RI:		store_ED_opcode(LD_A_I); return;
				case RR:		store_ED_opcode(LD_A_R); return;
				}
			}

			if (j==RA)
			{	switch (i)
				{
				case 100-1:		storeOpcode(LD_xNN_A); storeWord(n); return;
				case 100+BC:	storeOpcode(LD_xBC_A); return;
				case 100+DE:	storeOpcode(LD_xDE_A); return;
				case RI:		store_ED_opcode(LD_I_A); return;
				case RR:		store_ED_opcode(LD_R_A); return;
				}
			}

			if (i==100+IX)
			{
				if (j==-1)				// ld (ix+dis),nn
				{
					store_IX_byte_opcode(LD_xHL_N,n,v); storeByte(m,u); return;
				}
				else if (j>=RB&&j<=RA)	// ld (ix+dis),reg
				{
					store_IX_byte_opcode(LD_xHL_B+j,n,v); return;
				}
			}

			if (i==100+IY)
			{
				if (j==-1)				// ld (ix+dis),nn
				{
					store_IY_byte_opcode(LD_xHL_N,n,v); storeByte(m,u); return;
				}
				else if (j>=RB&&j<=RA)	// ld (ix+dis),reg
				{
					store_IY_byte_opcode(LD_xHL_B+j,n,v); return;
				}
			}
		}
		else					// word operations
		{
			if (i==100+HL)		// Goodie: ld (hl),dreg
			{
				if(j==BC)
				{
					storeOpcode(LD_xHL_C); storeOpcode(INC_HL);
					storeOpcode(LD_xHL_B); storeOpcode(DEC_HL);
					return;
				}
				if(j==DE)
				{
					storeOpcode(LD_xHL_E); storeOpcode(INC_HL);
					storeOpcode(LD_xHL_D); storeOpcode(DEC_HL);
					return;
				}
			}

			if (j==100+HL)		// Goodie: ld dreg,(hl)
			{
				if(i==BC)
				{
					storeOpcode(LD_C_xHL); storeOpcode(INC_HL);
					storeOpcode(LD_B_xHL); storeOpcode(DEC_HL);
					return;
				}
				if(i==DE)
				{
					storeOpcode(LD_E_xHL); storeOpcode(INC_HL);
					storeOpcode(LD_D_xHL); storeOpcode(DEC_HL);
					return;
				}
			}

			if (i>=BC&&i<SP&&j>=BC&&j<SP)		// goodie: ld dreg,dreg
			{
				n = (i-BC)*8+(j-BC);
				storeOpcode(LD_B_B+n*2);		// high byte
				storeOpcode(LD_C_C+n*2);		// low byte
				return;
			}

			if (i==IX) { i=HL; storeOpcode(PFX_IX); }	else
			if (i==IY) { i=HL; storeOpcode(PFX_IY); }	else
			if (j==IX) { j=HL; storeOpcode(PFX_IX); }	else
			if (j==IY) { j=HL; storeOpcode(PFX_IY); }

			if (i==100-1)		// (NN)
			{
				switch(j)
				{
				case HL:	storeOpcode(LD_xNN_HL); break;
				case BC:	store_ED_opcode(LD_xNN_BC); break;
				case DE:	store_ED_opcode(LD_xNN_DE); break;
				case SP:	store_ED_opcode(LD_xNN_SP); break;
				default:	goto ill_source;
				}
				storeWord(n);
				return;
			}

			if (j==100-1)		// (NN)
			{
				switch(i)
				{
				case HL:	storeOpcode(LD_HL_xNN); break;
				case BC:	store_ED_opcode(LD_BC_xNN); break;
				case DE:	store_ED_opcode(LD_DE_xNN); break;
				case SP:	store_ED_opcode(LD_SP_xNN); break;
				default:	goto ill_source;
				}
				storeWord(m);
				return;
			}

			if (j==-1)			// NN
			{
				switch(i)
				{
				case HL:	storeOpcode(LD_HL_NN); break;
				case BC:	storeOpcode(LD_BC_NN); break;
				case DE:	storeOpcode(LD_DE_NN); break;
				case SP:	storeOpcode(LD_SP_NN); break;
				default:	goto ill_source;
				}
				storeWord(m);
				return;
			}

			if (i==SP&&j==HL)	{ storeOpcode(LD_SP_HL); return; }
		}
		throw syntax_error ( "parameter error" );

	}
	}


// opcode len = 3:

wlen3:
	switch(peek3X(w)|0x20202020)
	{
	default:		goto unknown_opcode;	// error
	case ' mov':	throw fatal_error("this is no Z80 assembler source. ('mov' is no Z80 instruction)");
	case ' scf':	storeOpcode(SCF); return;
	case ' ccf':	storeOpcode(CCF); return;
	case ' cpl':	storeOpcode(CPL); return;
	case ' daa':	storeOpcode(DAA); return;
	case ' rra':	storeOpcode(RRA); return;
	case ' rla':	storeOpcode(RLA); return;
	case ' nop':	storeOpcode(NOP); return;
	case ' neg':	store_ED_opcode(NEG); return;
	case ' exx':	storeOpcode(EXX); return;
	case ' rrd':	store_ED_opcode(RRD); return;
	case ' rld':	store_ED_opcode(RLD); return;
	case ' ldi':	store_ED_opcode(LDI); return;
	case ' cpi':	store_ED_opcode(CPI); return;
	case ' ini':	store_ED_opcode(INI); return;
	case ' ldd':	store_ED_opcode(LDD); return;
	case ' cpd':	store_ED_opcode(CPD); return;
	case ' ind':	store_ED_opcode(IND); return;
	case ' and':	i = AND_B; goto cp;
	case ' xor':	i = XOR_B; goto cp;
	case ' sub':	i = SUB_B; goto cp;
	case ' rlc':	i = RLC_B; goto rr;
	case ' rrc':	i = RRC_B; goto rr;
	case ' sla':	i = SLA_B; goto rr;
	case ' sra':	i = SRA_B; goto rr;
	case ' sll':	i = SLL_B; goto rr;
	case ' srl':	i = SRL_B; goto rr;
	case ' res':	i = RES0_B; goto bit;
	case ' set':	i = SET0_B; goto bit;
	case ' dec':	i = 8; goto inc;
	case ' org':
	{
org:	// org <value>	; set "logical" code address
		// org $$		; $$ = "physical" code address = segment.address + dpos
		n = value(q, pAny, v=1);
		current_segment().setOrigin(n,v);
		return;
	}
	case ' rst':
	{
		n = value(q,pAny,v=1);
		if(n%8==0) n>>=3;
		if(n<0 || n>7) { if(v) throw syntax_error( "illegal vector" ); else n=0; }
		//else		??!?
		storeOpcode(RST00+n*8);
		return;
	}
	case ' add':
	{
		s=q.p; n=getRegister(q); if(n<=YL/*illegals added 2006-09-08 kio*/) { q.p=s; i = ADD_B; goto cp; }
		if(n!=HL&&n!=IX&&n!=IY) goto ill_reg;
		q.expectComma(); 					// hl/ix/iy
		if(n==IX) storeOpcode(PFX_IX);
		if(n==IY) storeOpcode(PFX_IY);
		i = getRegister(q);
		switch(i)
		{
		case BC:	storeOpcode(ADD_HL_BC); return;
		case DE:	storeOpcode(ADD_HL_DE); return;
		case SP:	storeOpcode(ADD_HL_SP); return;
		case HL:
		case IX:
		case IY:	storeOpcode(ADD_HL_HL); if(i==n) return;
		}
		goto ill_reg;
	}
	case ' sbc':
	{
		s=q.p; n=getRegister(q); if(n<=YL/*illegals added 2006-09-08 kio*/) { q.p=s; i = SBC_B; goto cp; }
		i=SBC_HL_BC;
sbc:	if(n!=HL) goto ill_reg;
		q.expectComma();
		switch( getRegister(q) )
		{
		case BC:	store_ED_opcode(i   ); return;
		case DE:	store_ED_opcode(i+16); return;
		case HL:	store_ED_opcode(i+32); return;
		case SP:	store_ED_opcode(i+48); return;
		}
		goto ill_reg;
	}
	case ' adc':
	{
		s=q.p; n=getRegister(q); if(n<=YL/*illegals added 2006-09-08 kio*/) { q.p=s; i = ADC_B; goto cp; }
		i=ADC_HL_BC; goto sbc;
	}
	case ' bit':
	{
		i = BIT0_B;
bit:	n = value(q,pAny,v=1); if (v && (n<0 || n>7)) throw syntax_error("illegal bit number");
		i += n*8;
		q.expectComma();
		s = q.p;
		n = getRegister(q);
		int r;

		if(n<0)		// n(XY)
		{
			q.p = s;
			m = value(q,pAny,v);
			q.expect('(');
			r = getRegister(q);
			if(r==IX || r==IY) goto bitxy;
			else goto ix_iy_expected;
//			q.expectClose();
//			store_XYCB_op(r==IX?PFX_IX:PFX_IY,i+OPEN, m, v);
		}
		else if(n==OPEN)
		{
			r = getRegister(q);
			if(r==HL)
			{
				store_CB_opcode(i+n);
				q.expectClose();
			}
			else if(r==IX||r==IY) 				// 2007-09-25 kio: also support (IX) w/o offset
			{
				m=0; v=1; if(q.peekChar()!=')') m = value(q,pAny,v);
bitxy:			q.expectClose();
				int r2 = OPEN;
				if(enable_illegal_ixcb_r2_instructions && q.testComma()) // illegal: bit b,(ix+d),r ***NOT ALL Z80 CPUs!***
				{
					r2 = getRegister(q);
					if(r2<RB || r2>RA || r2==OPEN) goto ill_target;
				}
				store_XYCB_op(r==IX?PFX_IX:PFX_IY, i+r2, m, v);
			}
			else goto ill_reg;
		}
		else if(n>=RB && n<=RA)
			store_CB_opcode(i+n);
		else if(enable_illegal_ixcb_xh_instructions && n>=XH && n<=YL) // illegal: bit b,xh ***NOT ALL Z80 CPUs!***
		{
			int pfx = n>=YH ? PFX_IY : PFX_IX;
			n -= n>=YH ? YH-RH : XH-RH;
			store_XYCB_op(pfx, i+n, 0, true);
		}
		else
			goto ill_target;
		return;
	}
	case ' inc':
	{
		i = 0;
inc:	s=q.p;
		n=getRegister(q);

		if (n>=BC)
		{	if (i) i=DEC_BC-INC_BC;
			switch(n)
			{
			case BC:	storeOpcode(INC_BC+i); return;
			case DE:	storeOpcode(INC_DE+i); return;
			case HL:	storeOpcode(INC_HL+i); return;
			case SP:	storeOpcode(INC_SP+i); return;
			case IX:	store_IX_opcode(INC_HL+i); return;
			case IY:	store_IY_opcode(INC_HL+i); return;
			default:	goto ill_reg;
			}
		}

		if (n>=XH)					// XH XL YH YL
		{
			if (i) i=DEC_H-INC_H;	// 1
			switch(n)
			{
			case XH:	store_IX_opcode(INC_H+i); return;
			case XL:	store_IX_opcode(INC_L+i); return;
			case YH:	store_IY_opcode(INC_H+i); return;
			case YL:	store_IY_opcode(INC_L+i); return;
			default:	goto ill_reg;
			}
		}

		if (i) i=DEC_B-INC_B;	// 1

		if (n==OPEN)			// (HL) (IX+dis) (IY+dis)
		{
			switch(getRegister(q))
			{
			case HL:
				storeOpcode(INC_xHL+i);
				q.expectClose(); return;
			case IX: 				// (IX) or (IX±dis)
				n=0; v=1; if(q.peekChar()!=')') n = value(q,pAny,v);
				store_IX_byte_opcode(INC_xHL+i,n,v);
				q.expectClose(); return;
			case IY: 				// (IY) or (IY±dis)
				n=0; v=1; if(q.peekChar()!=')') n = value(q,pAny,v);
				store_IY_byte_opcode(INC_xHL+i,n,v);
				q.expectClose(); return;
			default:
				goto ill_reg;
			}
		}

		if(n>=RB)				// B C D E H L A
		{
			storeOpcode(INC_B+i+n*8); return;
		}

		else					// dis(IX)  dis(IY)
		{
			XXXASSERT(n==-1);
			q.p=s;	// unGetWord
			n=value(q,pAny,v=1);
			q.expect('(');
			int r=getRegister(q);
			if(r!=IX&&r!=IY) goto ix_iy_expected;
			q.expectClose();
			store_XY_byte_op(r==IX?PFX_IX:PFX_IY,INC_xHL+i,n,v);
			return;
		}

	}
	case ' out':
	{
		s=q.p;
		q.expect('(');
		n=getRegister(q);
		if(n<0)
		{
			q.p=s; n=value(q,pAny,v=1);
			q.expectComma();
			if(getRegister(q)!=RA) goto ill_reg;
			storeOpcode(OUTA);
			storeByte(n,v);
			return;
		}
		if (n!=RC&&n!=BC) goto ill_reg;
		q.expectClose();
		q.expectComma();
		s=q.p;
		n = getRegister(q);
		if(n==OPEN) goto ill_reg;
		if(n<0)   { q.p=s; if(value(q,pAny,v=1)!=0 && v) goto ill_reg; else n=OPEN; }
		if(n<=RA) { store_ED_opcode(OUT_xC_B+n*8); return; }
		goto ill_reg;
	}
	case ' ret':
	{
		if(q.testEol()) { storeOpcode(RET); return; }
		storeOpcode( RET_NZ+getCondition(q.nextWord()) );
		return;
	}
	case ' pop':
	{
		i = 0;
pop:	switch( getRegister(q) )
		{
		case BC:	storeOpcode(POP_BC+i); return;
		case DE:	storeOpcode(POP_DE+i); return;
		case HL:	storeOpcode(POP_HL+i); return;
		case AF:	storeOpcode(POP_AF+i); return;
		case IX:	store_IX_opcode(POP_HL+i); return;
		case IY:	store_IY_opcode(POP_HL+i); return;
		}
		goto ill_reg;
	}
	}


// opcode len = 4:

wlen4:
	switch(peek4X(w)|0x20202020)
	{
	default:		goto unknown_opcode;	// error
	case 'defs':	// space
	{
ds:		q.is_data = yes;
		n = value(q,pAny,v=1);
		if(q.testComma()) { bool u=1; storeSpace(n,v,value(q,pAny,u)); } else storeSpace(n,v);
		return;
	}
	case 'defw':
	{
dw:		q.is_data = yes;
		do { storeWord(value(q,pAny,v=1)); } while(q.testComma());
		return;
	}
	case 'defb':	// defb und defm synonym behandeln.
	case 'defm':	// erlaube jede Mixtur von literal, label, "text", 'c' Char, $abcdef stuffed hex, usw.
	{
dm:db:	q.is_data = yes;
		w = q.nextWord();
		if(*w==0) throw syntax_error("value expected");

	// Text string:
		if(w[0]=='"')
		{
			n = strlen(w);
			if(n<3 || w[n-1]!='"') throw syntax_error("closing quotes expected");
			w = unquotedstr(w);
cb:			if(charset) { w = (cstr) charset->translate(w); n = (uint8) *w++; }
			else		{ w = fromutf8str(w); n = strlen(w); }
			storeBlock(w,n);
			q.skip_spaces();
			if(q.test_char('+'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() + n,v); } else
			if(q.test_char('-'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() - n,v); } else
			if(q.test_char('|'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() | n,v); } else
			if(q.test_char('&'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() & n,v); } else
			if(q.test_char('^'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() ^ n,v); } else
			if(q.test_char('*'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() * n,v); } else
			if(q.test_char('%'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() % n,v); } else
			if(q.test_char('\\'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() % n,v); } else
			if(q.test_char('/'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() / n,v); }
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
		if(*w=='_')
		{
			if(eq(w,"__date__")) { w = datestr(timestamp); w += *w==' ';  goto cb; }
			if(eq(w,"__time__")) { w = timestr(timestamp); w += *w==' ';  goto cb; }
			if(eq(w,"__file__")) { w = q.sourcefile; goto cb; }
		}

	// anything else:
		q -= strlen(w);	// put back opcode
		n=value(q,pAny,v=1); storeByte(n,v);
		if(q.testComma()) goto dm; else return;
	}
	case 'rlca':	storeOpcode(RLCA); return;
	case 'rrca':	storeOpcode(RRCA); return;
	case 'halt':	storeOpcode(HALT); return;
	case 'outi':	store_ED_opcode(OUTI); return;
	case 'outd':	store_ED_opcode(OUTD); return;
	case 'ldir':	store_ED_opcode(LDIR); return;
	case 'cpir':	store_ED_opcode(CPIR); return;
	case 'inir':	store_ED_opcode(INIR); return;
	case 'otir':	store_ED_opcode(OTIR); return;
	case 'lddr':	store_ED_opcode(LDDR); return;
	case 'cpdr':	store_ED_opcode(CPDR); return;
	case 'indr':	store_ED_opcode(INDR); return;
	case 'otdr':	store_ED_opcode(OTDR); return;
	case 'reti':	store_ED_opcode(RETI); return;
	case 'retn':	store_ED_opcode(RETN); return;
	case 'push':	i = PUSH_BC-POP_BC; goto pop;
	case 'call':
	{
		s=q.p; w=q.nextWord();
		if(q.testComma())	i = CALL_NZ + getCondition(w); 	// conditional jump
		else			    i = CALL, q.p=s;	 			// unconditional jump
		n=value(q,pAny,v=1);
		storeOpcode(i);
		storeWord(n);
		return;
	}
	case 'djnz':
	{
		n = value(q,pAny,v=1);		// vor storeOpcode(DJNZ) wg. Bezug eines evtl. genutzen $
		storeOpcode(DJNZ);
		storeOffset( n - (currentAddress()+1), v && currentAddressValid() );
		return;
	}
	}

wlenXL:
	if(eq(w,"align"))				// align <value> [,<filler>]
	{								// note: current address is evaluated as uint
		q.is_data = yes;
		n = value(q,pAny,v=1);
		if(v&&n<1) throw syntax_error("alignment value must be ≥ 1");
		if(v&&n>0x4000) throw syntax_error("alignment value must be ≤ $4000");

		int32 a = current_segment_ptr->logicalAddress();
		v = v && current_segment_ptr->logicalAddressValid();
		if(v && a<0 && (1<<(msbit(n)))!=n) throw syntax_error("alignment value must be 2^N if $ < 0");

		n = n-1 - ((uint16)a+n-1) % n;

		if(q.testComma()) { bool u=1; storeSpace(n,v,value(q,pAny,u)); } else storeSpace(n,v);
	}
	else goto unknown_opcode;		// error


// generate error
unknown_opcode:	throw syntax_error(catstr("unknown opcode: ",w));
ill_reg:		throw syntax_error("illegal register");
ill_cond:		throw syntax_error("illegal condition");
ill_target:		throw syntax_error("illegal target");
ill_source:		throw syntax_error("illegal source");
ill_dest:		throw syntax_error("illegal destination");
ix_iy_expected:	throw syntax_error("IX or IY expected");
}

#else


// --------------------------------------------------
//				Assemble Opcode
// --------------------------------------------------


/*	store signed or unsigned byte
	validates byte if valid=true
*/
void Z80Assembler::storeByte(int byte, bool valid) throw(any_error)
{
	if(valid && (byte>255||byte<-128)) throw syntax_error( "byte value out of range" );
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


// enumeration of identifiers:
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
	}
	else if(*w==0)	// strlen = 2
	{
		if(c1=='n') { if(c2=='z') return NZ; if(c2=='c') return NC; }
		if(c1=='p') { if(c2=='o') return PO; if(c2=='e') return PE; }
	}
	throw syntax_error("illegal condition");
}


/*	test and skip over register or value
	in:	q: source line
		v: selects error message
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

	char c1 = *w++ | 0x20;	if(c1==0) throw syntax_error("unexpected end of line");
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
		case 'i':	if(!target_8080) return RI;
		case 'r':	if(!target_8080) return RR;

is_z80_reg:			if(registers_8080) break;
no_8080_reg:		throw syntax_error("no 8080 register");

		case '(':
			{
				if(q.testWord("hl")) { q.expect(')'); return XHL; }
				if(q.testWord("de")) { q.expect(')'); return XDE; }
				if(q.testWord("bc")) { q.expect(')'); return XBC; }
				if(q.testWord("sp")) { q.expect(')'); return XSP; }

				int r = XNN; n=0; v=1;

				if(!registers_8080)
				{
					if(q.testWord("ix")) { if(target_8080) goto no_8080_reg; r = XIX; if(q.testChar(')')) return r; }
					if(q.testWord("iy")) { if(target_8080) goto no_8080_reg; r = XIY; if(q.testChar(')')) return r; }
				}
				if(q.testWord("c"))  { if(target_8080) goto no_8080_reg; q.expect(')'); return XC; }

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
		case 'i':	if(target_8080 && (c2=='x' || c2=='y')) goto is_z80_reg; else
					if(c2=='x') return IX; else if(c2=='y') return IY; else break;
		case 'x':	if(target_8080 && (c2=='h' || c2=='l')) goto is_z80_reg; else
					if(c2=='h') return XH; else if(c2=='l') return XL; else break;
		case 'y':	if(target_8080 && (c2=='h' || c2=='l')) goto is_z80_reg; else
					if(c2=='h') return YH; else if(c2=='l') return YL; else break;
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

	switch(strlen(w))
	{
	case 0:		return;					// end of line
	case 1:		goto wlen1;
	case 2:		goto wlen2;
	case 3:		goto wlen3;
	case 4:		goto wlen4;
	default:	goto wlenXL;
	}


// opcode len = 1:

wlen1:
	if(*w=='.')							// SDASZ80 opcodes
	{
		w = lowerstr(q.nextWord());

		if(eq(w,"module"))				// for listing
		{
			q.skip_to_eol();
			return;
		}
		if(eq(w,"optsdcc"))				// .optsdcc -mz80
		{
			if(!q.testChar('-') )		throw syntax_error("-mz80 expected");
			if(ne(q.nextWord(),"mz80"))	throw syntax_error("-mz80 expected");
			return;
		}
		if(eq(w,"area"))				// select segment for following code
		{
			cstr name = upperstr(q.nextWord());
			if(!is_letter(*name) && *name!='_') throw fatal_error("segment name expected");
			Segment* segment = segments.find(name);
			if(!segment) throw fatal_error("segment not found");

			current_segment_ptr = segment;
			q.segment = current_segment_ptr;
			q.byteptr = currentPosition();
			XXXASSERT(q.bytecount==0);

			if((eq(name,"_CABS")||eq(name,"_DABS")||eq(name,"_RSEG"))	// SDCC generates: " .area _CABS (ABS)"
				&& q.testChar('('))										// KCC  generates: " .area _RSEG (ABS)"
			{
				if(!q.testWord("ABS")) throw syntax_error("'ABS' expected");
				q.expect(')');
			}
			return;
		}
		if(eq(w,"globl"))				// declare global label for linker: mark label for #include library "libdir"
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
		if(eq(w,"ds"))	 goto ds;
		if(eq(w,"dw"))	 goto dw;
		if(eq(w,"db"))	 goto db; // SDASZ80: truncates value to byte (not implemented, done if used this way by SDCC)
		if(eq(w,"byte")) goto db; // SDASZ80: truncates value to byte (not implemented, done if used this way by SDCC)
		if(eq(w,"org"))	 goto org;
		if(eq(w,"ascii"))goto dm;
		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
	}
	goto unknown_opcode;	// error


// opcode len = 2:

wlen2:
	switch(peek2X(w)|0x20202020)
	{
	default:		goto unknown_opcode;	// error
	case '  db':	goto db;
	case '  dw':	goto dw;
	case '  ds':	goto ds;
	case '  dm':	goto dm;
	case '  ei':	store(EI); return;
	case '  di':	store(DI); return;

	case '  jp':
		// jp NN
		// jp rr	hl ix iy
		// jp (rr)	hl ix iy
		r2 = getCondition(q,yes);
		r  = getRegister(q,n,v);
		if(r==NN) { store(r2==NIX ? JP : JP_NZ+(r2-NZ)*8); storeWord(n); return; }
		if(target_8080) goto ill_8080;
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
		if(r2==XNN)
		{
			if(r!=RA) goto ill_dest;
			store(INA); storeByte(n2,v2); return;
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
					if(r2==AF) store(EX_AF_AF); else goto ill_source; q.expect('\''); return; }
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
		if(r==NN) { store(instr+CP_N-CP_B); storeByte(n,v); return; }	// TODO:	8080: CP => CALL P,nnnn

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
			// ld iy,rr		bc de			Goodie
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
			// ld bc(hl)					Goodie
			// ld bc,(ix+d)					Goodie
			if(r2==NN)  { store(LD_BC_NN); storeWord(n2); return; }
			if(r2==XNN) { storeEDopcode(LD_BC_xNN); storeWord(n2); return; }
			if(r2==DE)  { store(LD_B_D, LD_C_E); return; }
			if(r2==HL)  { store(LD_B_H, LD_C_L); return; }
			if(r2==IX)  { store(PFX_IX, LD_B_H, PFX_IX, LD_C_L); return; }
			if(r2==IY)  { store(PFX_IY, LD_B_H, PFX_IY, LD_C_L); return; }
			if(r2==XHL)	{ store(LD_C_xHL, INC_HL, LD_B_xHL, DEC_HL); return; }
			if(r2==XIX)	{ store(PFX_IX, LD_C_xHL); storeOffset(n2,v2); store(PFX_IX, LD_B_xHL); storeOffset(n2+1,v2); return; }
			if(r2==XIY)	{ store(PFX_IY, LD_C_xHL); storeOffset(n2,v2); store(PFX_IY, LD_B_xHL); storeOffset(n2+1,v2); return; }
			goto ill_source;

		case DE:
			// ld de,NN
			// ld de,(NN)
			// ld de,rr		bc hl ix iy		Goodie
			// ld de(hl)					Goodie
			// ld de,(ix+d)					Goodie
			if(r2==NN)  { store(LD_DE_NN); storeWord(n2); return; }
			if(r2==XNN) { storeEDopcode(LD_DE_xNN); storeWord(n2); return; }
			if(r2==BC)  { store(LD_D_B, LD_E_C); return; }
			if(r2==HL)  { store(LD_D_H, LD_E_L); return; }
			if(r2==IX)  { store(PFX_IX, LD_D_H, PFX_IX, LD_E_L); return; }
			if(r2==IY)  { store(PFX_IY, LD_D_H, PFX_IY, LD_E_L); return; }
			if(r2==XHL)	{ store(LD_E_xHL, INC_HL, LD_D_xHL, DEC_HL); return; }
			if(r2==XIX)	{ store(PFX_IX, LD_E_xHL); storeOffset(n2,v2); store(PFX_IX, LD_D_xHL); storeOffset(n2+1,v2); return; }
			if(r2==XIY)	{ store(PFX_IY, LD_E_xHL); storeOffset(n2,v2); store(PFX_IY, LD_D_xHL); storeOffset(n2+1,v2); return; }
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
			if(r2==NN) { store(instr,LD_xHL_N); storeOffset(n,v); storeByte(n2,v2); return; }
			if(r2==HL) { store(instr,LD_xHL_L); storeOffset(n,v); store(instr, LD_xHL_H); storeOffset(n+1,v); return; }
			if(r2==DE) { store(instr,LD_xHL_E); storeOffset(n,v); store(instr, LD_xHL_D); storeOffset(n+1,v); return; }
			if(r2==BC) { store(instr,LD_xHL_C); storeOffset(n,v); store(instr, LD_xHL_B); storeOffset(n+1,v); return; }
			goto ill_source;

		case XHL:
			// ld (hl),r		a b c d e h l a
			// ld (hl),n
			// ld (hl),rr		bc de			Goodie
			if(r2<=RA && r2!=XHL) { store(LD_xHL_B+r2-RB); return; }
			if(r2==NN) { store(LD_xHL_N); storeByte(n2,v2); return; }
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
			if(r2==NN)  { store(LD_B_N+(r-RB)*8); storeByte(n2,v2); return; }
			if(r2==XH||r2==XL) { store(PFX_IX,LD_B_H+(r2-XH)+(r-RB)*8); return; }
			if(r2==YH||r2==YL) { store(PFX_IY,LD_B_H+(r2-YH)+(r-RB)*8); return; }
			goto ill_source;

		case NN:
			goto ill_dest;

		default:
			//IERR();
			goto ill_dest;
		}
	}


// opcode len = 3:

wlen3:
	switch(peek3X(w)|0x20202020)
	{
	default:		goto unknown_opcode;	// error
	case ' mov':	throw fatal_error("this is 8080 assembler source: 'mov' is no Z80 instruction. (TODO)");
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
	case ' sll':	instr = SLL_B; goto rr;
	case ' srl':	instr = SRL_B; goto rr;

	case ' slp':
		if(!target_hd64180) goto ill_hd64180;
		storeEDopcode(0x76); return;

	case ' in0':
		// in0 r,(n)		a b c d e h l f
		if(!target_hd64180) goto ill_hd64180;
		if(q.testWord("f")) r = XHL;
		else { r = getRegister(q,n,v); if(r>RA||r==XHL) goto ill_dest; }
		q.expectComma();
		r2 = getRegister(q,n2,v2);
		if(r2==XNN) { storeEDopcode(0x00+8*(r-RB)); storeByte(n2,v2); return; }
		goto ill_source;

	case ' tst':
		// tst r		b c d e h l (hl) a
		// tst n
		if(!target_hd64180) goto ill_hd64180;
		r = getRegister(q,n,v);
		if(r==NN) { storeEDopcode(0x64); storeByte(n,v); return; }
		if(r<=RA) { storeEDopcode(0x04+8*(r-RB)); return; }
		goto ill_source;

	case ' org':
org:	// org <value>	; set "logical" code address
		// org $$		; $$ = "physical" code address = segment.address + dpos
		n = value(q, pAny, v=1);
		current_segment().setOrigin(n,v);
		return;

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

	case ' set':
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

		r = getRegister(q,n,v);
		depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);

		if(r==XC || r==XBC)
		{
			if(r2<=RA && r2!=XHL) { storeEDopcode(OUT_xC_B+(r2-RB)*8); return; }
			if(r2==NN) { if(v2&&n2!=0) goto ill_source; storeEDopcode(OUT_xC_0); return; }
			goto ill_source;
		}
		if(r==XNN)
		{
			if(r2!=RA) goto ill_source;
			if(v && (n<-128||n>255)) q.p=depp;	// storeByte() will throw
			store(OUTA); storeByte(n,v); return;
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
	}


// opcode len = 4:

wlen4:
	switch(peek4X(w)|0x20202020)
	{
	default:		goto unknown_opcode;	// error

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

	case 'mult':
		// mult rr		bc de hl sp
		if(!target_hd64180) goto ill_hd64180;
		r = getRegister(q,n,v);
		if(r>=BC && r<=SP) { store(PFX_ED,0x4c+16*(r-BC)); return; }
		goto ill_source;

	case 'otim':
		if(!target_hd64180) goto ill_hd64180;
		store(PFX_ED,0x83); return;

	case 'otdm':
		if(!target_hd64180) goto ill_hd64180;
		store(PFX_ED,0x8b); return;

	case 'out0':
		// out0 (n),r		b c d e h l a
		if(!target_hd64180) goto ill_hd64180;
		r = getRegister(q,n,v); if(r!=XNN) goto ill_dest;
		depp=q.p; q.expectComma();
		r2 = getRegister(q,n2,v2);
		if(r2<=RA && r2!=XHL)
		{
			if(v && (n<-128||n>255)) q.p=depp;	// storeByte() will throw
			store(PFX_ED, 0x01+8*(r2-RB)); storeByte(n,v); return;
		}
		goto ill_source;

	case 'defs':
		// store space: (gap)
		// defs cnt
		// defs cnt, fillbyte
ds:		q.is_data = yes;
		n = value(q,pAny,v=1);
		if(q.testComma()) storeSpace(n,v,value(q,pAny,v2)); else storeSpace(n,v);
		return;

	case 'defw':
		// store words:
		// defw nn [,nn ..]
dw:		q.is_data = yes;
		do { storeWord(value(q,pAny,v=1)); } while(q.testComma());
		return;

	case 'defb':
		// store bytes:
		// defb n [, …]
db:		q.is_data = yes;
		do { n=value(q,pAny,v=1); storeByte(n,v); } while(q.testComma());
		return;

	case 'defm':
		// store bytes:
		// erlaubt jede Mixtur von literal, label, "text", 'c' Char, $abcdef stuffed hex, usw.
		// ACHTUNG: '…' wird als String behandelt! Das wird z.B. im Source  des ZXSP-Roms so verwendet.
		// defb n, "…", "…"+n, '…', '…'+n, 0xABCDEF…, __date__, __time__, __file__, …
dm:		q.is_data = yes;
		w = q.nextWord();
		if(w[0]==0) throw syntax_error("value expected");

	// Text string:
		if(w[0]=='"' || w[0]=='\'')
		{
			n = strlen(w);
			if(n<3 || w[n-1]!=w[0]) throw syntax_error("closing quotes expected");
			w = unquotedstr(w);
cb:			if(charset) { w = (cstr) charset->translate(w); n = (uint8) *w++; }
			else		{ w = fromutf8str(w); n = strlen(w); }
			storeBlock(w,n);
			q.skip_spaces();
			if(q.test_char('+'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() + n,v); } else
			if(q.test_char('-'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() - n,v); } else
			if(q.test_char('|'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() | n,v); } else
			if(q.test_char('&'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() & n,v); } else
			if(q.test_char('^'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() ^ n,v); } else
			if(q.test_char('*'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() * n,v); } else
			if(q.test_char('%'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() % n,v); } else
			if(q.test_char('\\'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() % n,v); } else
			if(q.test_char('/'))	{ n=value(q,pAny,v=1); storeByte(popLastByte() / n,v); }
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
		}

	// anything else:
		q -= strlen(w);	// put back opcode
		n = value(q,pAny,v=1); storeByte(n,v);
		if(q.testComma()) goto dm; else return;

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
	}

wlenXL:
	w = lowerstr(w);
	if(eq(w,"align"))	// align <value> [,<filler>]
	{					// note: current address is evaluated as uint
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
	else if(eq(w,"otimr"))
	{
		if(!target_hd64180) goto ill_hd64180;
		store(PFX_ED,0x93); return;
	}
	else if(eq(w,"otdmr"))
	{
		if(!target_hd64180) goto ill_hd64180;
		store(PFX_ED,0x9b); return;
	}
	else if(eq(w,"tstio"))
	{
		// tstio n
		if(!target_hd64180) goto ill_hd64180;
		r = getRegister(q,n,v);
		if(r==NN) { store(PFX_ED,0x74); storeByte(n,v); return; }
		goto ill_source;
	}
	else goto unknown_opcode;		// error


// generate error
unknown_opcode:	throw syntax_error("unknown opcode: ");
ill_target:		if(depp) q.p=depp; throw syntax_error("illegal target");		// 1st arg
ill_source:		throw syntax_error("illegal source");		// 2nd arg
ill_dest:		if(depp) q.p=depp; throw syntax_error("illegal destination");	// jp etc., ld, in, out: destination
ill_hd64180:	throw syntax_error("hd64180 opcode (no option --hd64180)");
ill_8080:		throw syntax_error("no i8080 opcode (option --8080)");
}



#endif



























