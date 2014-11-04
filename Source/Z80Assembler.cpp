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
#include <sys/time.h>
#include "unix/FD.h"
#include "unix/files.h"
#include "Z80Assembler.h"
#include "Segment.h"
#include "Z80/Z80opcodes.h"
#include "Templates/HashMap.h"
extern char** environ;



// Priorities for Z80Assembler::value(…)
//
enum
{ 	pAny = 0,		// whole expression: up to ')' or ','
	pCmp, 			// comparisions:	 lowest priority
	pAdd, 			// add, sub
	pMul, 			// mul, div, rem
	pBoo, 			// bool/masks:		 higher than add/mult
	pRot, 			// rot/shift
	pUna			// unary operator:	 highest priority
};


static double now()
{
	struct timeval tv;
	gettimeofday ( &tv, NULL );
	return tv.tv_sec + tv.tv_usec/1000000.0;
}


// name for default code segment, if no #target is given:
//
const char DEFAULT_CODE_SEGMENT[] = "(none)";


// for character set translation:
//
char const zx80_chars[] = " \"###########$:?()-+*/=><;,.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";	// char(12)='£'
char const zx81_chars[] = " ##########\"#$:?()><=+-*/;,.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";	// char(12)='£'



// --------------------------------------------------
//					Creator
// --------------------------------------------------


Z80Assembler::Z80Assembler()
:	timestamp(now()),
	source_directory(NULL),
	source_filename(NULL),
	temp_directory(NULL),
	target(NULL),
	current_sourceline_index(0),
	current_segment_ptr(NULL),
	local_labels_index(0),
	local_blocks_count(0),
//	temp_label_seen(no),
	cond_off(0),
	max_errors(5),	// 30
	pass(0),
	final(0),
	end(0),
	verbose(1),
	c_compiler(NULL),
	c_qi(0),
	c_zi(0)
{
	cond[0] = no_cond;			// memset(cond,no_cond,sizeof(cond));
//	temp_label_suffix[0] = 0;	// memcpy(temp_label_suffix,"_0",3);
	c_flags.append("-mz80");
	c_flags.append("-S");
}


// --------------------------------------------------
//					Helper
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
void Z80Assembler::assembleFile( cstr sourcefile, cstr destpath, cstr listpath, cstr temppath,
								 int liststyle, int deststyle ) throw()
{
	sourcefile = fullpath(sourcefile);			XXASSERT(errno==ok && is_file(sourcefile));
	if(destpath) destpath = fullpath(destpath); XXASSERT(errno==ok || errno==ENOENT);
	if(listpath) listpath = fullpath(listpath); XXASSERT(errno==ok || errno==ENOENT);
	if(temppath) temppath = fullpath(temppath); XXASSERT(errno==ok && is_dir(temppath));

	XXASSERT(liststyle>=0 && liststyle<=7);
	XXASSERT(deststyle==0 || deststyle=='b' || deststyle=='x' || deststyle=='s');


	source_directory = directory_from_path(sourcefile);
	source_filename  = filename_from_path(sourcefile);
	cstr basename    = basename_from_path(source_filename);

	cstr dest_directory = destpath ? directory_from_path(destpath) : destpath=source_directory;
	XXXASSERT(is_dir(dest_directory));

	cstr list_directory = listpath ? directory_from_path(listpath) : listpath=dest_directory;
	XXXASSERT(is_dir(list_directory));

	temp_directory = temppath ? temppath : dest_directory;
	XXXASSERT(is_dir(temp_directory));

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
	labels.append(new Labels(0));			// global_labels must exist

	// setup segments:
	segments.purge();
	segments.append(new Segment(DEFAULT_CODE_SEGMENT,no,0xff,no,yes));	// current_segment must exist
	segments[0].address_valid = yes;		// "physical" address = $0000 is valid
	segments[0].org_valid = yes;			// "logical"  address = $0000 is valid too
	global_labels().add(new Label(DEFAULT_CODE_SEGMENT,&segments[0],0,0,yes));

	// setup errors:
	errors.purge();

	// setup conditional assembly:
	cond_off = 0x00;
	cond[0] = no_cond;

	Array<Segment*> oldsegs;

	// DOIT:
	for(pass=1,final=no; pass<9 && !final && errors.count()==0; pass++)
	{
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
//		temp_label_seen = no;
//		memcpy(temp_label_suffix,"$0",3);
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
			if(pass==1) { assert(oldsegs.count()==i); oldsegs.append(source[i].segment); }			// TODO XXX
			else assert(oldsegs[i]==source[i].segment);
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
#ifndef NDEBUG					// test suite:
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
#endif
	else						// [label:] + opcode
	{
		if((uint8)q[0] > ' ' && q[0]!=';') asmLabel(q);	// label definition
		asmInstr(q);			// opcode or pseudo opcode
		q.expectEol();			// expect end of line

		if(q.segment==current_segment_ptr) q.bytecount = currentPosition() - q.byteptr;
		else
		{
			q.segment = current_segment_ptr;	// .area instruction
			q.byteptr = currentPosition();		// Für Temp Label Resolver & Logfile
			XXXASSERT(q.bytecount==0);
		}
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
	cstr w = q.nextWord();				// get next word
	if(w[0]==0) goto syntax_error;		// empty word

	if(w[1]==0)							// 1 char word
	{
		switch(w[0])
		{
		case ';':	throw syntax_error("value expected");	// comment  =>  unexpected end of line
		case '+':	n = +value(q,pUna,valid); goto op;		// plus sign
		case '-':	n = -value(q,pUna,valid); goto op;		// minus sign
		case '~':	n = ~value(q,pUna,valid); goto op;		// complement
		case '!':	n = !value(q,pUna,valid); goto op;		// negation
		case '(':	n =  value(q,pAny,valid); q.expect(')'); goto op;	// brackets
		case '$':	n = currentAddress();					// $ = "logical" address at current code position
					valid = valid && currentAddressValid();
					if(!valid) final = false; goto op;
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
		{
			if(w[2]==0) goto syntax_error;
			while (w[2]) { w++; n = (n<<8) + *w; }
			if(w[1]=='\'') goto op; else goto syntax_error;
		}
		else if(is_dec_digit(w[0]))	// decimal number
		{
			if(w[0]=='0')
			{
				if(tolower(w[1])=='x' && w[2]) { w+=2; goto hex_number; }	// 0xABCD
				if(tolower(w[1])=='b' && w[2]) { w+=2; goto bin_number; }	// 0b0101
			}
			c = tolower(lastchar(w));
			if( c=='h' ) goto hex_number;	// hex number    indicated by suffix
			if( c=='b' ) goto bin_number;	// binary number indicated by suffix
		}
	}

	if(is_dec_digit(w[0]))			// decimal number or temp. label
	{
		if(q.test_char('$'))		// reusable label
		{
			w = catstr(reusable_label_basename,".",w,"$");
//			temp_label_seen = true;
			goto label;
		}
		else						// decimal number
		{
			while(is_dec_digit(*w)) { n = n*10 + *w-'0'; w++; }
			if(*w!=0) goto syntax_error;
			goto op;
		}
	}

	if(is_idf(w[0]))				// name
	{
label:	Label* l = &local_labels().find(w);
		if(l)						// lokales Label
		{
			n = l->value;
			valid = valid && l->is_valid;
			if(!valid) final = false;
		}
		else if(local_labels_index!=0 && (l = &global_labels().find(w)))		// globales Label
		{
			n = l->value;
			valid = valid && pass>1 && l->is_valid;	// in pass1 kann ein gefundenes glob. label noch durch
													// ein später definiertes lokales label verdeckt werden
			if(!valid) final = false;
		}
		else	// Label nicht gefunden
		{
			if(pass>1) throw syntax_error(usingstr("label \"%s\" not found",w));
			//n = 0;
			valid = no;
			final = false;
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
	case '&':	if(pBoo<=prio) break; n = n & value(++q,pBoo,valid); goto op;	// boolean and
	case '|':	if(pBoo<=prio) break; n = n | value(++q,pBoo,valid); goto op;	// boolean or
	case '^':	if(pBoo<=prio) break; n = n ^ value(++q,pBoo,valid); goto op;	// boolean xor

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


/*	handle label definition
	Unterscheidet global - lokal - temporär
	Bei jedem nicht-temporären echten Label wird der temp_label_suffix erhöht
*/
void Z80Assembler::asmLabel(SourceLine& q) throw(any_error)
{
//	label	equ		expr			add "::" for global labels
//	label	defl	expr			add "::" for global labels
// SDASZ80:
//	0123$:							reusable label
//	label:							local label  (except if declared with .globl)
//	label::							global label
//	label	=		expr			label
//	label	==		expr			global label
//	label	=:		expr			local label
//	label	.equ    expr			label
//	label	.gblequ expr			global label
//	label	.lclequ expr			local label

#ifdef NDEBUG
	XXXASSERT(q.p==q.text);			// may happen in test suite: lines with "!" which must fail
#endif

	cstr name = q.nextWord();
	bool is_reusable = is_dec_digit(name[0]) && q.test_char('$');	// sdasz80 syntax
	bool is_global   = q.test_char(':') && !is_reusable && q.test_char(':');

	cstr s = q.p;
	cstr w = q.nextWord();
	if(w[0]=='.')							// sdasz80
	{
		w = q.nextWord();
		if(eq(w,"equ"))			{}									// TODO: sdasz80 hat evtl. andere syntax
		else if(eq(w,"gblequ"))	{ w="equ"; is_global = true; }		// TODO: sdasz80 hat evtl. andere syntax
		else if(eq(w,"lclequ"))	{ w="equ"; is_global = false; }		// TODO: sdasz80 hat evtl. andere syntax
		else throw(syntax_error(catstr("unknown opcode .",w)));
	}
	if(w[0]=='=')							// sdasz80
	{
		if(w[1]==':') { w = "equ"; is_global = false; }				// TODO: sdasz80 hat evtl. andere syntax
		if(w[1]=='=') { w = "equ"; is_global = true; }				// TODO: sdasz80 hat evtl. andere syntax
		if(w[1]==0)   { w = "equ"; }								// TODO: sdasz80 hat evtl. andere syntax
	}

//	XXXASSERT(is_reusable == is_dec_digit(name[0]));				// Assumption: SDCC does not use it vice versa
//	if(is_reusable) { name = catstr(name,temp_label_suffix); temp_label_seen = true; }
	if(is_reusable) name = catstr(reusable_label_basename,".",name,"$");

	bool is_valid;
	int32 value;

	if(eq(w,"equ") || eq(w,"defl"))
	{
		value = this->value(q,pAny,is_valid=true);	// calc assigned value
	}
	else	// label: <opcode>
	{
		q.p = s;									// put back opcode
		value = currentAddress();
		is_valid = currentAddressValid();

		if(!is_reusable) reusable_label_basename = name;

//		// increment temp_label_suffix:
//		if(temp_label_seen)
//		{
//			ptr p = temp_label_suffix +1;	// suffix = "_12345"
//			for(;;)
//			{
//				if(++*p <= '9') break;		// incr. char from '0' -> '9'
//				*p++ = '0';					// overflow => char := '0'
//				if(*p) continue;			// and incr. next char
//				*p++='0'; *p=0;				// at end of string append '0'
//				break;
//			}
//			temp_label_seen = no;
//		}
	}

	Labels& labels = is_global ? global_labels() : local_labels();
	Label* l = &labels.find(name);

	if(l)
	{
		XXXASSERT(is_valid || !l->is_valid);
		XXXASSERT(l->segment == current_segment_ptr);
		XXXASSERT(l->sourceline == current_sourceline_index);

		if(l->is_valid && l->value!=value) throw syntax_error("value redefined");
		l->value    = value;
		l->is_valid = is_valid;
	}
	else
	{
		l = new Label(name, &current_segment(), current_sourceline_index, value, is_valid);
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
		if(eq(w,"end"))		  asmEnd(q);		else throw fatal_error("unknown assembler directive");
	}
	catch(fatal_error& e) { throw e; }
	catch(any_error& e)   { throw fatal_error(e.what()); }
}


#if 0
/*	#cc "path/to/c-compiler" <options>
	define path to c compiler and options
*/
void Z80Assembler::asmCc(SourceLine& q) throw(any_error)
{
	cstr compiler = q.nextWord();
	if(compiler[0]!='"') throw fatal_error("quoted filepath expected");
	compiler = unquotedstr(compiler);

	if(compiler[0]!='/')
	{
		Array<str> ss;
		split(ss, getenv("PATH"), ':');
		for(uint i=0; i<ss.count(); i++)
		{
			cstr s = catstr(ss[i],"/",compiler);
			if(is_file(s)) { compiler = s; break; }
		}
	}
	if(!exists_node(compiler))	 throw fatal_error("file not found");
	if(!is_file(compiler))		 throw fatal_error("not a regular file");
	if(!is_executable(compiler)) throw fatal_error("not executable");

	cc[0] = compiler;
	cc_qi = 0;
	cc_zi = 0;

	cstr options = "";		// accu for subdir in tempdir
	uint i = 1;
	while(!q.testEol())
	{
		if(i==NELEM(cc)-1) throw fatal_error("too many options to c-compiler call (max. 8 allowed)");
		cptr a = q.p;
		while((uint8)*q>' ') ++q;
		cstr s = substr(a,q.p);
		if(s[0]=='"') s = unquotedstr(s);
		if(s[0]=='$' && eq(s,"$$SOURCE$$")) cc_qi = i;
		if(s[0]=='$' && eq(s,"$$DEST$$"))   cc_zi = i;
		cc[i++] = s;
		options = catstr(options, " ", s);
	}
	cc[i] = NULL;

	if(cc_qi==0) throw fatal_error("argument '$$SOURCE$$' is missing");
	if(cc_zi==0) throw fatal_error("argument '$$DEST$$' is missing");

	cstr tempdir = "/tmp/";
	if(!is_dir(tempdir)) tempdir = getenv("TMPDIR");
	if(!is_dir(tempdir)) throw fatal_error("temp dir not found!");
	if(!is_writable(tempdir)) throw fatal_error("temp dir not writable!");
	cc_basedir = fullpath(catstr(tempdir, "/", filename_from_path(compiler), options, "/"),1,1);
	if(errno) throw fatal_error(usingstr("creating temp dir for intermediate files failed: %s",strerror(errno)));
}
#endif


/*	#CFLAGS -opt1 -opt2 …
	arguments may be quoted
	detects special arguments $SOURCE, $DEST and $CFLAGS
	note: argv[0] (the executable's path) is not included in c_flags[].
		  $SOURCE and $DEST may be present or missing: then c_qi or c_zi = 0
		  default in c'tor: c_flags = { "-S", "-mz80" }
		  in #include: default argv[] = { "…/sdcc", "-S", "-mz80", "-o", outfile, sourcefile }
*/
void Z80Assembler::asmCFlags( SourceLine& q ) throw(any_error)
{
	if(pass>1) { q.skip_to_eol(); return; }

	Array<cstr> old_cflags = c_flags;		// moves contents

	while(!q.testEol())
	{
		cptr a = q.p;
		while((uint8)*q>' ') ++q;
		cstr s = substr(a,q.p);
		if(s[0]=='"') s = unquotedstr(s);
		if(s[0]=='$' && eq(s,"$SOURCE")) c_qi = c_flags.count();
		if(s[0]=='$' && eq(s,"$DEST"))   c_zi = c_flags.count();
		if(s[0]=='$' && eq(s,"$CFLAGS"))
		{
			if(c_qi) c_qi += old_cflags.count();
			if(c_zi) c_zi += old_cflags.count();
			c_flags.append(old_cflags);	// moves contents
		}
		c_flags.append(s);
	}
}


/*	#end
	force end of assembler source
	must not be within #if …
*/
void Z80Assembler::asmEnd(SourceLine&) throw(any_error)
{
	end = true;
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

	bool v = yes;
	bool f = cond_off || value(q,pAny,v);			// higher nesting level off => ignore; else evaluate value
	if(!v) throw fatal_error("condition must be evaluatable in pass1");
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
		else f = value(q,pAny,v);	// else evaluate value
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


/*	#include "sourcefile"
	the file is included in pass 1
	filenames ending on ".c" are compiled with sdcc (or the preset compiler) into the temp directory
*/
void Z80Assembler::asmInclude( SourceLine& q ) throw(any_error)
{
	if(pass>1) { q.skip_to_eol(); return; }

	cstr fqn = q.nextWord();
	if(fqn[0]!='"') throw syntax_error("quoted filename expected");
	fqn = unquotedstr(fqn);
	if(fqn[0]!='/') fqn = catstr(directory_from_path(q.sourcefile),fqn);

	if(endswith(fqn,".c"))
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

			c_flags.purge();
		}

		if(c_flags.count()==0)			// --> sdcc -mz80 -S
		{
			c_flags.append("-mz80");
			c_flags.append("-S");
		}

		cstr fqn_q = fqn;
		cstr fqn_z = fqn = catstr(temp_directory, basename_from_path(fqn), ".s");

		// if the .a file does not exists or is older than the .c file, then compile the .c file:
		// note: this does not handle modified header files or modified CFLAGS
		if(!exists_node(fqn_z) || file_mtime(fqn_z) <= file_mtime(fqn_q))
		{
			pid_t child_id = fork();	// fork a child process
			XXXASSERT(child_id!=-1);	// fork failed: can't happen

			if(child_id==0)				// child process:
			{
				if(c_zi==0) { c_flags.append("-o"); c_flags.append(fqn_z); } else { c_flags[c_zi] = fqn_z; }
				if(c_qi==0) {                       c_flags.append(fqn_q); } else { c_flags[c_qi] = fqn_q; }
				c_flags.append(NULL);
				c_flags.insertat(0,c_compiler);

				execve(c_compiler, (char**)c_flags.getData(), environ);	// exec cmd
				exit(errno);			// exec failed: return errno: will be printed in error msg,
										//				but is ambiguous with cc exit code
			}
			else						// parent process:
			{
				int status;
				for(int err; (err = waitpid(child_id,&status,0)) != child_id; )
				{
					XXXASSERT(err==-1);
					if(errno!=EINTR) throw fatal_error(usingstr("waitpid: %s",strerror(errno)));
				}

				if(WIFEXITED(status))				// child process exited normally
				{
					if(WEXITSTATUS(status)!=0)		// child process returned error code
						throw fatal_error(usingstr("%s returned exit code %i",
							quotedstr(c_compiler), (int)WEXITSTATUS(status)));
				}
				else if(WIFSIGNALED(status))		// child process terminated by signal
				{
					throw fatal_error(usingstr("%s terminated by signal %i",
							quotedstr(c_compiler), (int)WTERMSIG(status)));
				}
				else IERR();
			}
		}
	}

	source.includeFile(fqn, current_sourceline_index+1);
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

	cstr name = upperstr(q.nextWord());
	if(!is_letter(*name) && *name!='_') throw fatal_error("segment name expected");
	Segment* segment = segments.find(name);
	XXXASSERT(!segment || eq(segment->name,name));
	if(segment && segment->is_data != is_data) throw fatal_error("#code/#data mismatch");
	if(pass==1 ? segment!=NULL : q.peekChar()!=',') { current_segment_ptr = segment; return; }	// --> expect eol

	int32 address	= 0;
	int32 size		= 0;
	int32 flags		= 0;
	bool  address_is_valid	= no;
	bool  size_is_valid		= no;
	bool  flags_is_valid	= no;
	bool  relocatable		= yes;
	bool  resizable			= yes;

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
		flags = value(q, pAny, flags_is_valid=yes);
		if(flags_is_valid && flags!=(uint8)flags) throw syntax_error("value out of range");
	}

	if(segment==NULL)	// new segment in pass 1
	{
		XXXASSERT(pass==1);

		uint8 fillbyte = is_data || ne(target,"ROM") ? 0x00 : 0xFF;
		segment = new Segment(name,is_data,fillbyte,relocatable,resizable);
		segments.append(segment);
		global_labels().add(new Label(name,segment,q.sourcelinenumber,address,address_is_valid));
		reusable_label_basename = name;
	}
	current_segment_ptr = segment;

	if(address_is_valid) { segment->setAddress(address); segment->setOrigin(address,yes); }	// throws
	if(size_is_valid)    { segment->setSize(size); }			// throws
	if(flags_is_valid)   { segment->setFlag(flags); }			// throws
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
	local_labels_index = local_labels().outer_index;
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
	if(*w=='.')	// SDASZ80
	{
		w = lowerstr(q.nextWord());

		if(eq(w,"module"))				// for listing
		{
			q.skip_to_eol();
			return;
		}
		if(startswith(w,"optsdcc"))		// .optsdcc -mz80
		{
			if(!q.testChar('-') )
				throw syntax_error("- expected");
			if(ne(q.nextWord(),"mz80"))
				throw syntax_error("mz80 expected");
			return;
		}
		if(startswith(w,"area"))		// select segment for following code
		{
			cstr name = upperstr(q.nextWord());
			if(!is_letter(*name) && *name!='_') throw fatal_error("segment name expected");
			Segment* segment = segments.find(name);
			if(!segment) throw fatal_error("segment not found");
			current_segment_ptr = segment;
			q.skip_to_eol();			// TODO: parse & validate acc. to SDASZ80 syntax
			return;
		}
		if(startswith(w,"globl"))		// declare global label for linker
		{
			q.skip_to_eol();
			if(pass==1) fprintf(stderr,"SDASZ80 opcode \".%s\": TODO\n",w);
			return;
		}
		if(startswith(w,"ds"))
			goto ds;
		if(startswith(w,"dw"))
		{
			q.expect('#');				// wahrscheinlich TODO ...
			goto dw;
		}
			throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
		if(startswith(w,"db"))
			throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
		if(startswith(w,"ascii"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
/*		if(eq(opcode,"title"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"sbttl"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"list"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"nlist"))			{ q.skip_to_eol(); return; }	// for listing
		if(eq(opcode,"page"))			{ q.skip_to_eol(); return; }	// for listing
		if(startswith(opcode,"if"))		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"iif"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(eq(opcode,"else"))			throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// after .if
		if(eq(opcode,"endif"))			throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// after .if
		if(startswith(opcode,"byte"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"fcb"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"word"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"fcw"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"3byte"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"triple"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"4byte"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"quad"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"blkb"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"rmb"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"rs"))		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"blkw"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"blk3"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"blk4"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"str"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"fcc"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"ascis"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"strs"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"asciz"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"strz"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"radix"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"even"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"odd"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"bndry"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"arg"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"local"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"equ"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"gblequ"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"lclequ"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"include"))throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"define"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"undefine"))throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"setdp"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"16bit"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"24bit"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"32bit"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"end"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
		if(startswith(opcode,"macro"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"endm"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"mexit"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"narg"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"nchr"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"ntyp"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"nval"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"irp"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"irpc"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"rept"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"mdelete"))throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"mlib"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
		if(startswith(opcode,"mcall"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));	// macro
*/
		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
	}


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
rr:		n = getRegister(q);
		if(n<RB||n>RA) goto ill_target;
		if(n==OPEN) 	// hl/ix/iy register indirect ?
		{
			switch(getRegister(q))
			{
			case IX:	n=value(q,pAny,v=1); store_IXCB_opcode(i+OPEN,n,v); break;
			case IY:	n=value(q,pAny,v=1); store_IYCB_opcode(i+OPEN,n,v); break;
			case HL:	store_CB_opcode  (i+OPEN);          break;
			default:	goto ill_reg;
			}
			q.expectClose();
			return;
		}
		store_CB_opcode(i+n);
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
			storeOpcode(i+CP_N-CP_B);
			n=value(q,pAny,v=1); storeByte(n,v);
			return;
		}
		case OPEN:		// hl/ix/iy register indirect ?
		{
			switch(getRegister(q))
			{
			case IX: 				// 2007-09-25 kio: (IX) w/o offset
				n=0; v=1; if(q.peekChar()!=')') n = value(q,pAny,v);
				store_IX_byte_opcode(i+r,n,v);
				break;
			case IY:				// 2007-09-25 kio: (IY) w/o offset
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
		i = getRegister(q);	if(i<0) goto ill_dest;
		if (i==OPEN)
		{
			s=q.p;
			i = getRegister(q);
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
		}
		else if (j==OPEN)
		{
			s=q.p;
			j = getRegister(q);
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
		// org <value>	; set "logical" code address
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
		n = getRegister(q);
		if(n<RB||n>RA) goto ill_target;
		if(n!=OPEN) { store_CB_opcode(i+n); return; }
		switch(getRegister(q))
		{
		case HL:
			store_CB_opcode(i+n);
			break;
		case IX: 				// 2007-09-25 kio: (IX) w/o offset
			m=0; v=1; if(q.peekChar()!=')') m = value(q,pAny,v);
			store_IXCB_opcode(i+n, m, v);
			break;
		case IY:				// 2007-09-25 kio: (IY) w/o offset
			m=0; v=1; if(q.peekChar()!=')') m = value(q,pAny,v);
			store_IYCB_opcode(i+n, m, v);
			break;
		default:
			goto ill_reg;
		}
		q.expectClose();
		return;
	}
	case ' inc':
	{
		i = 0;
inc:	n=getRegister(q);
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
			default:	goto ill_reg;	// added: zasm failed to detect some ill. condidions	2006-09-17 kio
			}
		}
		if (n>=XH)						// inc/dec XH..YL added		2006-09-17 kio
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
		if (n!=OPEN)
		{
			storeOpcode(INC_B+i+n*8); return;
		}
		switch(getRegister(q))
		{
		case HL:
			storeOpcode(INC_xHL+i);
			break;
		case IX: 				// 2007-09-25 kio: (IX) w/o offset
			n=0; v=1; if(q.peekChar()!=')') n = value(q,pAny,v);
			store_IX_byte_opcode(INC_xHL+i,n,v);
			break;
		case IY: 				// 2007-09-25 kio: (IX) w/o offset
			n=0; v=1; if(q.peekChar()!=')') n = value(q,pAny,v);
			store_IY_byte_opcode(INC_xHL+i,n,v);
			break;
		default:
			goto ill_reg;
		}
		q.expectClose();
		return;
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
ds:		n = value(q,pAny,v=1);
		if(q.testComma()) { bool u=1; storeSpace(n,v,value(q,pAny,u)); } else storeSpace(n,v);
		return;
	}
	case 'defw':
	{
dw:		do { storeWord(value(q,pAny,v=1)); } while(q.testComma());
		return;
	}
	case 'defb':	// defb und defm synonym behandeln.
	case 'defm':	// erlaube jede Mixtur von literal, label, "text", 'Text', 'c' Char, $abcdef stuffed hex, usw.
	{
dm:db:	w = q.nextWord();
		if(*w==0) throw syntax_error("value expected");

	// Text string or character literal:
		if(w[0]=='\'' || w[0]=='"')
		{
			n = strlen(w);
			if(n<2 || w[n-1]!=w[0]) throw syntax_error("closing quotes expected");
			w = unquotedstr(w);
cb:			storeBlock(w, strlen(w));
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
	if(eq(w,"align"))		// align <value> [,<filler>]	// note: current address is evaluated as uint
	{
		n = value(q,pAny,v=1);
		if(v&&n<1) throw syntax_error("alignment value must be ≥ 1");
		if(v&&n>0x4000) throw syntax_error("alignment value must be ≤ $4000");

		int32 a = current_segment_ptr->logicalAddress();
		v = v && current_segment_ptr->logicalAddressValid();
		if(v && a<0 && (1<<(msbit(n)))!=n) throw syntax_error("alignment value must be 2^N if $ < 0");

		n = n-1 - ((uint16)a+n-1) % n;

		if(q.testComma()) { bool u=1; storeSpace(n,v,value(q,pAny,u)); } else storeSpace(n,v);
	}
	else goto unknown_opcode;	// error


// generate error
unknown_opcode:	throw syntax_error(catstr("unknown opcode: ",w));
ill_reg:		throw syntax_error("illegal register");
ill_cond:		throw syntax_error("illegal condition");
ill_target:		throw syntax_error("illegal target");
ill_source:		throw syntax_error("illegal source");
ill_dest:		throw syntax_error("illegal destination");
}





























