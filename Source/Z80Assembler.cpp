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
#define LOG 1
#include <sys/time.h>
#include "unix/FD.h"
#include "unix/files.h"
#include "Z80Assembler.h"
#include "Segment.h"
#include "Z80/Z80opcodes.h"
#include "Templates/HashMap.h"


// Priorities for Z80Assembler::value(…)
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




// --------------------------------------------------
//					Creator
// --------------------------------------------------


Z80Assembler::Z80Assembler()
:	timestamp(now()),
	source_directory(NULL),
	source_filename(NULL),
	target(NULL),
	current_sourceline_index(0),
	current_segment_ptr(NULL),
	local_labels_index(0),
	local_blocks_count(0),
	temp_label_seen(no),
	cond_off(0),
	max_errors(5),	// 30
	pass(0),
	final(0)
{
	cond[0] = no_cond;			// memset(cond,no_cond,sizeof(cond));
	temp_label_suffix[0] = 0;	// memcpy(temp_label_suffix,"_0",3);
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
void Z80Assembler::assembleFile(cstr sname, cstr dname, cstr lname, bool v, bool w, char style) throw()
{
	XXASSERT(is_file(sname));

	source_directory = directory_from_path(sname);
	source_filename  = filename_from_path(sname);

	StrArray source;
	source.append( catstr("#include ", quotedstr(sname)) );
	assemble(source);

	if(dname) writeTargetfile(dname,style);
	if(lname) writeListfile(lname, v, w);
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
	source.purge();
	for(uint i=0;i<sourcelines.count();i++) { source.append(new SourceLine("", i, dupstr(sourcelines[i]))); }
	//current_sourceline_index = 0;

	target = NULL;

	labels.purge();
	labels.append(new Labels(0));			// global_labels must exist
	//local_labels_index = 0;
	//local_blocks_count = 1;
	//unresolved_temp_labels.purge();
	//temp_labels.purge();

	segments.purge();
	segments.append(new Segment("CODE_DEFAULT",no,0xff));	// current_segment must exist
	//current_segment_ptr = &segments[0];

	for(pass=1,final=no; pass<9 && !final; pass++)
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

		// Errors vorbereiten:
		errors.purge();
		//max_errors = 30;

		// Cond. Assemblierung vorbereiten:
		cond_off = 0x00;
		cond[0] = no_cond;	// memset(cond,no_cond,sizeof(cond));

		// Segmente vorbereiten:
		current_segment_ptr = &segments[0];
		for(uint i=0;i<segments.count();i++)
		{
			Segment& segment = segments[i];
			segment.rewind();
		}

		// Labels vorbereiten:
		local_labels_index = 0;
		local_blocks_count = 1;
		temp_label_seen = no;
		memcpy(temp_label_suffix,"$0",3);

		// Source assemblieren:
		for(uint i=0; i<source.count() && !end; i++)
		{
			try
			{
				current_sourceline_index = i;	// req. for Errors and Labels
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

		// Auf Fehler prüfen:
		if(cond[0]!=no_cond) { addError("#endif missing"); }
		if(errors.count()) return;
		XXXASSERT(!cond_off);

		// Segmente aneinanderhängen:
		int32 data_address = 0; bool data_address_valid = yes;
		int32 code_address = 0; bool code_address_valid = yes;
		for(uint i=0;i<segments.count();i++)
		{
			Segment& segment = segments[i];
			XXXASSERT(segment.dptr==segment.size || !segment.dptr_valid || !segment.size_valid);
			XXXASSERT(!segment.address_valid || !segment.relocatable);

			if(segment.dptr_valid) segment.setSize(segment.dptr);
			if(segment.relocatable && segment.size_valid && segment.size==0) continue;	// unused segment

			int32& address		 = segment.is_data ? data_address		: code_address;
			bool&  address_valid = segment.is_data ? data_address_valid : code_address_valid;

			if(segment.relocatable && address_valid) segment.setAddress(address);

			if(segment.currentAddressValid())
			{
				address = segment.currentAddress();
				address_valid = yes;
			}
			else if(segment.address_valid && segment.size_valid)
			{
				address = segment.address + segment.size;
				address_valid = yes;
			}
		}
	}

	if(!final) { addError("some labels failed to resolve"); return; }

	if(XXXSAFE) for(uint i=0;i<segments.count();i++)
	{
		Segment& s = segments[i];
		if(i==0 && s.size==0) continue;
		ASSERT(s.address_valid);
		ASSERT(s.size_valid);
		ASSERT(s.currentAddressValid());
		ASSERT(!s.relocatable);
	}
}


/*	Assemble SourceLine
*/
void Z80Assembler::assembleLine(SourceLine& q) throw(any_error)
{
	q.rewind();							// falls Pass 2++
	q.segment = current_segment_ptr;	// Für Temp Label Resolver
	q.byteptr = currentPosition();		// Für Temp Label Resolver & Logfile
//	q.bytecount = 0;					// Für Logfile und skip over error in pass≥2

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
	else						// [label:] + opcode
	{
		if((uint8)q[0] > ' ' && q[0]!=';') asmLabel(q);	// label definition
		asmInstr(q);			// opcode or pseudo opcode
		q.expectEol();			// expect end of line

		if(q.segment==current_segment_ptr) q.bytecount = currentPosition() - q.byteptr;
		else q.segment = current_segment_ptr;	// .area instruction
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
		case '$':	n = currentAddress();	// pc
					valid = valid && currentAddressValid();
					if(!valid) final = false; goto op;
		}
	}
	else							// multi-char word:
	{
		char c = 0;

		if (w[0]=='$')				// hex number
		{
			w++;
hex_number:	while( is_hex_digit(*w) ) { n = (n<<4)+(*w&0x0f); if(*w>'9') n+=9; w++; }
			if(w[c!=0]==0) goto op; else goto syntax_error;
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
			c = w[strlen(w)-1];
			if( tolower(c)=='h' ) goto hex_number;	// hex number    indicated by suffix
			if( tolower(c)=='b' ) goto bin_number;	// binary number indicated by suffix
		}
	}

	if(is_dec_digit(w[0]))			// decimal number or temp. label
	{
		if(q.test_char('$'))		// temp. label
		{
			w = catstr(w,temp_label_suffix);
			temp_label_seen = true;
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
			valid = valid && pass>1 && l->is_valid;	// in pass1 kann ein gefundenes glob. label noch durch ein später definiertes lokales label verdeckt werden
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
//	0123$:							local label
//	label:							label
//	label::							global label
//	label	=		expr			label
//	label	==		expr			global label
//	label	=:		expr			local label
//	label	.equ    expr			label
//	label	.gblequ expr			global label
//	label	.lclequ expr			local label

	XXXASSERT(q.p==q.text);

	cstr name = q.nextWord();
	bool is_temporaer = q.test_char('$');	// sdasz80 syntax
	bool is_global    = q.test_char(':') && !is_temporaer && q.test_char(':');

//	if(eq(name,"M323F"))
//	Log("");

	cstr s = q.p;
	cstr w = q.nextWord();
	if(w[0]=='.')							// sdasz80
	{
		w = q.nextWord();
		if(eq(w,"equ"))			{}									// TODO: sdasz80 hat evtl. andere syntax
		else if(eq(w,"gblequ"))	{ w="equ"; is_global = true; }		// TODO: sdasz80 hat evtl. andere syntax
		else if(eq(w,"lclequ"))	{ w="equ"; is_temporaer = true; }	// TODO: sdasz80 hat evtl. andere syntax
		else throw(syntax_error(catstr("unknown opcode .",w)));
	}
	if(w[0]=='=')							// sdasz80
	{
		if(w[1]==':') { w = "equ"; is_temporaer = true; }			// TODO: sdasz80 hat evtl. andere syntax
		if(w[1]=='=') { w = "equ"; is_global = true; }				// TODO: sdasz80 hat evtl. andere syntax
		if(w[1]==0x0) { w = "equ"; }								// TODO: sdasz80 hat evtl. andere syntax
	}

	XXXASSERT(is_temporaer == is_dec_digit(name[0]));				// Assumption: SDCC does not use it vice versa
	if(is_temporaer) { name = catstr(name,temp_label_suffix); temp_label_seen = true; }

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

		// increment temp_label_suffix:
		if(temp_label_seen)
		{
			ptr p = temp_label_suffix +1;	// suffix = "_12345"
			for(;;)
			{
				if(++*p <= '9') break;		// incr. char from '0' -> '9'
				*p++ = '0';					// overflow => char := '0'
				if(*p) continue;			// and incr. next char
				*p++='0'; *p=0;				// at end of string append '0'
				break;
			}
			temp_label_seen = no;
		}
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
		if(eq(w,"local"))	  asmLocal(q);		else
		if(eq(w,"endlocal"))  asmEndLocal(q);	else
		if(eq(w,"end"))		  asmEnd(q);		else throw fatal_error("unknown assembler directive");
	}
	catch(fatal_error& e) { throw e; }
	catch(any_error& e)   { throw fatal_error(e.what()); }
}


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
	known targets are: 'ROM', 'BIN', 'Z80', 'SNA', 'TAP', 'TAPE', 'O', 'P', '80', '81', 'ACE'
*/
void Z80Assembler::asmTarget( SourceLine& q ) throw(any_error)
{
	if(pass>1) q.skip_to_eol();
	else if(target) throw fatal_error("#target redefined");
	else { target = upperstr(q.nextWord()); if(!is_idf(*target)) throw syntax_error("target name expected"); }
}


/*	#include "sourcefile"
	the file is included in pass 1
*/
void Z80Assembler::asmInclude( SourceLine& q ) throw(any_error)
{
	cstr fqn = q.nextWord();
	if(fqn[0]!='"') throw syntax_error("quoted filename expected");

	fqn = unquotedstr(fqn);
	if(fqn[0]!='/') fqn = catstr(directory_from_path(q.sourcefile),fqn);

	if(pass==1) source.includeFile(fqn, current_sourceline_index+1);
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
	on first occurance start and size may be defined
	on subsequent re-opening of segment redefining start and size is not allowed
*/
void Z80Assembler::asmSegment( SourceLine& q, bool is_data ) throw(any_error)
{
	if(target==NULL) target = "ROM";

	cstr name = upperstr(q.nextWord());		// TODO: allow segment name with "-" without req. to quote name
	if(*name=='"') name = unquotedstr(name);
	else if(!is_letter(*name) && *name!='_') throw syntax_error("segment name expected");
	if(!q.testEol() && q.peekChar()!=',') q.testComma();

	bool  address_is_valid = no;
	int32 address = 0;
	bool  size_is_valid = no;
	int32 size = 0x10000;

	Segment* segment = segments.find(name);

	if(q.testComma())
	{
		if(pass==1 && segment!=NULL) throw syntax_error("segment redefined");

		address = value(q,pAny,address_is_valid=yes);
	}

	if(q.testComma())
	{
		size = value(q,pAny,size_is_valid=yes);
	}

	if(segment)	// segment already exists
	{
		if(size_is_valid) segment->setSize(size);			// throws
		if(address_is_valid) segment->setAddress(address);	// throws
		// note: Das Anpassen aller echten Labels in diesem Segment ist leider nutzlos,
		// weil sie dadurch nicht valid werden, wenn wir nicht auch noch defs etc. genau parsen wollen...
	}
	else		// new segment (pass1)
	{
		XXXASSERT(pass==1);
		uint8 fillbyte = !is_data && eq(target,"ROM") ? 0xFF : 0x00;
		segment = new Segment(name,address,size,is_data,fillbyte,address_is_valid,size_is_valid);	// throws
		segments.append(segment);
	}

	current_segment_ptr = segment;
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
	default:	goto unknown_opcode;	// error
	}


// opcode len = 1:

wlen1:
	if(*w=='.')	// SDASZ80
	{
		w = lowerstr(q.nextWord());

		if(eq(w,"module"))			{ q.skip_to_eol(); return; }	// for listing
		if(startswith(w,"optsdcc"))	{ q.skip_to_eol(); return; }	// .optsdcc -mz80
		if(startswith(w,"area"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
		if(startswith(w,"globl"))	throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
		if(startswith(w,"db"))		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
		if(startswith(w,"ds"))		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",w));
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
		if(startswith(opcode,"dw"))		throw fatal_error(usingstr("SDASZ80 opcode \".%s\": TODO",opcode));
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
//		else if(i==IX&&n==DE) store_IX_opcode(EX_DE_HL); 		// ex de,hl always changes only de and hl. 2006-09-13 kio
		else if(i==IX&&n==OPEN) store_IX_opcode(EX_HL_xSP); 	// valid illegal. 2006-09-13 kio
//		else if(i==IY&&n==DE) store_IY_opcode(EX_DE_HL); 		// ex de,hl always changes only de and hl. 2006-09-13 kio
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
				if(i==IX||i==IY) n = q.peekChar()==')' ? 0 : value(q,pAny,v);	// 2007-09-25 kio: added (IX) and (IY) w/o offset
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
				if(j==IX||j==IY) m = q.peekChar()==')' ? 0 : value(q,pAny,u);	// 2007-09-25 kio: added (IX) and (IY) w/o offset
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
				case XH:														// 2006-09-08 kio: for ill. IX/IY opcodes
				case XL:	if (i==RH||i==RL||i==OPEN) break;					// 2006-09-08 kio: for ill. IX/IY opcodes
							store_IX_opcode(LD_B_B+i*8+(j-XH+RH)); return;		// 2006-09-08 kio: for ill. IX/IY opcodes
				case YH:														// 2006-09-08 kio: for ill. IX/IY opcodes
				case YL:	if (i==RH||i==RL||i==OPEN) break;					// 2006-09-08 kio: for ill. IX/IY opcodes
							store_IY_opcode(LD_B_B+i*8+(j-YH+RH)); return;		// 2006-09-08 kio: for ill. IX/IY opcodes
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
	case ' org':	n = value(q, pAny, v=1); setOrigin(n,v); return;
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
		if(q.testComma()) { bool u=1; storeSpace(value(q,pAny,u),n,v); } else storeSpace(n,v);
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
		n = value(q,pAny,v=1);			// kio 2014-02-09: vor storeOpcode(DJNZ) vorgezogen wg. Bezug eines evtl. genutzen $
		storeOpcode(DJNZ);
		storeOffset( n - (currentAddress()+1), v && currentAddressValid() );
		return;
	}
	}

// generate error
unknown_opcode:	throw syntax_error(catstr("unknown opcode: ",w));
ill_reg:		throw syntax_error("illegal register");
ill_cond:		throw syntax_error("illegal condition");
ill_target:		throw syntax_error("illegal target");
ill_source:		throw syntax_error("illegal source");
ill_dest:		throw syntax_error("illegal destination");
}



/* ==============================================================
		Write List File
============================================================== */


void Z80Assembler::writeListfile(cstr listpath, bool v, bool w) throw(any_error)
{
	XXXASSERT(listpath && *listpath);
	XXXASSERT(source.count()); 	// da muss zumindest das selbst erzeugte #include in Zeile 0 drin sein

	FD fd(listpath,'w');

	uint i,e;
	for(i=0,e=0;i<source.count();i++)
	{
		SourceLine& sourceline = source[i];

		if(v)
		{
			uint ocode_size  = sourceline.bytecount;

			if(ocode_size==0) fd.write_str("              \t");
			else
			{
				Segment* segment = sourceline.segment;
				uint ocode_index = sourceline.byteptr;

				XXXASSERT(segment);
				XXXASSERT(ocode_size<=0x10000);
				XXXASSERT(ocode_index+ocode_size <= segment->size);

				uint8* core = segment->core.getData();

				while(ocode_size>4)
				{
					fd.write_fmt("%04X: ",ocode_index);					// Adresse
					fd.write_fmt("%08X\n",peek4X(core+ocode_index));	// 4 Datenbytes; note: assumes int==int32
					ocode_index += 4;
					ocode_size  -= 4;
				}

				fd.write_fmt("%04X: ",ocode_index);						// Adresse
				for(uint n=0;n<ocode_size;n++) { fd.write_fmt("%02X",core[ocode_index++]); }  // 0..4 Datenbytes
				fd.write_str(&"        \t"[ocode_size*2]);
			}
		}

		fd.write_str(sourceline.text); fd.write_char('\n');
		while(e<errors.count() && errors[e].sourceline && errors[e].sourceline->sourcelinenumber==i) { fd.write_fmt("***\t\t--> %s\n",errors[e++].text); }	// TODO: error column marker
	}

	while(e<errors.count()) { fd.write_fmt("***\t\t--> %s\n",errors[e++].text); }

	// TODO: Labelliste
	if(w) addError("writeListfile: write label list: TODO");

	fd.close_file();
}


void Z80Assembler::writeTargetfile(cstr dname,int style) throw(any_error)
{
	addError("writeTargetfile: TODO");
}







#if 0
/* ----	assemble single line ------------------
		if you assemble line by line:
			reset()					<- only if reusing Ass instance
			n* assembleLine(str)	<- only if forward references used
			setupPass2()
			n* assembleLine(str)
*/
void Z80Assembler::assembleLine( cstr source ) throw(any_error)
{
	current_sourceline_index = i;
	current_sourceline = source[i];

	XXASSERT(source);

	cstr w;
	const char* oldSrcPtr = q.p;		// in case of recursive calls
	q.p   = source;
	dest_ptr0 = dest_ptr;
	cstr error = NULL;

	line++;
	lines++;

	if(*source==0) {}			// empty line ?
	else if(*source==';') {}	// comment starting in column 1 ?

	else if(*source=='#')		// #directive ?
	{
		q.p++;
		w = lowerstr(nextWord());
		try
		{
			if(!assDirect(w)) throw fatal_error( catstr("unknown directive '#",w,"'") );
		}
		catch(syntax_error& e)
		{
			throw fatal_error(e.error(),e.what());
		}
	}

	else if(cond_off) {}		// assembling conditionally off ?

	else try					// label definition + opcode:
	{
	// label definition:
		if((uchar)*source>' ') defineLabel(nextWord());

	// opcode or pseudo opcode:
		w = lowerstr(nextWord());
		if(!assInstr(w)) throw syntax_error( "unknown opcode " );
		expectEol();
	}
	catch(syntax_error& e)
	{
		error = e.what();
		errors++;
	}

// write listing
	if(listv && listf) writeHexDump(fd_listing);
	if(listf) write_fmt(fd_listing,"%s\n",source);

// list error:
	if(error)
	{
		if(listf) write_fmt( fd_listing, "%s%s^ ***ERROR***: %s\n", spacestr(15*(listv&&listf)), whitestr(substr(source,q.p-1)), error );
		if(!listf || fd_listing!=STDERR) write_fmt( STDERR, "%s\n%s^ ***ERROR***: %s\n", source, whitestr(substr(source,q.p-1)), error );
	}

	q.p = oldSrcPtr;		// in case of recursive calls
}
#endif




#if 0


static char sna_dflt[] = {	0x3f,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,1,7 };
static char z80_dflt[] = {	0,0,0,0,0,0,0,0,0,0,0,0,0x3f,0,7<<1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1+(1<<6),
							0,23,0,0,0,0,0,7,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
 							0,0,0,0,0,0,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
							0	};


/* ==========================================================
				z80 assembler specific methods
========================================================== */

/*	compress ram page for Jupiter Ace
*/
void Z80Assembler::compressPageAce( Array<uint8>& zbu )
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

/*	compress .z80 block v2.01 or later
	block layout:
		dc.w		length of data (without this header; low byte first)
		dc.b		page number of block
		dc.s		compressed data follows
	compression scheme:
		dc.b $ed, $ed, count, char
*/
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


void Z80Assembler::compressPageZ80(Array<uint8>& qbu)
{
	TODO();

//	if(in_head) return;			// header data is not compressed

//// compress page(s):

//	uint qsize = qbu.count();
//	Array<uint8> zbu; zbu.grow(qsize*5/3+3*8);	// worst case size
//	uint8* q = qbu.getData();
//	uint8* z = zbu.getData();

//	if(z80_page!=0xff)			// zxsp memory page or rom
//	{
//		XXXASSERT(qsize==0x4000);	// todo: rom

//		z = compress_z80( z80_page, q, qsize, z );
//	}
//	else if(z80_varying_ramsize)	// zx80 clones, jupiter et. al.
//	{
//		XXXASSERT(qsize>=0x400 && qsize<=0x20000 && (qsize&0x3ff)==0);

//		uint32 addr = 0;
//		for(int i=0;i<8;i++)	// save pages with id 3..10 and size 1k..128k
//		{
//			uint32 n = 0x400<<i;
//			if(qsize & n) { z = compress_z80( 3+i, q+addr, n, z); addr += n; }
//		}
//	}
//	else						// zxsp with no paged ram or default ram pages
//	{
//		XXXASSERT(qsize==0x4000 || qsize==0x8000 || qsize==0xC000);

//						 z = compress_z80(8, q+0x0000, 0x4000, z);
//		if(qsize>0x4000) z = compress_z80(4, q+0x4000, 0x4000, z);
//		if(qsize>0x8000) z = compress_z80(5, q+0x8000, 0x4000, z);
//	}

//	zbu.shrink(z-zbu.getData());
//	qbu.swap(zbu);
}






/* ==============================================================
		handle TAP directives
============================================================== */

void Z80Assembler::writeTapeBlock()
{
	if(!dest_bu) return;

// store length
	dest_bu[0] = (dest_ptr-(dest_bu+2)+1);
	dest_bu[1] = (dest_ptr-(dest_bu+2)+1)>>8;

// calc checksum
	char crc = 0;
	if(pass2) for( char*p=dest_bu+2; p<dest_ptr; p++ ) { crc^=*p; }
	storeByte(crc);

	writeSegment(yes);		// TAP: all segments are truncated
}


void Z80Assembler::handleCodeTap()
{
 	writeTapeBlock();

// #code start,len,sync

	int n = value(pAny|pForce);	expectComma(); if (n!=(int16)n&&n!=(uint16)n) throw fatal_error("illegal origin");
	int m = value(pAny|pForce); expectComma(); if (m<=0||m>=0x10000) throw fatal_error("illegal length");
	int s = value(q); if (s<-128||s>255) throw fatal_error("sync byte out of range");
//	if (error) return;

//			defw	len
// 			defb	sync
//	org:	defb	<object code>
//			defb	crc

	newDestBu(m+4,0); //if (error) return;	// preset with $00 for defs instruction
	org = n-3;
	storeWord(0);				// length (not yet known: sized to fit)
	storeByte(s);				// store the sync byte
}


void Z80Assembler::handleEndTap()
{
	writeTapeBlock();
	Assembler::handleEnd(yes);
}


/* ==============================================================
		handle .80 / .o file directives  (tape for ZX80)
============================================================== */

void Z80Assembler::handleCodeZX80 ( )
{
	if(dest_bu) throw fatal_error(".80 / .o files cannot have multiple segments");

//	#code $4000, len
	int n = value(pAny|pForce);	expectComma(); if (n!=0x4000) throw fatal_error("origin must be $4000");
	int m = value(pAny|pForce); if (m<=0x0028||m>=0x0c000) throw fatal_error("illegal length");

	newDestBu(m,0);											// preset with $00 for defs instruction
	org = 0x4000;
}

void Z80Assembler::handleEndZX80()
{
	poke2Z( dest_bu+0x000A, 0x4000+dest_ptr-dest_bu );		// E_LINE  (end of input line == end of used ram)
	Assembler::handleEnd(yes);
}


/* ==============================================================
		handle .81 / .p file directives  (tape for ZX81)
============================================================== */

void Z80Assembler::handleCodeZX81 ( )
{
	if(dest_bu) throw fatal_error(".81 / .p files cannot have multiple segments");


//	#code $4009, len
	int n = value(pAny|pForce);	expectComma(); if (n!=0x4009) throw fatal_error("origin must be $4009");
	int m = value(pAny|pForce); if (m<=0x3c-0x09||m>=0x0c000-0x09) throw fatal_error("illegal length");

	newDestBu(m,0);											// preset with $00 for defs instruction
	org = 0x4009;
}

void Z80Assembler::handleEndZX81()
{
	poke2Z(dest_bu+0x14-0x09, 0x4009+dest_ptr-dest_bu);		// E_LINE  (end of input line == end of used ram)
	Assembler::handleEnd(yes);
}


/* ==============================================================
		handle ACE directives
============================================================== */

//	$2000 - $2400: all-zero: echo of video ram, fast access for CPU ($2000-$2148: ACE32 settings and Z80 registers)
//	$2400 - $2800: 1k video ram
//	$2800 - $2C00: all-zero: echo of character ram, fast access for CPU
//	$2C00 - $3000: 1k character ram
//	$3000 - $3C00: all-zero: 3 echoes of built-in program ram
//	$3C00 - $4000: 1k buint-in program ram
//	$4000 -		 : external ram pack

void Z80Assembler::handleCodeAce()
{
	XXXASSERT(target==' ace');
	XXXASSERT(targetstyle=='b');

	uint addr = value(pAny|pForce);
//	if(addr<0x2000) throw fatal_error("#code address must be ≥ $2000");
	if(addr&0x3FF)  throw fatal_error("#code address must be a multiple of 1K");
	expectComma();
	uint size = value(pAny|pForce);
	if(size&0x3FF || size==0)  throw fatal_error("#code size must be a multiple of 1K");
	if(addr+size>0x10000) throw fatal_error("#code addr+size exceeds $10000");

	if(dest_bu && pass2)
	{
		uint sz = dest_end-dest_bu;
		Array<uint8> bu; bu.grow(sz);
		memcpy(bu.getData(),dest_bu,sz);
		compressPageAce(bu);
		write_bytes(fd_dest,bu.getData(),bu.count());
	}

	newDestBu(size,0);
	org = addr;
}


void Z80Assembler::handleEndAce()
{
	XXXASSERT(target==' ace');
	XXXASSERT(targetstyle=='b');

	if(!dest_bu) throw syntax_error("no #code segment found");

	if(pass2)
	{
		uint sz = dest_end-dest_bu;
		Array<uint8> bu; bu.grow(sz);
		memcpy(bu.getData(),dest_bu,sz);
		compressPageAce(bu);
		write_bytes(fd_dest,bu.getData(),bu.count());
		static uint8 ed00[2]={0xed,0};
		write(fd_dest,ed00,2);
	}

	delete[] dest_bu;
	org += dest_ptr-dest_bu;		// => list code position in #end
	dest_bu = dest_ptr0 = dest_ptr = dest_end = NULL;

// if intel hex file append closing record
//	if(fd_dest && targetstyle=='x') write_str(fd_dest, ":00000001FF\r\n");

// close target file (if not stdout/stderr)
	if (fd_dest>2) close_file(fd_dest); fd_dest = -1;
	target = 0;
}


/* ==============================================================
		handle SNA directives
============================================================== */

void Z80Assembler::handleHeadSna()
{
	if(dest_bu) throw syntax_error("#head segment already defined");

	int n = value(pAny|pForce);
	if(n!=27) throw syntax_error("#head segment size must be 27 for sna files");

	newDestBu(n,0);

	org = 0;
	memcpy(dest_bu, sna_dflt, n);
	in_head = true;				// flag: #head segment in progress
}


void Z80Assembler::handleCodeSna()
{
	if(!dest_bu) throw syntax_error("#head segment missing");
	if(!in_head) throw syntax_error("#code segment already defined");

	// write #head segment
	writeSegment(no);			// not truncated
	in_head = false;

	org = value(pAny|pForce); if(org!=0x4000) throw syntax_error("address must be $4000");
	expectComma();
	int size = value(pAny|pForce); if(size!=0x4000 && size!=0xC000) throw syntax_error("length must be 16K or 48K");
	newDestBu(size,0);
}


void Z80Assembler::handleEndSna()
{
	if(!dest_bu||in_head) throw syntax_error("#code segment missing");

	Assembler::handleEnd(no);
}


/* ==============================================================
		handle Z80 directives
============================================================== */


void Z80Assembler::handleHeadZ80()
{
	XXXASSERT(target==' z80');

	if(dest_bu) throw fatal_error("#head segment already defined");

	uint size = value(pAny|pForce);
	if(size<z80v1len||size>255||size==z80v1len+1) throw syntax_error(".z80: illegal size");

	newDestBu(size,0);
	org = 0;

	memcpy(dest_bu, z80_dflt, min(size,(uint)sizeof(z80_dflt)));
	if(size>z80v1len) poke2Z(&((Z80Head*)dest_bu)->h2lenl, size -(z80v1len+2));
	in_head = true;
}


void Z80Assembler::handleCodeZ80()
{
	XXXASSERT(target==' z80');
	XXXASSERT(targetstyle=='b');

	if(!dest_bu) throw fatal_error("#head segment expected");

	writeSegment(no);

	uint addr = value(pAny|pForce);		// code address
	expectComma();
	uint size = value(pAny|pForce);		// code size

	if(in_head)
	{
		Z80Head* head = ((Z80Head*)dest_bu);
		int head_size = dest_end - dest_bu;
		if((head->pch||head->pcl) && head_size!=z80v1len) throw fatal_error("head segment size must be 30 for a version 1.45 file");
		if(head_size>z80v1len && z80v1len+2+peek2Z(&head->h2lenl)!=head_size)
		{
			LogLine("h2len = %i",(int)peek2Z(&head->h2lenl));
			throw fatal_error("h2len does not match head segment size");
		}
		z80_model = head->getZxspModel(); if(z80_model==(Model)-1) throw fatal_error("illegal model number!");
		z80_varying_ramsize = head->varyingRamsize();
		z80_pages = head->spectator;
		in_head = false;
	}

	z80_page = 0xff;
	if(testComma())						// page id
	{
		z80_page = value(pAny|pForce);
		if(z80_page>=64) throw fatal_error("illegal page id");
		if(addr&0x3FFF) throw fatal_error("code address must be a multiple of 16K");	// TODO: roms
		if(size!=0x4000) throw fatal_error("size must be 16K");							// TODO: roms
	}
	else if(z80_varying_ramsize)		// zx80 clones, jupiter et. al.
	{
		if(addr&0x3FF) throw fatal_error("code address must be a multiple of 1K");
		if(size&0x3ff) throw fatal_error("code size must be a multiple of 1K");
		if((z80_pages<<10) != size) throw fatal_error("size does not match value stored in head.spectator");
	}
	else								// zxsp with no paged memory or default pages
	{
		if(addr&0x3FFF) throw fatal_error("code address must be a multiple of 16K");
		if(size!=0x4000 && size!=0x8000 && size!=0xC000) throw fatal_error("code size must be a multiple of 16K");
	}

	newDestBu(size,0);
	org = addr;
}


void Z80Assembler::handleEndZ80()
{
	XXXASSERT(target==' z80');
	XXXASSERT(targetstyle=='b');

	if(!dest_bu) throw fatal_error("#head segment expected");
	if(in_head) throw fatal_error("#code segment expected");

	Assembler::handleEnd(no);
}



/* ==============================================================
		AssDirect() virtual method
		handle assembler directive
============================================================== */


bool Z80Assembler::assDirect( cstr w )
{
// check for conditional assembly off:
	if (cond_off) return Assembler::assDirect(w);

// #target:
	if(eq(w,"target"))
	{
		Assembler::assDirect(w);

		switch(target)
		{
		case 'tape':	target = ' tap';
		case    'o':
		case    'p':
		case '  80':
		case '  81':
		case ' tap':
		case ' sna':
		case ' z80': 	in_head = false;
		case ' ace':
		case ' bin':
		case ' rom':	return true;
		default:		throw syntax_error("unknown #target");
		}
	}

// #head:
	if(eq(w,"head"))
	{
		switch(target)
		{
		case     0 :	throw syntax_error("#head segment without #target");
		case ' sna':	handleHeadSna(); return true;
		case ' z80':	handleHeadZ80(); return true;
		default:		throw syntax_error("#head segment not allowed for this #target");
		}
	}

// #code:
	if(eq(w,"code"))
	{
		switch(target)
		{
		case ' sna':	handleCodeSna(); return true;
		case ' z80':	handleCodeZ80(); return true;
		case ' tap':	handleCodeTap(); return true;
		case    'o':
		case '  80':	handleCodeZX80(); return true;
		case    'p':
		case '  81':	handleCodeZX81(); return true;
		case ' ace':	handleCodeAce();  return true;
		default:		break; // call super
		}
	}

// #end:
	if(eq(w,"end"))
	{
		switch(target)
		{
		case ' sna':	handleEndSna(); return true;
		case ' z80':	handleEndZ80(); return true;
		case ' tap':	handleEndTap(); return true;
		case    'o':
		case '  80':	handleEndZX80(); return true;
		case    'p':
		case '  81':	handleEndZX81(); return true;
		case ' ace':	handleEndAce();  return true;
		default:		break; // call super
		}
	}

	/*	no recognized #directive
		=> try general Ass #directives
	*/
	return Assembler::assDirect(w);
}


void Z80Assembler::compressPage( Array<uint8>& zbu )
{
	if(target==' ace') compressPageAce(zbu);
	if(target==' z80') compressPageZ80(zbu);
}





#endif


























