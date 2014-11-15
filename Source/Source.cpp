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
#define LOG  1
#include "Source.h"





// ----------------------------------------------
//					Source
//
//			Source = SourceLine[]
//
// ----------------------------------------------


/*	include another assembler source file at sourceAE_index into this.source[]
	die Text-Pointer der SourceLines zeigen alle in tempmem!
	die Sourcefile-Pointer zeigen alle auf filename_fqn!
*/
void Source::includeFile(cstr filename_fqn, uint sourceAE_index) throw(fatal_error)
{
	try
	{
		FD fd(filename_fqn,'r');
		off_t sz = fd.file_size();
		if(sz>=3 && fd.read_int24_x() != 0x00efbbbf) fd.rewind_file();				// skip BOM
		if(sz>10000000) throw any_error("source file exceeds 10,000,000 bytes");		// sanity

		ObjArray<SourceLine> zsource;
		for(cstr s = fd.read_str(); s!=NULL; s = fd.read_str())
		{
			zsource.append(new SourceLine(filename_fqn, zsource.count(), s));
		}

		insertat(sourceAE_index,zsource);
		if(count()>1000000) throw any_error("total source exceeds 1,000,000 lines");	// sanity
	}
	catch(any_error& e)
	{
		throw fatal_error(usingstr("file \"%s\" could not be read: %s", filename_fqn, e.what()));
	}
}



// ----------------------------------------------
//				  SourceLine
// ----------------------------------------------


/*	Creator
*/
SourceLine::SourceLine(cstr sourcefile, uint linenumber, cstr text)
:	text(text),						// 2nd ptr
	sourcefile(sourcefile),			// 2nd ptr
	sourcelinenumber(linenumber),	// 0-based
	segment(NULL),
	byteptr(0),
	bytecount(0),
	p(text)
{}



/*	skip spaces and peek next char
	returns 0 at end of line
*/
char SourceLine::peekChar()
{
	skip_spaces();
	return *p;
}

/*	skip spaces and test for and skip next char
	returns true if the expected char was found
*/
bool SourceLine::testChar( char c )
{
	skip_spaces();
	return test_char(c);
}

bool SourceLine::testWord( cstr z )
{
	skip_spaces();
	cptr q = p;
	while(*z==*q) { z++; q++; }
	if(*z) return no;				// character mismatch
	if(is_idf(*q)) return no;		// word in this.text longer than tested word
	p = q; return yes;				// hit! => skip word and return true
}

/*	test for logical end of line
	which may be physical end of line or start of a comment
*/
bool SourceLine::testEol()
{
	skip_spaces();
	char c = *p;
	return c==';' || c==0;
}

/*	skip spaces and test for and skip char
	throw error if char does not match
*/
void SourceLine::expect( char c ) throw (syntax_error)
{
	if(!testChar(c)) throw syntax_error(catstr("'",charstr(c),"' expected"));
}

/*	test for logical end of line
	which may be physical end of line or start of a comment
	throw error if not at eol
*/
void SourceLine::expectEol() throw (syntax_error)
{
	if(!testEol()) throw syntax_error("end of line expected");
}


/*	get next word from source line
	returns word is const or temp
	returns "" at eol
*/
cstr SourceLine::nextWord()
{
	skip_spaces();
	if(*p==';') skip_to_eol();
	if(*p==0)   return "";					// endofline

	cstr word = p++;
	char c    = *word;

	switch(c)
	{
	case '+':	return "+";
	case '-':	return "-";
	case '*':	return "*";
	case '/':	return "/";
	case '\\':	return "\\";
	case '~':	return "~";
	case '(':	return "(";
	case ')':	return ")";
	case '&':	return "&";
	case '|':	return "|";
	case ',':	return ",";
	case '.':	return ".";
	case '=':	return "=";

	case '"':								// "abcd"
	case '\'':								// 'abcd'
		while( *p!=c && *p ) p++;
		if (*p==c) p++;
		break;

	case '$':								// $, $$ or hex number
		if(*p=='$') p++;
		else while(is_hex_digit(*p)) p++;
		break;

	case '%':								// binary number
		while(is_bin_digit(*p)) p++;
		break;

	case '!':
		c = *p++;
		if(c=='=') return "!=";
		p--;	   return "!";

	case '<':
		c = *p++;
		if (c=='>') return "<>";
		if (c=='=') return "<=";
		if (c=='<') return "<<";
		p--;	    return "<";

	case '>':
		c = *p++;
		if (c=='>') return ">>";
		if (c=='=') return ">=";
		p--;		return "<";

	default:			 		// name, decimal number, garbage
		if (is_idf(c)) while (is_idf(*p)) p++;
		break;
	}

	return substr(word, p);
}


























