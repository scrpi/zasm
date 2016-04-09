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


#ifndef ZASMSOURCELINE_H
#define ZASMSOURCELINE_H


#include "kio/kio.h"
#include "Templates/ObjArray.h"
#include "SyntaxError.h"

class Label;
class Segment;


inline bool is_idf	 ( char c ) { return is_letter(c) || is_dec_digit(c) || c=='_'; }
inline char lc       ( char c ) { return c|0x20; }

//inline bool is_upper ( char c ) { return c>='A' && c<='Z'; }
//inline bool is_lower ( char c ) { return c>='a' && c<='z'; }
//inline bool is_alpha ( char c ) { return lc(c)>='a' && lc(c)<='z'; }

//inline bool is_bin   ( char c ) { return c>='0' && c<='1'; }
//inline bool is_num   ( char c ) { return c>='0' && c<='9'; }
//static bool is_hex	 ( char c ) { return is_num(c) || (lc(c)>='a' && lc(c)<='f'); }



class SourceLine
{
public:
	cstr	text;				// tempmem / shared
	cstr	sourcefile;			// tempmem / shared between all sourcelines of this file
	uint	sourcelinenumber;	// line number in source file; 0-based

	Segment* segment;			// of object code
	uint	byteptr;			// index of object code in segment
	uint	bytecount;			// of bytes[]

	Label*	label;				// if a label is defined in this line
	bool	is_data;			// if generated data is no executable code

	cptr	p;					// current position of source parser

public:
	SourceLine(cstr sourcefile, uint linenumber, cstr text);

	char		operator*	()		{ return *p; }
	char		operator[]	(uint i){ return text[i]; }
	SourceLine&	operator++	()		{ XXXASSERT(*p);     ++p; return *this; }	// prefix
	SourceLine&	operator--	()		{ XXXASSERT(p>text); --p; return *this; }	// prefix
	SourceLine&	operator+=	(int n)	{ XXXASSERT(n>0?p+n<=strchr(p,0):p+n>=text); p+=n; return *this; }
	SourceLine&	operator-=	(int n)	{ XXXASSERT(n<0?p-n<=strchr(p,0):p-n>=text); p-=n; return *this; }

	void	rewind		()			{ p = text; }
	void	skip_spaces	()			{ while(is_space(*p)) p++; }
	void	skip_char	(char c)	{ if(*p==c) p++; }
	void	skip_to_eol	()			{ p = strchr(p,0); }
	bool	test_char	(char c)	{ if(*p==c) { p++; return true; } else return false; }

	// these automatically skip white space:
	char	peekChar	();
	bool	testChar	(char);
	bool	testWord	(cstr);
	bool	testDotWord	(cstr);
	bool	testComma	()			{ return testChar(','); }
	bool	testEol		();

	void	expect		(char)		throw(syntax_error);
	void	expectComma	()			throw(syntax_error)		{ expect(','); }
	void	expectOpen	()			throw(syntax_error)		{ expect('('); }
	void	expectClose	()			throw(syntax_error)		{ expect(')'); }
	void	expectEol	()			throw(syntax_error);

	cstr	nextWord	();

	uint	column		()			{ return p-text; }		// 0-based
	cstr	whitestr	();			// whitestr() up to error column
};


class Source : private ObjArray<SourceLine>
{
public:
	void	purge		()						{ ObjArray<SourceLine>::purge(); }
	uint	count		()						{ return ObjArray<SourceLine>::count(); }
	SourceLine& operator[](uint i)				{ return ObjArray<SourceLine>::operator[](i); }
	void	append		(SourceLine* s)			{ ObjArray<SourceLine>::append(s); }
	void	insertat	(uint i, SourceLine* l)	{ ObjArray<SourceLine>::insertat(i,l); }
	void	insertat	(uint i, ObjArray& q)	{ ObjArray<SourceLine>::insertat(i,q); }

	void	includeFile	(cstr filename_fqn, uint zeilen_index)	throw(fatal_error);
};





#endif // ZASMSOURCELINE_H





























