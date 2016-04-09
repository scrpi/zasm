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
#define LOG  1
#include "CharMap.h"


#define NC 0xff		// mark unset char in this.charmap[]

// from kio/c_defines.h:
#define	RMASK(n)	(~(0xFFFFFFFF<<(n)))	// mask to select n bits from the right

// from Unicode/UTF-8.h:
INL bool utf8_is_7bit	( char c )		{ return c>=0; }
INL bool utf8_is_fup	( char c )		{ return c< char(0xc0);  }
INL bool utf8_no_fup	( char c )		{ return c>=char(0xc0);  }
INL void UTF8SkipChar	( char*& p )	{ while(utf8_is_fup(*++p)) {} }



/*	Helper:
	convert UTF-8 char to UCS-2
	throws on error
	from Unicode/UTF-8.h
*/
static
UCS2Char ucs2_from_utf8( cptr s ) throw(data_error)
{
	UCS4Char n; uint i;

	XXXASSERT(s);

	n = uchar(*s);							// UCS-4 char code akku
	if(utf8_is_7bit(n)) return (UCS2Char)n;	// 7-bit ascii char
	if(utf8_is_fup(n)) throw data_error("broken character in map (unexpected UTF-8 fup character)");
											// 0x80 … 0xBF: unexpected fups
// multi-byte character:
	i = 0;									// UTF-8 character size
	int8 c = n & ~0x02;						// force stop at i=6
	while(char(c<<(++i)) < 0)				// loop over fup bytes
	{
		uchar c1 = *(++s);
		if(utf8_no_fup(c1)) throw data_error("broken character in map (truncated UTF-8 character)");
		n = (n<<6) + (c1&0x3F);
	}

// now: i = total number of bytes
//      n = UCS4 char code with some of the '1' bits from c0
	n &= RMASK(2+i*5);
	if(n!=(UCS2Char)n) throw data_error("UTF-8 character outside the UCS2 code range in map");

// ill. overlong encodings:
	if(n < 1u<<(i*5-4)) throw data_error("illegal character in map (illegal overlong UTF-8 encoding)");

// ok => return code
	return (UCS2Char)n;
}


/*	Create Character Map with no mappings
	Mappings must be added with add() or addMappings()
*/
CharMap::CharMap()
:	HashMap(8)
{
	memset(charmap,NC,128);
}


/*	Create Character Map for target charset
*/
CharMap::CharMap(CharSet charset)
:	HashMap(32)
{
	memset(charmap,NC,128);
	switch(charset)
	{
	case ZX80:  addMappings(" \"▌▄▘▝▖▗▞...£$:?()-+*/=><;,.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",0);
				addMappings("█",128);
				addMappings("▐▀▟▙▜▛▚",130);
				break;
	case ZX81:	addMappings(" ▘▝▀▖▌▞▛...\"£$:?()><=+-*/;,.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",0);
				addMappings("█▟▙▄▜▐▚▗",128);
				break;
	case ZXSP:	for(int c=32;c<127;c++) charmap[c] = c;
			//	charmap[9]  = 6;		// tab
			//	charmap[10] = 13;		// nl
			//	charmap[8]  = 8;		// cursor left
			//	charmap[12] = 9;		// cursor right
				addMappings("£",96);
				addMappings("©\u00A0▝▘▀▗▐▚▜▖▞▌▛▄▟▙█",127);	// \u00A0 = nbsp
															// note: Qt Creator silently replaces nbsp with space :-(
				break;
	case JUPITER:
				for(int c=32;c<127;c++) charmap[c] = c;
			//	charmap[10] = 13;		// nl
				addMappings("█▙▟▄▛▌▞▖",16);
				addMappings("£",96);
				addMappings("©",127);
				addMappings("\u00A0▝▘▀▗▐▚▜",144);	// \u00A0 = nbsp
													// note: Qt Creator silently replaces nbsp with space :-(
				break;
//	case ASCII:
//	case NONE:
	default:	for(int c=0;c<127;c++) charmap[c] = c;
				break;
 	}
}

//helper:
//static
CharMap::CharSet CharMap::charsetFromName (cstr w)
{
	w = lowerstr(w);

	return  eq(w,"zx80") ? ZX80 : eq(w,"zx81") ? ZX81 :
			eq(w,"ascii") ? ASCII : startswith(w,"zxsp") ? ZXSP :
			startswith(w,"jup")||endswith(w,"ace") ? JUPITER : NONE;
}

void CharMap::purge()
{
	memset(charmap,NC,128);
	HashMap::purge();
}

bool CharMap::contains(UCS2Char key) const
{
	if(key<128 && charmap[key]!=NC) return true;
	return HashMap::contains(key);
}

void CharMap::add(UCS2Char key, uchar item)
{
	if(key<128) { charmap[key] = item; if(item!=NC) return; }
	HashMap::add(key,item);
}

void CharMap::remove(UCS2Char key)
{
	HashMap::remove(key);
	if(key<128) charmap[key] = NC;
}

void CharMap::addMappings(cUTF8Str map, uint first_char_in_map) throw(data_error)
{
	uint c = first_char_in_map;
	cptr p = map;

	while(*p)
	{
		add(ucs2_from_utf8(p),c++);
		while(utf8_is_fup(*++p)) {}
	}
}

void CharMap::removeMappings (cUTF8Str s) throw(data_error)
{
	while(*s)
	{
		remove(ucs2_from_utf8(s));
		while(utf8_is_fup(*++s)) {}
	}
}

uchar CharMap::get(UCS2Char key, uchar dflt) const
{
	if(key<128 && charmap[key]!=NC) return charmap[key];
	return HashMap::get(key,dflt);
}

uchar CharMap::get(UCS2Char key) const throw(syntax_error)
{
	if(key<128 && charmap[key]!=NC) return charmap[key];
	uchar c = HashMap::get(key,0); if(c) return c;
	static_assert(NC!=0,"const NC must be non-zero here");

	cstr fmt = key>=' ' && key<=0x7F
		? "Character '%c' is not in the target character set"
		: "Character 0x%04X is not in the target character set";
	throw syntax_error(usingstr(fmt,key));
}

uchar CharMap::operator[](UCS2Char key) const throw(index_error)
{
	if(key<128 && charmap[key]!=NC) return charmap[key];
	return HashMap::operator[](key);
}

pstr CharMap::translate( cptr q ) throw(data_error)
{
	pstr zstr = (pstr) tempstr(strlen(q));
	uptr z = zstr;

	while(*q)
	{
		cptr q0 = q;
		UCS2Char key = ucs2_from_utf8(q);
		while(utf8_is_fup(*++q)) {}

		if(key<128 && charmap[key]!=NC)
		{
			*z++ = charmap[key];
		}
		else
		{
			int idx = indexof(key);
			if(idx<0) throw data_error(usingstr("target character set does not contain '%s'",substr(q0,q)));
			*z++ = items[idx];
		}
	}

	*zstr = z-zstr+1;
	return zstr;
}














