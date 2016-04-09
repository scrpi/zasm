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


#ifndef ZASMLABEL_H
#define ZASMLABEL_H

#include "cstrings/cstrings.h"
#include "Templates/HashMap.h"




class Segment;


class Label
{
public:
	cstr		name;
	Segment*	segment;
	uint		sourceline;			// of DEFINITION
	int32		value;
	bool		is_valid;			// value is valid
	bool		is_global;			// global label: item in Z80Assembler.labels[0]
	bool		is_defined;			// label is defined (value may be still in_valid) not only declared
	bool		is_used;			// label is actually used

public:
	Label(cstr name, Segment*, uint sourceline, int32 value, bool is_valid, bool is_global, bool is_defined, bool is_used);
	Label(Label const&);
};



class Labels : private ObjHashMap<cstr,Label>
{
public:
	uint		outer_index;		// index of surrounding Labels block in list of all Labels blocks
	bool		is_global;			// this is the global Labels[]

	enum GFlag { GLOBALS };

public:
				Labels(GFlag)				:outer_index(0),is_global(yes){}
				Labels(uint outer_index)	:outer_index(outer_index),is_global(no){}
				~Labels();
	void		purge()						{ ObjHashMap::purge(); }
	void		add(Label* l)				{ ObjHashMap::add(l->name,l); }
	Label&		find(cstr name)				{ return ObjHashMap::get(name); }
	Label const& find(cstr name) const		{ return ObjHashMap::get(name); }
	Array<Label*>& getItems()				{ return ObjHashMap::getItems(); }
	void		remove(cstr name)			{ ObjHashMap::remove(name); }
	bool		contains(cstr name) const	{ return indexof(name) != -1; }
};




#endif // ZASMLABEL_H


















