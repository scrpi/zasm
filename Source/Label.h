﻿/*	Copyright  (c)	Günter Woigk 2014 - 2014
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
	uint		sourceline;
	int32		value;
	bool		is_valid;

public:
	Label(cstr name, Segment* segment, uint sourceline, int32 value, bool is_valid);
};



class Labels : private ObjHashMap<cstr,Label>
{
public:
	uint		outer_index;		// index of surrounding Labels block in list of all Labels blocks

public:
				Labels(uint outer_index)	:outer_index(outer_index){}
	void		purge()						{ ObjHashMap::purge(); }
	void		add(Label* l)				{ ObjHashMap::add(l->name,l); }
	Label&		find(cstr name)				{ return ObjHashMap::get(name); }
	Label const& find(cstr name) const		{ return ObjHashMap::get(name); }
};




#endif // ZASMLABEL_H


















