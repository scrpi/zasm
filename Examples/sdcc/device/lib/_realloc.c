/*-------------------------------------------------------------------------
   _realloc.c - reallocate allocated memory.

   Copyright (C) 2004, Maarten Brock, sourceforge.brock@dse.nl

   This library is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License 
   along with this library; see the file COPYING. If not, write to the
   Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.

   As a special exception, if you link this library with other files,
   some of which are compiled with SDCC, to produce an executable,
   this library does not by itself cause the resulting executable to
   be covered by the GNU General Public License. This exception does
   not however invalidate any other reasons why the executable file
   might be covered by the GNU General Public License.


   kio 2014-11-26	removed #define CRITICAL __critical
   kio 2014-11-26	removed __xdata
   kio 2014-11-26	removed the MAH version, because z80 uses MLH version
*/


#include <sdcc-lib.h>
#include <malloc.h>
#include <string.h>


typedef struct _MEMHEADER MEMHEADER;

struct _MEMHEADER
{
  MEMHEADER *   next;
  MEMHEADER *   prev;
  unsigned int  len;
  unsigned char mem;
};

#define HEADER_SIZE (sizeof(MEMHEADER)-sizeof(char))
#define MEM(x)      (&x->mem)


extern MEMHEADER * _sdcc_prev_memheader;

// apart from finding the header
// this function also finds it's predecessor
extern MEMHEADER * _sdcc_find_memheader(void * p);

void * realloc (void * p, size_t size)
{
  register MEMHEADER * pthis;
  register MEMHEADER * pnew;
  register void * ret;

  __critical
  {
    pthis = _sdcc_find_memheader(p);
    if (pthis)
    {
      if (size > (0xFFFF-HEADER_SIZE))
      {
        ret = (void *) NULL; //To prevent overflow in next line
      }
      else
      {
        size += HEADER_SIZE; //We need a memory for header too

        if ((((unsigned int)pthis->next) - ((unsigned int)pthis)) >= size)
        {//if spare is more than needed
          pthis->len = size;
          ret = p;
        }
        else
        {
          if ((_sdcc_prev_memheader) &&
              ((((unsigned int)pthis->next) -
                ((unsigned int)_sdcc_prev_memheader) -
                _sdcc_prev_memheader->len) >= size))
          {
            pnew = (MEMHEADER * )((char *)_sdcc_prev_memheader + _sdcc_prev_memheader->len);
            _sdcc_prev_memheader->next = pnew;

            pthis->next->prev = pnew;

            memmove(pnew, pthis, pthis->len);
            pnew->len = size;
            ret = MEM(pnew);
          }
          else
          {
            ret = malloc(size - HEADER_SIZE);
            if (ret)
            {
              memcpy(ret, MEM(pthis), pthis->len - HEADER_SIZE);
              free(p);
            }
          }
        }
      }
    }
    else
    {
      ret = malloc(size);
    }
  }
  return ret;
}

