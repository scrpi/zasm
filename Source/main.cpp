/*	Copyright  (c)	Günter Woigk 1994 - 2014
  					mailto:kio@little-bat.de

	This file is free software

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

	2002-01-20	kio	port to unix started
	2002-01-28	kio	3.0.0 released
	2014-05-21	kio work on 4.0.0 started
*/


#define	LOG 	2
#define	SAFE	3

#include	"config.h"
#include	<stdlib.h>
#include	<dirent.h>
#include	"unix/FD.h"
#include	"unix/files.h"
#include	"kio/kio.h"
#include	"Z80Assembler.h"



cstr appl_name = "zasm";
cstr version   = "4.0α";


/* helper: get the compile date in preferred format "yyyy-mm-dd":
*/
static cstr compiledatestr()
{
	static char ansidate[] = __DATE__;		// "Jan  1 2014"
	static char months[] = "JanFebMarAprMayJunJulAugSepOctNovDec";

	uint m=0; while( strncmp(ansidate, months+3*m++, 3) ) {}
	uint d = strtol(ansidate+4,NULL,10);
	cptr y = ansidate+7;
	return usingstr("%s-%02u-%02u",y,m,d);
}



/* ---- Hilfstext -----------------------------------
		Anzeige optimiert für 80-Zeichen-Terminal
*/
static cstr help =
"–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––\n"
"  zasm - z80 assembler (c) 1994-2014 Günter Woigk.\n"
"  version %s, %s, for %s.\n"								// version, date, platform
"  send bug reports to: kio@little-bat.de\n\n"

"syntax:\n"
"  zasm [-vwbxs] [-i] inputfile [[-l] listfile] [[-o] outfile]\n\n"

"examples:\n"
"  zasm speccirom.asm\n"
"  zasm -vw speccirom.src rom_v2.0.1.rom\n\n"

"options:\n"
"  -v  include object code in list file\n"
"  -w  include label list in list file\n"
"  -b  write output to binary file (default)\n"
"  -x  write output in intel hex format\n\n"
"  -s  silent: no summary on stderr\n\n"

"If no listfile is given, then no list file is generated.\n"
"The listfile may refer to a directory.\n"
"The outfile may be omited or refer to a directory.\n"
"–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––\n"
"";





/* ---- program entry ---------------------------
*/
int main( int argc, cstr argv[] )
{
// options:
	bool listv   = no;		// object code included in listing
	bool listw   = no;		// label listing appended to listing
	bool silent	 = no;		// no summary on stderr (except if the Ass is not even started due to file not found etc.)
	char style	 ='b';		// 'b' / 'x' == default/binary/intel hex
// filepaths:
	cstr inputfile  = NULL;
	cstr outputfile = NULL;
	cstr listfile   = NULL;

//	eval arguments:
	int i=1;
	while(i<argc)
	{
		cptr s = argv[i++];

		if(*s != '-')
		{
			if(!inputfile)  { inputfile = s; continue; }
			if(!outputfile && i==argc) { outputfile = s; continue; }	// if outfile is not prefixed with -o then it must be the last argument
			if(!listfile)   { listfile = s; continue; }
			goto h;
		}

		while(char c = *++s)
		{
			switch(c)
			{
			case 'v': listv=yes; continue;
			case 'w': listw=yes; continue;
			case 's': silent=yes;continue;
			case 'x': style='x'; continue;
			case 'b': style='b'; continue;
			case 'i': if(inputfile  || i==argc) goto h; else inputfile  = argv[i++]; continue;
			case 'o': if(outputfile || i==argc) goto h; else outputfile = argv[i++]; continue;
			case 'l': if(listfile   || i==argc) goto h; else listfile   = argv[i++]; continue;
			default:  goto h;
			}
		}
	}

// check source file:
	if(!inputfile) h: abort(help, version, compiledatestr(), _PLATFORM);
	inputfile = fullpath(inputfile);
	if(errno) { fprintf(stderr, "--> sourcefile: %s\nzasm: 1 error\n", strerror(errno)); return 1; }
	if(!is_file(inputfile)) { fprintf(stderr, "--> sourcefile: not a regular file\nzasm: 1 error\n");  return 1; }

// check list file:
	if(listfile)
	{
		listfile = fullpath(listfile);
		if(errno && errno!=ENOENT) { fprintf(stderr, "--> listfile: %s\nzasm: 1 error\n", strerror(errno)); return 1; }
		if(lastchar(listfile)=='/') listfile = catstr( listfile, basename_from_path(inputfile), ".txt" );
	}

// check output file:
	if(!outputfile) outputfile = directory_from_path(inputfile);
	outputfile = fullpath(outputfile);
	if(errno && errno!=ENOENT) { fprintf(stderr, "--> outputfile: %s\nzasm: 1 error\n", strerror(errno)); return 1; }
	if(lastchar(outputfile)=='/') outputfile = catstr( outputfile, basename_from_path(inputfile), ".$" );	// '$' will be replaced by #target

// DO IT!
	Z80Assembler ass;
	ass.assembleFile( inputfile, outputfile, listfile, listv, listw, style);

	if(silent) return ass.errors.count()>0;		// 0=ok, 1=error(s)

	if(ass.errors.count()==0)
	{
		fprintf(stderr,"zasm: no errors\n");
		return 0; 	// 0 = ok
	}

// show errors on stderr:
	cstr current_file = NULL;
	for(uint i=0; i<min(10u,ass.errors.count()); i++)
	{
		Error const& e = ass.errors[i];
		SourceLine* sourceline = e.sourceline;
		if(!sourceline) { if(current_file) fprintf(stderr,"\n"); current_file=NULL; fprintf(stderr,"--> %s\n",e.text); continue; }

		cstr filename = sourceline->sourcefile;
		if(filename!=current_file)				// note: compare pointers!
		{
			current_file = filename;
			fprintf(stderr, "\nin file %s:\n", filename_from_path(filename));
		}

		cstr linenumber = numstr(sourceline->sourcelinenumber+1);
		fprintf(stderr, "%s: %s\n", linenumber, sourceline->text);
		fprintf(stderr, "%s%s^ %s\n", spacestr(strlen(linenumber)+2), sourceline->whitestr(), e.text);
	}

	if(ass.errors.count()>1) fprintf(stderr,"\nzasm: %i errors\n\n", (int)ass.errors.count());
	else					 fprintf(stderr,"\nzasm: 1 error\n\n");
	return 1;
}









































