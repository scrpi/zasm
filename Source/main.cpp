/*	Copyright  (c)	Günter Woigk 1994 - 2015
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
#include	"helpers.h"



cstr appl_name = "zasm";
cstr version   = "4.0.4";


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
"  zasm - z80 assembler (c) 1994-2015 Günter Woigk.\n"
"  version %s, %s, for %s.\n"										// version, date, platform
"  homepage: k1.spdns.de/Develop/Projects/zasm/\n"
"  send bug reports to: kio@little-bat.de\n\n"

"syntax:\n"
"  zasm [options] [-i] inputfile [[-l] listfile|dir] [[-o] outfile|dir]\n\n"

"  default output dir = source dir\n"
"  default list dir = output dir\n\n"

"examples:\n"
"  zasm speccirom.asm\n"
"  zasm -uwy emuf_rom.asm rom_v2.0.1.rom\n\n"

"options:\n"
"  -u  --opcodes   include object code in list file\n"
"  -w  --labels    append label listing to list file\n"
"  -y  --cycles    include cpu clock cycles in list file\n"
"  -b  --bin       write output to binary file (default)\n"
"  -x  --hex       write output in intel hex format\n"
"  -s  --s19       write output in motorola s-record format\n"
"  -z  --clean     clear intermediate files, e.g. compiled c files\n"
"  -o0             don't write output file\n"
"  -l0             don't write list file\n"
"  --z80           target Zilog Z80 (default)\n"
"  --hd64180       target Hitachi HD64180: enable additional instructions\n"
"  --8080          target Intel 8080: limit registers and instructions to 8080\n"
"  --asm8080       target Intel 8080: use 8080 assembler syntax\n"
"  -v[0,1,2]       verbosity of messages to stderr (0=off,1=default,2=more)\n"
"  --ixcbr2 | …xh  enable ill. instructions like 'set b,(ix+d),r' or 'set b,xh'\n"
"  --dotnames      allow label names starting with a dot '.'\n"
"  --nocolon       colons ':' after program label def. may be omitted\n"
"                  => label defs. must and instructions must not start in col. 1\n"
"  --casefold      label names are case blind (asm8080: implied)\n"
"  -c path/to/cc   set path to c compiler (default: sdcc in $PATH)\n"
"  -t path/to/dir  set path to temp dir for c compiler (default: output dir)\n"
"  -I path/to/dir  set path to c system header dir (default: sdcc default)\n"
"  -L path/to/dir  set path to standard library dir (default: none)\n\n"

"–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––\n"
"";





/* ---- program entry ---------------------------
*/
int main( int argc, cstr argv[] )
{
	double start = now();

// options:
	uint verbose     = 1;	// 0=off, 1=default, 2=verbose
	uint outputstyle = 'b';	// 0=none, 'b'=binary, 'x'=intel hex, 's'=motorola s-records
	uint liststyle   = 1;	// 0=none, 1=plain, 2=with objcode, 4=with label listing, 6=both, 8=clock cycles
	bool clean		 = no;
	bool ixcbr2		 = no;
	bool ixcbxh		 = no;
	bool targetZ80	 = no;
	bool target8080	 = no;
	bool target64180 = no;
	bool asm8080	 = no;
	bool dotnames    = no;
	bool nocolon     = no;	// note: complement meaning of ass.require_colon!
	bool casefold    = no;
// filepaths:
	cstr inputfile  = NULL;
	cstr outputfile = NULL;	// or dir
	cstr listfile   = NULL;	// or dir
	cstr tempdir    = NULL;
	cstr c_compiler = NULL;
	cstr c_includes	= NULL;
	cstr libraries	= NULL;

//	eval arguments:
	for(int i=1; i<argc; )
	{
		cptr s = argv[i++];

		if(s[0] != '-')
		{
			if(!inputfile)  { inputfile = s; continue; }
			// if outfile is not prefixed with -o then it must be the last argument:
			if(!outputfile && i==argc) { outputfile = s; continue; }
			if(!listfile)   { listfile = s; continue; }
			goto h;
		}

		if(s[1]=='-')
		{
			if(eq(s,"--clean"))    { clean = yes;     continue; }
			if(eq(s,"--bin"))	   { outputstyle='b'; continue; }
			if(eq(s,"--hex"))	   { outputstyle='x'; continue; }
			if(eq(s,"--s19"))	   { outputstyle='s'; continue; }
			if(eq(s,"--opcodes"))  { liststyle |= 2;  continue; }
			if(eq(s,"--labels"))   { liststyle |= 4;  continue; }
			if(eq(s,"--cycles"))   { liststyle |= 8;  continue; }
			if(eq(s,"--ixcbr2"))   { ixcbr2 = 1;      continue; }
			if(eq(s,"--ixcbxh"))   { ixcbxh = 1;      continue; }
			if(eq(s,"--z80"))      { targetZ80 = 1;   continue; }
			if(eq(s,"--8080"))     { target8080 = 1;  continue; }
			if(eq(s,"--asm8080"))  { asm8080 = 1;     continue; }
			if(eq(s,"--hd64180"))  { target64180 = 1; continue; }
			if(eq(s,"--dotnames")) { dotnames = 1;    continue; }
			if(eq(s,"--nocolon"))  { nocolon = 1;      continue; }
			if(eq(s,"--caseblind")){ casefold = 1;    continue; }
			if(eq(s,"--casefold")) { casefold = 1;    continue; }
			goto h;
		}

		while(char c = *++s)
		{
			switch(c)
			{
			case 'u': liststyle |= 2; continue;
			case 'w': liststyle |= 4; continue;
			case 'y': liststyle |= 8; continue;
			case 's': outputstyle=c; continue;
			case 'x': outputstyle=c; continue;
			case 'b': outputstyle=c; continue;
			case 'z': clean=yes; continue;

			case 'v': if(*(s+1)>='0' && *(s+1)<='3') verbose = *++s - '0'; else ++verbose; continue;

			case 'i': if(inputfile  || i==argc) goto h; else inputfile  = argv[i++]; continue;
			case 'o': if(*(s+1)=='0') { outputstyle = 0; ++s; continue; }
					  if(outputfile || i==argc) goto h; else outputfile = argv[i++]; continue;
			case 'l': if(*(s+1)=='0') { liststyle = 0; ++s; continue; }
					  if(listfile   || i==argc) goto h; else listfile   = argv[i++]; continue;

			case 'I': if(c_includes || i==argc) goto h; else c_includes = argv[i++]; continue;
			case 'L': if(libraries|| i==argc) goto h; else libraries= argv[i++]; continue;
			case 'c': if(c_compiler || i==argc) goto h; else c_compiler = argv[i++]; continue;
			case 't': if(tempdir    || i==argc) goto h; else tempdir    = argv[i++]; continue;
			default:  goto h;
			}
		}
	}


// check options:
	if(asm8080 || target8080)
	{
		if(target64180 || targetZ80)
		{
			fprintf(stderr,"--> %s\nzasm: 1 error\n", "i8080 and z80|hd64180 are mutually exclusive.");
			return 1;
		}

		if(ixcbr2 || ixcbxh)
		{
			fprintf(stderr,"--> %s\nzasm: 1 error\n", "i8080 has no prefix 0xCB instructions.");
			return 1;
		}

		target8080 = yes;
		if(asm8080) casefold = yes;
	}
	else // z80:
	{
		if(ixcbr2 && ixcbxh)
		{
			fprintf(stderr,"--> %s\nzasm: 1 error\n", "--ixcbr2 and --ixcbxh are mutually exclusive.");
			return 1;
		}

		targetZ80 = yes;
	}


// check source file:
	if(!inputfile) h: abort(help, version, compiledatestr(), _PLATFORM);
	inputfile = fullpath(inputfile);
	if(errno)
	{
		if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", inputfile, strerror(errno));
		return 1;
	}
	if(!is_file(inputfile))
	{
		if(verbose) fprintf(stderr, "--> %s: not a regular file\nzasm: 1 error\n", inputfile);
		return 1;
	}

// check output file or dir:
	if(!outputfile) outputfile = directory_from_path(inputfile);
	outputfile = fullpath(outputfile);
	if(errno && errno!=ENOENT)
	{
		if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", outputfile, strerror(errno));
		return 1;
	}

// check list file or dir:
	if(!listfile) listfile = directory_from_path(outputfile);
	listfile = fullpath(listfile);
	if(errno && errno!=ENOENT)
	{
		if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", listfile, strerror(errno));
		return 1;
	}

// check temp dir:
	if(!tempdir) tempdir = directory_from_path(outputfile);
	tempdir = fullpath(tempdir);
	if(errno && errno!=ENOENT)
	{
		if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", tempdir, strerror(errno));
		return 1;
	}
	if(lastchar(tempdir)!='/')
	{
		if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", tempdir, strerror(ENOTDIR));
		return 1;
	}

// check c_includes path:
	if(c_includes)
	{
		c_includes = fullpath(c_includes);
		if(errno==ok && lastchar(c_includes)!='/') errno = ENOTDIR;
		if(errno)
		{
			if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", c_includes, strerror(errno));
			return 1;
		}
	}

// check c_libraries path:
	if(libraries)
	{
		libraries = fullpath(libraries);
		if(errno==ok && lastchar(libraries)!='/') errno = ENOTDIR;
		if(errno)
		{
			if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", libraries, strerror(errno));
			return 1;
		}
	}

// check cc_path:
	if(c_compiler)
	{
		if(c_compiler[0]!='/')
		{
			cstr s = quick_fullpath(c_compiler);
			if(is_file(s))
				c_compiler = s;
			else
			{
				Array<str> ss;
				split(ss, getenv("PATH"), ':');
				for(uint i=0; i<ss.count(); i++)
				{
					s = catstr(ss[i],"/",c_compiler);
					if(is_file(s)) { c_compiler = s; break; }
				}
			}
		}

		c_compiler = fullpath(c_compiler);
		if(errno)
		{
			if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", c_compiler, strerror(errno));
			return 1;
		}
		if(!is_file(c_compiler))
		{
			if(verbose) fprintf(stderr, "--> %s: not a regular file\nzasm: 1 error\n", c_compiler);
			return 1;
		}
		if(!is_executable(c_compiler))
		{
			if(verbose) fprintf(stderr, "--> %s: not executable\nzasm: 1 error\n", c_compiler);
			return 1;
		}
	}

// DO IT!
	Z80Assembler ass;
	ass.verbose = verbose;
	ass.ixcbr2_enabled = ixcbr2;
	ass.ixcbxh_enabled = ixcbxh;
	ass.target_8080    = target8080;
	ass.target_z80     = targetZ80;
	ass.target_hd64180 = target64180;
	ass.syntax_8080    = asm8080;
	ass.require_colon  = !nocolon;
	ass.allow_dotnames = dotnames;
	ass.casefold_labels= casefold;
	if(c_includes) ass.c_includes = c_includes;
	if(libraries) ass.stdlib_dir = libraries;
	if(c_compiler) ass.c_compiler = c_compiler;
	ass.assembleFile( inputfile, outputfile, listfile, listfile, liststyle, outputstyle, clean );

	uint errors = ass.errors.count();

	if(verbose)		// show errors on stderr:
	{
		cstr current_file = NULL;
		for(uint i=0; i<min(10u,errors); i++)
		{
			Error const& e = ass.errors[i];
			SourceLine* sourceline = e.sourceline;
			if(!sourceline)
			{
				if(current_file) fprintf(stderr,"\n"); current_file=NULL; fprintf(stderr,"--> %s\n",e.text);
				continue;
			}

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

		fprintf(stderr, "assemble: %u lines\n", ass.source.count());
		fprintf(stderr, "time: %3.4f sec.\n", now()-start);
		if(errors>1) fprintf(stderr, "\nzasm: %u errors\n\n", errors);
		else fprintf(stderr, errors ? "\nzasm: 1 error\n\n" : "zasm: no errors\n");
	}

	return errors>0;	// 0=ok, 1=errors
}









































