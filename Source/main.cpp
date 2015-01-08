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
//#include	<dirent.h>
#include	"unix/FD.h"
#include	"unix/files.h"
#include	"kio/kio.h"
#include	"Z80Assembler.h"
#include	"helpers.h"



cstr appl_name = "zasm";
cstr version   = "4.0.5";


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
"  -e  --compare   compare own output to existing output file\n"
"  -T  --test      run self test (requires developer source tree)\n"
"  --maxerrors=NN  set maximum for reported errors (default=30, max=999)\n"
"  -o0             don't write output file\n"
"  -l0             don't write list file\n"
"  --z80           target Zilog Z80 (default)\n"
"  --z180          target Z180 / HD64180: enable additional instructions\n"
"  --8080          target Intel 8080: limit registers and instructions to 8080\n"
"  --asm8080       target Intel 8080: use 8080 assembler syntax\n"
"  -v[0,1,2]       verbosity of messages to stderr (0=off,1=default,2=more)\n"
"  --ixcbr2 | …xh  enable ill. instructions like 'set b,(ix+d),r' or 'set b,xh'\n"
"  --dotnames      allow label names starting with a dot '.'\n"
"  --reqcolon      colon ':' after program label definitions required\n"
"                  => label definitions and instructions may start in any column\n"
"  --casefold      label names are case insensitive (asm8080: implied)\n"
"  --flatops       no operator precedence: evaluate strictly from left to right\n"
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
	bool targetZ180  = no;
	bool asm8080	 = no;
	bool dotnames    = no;
	bool reqcolon    = no;
	bool casefold    = no;
	bool flatops	 = no;
	bool compare     = no;
	bool selftest    = no;
	uint maxerrors   = 30;
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
			if(eq(s,"--z180"))     { targetZ180 = 1;  continue; }
			if(eq(s,"--dotnames")) { dotnames = 1;    continue; }
			if(eq(s,"--reqcolon")) { reqcolon = 1;    continue; }
			if(eq(s,"--casefold")) { casefold = 1;    continue; }
			if(eq(s,"--flatops"))  { flatops = 1;     continue; }
			if(eq(s,"--compare"))  { compare = 1;     continue; }
			if(eq(s,"--test"))     { selftest = 1;    continue; }
			if(startswith(s,"--maxerrors="))
				{
					char* ep; ulong n = strtoul(s+12,&ep,10);
					if(*ep||n==0||n>999) goto h;
					maxerrors = (uint)n; continue;
				}
			goto h;
		}

		while(char c = *++s)
		{
			switch(c)
			{
			case 'e': compare = 1; continue;
			case 'T': selftest = 1; continue;
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

	if(selftest)
	{
		// assemble a bunch of sources from the Test/ directory
		// and compares them to old versions found in the original/ subdirectories.
		// if no path to the Test/ directory is given,
		// then a default path relative to the zasm executable is used: "$ZASM/../Test/"
		// all expected sources and original output files must be in place and match.

		cstr zasm = argv[0];
		cstr testdir = fullpath( inputfile ? inputfile : outputfile ? outputfile :
					   catstr(directory_from_path(zasm),"../Test/") );
		if(errno==ok && lastchar(testdir)!='/') errno = ENOTDIR;
		if(errno)
		{
			if(verbose) fprintf(stderr, "--> %s: %s\nzasm: 1 error\n", testdir, strerror(errno));
			return 1;
		}

		uint errors = 0;
		char opt[] = "-v0e"; opt[2] += verbose;

		change_working_dir(catstr(testdir,"Z80/"));
		{
		cstr a[] = { zasm, opt, "ZX Spectrum Rom/zx82.asm", "original/" };
		errors += main(NELEM(a),a);

		cstr c[] = { zasm,  opt, "--casefold", "CAMEL80-12/camel80.asm", "original/" };
		errors += main(NELEM(c),c);

		cstr d[] = { zasm, opt, "monitor_32k.asm", "original/" };
		errors += main(NELEM(d),d);

		cstr e[] = { zasm, opt, "allsrc.asm", "original/" };
		errors += main(NELEM(e),e);

		cstr f[] = { zasm, opt, "basic.asm", "original/" };
		errors += main(NELEM(f),f);

		cstr g[] = { zasm, opt, "MS-Basic.asm", "original/" };
		errors += main(NELEM(g),g);

		cstr h[] = { zasm, opt, "--reqcolon", "5bsort018.asm", "original/" };
		errors += main(NELEM(h),h);

		cstr i[] = { zasm, opt, "64#4+016.asm", "original/" };
		errors += main(NELEM(i),i);

		cstr j[] = { zasm, opt, "--8080", "CPM22.asm", "original/" };
		errors += main(NELEM(j),j);

		cstr k[] = { zasm, opt, "EMUF/EMUF.asm", "original/" };
		errors += main(NELEM(k),k);

		cstr l[] = { zasm, opt, "G007_MON_source_recreation.asm", "original/" };
		errors += main(NELEM(l),l);

		cstr n[] = { zasm, opt, "--reqcolon", "Hello World.asm", "original/" };
		errors += main(NELEM(n),n);

		cstr o[] = { zasm, opt, "--8080", "m80b.asm", "original/" };
		errors += main(NELEM(o),o);

		cstr p[] = { zasm, opt, "monitor_32k.asm", "original/" };
		errors += main(NELEM(p),p);

		cstr q[] = { zasm, opt, "MS-Basic.asm", "original/" };
		errors += main(NELEM(q),q);

		cstr r[] = { zasm, opt, "mybios4_mod.asm", "original/" };
		errors += main(NELEM(r),r);

		cstr s[] = { zasm, opt, "--dotnames", "--reqcolon", "OpenSE Basic/opense.asm", "original/" };
		errors += main(NELEM(s),s);

		cstr b[] = { zasm, opt, "ZX Spectrum Rom/sc178.asm", "original/" };
		errors += main(NELEM(b),b);

		cstr m[] = { zasm, opt, "test z80 cpu - prelim.asm", "original/" };
		errors += main(NELEM(m),m);

		cstr t[] = { zasm, opt, "--reqcolon", "VZ200 RS232 Terminal/VZ RS232.asm", "original/" };
		errors += main(NELEM(t),t);

		cstr u[] = { zasm, opt, "wmfw/wmfw2_5_orig.asm", "original/" };
		errors += main(NELEM(u),u);

		cstr v[] = { zasm, opt, "z80mon.asm", "original/" };
		errors += main(NELEM(v),v);

		cstr w[] = { zasm, opt, "z80sourc.asm", "original/" };
		errors += main(NELEM(w),w);

		cstr x[] = { zasm, opt, "--reqcolon", "zx81v2.asm", "original/" };
		errors += main(NELEM(x),x);

		cstr y[] = { zasm, opt, "--ixcbr2", "zasm-test-opcodes.asm", "original/" };
		errors += main(NELEM(y),y);
		}

		change_working_dir(catstr(testdir,"8080/"));
		{
		cstr a[] = { zasm, opt, "--asm8080", "--reqcolon", "Altair8800_Monitor.asm", "original/" };
		errors += main(NELEM(a),a);

		cstr b[] = { zasm, opt, "8080PRE.asm", "original/" };
		errors += main(NELEM(b),b);

		cstr y[] = { zasm, opt, "zasm-test-opcodes.asm", "original/" };
		errors += main(NELEM(y),y);
		}

		change_working_dir(catstr(testdir,"Z180/"));
		{
		cstr a[] = { zasm, opt, "--z180", "first.asm", "original/" };
		errors += main(NELEM(a),a);

		cstr b[] = { zasm, opt, "--z180", "counter master.asm", "original/" };
		errors += main(NELEM(b),b);

		cstr y[] = { zasm, opt, "zasm-test-opcodes.asm", "original/" };
		errors += main(NELEM(y),y);
		}

		if(verbose)
		{
			fprintf(stderr, "total time: %3.4f sec.\n", now()-start);
			if(errors>1) fprintf(stderr, "\nzasm: %u errors\n\n", errors);
			else fprintf(stderr, errors ? "\nzasm: 1 error\n\n" : "zasm: no errors\n");
		}
		return errors>0;
	}

// check options:
	if(asm8080)
	{
		target8080 = yes;
		casefold = yes;
	}

	if(target8080)
	{
		if(targetZ180 || targetZ80)
		{
			fprintf(stderr,"--> %s\nzasm: 1 error\n", "i8080 and z80|z180 are mutually exclusive.");
			return 1;
		}

		if(ixcbr2 || ixcbxh)
		{
			fprintf(stderr,"--> %s\nzasm: 1 error\n", "i8080 has no prefix 0xCB instructions.");
			return 1;
		}
	}
	else if(targetZ180)
	{
		if(ixcbr2 || ixcbxh)
		{
			fprintf(stderr,"--> %s\nzasm: 1 error\n", "no --ixcb… allowed: the Z180 traps illegal instructions");
			return 1;
		}

		targetZ80 = yes;		// implied
	}
	else // Z80
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
	ass.target_z180    = targetZ180;
	ass.syntax_8080    = asm8080;
	ass.require_colon  = reqcolon;
	ass.allow_dotnames = dotnames;
	ass.casefold_labels= casefold;
	ass.flat_operators = flatops;
	ass.max_errors     = maxerrors;
	ass.compare_to_old = compare;
	if(c_includes) ass.c_includes = c_includes;
	if(libraries) ass.stdlib_dir  = libraries;
	if(c_compiler) ass.c_compiler = c_compiler;
	ass.assembleFile( inputfile, outputfile, listfile, listfile, liststyle, outputstyle, clean );

	uint errors = ass.errors.count();

	if(verbose)		// show errors on stderr:
	{
		cstr current_file = NULL;
		for(uint i=0; i<errors; i++)
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









































