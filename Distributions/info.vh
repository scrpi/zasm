/*
	this file is included by index.cgi
*/


var TITLE	 = «zasm - z80 assembler - download page»
var KEYWORDS = «zasm, z80 assembler, z80, asm, ZX Spectrum, kio, Assembler, download, source, binary, appl»
var ROBOTS	 = «index,nofollow»

var DESCRIPTION =
«zasm - z80 assembler - download page.
zasm is a command line z80 assembler for unix-style operating systems.
it is available as source and some precompiled binaries, e.g. for Linux and MacOS X.
it creates output in binary or intel hex file format and can create
some special formats for ZX Spectrum emulators, e.g. ".TAP" and ".SNA" files.»


var MAIN = 
«
h2	Welcome to the zasm download page
p	zasm is a command-line z80 assembler. it is available as source, which should compile on many unix-style operating systems, and as pre-compiled binary. Choose a version from the list below. Archive names indicate version, date and build host. Select a binary which matches your OS best. If it fails to run then use the source.

h4	What's new
p	<b>4.0</b> started a new branch, added a <a href="/Git/">Git repository</a>.

h4 Git Repository
p	You can checkout the source from my Git repository:
	• git clone http://k1.spdns.de/Git/zasm.git
	• git clone http://k1.spdns.de/Git/Libraries.git
	
h4	Online Assembler
p	A cgi interface for online assembling is at <http:/cgi-bin/zasm.cgi>. It allows to assemble z80 assembler source files with up to 1 include file. It can generate binary or Intel hex output and a listing, which can optionally include the generated assembler source and a label listing. Don't assemble closed-source sources here. <b>;-)</b>

h4	Archive
»


