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
it creates output in binary intel hex or motorola S19 file format and can create
some special formats for ZX Spectrum emulators, e.g. ".TAP" and ".SNA" files.
zasm can include c source files which are compiled with the help of sdcc.»


var MAIN = 
«
h2	Welcome to the zasm download page
p	zasm is a command-line z80 assembler. it is available as source, which should compile on many unix-style operating systems, and as pre-compiled binary. Choose a version from the list below. Archive names indicate version, date and build host. Select a binary which matches your OS best. If it fails to run then use the source.

h4	Overview
p	zasm supports assembling code for the <span class=blue>Intel 8080</span> using Z80 syntax. 
	<i>Support for the weird 8080 syntax will come soon.</i>
	It also supports the additional opcodes for the <span class=blue>Hitachi HD64180</span>.
	zasm can generate <span class=blue>binary files</span> or <span class=blue>Intel Hex</span> or <span class=blue>Motorola S19</span> files.
	zasm supports generating various specialized files for <span class=blue>Sinclair</span> and <span class=blue>Jupiter Ace</span>.
	The list file can include the <span class=blue>generated opcodes</span> and <span class=blue>accumulated cpu cycles</span>.
	zasm supports various historically used syntax variants and the syntax emitted by sdcc.
	zasm supports <span class=blue>character set conversion</span>, e.g. for the ZX80 and ZX81.

h4	What's new
p	<b>4.0</b> started a new branch, added a <a href="/Git/">Git repository</a>.
    <b>2014-12-28</b>: Version 4.0.0 released.

h4 Git Repository
p	You can checkout the source from my Git repository:
	• git clone http://k1.spdns.de/Git/zasm.git
	• git clone http://k1.spdns.de/Git/Libraries.git
	
h4	Online Assembler
p	A cgi interface for online assembling is at <http:/cgi-bin/zasm.cgi>. It allows to assemble z80 assembler source files with up to 1 include file. It can generate binary or Intel hex output and a listing, which can optionally include the generated assembler source and a label listing. Don't assemble closed-source sources here. <b>;-)</b>

h4	Archive
»


