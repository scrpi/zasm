/*
	this file is included by index.cgi
*/


var TITLE	 = «zasm - z80 assembler - download page»
var KEYWORDS = «zasm, z80, z180, 8080, ZX80, ZX81, Jupiter Ace, ZX Spectrum, CP/M, kio, Assembler, download, sdcc»
var ROBOTS	 = «index,nofollow»

var DESCRIPTION =
«zasm - z80 assembler - download page.
zasm is a command line z80 assembler for unix-style operating systems.
it is available as source and some precompiled binaries, e.g. for Linux and MacOS X.
it can create output in binary intel hex or motorola S19 file format and can create some special formats 
for ZX80, ZX81, ZX Spectrum and Jupiter Ace emulators, e.g. .TAP, .O, .P, .SNA, .Z80 and .ACE files.
zasm can include c source files which are compiled with the help of sdcc.»


var MAIN = 
«
h2	Welcome to the zasm download page
p	zasm is a command-line assembler for the Zilog Z80 cpu. it is available as source, which should compile on many unix-style operating systems, and as pre-compiled binary. Choose a version from the list below. Archive names indicate version, date and build host. Select a binary which matches your OS best. If it fails to run then use the source.
p.center	Quick links: <a href="../Documentation/">Documentation</a>, <a href="/cgi-bin/zasm.cgi">zasm online assembler</a>, <a href="/Git/zasm-4.0.git">Git repository</a>.

h4	Overview
p	zasm is a <span class=blue>Z80 assembler</span>.
	zasm can assemble code extended for the <span class=blue>Hitachi HD64180</span>.	
	zasm can assemble code limited to the <span class=blue>Intel 8080</span> e.g. for CP/M. 
	zasm can also assemble native 8080 assembler source.
	zasm can compile and <span class=blue>include c source files</span>. (choose the version with sdcc below)
	zasm can generate <span class=blue>binary files</span> or <span class=blue>Intel Hex</span> or <span class=blue>Motorola S19</span> files.
	zasm supports generating various special files for <span class=blue>Sinclair</span> and <span class=blue>Jupiter Ace</span> emulators.
	zasm supports <span class=blue>character set conversion</span>, e.g. for the ZX80 and ZX81.
	The list file can include the <span class=blue>generated opcodes</span> and <span class=blue>accumulated cpu cycles</span> and a list of all labels.
	zasm supports various historically used syntax variants and the syntax emitted by sdcc.
	zasm supports multiple code and data segments, nested conditional assembly and nested local scopes.

h4	Typical invocations
pre.white	# assemble file into myrom.bin and create plain list file 
	> <b>zasm myrom.asm	</b>
pre.bgltgrn	# assemble file into myrom.bin and create list file with opcodes, cpu cycles and labels list
	> <b>zasm -uwy myrom.asm </b>
pre.white	# assemble source for the Intel 8080 cpu using Z80 syntax
	> <b>zasm --8080 myrom.asm </b>
pre.bgltgrn	# assemble native 8080 assembler source
	> <b>zasm --asm8080 myrom.asm </b>
pre.white	# create Intel hex output
	> <b>zasm -x myrom.asm </b>
	

h4	Major changes from version 3.0 to version 4.0
ul
li	The command line options have changed: start <tt class=blue>zasm</tt> with no options to see a summary.
li	zasm can include <span class=blue>c source files</span>, with the help of <tt class=blue>sdcc</tt>. (a trimmed version of sdcc is included in the OSX distro.)
li	zasm can assemble native <span class=blue>8080 assembler</span> source files.
li	syntax for <tt class=blue>#code</tt> and <tt class=blue>#data</tt> has changed: an additional first argument, the segment name, is now required.
li	Alternately you can use just <tt class=blue>org</tt>, which will create and use a default code segment.
li	Behavior of <tt class=blue>org</tt> (in subsequent code) has changed: now it <u>does</u> insert space.
li	For the old behavior use <tt class=blue>.phase</tt> and <tt class=blue>.dephase</tt>.
li	<span class=blue>Macros</span> and <tt class=blue>rept</tt> are now supported.
li	The required source layouts for the various special output files (tape, sna, etc.) have changed. 
li	A <span class=blue>character set conversion</span> for strings and character literals can be defined. 

h4	What's new
p	<b>2015-01-01</b>: Version 4.0.2: added support for native 8080 assembler source
	<b>2015-01-01</b>: Version 4.0.3: added more support for alternate/various/weird syntax 
	<b>2015-01-04</b>: Version 4.0.4: added macro and rept, .phase and .dephase
	<b>2015-01-08</b>: Version 4.0.5: #define, test suite, --flatops, Linux version
	<b>2015-01-15</b>: Version 4.0.7: "extended arguments" in macros with '<' … '>'
	<b>2015-01-18</b>: Version 4.0.8: fixed bug in .ACE file export

h4 Git Repository
p	You can checkout the source from my Git repository:
	• git clone http://k1.spdns.de/Git/zasm-4.0.git
	• git clone http://k1.spdns.de/Git/Libraries.git
	
h4	Online Assembler
p	A cgi interface for online assembling is at <a href="/cgi-bin/zasm.cgi">cgi-bin/zasm.cgi</a>. It allows to assemble z80 assembler source files with some include files. It can generate binary, Intel hex or Motorola S19 output and a listing, which can optionally include the generated assembler source, a label listing and accumulated cpu cycles. You can even include c source files here! Don't assemble closed-source sources here. <b>;-)</b>

h4	Archive
p	Here you find zasm and matched versions of sdcc for OSX/BSD and Linux. If you have sdcc already installed you still need "<span class=blue>sdcc for zasm (headers and libs).zip</span>" for the modified <tt>libs/</tt> folder.
»


