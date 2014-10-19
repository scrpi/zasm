
	
; ---------------------------------------------------------------
; 					THE SYSTEM VARIABLES
; ---------------------------------------------------------------

; #target tap:
; The sysvars are actually not stored in most tape files.
; Just include this file to define names for the system variables so you can easily refer to them.
;
; #target sna or z80:
; The sysvar bytes are actually stored in the snapshot file.
; Copy&paste the sysvar definitions into a #code segment starting at $5C00 and fill in proper values.

	
RAMBEG:	equ	$4000	; start of ram
BILD:	equ	$4000	; 0x1800 bytes: screen pixels
ATTRSP: equ	$5800	; 0x300 bytes: screen attributes
PTRBUF: equ	$5B00	; 0x100 bytes: printer buffer


#data SYSVARS, 0x5C00, 23734-0x5C00

    
KSTATE: ds	8		; buffer for keyboard (2 keys)
LASTK:	db	0		; last key code
REPDEL: db	0		; delay until first key repeat
REPPER: db	0		; delay for auto repeat
DEFADD: dw	0		; argument for functions
KDATA:	db	0		; color of keybord input
TVDATA: dw	0		; Color, AT and TAB position
STRMS:	ds	38		; table of opened channels
CHARS:	dw	0		; address of character set -256 (~ address of char code 0x00)
RASP:	db	0		; length of warning buzz
PIP:	db	0		; 

;IY zeigt immer auf ERRNR

ERRNR:	db	0		;fuer Meldungen: Nummer -1; keine Meldung :0xFF
FLAGS:	db	0		;Bit 1 = Printer ein
TVFLAG: db	0		;Flagbyte fuer Bildschirm
ERRSP:	dw	0		;Errorstackpointer
LISTSP: dw	0		;Returnadresse bei LIST
MODE:	db	0		;Tastenmodus (K,L,C,E,G)
NEWPPC: dw	0		;Nr. der Zeile, wohin gesprungen wird (GOTO usw.)
NSPPC:	db	0		;Befehl der Zeile bei Sprung
PPC:	dw	0		;aktuelle Nr. der Basiczeile
SUBPPC: db	0		;Zeiger auf Befehl der Zeile
BORDCR: db	0		;Bordercolor * 8
EPPC:	dw	0		;aktuelle Editorzeile
VARS:	dw	0		;Beginn der Variablen
DEST:	dw	0		;Variablenadresse bei Zuweisung
CHANS:	dw	0		;Pointer fuer Kanaldaten
CURCHL: dw	0		;aktuelle I/O Infoadresse
PROG:	dw	0		;Start des Basicprogrammes
NXTLIN: dw	0		;Adresse der naechsten Basiczeile
DATADD: dw	0		;Zeiger auf Endbyte der letzten Daten
ELINE:	dw	0		;Adresse eines eingegebenen Befehls
KCUR:	dw	0		;Kursoradresse
CHADD:	dw	0		;naechstes zu interpret. Zeichen
XPTR:	dw	0		;Adresse des Zeichens nach ? bei Error
WORKSP: dw	0		;derzeitiger Workspace
STKBOT: dw	0		;Anfang des Calculatorstacks
STKEND: dw	0		;Anfang des freien Speichers
BREG:	db	0		;Calculator Hifsregister
MEM:	dw	0		;Zeiger auf Calculatorspeicher
FLAGS2: db	0		;Flags Teil 2
DFSZ:	db	0		;Zeilenanzahl+1 im unteren Bildschirmteil
STOP:	dw	0		;Nummer der obersten Zeile eines Listings
OLDPPC: dw	0		;Zeilennummer fuer Continue
OSPCC:	db	0		;naechster Befehl fuer Cont.
FLAGX:	db	0		;Flag Teil 3
STRLEN: dw	0		;Laenge eines Strings
TADDR:	dw	0		;Address of next ITEM in Syntax-Table
SEED:	dw	0		;Zufallszahl setzen durch Randomize
FRAMES: ds	3		;3 Byte Bildzaehler (Uhr)
UDG:	dw	0		;Adresse der User Grafikzeichen
COORDS: dw	0		;Koordinaten des letzten Plot
PPOSN:	db	0
PRCC:	dw	0		;fuer Printer - Buffer
ECHOE:	dw	0		;Position fuer Input
DFCC:	dw	0		;Printadresse im Displayfile
DFCCL:	dw	0		;Printadresse im unteren Teil
SPOSN:	dw	0		;33-Col/24-Zeilennr. fuer Print
SPOSNL: dw	0		;33-Col/24-Zeilennr. unt. Teil
SCRCT:	db	0		;Scrollzaehler
ATTRP:	db	0		;aktuelle Farben permanent
MASKP:	db	0
ATTRT:	db	0		;aktuelle Farben temporaer
MASKT:	db	0		;aktuelle Farbe transp./temp.
PFLAG:	db	0
MEMBOT: ds	30		;Calculatorspeicher
NMIREG: dw	0
RAMTOP: dw	0		;letzte Speicheradresse fuer Basic
PRAMT:	dw	0		;letzte Speicheradresse
KANMEM:	equ	$


