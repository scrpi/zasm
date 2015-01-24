; CDL's Z80 Macro Assembler C12012-0312                                  Page 1

; Exerpt from start of Zapple 2K monitor PRN file as from TDL
; Exerpt by Herb Johnson Aug 2012


;       << ZAPPLE 2-K MASKED ROM MONITOR SYSTEM >>
;                       by
;
;       COMPUTER DESIGN LABS
;       342 COLUMBUS AVE.
;       TRENTON, NEW JERSEY 08629
;
;
;       COPYRIGHT 1979 COMPUTER DESIGN LABS;
;
#target bin
;.PABS   ;THIS MONITOR IN ABSOLUTE FORMAT
;
;
#code BASE,0F000H,0800H
#data USER,BASE+800H
;BASE    = 0F000H
;USER    = BASE+800H
;
;
RST7    = 38H   ;RST 7 (LOCATION FOR TRAP)
IOBYT   = 76H   ;R/W PORT FOR TEMP. STORAGE
SENSE   = 7AH   ;SWITCH WORD FOR INITIAL DEFAULT
SWITCH  = 0FFH  ;TEST PORT TO ABORT READ OPERATION
RCP     = 7AH   ;READER CONTROL PORT (OUT)
NN      = 0F8H  ;"I" REGISTER INITIAL VALUE
;
;       <I/O DEVICES>
;
;-TELEPRINTER
;
TTI     = 71H   ;DATA IN PORT
TTO     = 71H   ;DATA OUT PORT
TTS     = 70H   ;STATUS PORT (IN)
TTYDA   = 1     ;DATA AVAILABLE MASK BIT
TTYBE   = 02    ;XMTR BUFFER EMPTY MASK
;
;-C.R.T. SYSTEM
;
CRTI    = 73H   ;DATA PORT (IN)
CRTS    = 72H   ;STATUS PORT (IN)
CRTO    = 73H   ;DATA PORT (OUT)
CRTDA   = 1     ;DATA AVAILABLE MASK
CRTBE   = 02    ;XMTR BUFFER EMPTY MASK
;
;-CASSETTE SYSTEM
;
RCSD    = 75H   ;DATA IN PORT
RCSS    = 74H   ;STATUS PORT (IN)
RCSDA   = 1     ;DATA AVAILABLE MASK
PCASO   = 75H   ;DATA PORT (OUT)
PCASS   = 74H   ;CONTROL PORT (OUT)
PCSBE   = 02    ;XMTR BUFFER EMPTY MASK
;
;       <CONSTANTS>
;
FALSE   = 0             ;ISN'T SO
TRUE    = # FALSE       ;IT IS SO
CR      = 0DH           ;ASCII CARRIAGE RETURN
LF      = 0AH           ;ASCII LINE FEED
BELL    = 7             ;DING
RUB     = 0FFH          ;RUB OUT
FIL     = 00            ;FILL CHARACTERS AFTER CRLF
MAX     = 7             ;NUMBER OF QUES IN EOF
;
;       <I/O CONFIGURATION MASKS>
;
CMSK    = 11111100B     ;CONSOLE DEVICE
RMSK    = 11110011B     ;STORAGE DEVICE (IN)
PMSK    = 11001111B     ;STORAGE DEVICE (OUT)
LMSK    = 00111111B     ;LIST DEVICE
;
;
;-CONSOLE CONFIGURATION
CTTY    = 0     ;TELEPRINTER
CCRT    = 1     ;C.R.T.
BATCH   = 2     ;READER FOR INPUT, LIST FOR OUTPUT
CUSE    = 3     ;USER DEFINED
;
;-STORAGE INPUT CONFIGURATION
RTTY    = 0     ;TELEPRINTER READER
RPTR    = 4     ;HIGH-SPEED RDR (EXTERNAL ROUTINE)
RCAS    = 8     ;CASSETTE
RUSER   = 0CH   ;USER DEFINED
;
;-STORAGE OUTPUT CONFIGURATION
PTTY    = 0     ;TELEPRINTER PUNCH
PPTP    = 10H   ;HIGH-SPEED PUNCH (EXTERNAL ROUTINE)
PCAS    = 20H   ;CASSETTE
PUSER   = 30H   ;USER DEFINED
;
;-LIST DEVICE CONFIGURATION
LTTY    = 0     ;TELEPRINTER PRINTER
LCRT    = 40H   ;C.R.T. SCREEN
LINE    = 80H   ;LINE PRINTER (EXTERNAL ROUTINE)
LUSER   = 0C0H  ;USER DEFINED
;
;
;       VECTORS FOR USER DEFINED ROUTINES
;
#data USER
;.LOC    USER
CILOC:  .BLKB 3 ;CONSOLE INPUT
COLOC:  .BLKB 3 ;CONSOLE OUTPUT
RPTPL:  .BLKB 3 ;HIGH-SPEED READER
RULOC:  .BLKB 3 ;USER DEFINED STORAGE (INPUT)
PTPL:   .BLKB 3 ;HIGH-SPEED PUNCH
PULOC:  .BLKB 3 ;USER DEFINED STORAGE (OUTPUT)
LNLOC:  .BLKB 3 ;LINE PRINTER
LULOC:  .BLKB 3 ;USER DEFINED PRINTER
CSLOC:  .BLKB 3 ;CONSOLE INPUT STATUS ROUTINE

;       PROGRAM CODE BEGINS HERE
;
#code BASE
;.LOC    BASE
        JMP     BEGIN   ;GO AROUND VECTORS
;
;       <VECTORS FOR CALLING PROGRAMS>
;
; THESE VECTORS MAY BE USED BY USER WRITTEN
; PROGRAMS TO SIMPLIFY THE HANDLING OF I/O
; FROM SYSTEM TO SYSTEM.  WHATEVER THE CURRENT
; ASSIGNED DEVICE, THESE VECTORS WILL PERFORM
; THE REQUIRED I/O OPERATION, AND RETURN TO
; THE CALLING PROGRAM. (RET)
;
; THE REGISTER CONVENTION USED FOLLOWS-
;
; ANY INPUT OR OUTPUT DEVICE-
;       CHARACTER TO BE OUTPUT IN 'C' REGISTER.
;       CHARACTER WILL BE IN 'A' REGISTER UPON
;       RETURNING FROM AN INPUT OR OUTPUT.
; 'CSTS'-
;       RETURNS TRUE (0FFH IN 'A' REG.) IF THERE IS
;       SOMETHING WAITING, AND ZERO (00) IF NOT.
; 'IOCHK'-
;       RETURNS WITH THE CURRENT I/O CONFIGURATION
;       BYTE IN 'A' REGISTER.
; 'IOSET'-
;       ALLOWS A PROGRAM TO DYNAMICALLY ALTER THE
;       CURRENT I/O CONFIGURATION, AND REQUIRES
;       THE NEW BYTE IN 'C' REGISTER.
; 'MEMCK'-
;       RETURNS WITH THE HIGHEST ALLOWED USER
;       MEMORY LOCATION. 'B'=HIGH BYTE, 'A'=LOW.
; 'TRAP'-
;       THIS IS THE 'BREAKPOINT' ENTRY POINT,
;       BUT MAY BE 'CALLED'. IT WILL SAVE
;       THE MACHINE STATE. RETURN CAN BE MADE WITH
;       A SIMPLE 'G[CR]' ON THE CONSOLE.
;
        JMP     CI      ;CONSOLE INPUT
        JMP     RI      ;READER INPUT
        JMP     CO      ;CONSOLE OUTPUT
        JMP     PO      ;PUNCH OUTPUT
        JMP     LO      ;LIST OUTPUT
        JMP     CSTS    ;CONSOLE STATUS
        IN      IOBYT   ;I/O CHECK
        RET
        JMP     IOSET   ;I/O SET
        JMP     MEMCK   ;MEMORY LIMIT CHECK
TRAP:   JMP     RESTART ;BREAKPOINT
;
;       ANNOUNCEMENT OF MONITOR NAME & VERSION
;
MSG:    .BYTE   CR,LF,FIL,FIL,FIL
        .ASCII  'Zapple V'
        .ASCII  '1.0R'
MSGL    = .-MSG
;
;       LET US BEGIN
;
BEGIN:  MVI     A,053H  ;INITIALIZE THE HARDWARE
        OUT     TTS
        OUT     CRTS
        OUT     RCSS
        MVI     A,051H
        OUT     TTS
        OUT     CRTS
        DCR     A
        OUT     RCSS
        XRA     A
        OUT     IOBYT+1
        OUT     RCP     ;CLEAR RDR CONTROL PORT
        DCR     A
        OUT     IOBYT
        MVI     A,4
        OUT     IOBYT+1 ;WHEW!
;
        IN      SENSE   ;INITIALIZE I/O CONFIGURATION
        OUT     IOBYT
        MVI     A,NN    ;INITIAL 'I' REG. CONFIGURATION
        STAI            ;SET FOR PAGE 'NN' ON INTERUPT

        LXI     SP,AHEAD-4      ;SET UP A FAKE STACK
        JMP     MEMSIZ+1        ;GET MEMORY SIZE
        .WORD   AHEAD
AHEAD:  SPHL            ;SET TRUE STACK
        XCHG
        LXI     B,ENDX-EXIT
        LXI     H,EXIT
        LDIR            ;MOVE TO RAM
        XCHG
        LXI     B,-5FH  ;SET UP A USER'S STACK VALUE
        DAD     B
        PUSH    H       ;PRE-LOAD STACK VALUE
        LXI     H,0     ;INITIALIZE OTHER REGISTERS
        MVI     B,10    ; (20 OF THEM)
STKIT:  PUSH    H       ; TO ZERO
        DJNZ    STKIT
HELLO:  MVI     B,MSGL  ;SAY HELLO TO THE FOLKS 
        CALL    TOM1    ;OUTPUT SIGN-ON MSG
START:  LXI     D,START ;MAIN 'WORK' LOOP
        PUSH    D       ;SET UP A RETURN TO HERE
        CALL    CRLF
        MVI     C,'>'
        CALL    CO
STAR0:  CALL    TI      ;GET A CONSOLE CHARACTER
        ANI     7FH     ;IGNORE NULLS
        JRZ     STAR0   ;GET ANOTHER
        SUI     'A'     ;QUALIFY THE CHARACTER
        RM              ;<A
        CPI     'Z'-'A'+1
        RNC             ;INVALID CHARACTER
        ADD     A       ;A*2
        LXI     H,TBL   ;POINT TO COMMAND TABLE
        ADD     L       ;ADD IN DISPLACEMENT
        MOV     L,A
        MOV     A,M
        INX     H
        MOV     H,M
        MOV     L,A
        MVI     C,2     ;SET C UP
        PCHL            ;GO EXECUTE COMMAND.
;
;               <COMMAND BRANCH TABLE>
;
TBL:
.WORD   ASSIGN  ;A - ASSIGN I/O
.WORD   BYE     ;B - SYSTEM SHUT-DOWN
.WORD   COMP    ;C - COMPARE MEMORY VS. READER INPUT
.WORD   DISP    ;D - DISPLAY MEMORY ON CONS. IN HEX
.WORD   EOF     ;E - END OF FILE TAG FOR HEX DUMPS
.WORD   FILL    ;F - FILL MEMORY WITH A CONSTANT
.WORD   GOTO    ;G - GOTO [ADDR]<,>BREAKPOINTS (2)
.WORD   HEXN    ;H - HEX MATH. <SUM>,<DIFFERENCE>
.WORD   J       ;I * USER DEFINED
        J=J+3   ;INCREMENT VECTOR ADDR
.WORD   TEST    ;J - NON-DESTRUCTIVE MEMORY TEST
.WORD   J       ;K * USER DEFINED
        J=J+3   ;INCREMENT VECTOR ADDR
.WORD   LOAD    ;L - LOAD A BINARY FORMAT FILE
.WORD   MOVE    ;M - MOVE BLOCKS OF MEMORY
.WORD   NULL    ;N - PUNCH NULLS ON PUNCH DEVICE
.WORD   J       ;O * USER DEFINED
.WORD   PUTA    ;P - 'PUT' ASCII INTO MEMORY.
.WORD   QUERY   ;Q - QI(N)=DISP. N; QO(N,V)=OUT N,V
.WORD   READ    ;R - READ A HEX FILE (W/CHECKSUMS)
.WORD   SUBS    ;S - SUBSTITUTE &/OR EXAMINE MEMORY
.WORD   TYPE    ;T - TYPE MEMORY IN ASCII
.WORD   UNLD    ;U - MEMORY TO PUNCH (BINARY FORMAT)
.WORD   VERIFY  ;V - COMPARE MEMORY AGAINST MEMORY
.WORD   WRITE   ;W - MEMORY TO PUNCH (HEX FORMAT)
.WORD   XAM     ;X - EXAMINE & MODIFY CPU REGISTERS
.WORD   WHERE   ;Y - FIND SEQUENCE OF BYTES IN MEM.
.WORD   SIZE    ;Z - ADDRESS OF LAST R/W LOCATION
;
;
;
;       THIS ROUTINE CONTROLS THE CONFIGURATION
; OF THE VARIOUS I/O DRIVERS & DEVICES. THIS IS
; ACCOMPLISHED VIA A HARDWARE READ/WRITE PORT.
;       THIS PORT IS INITIALIZED UPON SIGN-ON
; BY THE VALUE READ ON PORT 'SENSE'.  IT MAY BE
; DYNAMICALLY MODIFIED THROUGH CONSOLE COMMANDS.
;
; THE VALUE ON THE 'IOBYT' PORT REPRESENTS THE
; CURRENT CONFIGURATION.  IT IS STRUCTURED THUSLY:
;
; 000000XX - WHERE XX REPRESENTS THE CURRENT CONSOLE.
; 0000XX00 - WHERE XX REPRESENTS THE CURRENT READER.
; 00XX0000 - WHERE XX REPRESENTS THE CURRENT PUNCH.
; XX000000 - WHERE XX REPRESENTS THE CURRENT LISTER.
;
; WHEN XX = 00, THE DEVICE IS ALWAYS THE
; TELEPRINTER.  WHEN XX = 11, THE DEVICE IS ALWAYS THE

; USER DEFINED.  SEE OPERATORS MANUAL FOR FURTHER
; DETAILS.
;
ASSIGN: CALL    TI      ;GET DEVICE NAME
        LXI     H,LTBL  ;POINT TO DEVICE TABLE
        LXI     B,400H  ;4 DEVICES TO LOOK FOR
        LXI     D,5     ;IDENTIFIER + 4 DEV. IN TABLE
..A0:   CMP     M       ;LOOK FOR MATCH
        JRZ     ..A1
        DAD     D       ;GO THRU TABLE
        INR     C       ;KEEP TRACK OF DEVICE
        DJNZ    ..A0
        JMPR    ..ERR   ;WRONG IDENTIFIER
..A1:   MOV     E,C     ;SAVE DEVICE NUMBER
..A2:   CALL    TI      ;SCAN PAST '='
        CPI     '='
        JRNZ    ..A2
        CALL    TI      ;GET NEW ASSIGNMENT
        LXI     B,400H  ;4 POSSIBLE ASSIGNMENTS
..A3:   INX     H       ;POINT TO ASSIGNMENT NAME
        CMP     M       ;LOOK FOR PROPER MATCH
        JRZ     ..A4    ;MATCH FOUND
        INR     C       ;KEEP TRACK OF ASSIGNMENT NMBR

        DJNZ    ..A3
..ERR:  JMP     ERROR   ;NO MATCH, ERROR
..A4:   MVI     A,3     ;SET UP A MASK
        INR     E
..A5:   DCR     E       ;DEVICE IN E
        JRZ     ..A6    ;GOT IT
        SLAR    C       ;ELSE MOVE MASKS
        SLAR    C
        RAL
        RAL             ;A=DEVICE MASK
        JMPR    ..A5
..A6:   CMA             ;INVERT FOR AND'ING
        MOV     D,A     ;SAVE IN D
..A7:   CALL    PCHK    ;WAIT FOR [CR]
        JRNC    ..A7


