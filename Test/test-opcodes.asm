;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; $Id: testz80.asm 1.4 1998/02/25 12:18:20 toma Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; TASM  test file
; Test all instructions and addressing modes.
; Processor: Z80
;
; SEPT. 16,1987
; CARL A. WALL
; VE3APY
;
; 2014-10-29 kio



n:          equ 20h
nn:         equ 0584h
dddd:       equ 07h
addr16:     equ $1234
port:       equ 3
imm8:       equ 56h    ;immediate data (8 bits)
offset:     equ 7
offset_neg: equ -7


; some cases that have two expressions in the args and one is inside ().

     LD   (ix+offset),n+1+4+8-9
     ld   (ix+offset+5),n-1
     ld   (ix+dddd),n
     ld   (ix+offset),n
     ld   (ix-offset),n

; all possible (and impossible) instructions:

     ADC  a,(hl)
     adc  a,(ix+offset)
     adc  a,(ix+offset_neg)
     adc  a,(iy+offset)
     adc  a,(iy+offset_neg)
     adc  a,a
     adc  a,b
     adc  a,c
     adc  a,d
     adc  a,e
     adc  a,h
     adc  a,l
     adc  a,xh		; illegal
     adc  a,xl		; illegal
     adc  a,yh		; illegal
     adc  a,yl		; illegal
     adc  a,n
     adc  hl,bc
     adc  hl,de
     adc  hl,hl
     adc  hl,sp

     ADD  a,(hl)
     add  a,(ix+offset)
     add  a,(iy+offset)
     add  a,a
     add  a,b
     add  a,c
     add  a,d
     add  a,e
     add  a,h
     add  a,l
     add  a,xh		; illegal
     add  a,xl		; illegal
     add  a,yh		; illegal
     add  a,yl		; illegal
     add  a,n
     add  hl,bc
     add  hl,de
     add  hl,hl
     add  hl,sp
     add  ix,bc
     add  ix,de
     add  ix,ix
     add  ix,sp
     add  iy,bc
     add  iy,de
     add  iy,iy
     add  iy,sp

     AND  (HL)
     and  (IX+offset)
     and  (IY+offset)
     and  A
     and  B
     and  C
     and  D
     and  E
     and  H
     and  L
     and  n

     and  a,(hl)
     and  a,(ix+offset)
     and  a,(iy+offset)
     and  a,a
     and  a,b
     and  a,c
     and  a,d
     and  a,e
     and  a,h
     and  a,l
     and  a,XH		; illegal
     and  a,XL		; illegal
     and  a,YH		; illegal
     and  a,YL		; illegal
     and  a,n

     BIT  0,(hl)
     bit  0,(ix+offset)
     bit  0,(iy+offset)
     bit  0,a
     bit  0,b
     bit  0,c
     bit  0,d
     bit  0,e
     bit  0,h
     bit  0,l

     bit  1,(hl)
     bit  1,(ix+offset)
     bit  1,(iy+offset)
     bit  1,a
     bit  1,b
     bit  1,c
     bit  1,d
     bit  1,e
     bit  1,h
     bit  1,l

     bit  2,(hl)
     bit  2,(ix+offset)
     bit  2,(iy+offset)
     bit  2,a
     bit  2,b
     bit  2,c
     bit  2,d
     bit  2,e
     bit  2,h
     bit  2,l

     bit  3,(hl)
     bit  3,(ix+offset)
     bit  3,(iy+offset)
     bit  3,a
     bit  3,b
     bit  3,c
     bit  3,d
     bit  3,e
     bit  3,h
     bit  3,l

     bit  4,(hl)
     bit  4,(ix+offset)
     bit  4,(iy+offset)
     bit  4,a
     bit  4,b
     bit  4,c
     bit  4,d
     bit  4,e
     bit  4,h
     bit  4,l

     bit  5,(hl)
     bit  5,(ix+offset)
     bit  5,(iy+offset)
     bit  5,a
     bit  5,b
     bit  5,c
     bit  5,d
     bit  5,e
     bit  5,h
     bit  5,l

     bit  6,(hl)
     bit  6,(ix+offset)
     bit  6,(iy+offset)
     bit  6,a
     bit  6,b
     bit  6,c
     bit  6,d
     bit  6,e
     bit  6,h
     bit  6,l

     bit  7,(hl)
     bit  7,(ix+offset)
     bit  7,(iy+offset)
     bit  7,a
     bit  7,b
     bit  7,c
     bit  7,d
     bit  7,e
     bit  7,h
     bit  7,l

     CALL C,addr16
     call m,addr16
     call nc,addr16
     call nz,addr16
     call p,addr16
     call pe,addr16
     call po,addr16
     call z,addr16
     call addr16

     Ccf

     CP   (HL)
     cp   (IX+offset)
     cp   (iy+offset)
     cp   a
     cp   b
     cp   c
     cp   d
     cp   e
     cp   h
     cp   l
     cp   imm8

     cp   a,(hl)
     cp   a,(ix+offset)
     cp   a,(iy+offset)
     cp   a,a
     cp   a,b
     cp   a,c
     cp   a,d
     cp   a,e
     cp   a,h
     cp   a,l
     cp   a,xh		; illegal
     cp   a,xl		; illegal
     cp   a,yh		; illegal
     cp   a,yl		; illegal
     cp   a,imm8

     Cpd  
     Cpdr  
     Cpir   
     Cpi   
     Cpl   

     Daa   

     DEC  (HL)
     dec  (IX+offset)
     dec  (iy+offset)
     dec  a
     dec  b
     dec  c
     dec  d
     dec  e
     dec  h
     dec  l
     dec  xh		; illegal
     dec  xl		; illegal
     dec  yh		; illegal
     dec  yl		; illegal
     dec  bc
     dec  de
     dec  hl
     dec  ix
     dec  iy
     dec  sp
     Di
     Djnz $

     Ei
     EX   (sp),hl
     ex   (sp),ix
     ex   (sp),iy
     ex   af,af'
     ex   de,hl
     Exx    
     Halt     
     
     IM   0
     im   1
     im   2

     IN   a,(c)
     in   b,(c)
     in   c,(c)
     in   d,(c)
     in   e,(c)
     in   h,(c)
     in   l,(c)
     in   a,(port)

!     in0  b,(n)	; ed 00 n	hd 64180
!     in0  c,(n)	; ed 08 n	hd 64180
!     in0  d,(n)	; ed 10 n	hd 64180
!     in0  e,(n)	; ed 18 n	hd 64180
!     in0  h,(n)	; ed 20 n	hd 64180
!     in0  l,(n)	; ed 28 n	hd 64180
!     in0  (n)		; ed 30 n	hd 64180	sets flags only
!     in0  a,(n)	; ed 38 n	hd 64180

     INC  (hl)
     inc  (ix+offset)
     inc  (iy+offset)
     inc  a
     inc  b
     inc  c
     inc  d
     inc  e
     inc  h
     inc  l
     inc  xh		; illegal
     inc  xl		; illegal
     inc  yh		; illegal
     inc  yl		; illegal
     inc  bc
     inc  de
     inc  hl
     inc  ix
     inc  iy
     inc  sp

     Ind     
     Indr     
     Ini
     Inir    
     
     JP   addr16
     jp   (hl)
     jp   (ix)
     jp   (iy)
     jp   c,addr16
     jp   m,addr16
     jp   nc,addr16
     jp   nz,addr16
     jp   p,addr16
     jp   pe,addr16
     jp   po,addr16
     jp   Z,addr16

loop2:
     JR   C,loop2
     jr   nc,loop2
     jr   nz,loop2
     jr   z,loop2
!    jr   p,loop2
!    jr   m,loop2
!    jr   po,loop2
!    jr   pe,loop2
     jr   loop2

     LD   (BC),A
     ld   (de),a
     ld   (hl),a
     ld   (hl),b
     ld   (hl),c
     ld   (hl),d
     ld   (hl),e
     ld   (hl),h
     ld   (hl),l
     ld   (hl),n
     ld   (ix+offset),a
     ld   (ix+offset),b
     ld   (ix+offset),c
     ld   (ix+offset),d
     ld   (ix+offset),e
     ld   (ix+offset),h
     ld   (ix+offset),l
     ld   (ix+offset),n
     ld   (iy+offset),a
     ld   (iy+offset),b
     ld   (iy+offset),c
     ld   (iy+offset),d
     ld   (iy+offset),e
     ld   (iy+offset),h
     ld   (iy+offset),l
     ld   (iy+offset),n
     ld   (nn),a
     ld   (nn),bc
     ld   (nn),de
     ld   (nn),hl
     ld   (nn),ix
     ld   (nn),iy
     ld   (nn),sp
     ld   a,(bc)
     ld   a,(de)
     ld   a,(hl)
     ld   a,(ix+offset)
     ld   a,(iy+offset)
     ld   a,(nn)
     ld   a,a
     ld   a,b
     ld   a,c
     ld   a,d
     ld   a,e
     ld   a,h
     ld   a,l
     ld   a,XH		; illegal
     ld   a,xl		; illegal
     ld   a,yh		; illegal
     ld   a,yl		; illegal
     ld   a,i
     ld   a,r
     ld   a,n
     ld   b,(hl)
     ld   b,(ix+offset)
     ld   b,(iy+offset)
     ld   b,a
     ld   b,b
     ld   b,c
     ld   b,d
     ld   b,e
     ld   b,h
     ld   b,l
     ld   b,xh		; illegal
     ld   b,xl		; illegal
     ld   b,yh		; illegal
     ld   b,yl		; illegal
     ld   b,n
     ld	  bc,bc		; compound
     ld	  bc,de		; compound
     ld	  bc,hl		; compound
;    ld	  bc,ix		; compound, illegal, TODO
;    ld	  bc,iy		; compound, illegal, TODO
     ld   bc,(nn)
     ld   bc,nn
     ld   c,(hl)
     ld   c,(ix+offset)
     ld   c,(iy+offset)
     ld   c,a
     ld   c,b
     ld   c,c
     ld   c,d
     ld   c,e
     ld   c,h
     ld   c,l
     ld   c,xh		; illegal
     ld   c,xl		; illegal
     ld   c,yh		; illegal
     ld   c,yl		; illegal
     ld   c,n
     ld   d,(hl)
     ld   d,(ix+offset)
     ld   d,(iy+offset)
     ld   d,a
     ld   d,b
     ld   d,c
     ld   d,d
     ld   d,e
     ld   d,h
     ld   d,l
     ld   d,xh		; illegal
     ld   d,xl		; illegal
     ld   d,yh		; illegal
     ld   d,yl		; illegal
     ld   d,n
     ld	  de,bc		; compound
     ld	  de,de		; compound
     ld	  de,hl		; compound
;    ld	  de,ix		; compound, illegal, TODO
;    ld	  de,iy		; compound, illegal, TODO
     ld   de,(nn)
     ld   de,nn
     ld   e,(hl)
     ld   e,(ix+offset)
     ld   e,(iy+offset)
     ld   e,a
     ld   e,b
     ld   e,c
     ld   e,d
     ld   e,e
     ld   e,h
     ld   e,l
     ld   e,xh		; illegal
     ld   e,xl		; illegal
     ld   e,yh		; illegal
     ld   e,yl		; illegal
     ld   e,n
     ld   h,(hl)
     ld   h,(ix+offset)
     ld   h,(iy+offset)
     ld   h,a
     ld   h,b
     ld   h,c
     ld   h,d
     ld   h,e
     ld   h,h
     ld   h,l
     ld   h,n

!    ld   XH,(hl)
!    ld   xh,(ix+offset)
!    ld   xh,(iy+offset)
     ld   xh,a		; illegal
     ld   xh,b		; illegal
     ld   xh,c		; illegal
     ld   xh,d		; illegal
     ld   xh,e		; illegal
!    ld   xh,h
!    ld   xh,l
     ld   xh,xl
!    ld   xh,yl     
     ld   xh,n

     ld   hl,(nn)
     ld   hl,nn
     ld	  hl,bc		; compound
     ld	  hl,de		; compound
     ld	  hl,hl		; compound
!    ld	  hl,ix
!    ld	  hl,iy
     ld   I,a
     ld   ix,(nn)
     ld   ix,nn
     ld   iy,(nn)
     ld   iy,nn
     ld   l,(hl)
     ld   l,(ix+offset)
     ld   l,(iy+offset)
     ld   l,a
     ld   l,b
     ld   l,c
     ld   l,d
     ld   l,e
     ld   l,h
     ld   l,l
     ld   l,n

!    ld   YL,(hl)
!    ld   yl,(ix+offset)
!    ld   yl,(iy+offset)
     ld   yl,a		; illegal
     ld   yl,b		; illegal
     ld   yl,c		; illegal
     ld   yl,d		; illegal
     ld   yl,e		; illegal
!    ld   yl,h
!    ld   yl,l
     ld   yl,yh		; illegal
!    ld   yl,xh 
     ld   yl,n

     ld   R,a
     ld   SP,(nn)
     ld   sp,hl
     ld   sp,ix
     ld   sp,iy
     ld   sp,nn

     Ldd
     Lddr
     Ldi
     Ldir

!     mult  bc		; ed 4c		hd 64180
!     mult  de		; ed 5c		hd 64180
!     mult  hl		; ed 6c		hd 64180
!     mult  sp		; ed 7c		hd 64180

     Neg
     Nop

     OR   (hl)
     or   (ix+offset)
     or   (iy+offset)
     or   a
     or   b
     or   c
     or   d
     or   e
     or   h
     or   l
     or   imm8

     or   a,(hl)
     or   a,(ix+offset)
     or   a,(iy+offset)
     or   a,a
     or   a,b
     or   a,c
     or   a,d
     or   a,e
     or   a,h
     or   a,l
     or   a,xh		; illegal
     or   a,xl		; illegal
     or   a,yh		; illegal
     or   a,yl		; illegal
     or   imm8

     Otdr
     Otir

     OUT  (c),a
     out  (c),b
     out  (c),c
     out  (c),d
     out  (c),e
     out  (c),h
     out  (c),l
!    out  (c),xh
!    out  (c),yl
     out  (port),a

!    out0 (imm8),b		; ed 01 n	hd 64180
!    out0 (imm8),c		; ed 09 n	hd 64180
!    out0 (imm8),d		; ed 11 n	hd 64180
!    out0 (imm8),e		; ed 19 n	hd 64180
!    out0 (imm8),h		; ed 21 n	hd 64180
!    out0 (imm8),l		; ed 29 n	hd 64180
!    out0 (imm8),a		; ed 39 n	hd 64180

     Outd
     Outi
!    otim			; ed 83		hd 64180
!    otdm			; ed 8b		hd 64180
!    otimr			; ed 93		hd 64180
!    otdmr			; ed 9b		hd 64180

     POP  af
     pop  bc
     pop  de
     pop  hl
     pop  ix
     pop  iy

     PUSH af
     push bc
     push de
     push hl
     push ix
     push iy

     RES  0,(hl)
     res  0,(ix+offset)
     res  0,(iy+offset)
     res  0,a
     res  0,b
     res  0,c
     res  0,d
     res  0,e
     res  0,h
     res  0,l
!    res  0,xh
!    res  0,yl

     res  1,(hl)
     res  1,(ix+offset)
     res  1,(iy+offset)
     res  1,a
     res  1,b
     res  1,c
     res  1,d
     res  1,e
     res  1,h
     res  1,l

     res  2,(hl)
     res  2,(ix+offset)
     res  2,(iy+offset)
     res  2,a
     res  2,b
     res  2,c
     res  2,d
     res  2,e
     res  2,h
     res  2,l

     res  3,(hl)
     res  3,(ix+offset)
     res  3,(iy+offset)
     res  3,a
     res  3,b
     res  3,c
     res  3,d
     res  3,e
     res  3,h
     res  3,l

     res  4,(hl)
     res  4,(ix+offset)
     res  4,(iy+offset)
     res  4,a
     res  4,b
     res  4,c
     res  4,d
     res  4,e
     res  4,h
     res  4,l

     res  5,(hl)
     res  5,(ix+offset)
     res  5,(iy+offset)
     res  5,a
     res  5,b
     res  5,c
     res  5,d
     res  5,e
     res  5,h
     res  5,l

     res  6,(hl)
     res  6,(ix+offset)
     res  6,(iy+offset)
     res  6,a
     res  6,b
     res  6,c
     res  6,d
     res  6,e
     res  6,h
     res  6,l

     res  7,(hl)
     res  7,(ix+offset)
     res  7,(iy+offset)
     res  7,a
     res  7,b
     res  7,c
     res  7,d
     res  7,e
     res  7,h
     res  7,l

     RET
     ret  c
     ret  m
     ret  nc
     ret  nz
     ret  p
     ret  pe
     ret  po
     ret  z
     Reti
     Retn

     RL   (hl)
     rl   (ix+offset)
     rl   (iy+offset)
     rl   a
     rl   b
     rl   c
     rl   d
     rl   e
     rl   h
     rl   l
!    rl   xh
!    rl   yl
     Rla

     RLC  (hl)
     rlc  (ix+offset)
     rlc  (iy+offset)
     rlc  a
     rlc  b
     rlc  c
     rlc  d
     rlc  e
     rlc  h
     rlc  l
!    rlc  xh
!    rlc  yl
     Rlca
     Rld

     RR   (hl)
     rr   (ix+offset)
     rr   (iy+offset)
     rr   a
     rr   b
     rr   c
     rr   d
     rr   e
     rr   h
     rr   l
!    rr   xh
!    rr   yl
     Rra

     RRC  (hl)
     rrc  (ix+offset)
     rrc  (iy+offset)
     rrc  a
     rrc  b
     rrc  c
     rrc  d
     rrc  e
     rrc  h
     rrc  l
!    rrc  xh
!    rrc  yl
     Rrca
     Rrd

     RST  00h
     rst  08h
     rst  10h
     rst  18h
     rst  20h
     rst  28h
     rst  30h
     rst  38h

     rst  0
     rst  1
     rst  2
     rst  3
     rst  4
     rst  5
     rst  6
     rst  7
     
     SBC  a,n
     sbc  a,(hl)
     sbc  a,(ix+offset)
     sbc  a,(iy+offset)
     sbc  a,a
     sbc  a,b
     sbc  a,c
     sbc  a,d
     sbc  a,e
     sbc  a,h
     sbc  a,l
     sbc  a,xh		; illegal
     sbc  a,xl		; illegal
     sbc  a,yh		; illegal
     sbc  a,yl		; illegal
     sbc  hl,bc
     sbc  hl,de
     sbc  hl,hl
     sbc  hl,sp
!    sbc  ix,bc
     scf

     SET  0,(hl)
     set  0,(ix+offset)
     set  0,(iy+offset)
     set  0,a
     set  0,b
     set  0,c
     set  0,d
     set  0,e
     set  0,h
     set  0,l
!    set  0,xh
!    set  0,yl

     set  1,(hl)
     set  1,(ix+offset)
     set  1,(iy+offset)
     set  1,a
     set  1,b
     set  1,c
     set  1,d
     set  1,e
     set  1,h
     set  1,l

     set  2,(hl)
     set  2,(ix+offset)
     set  2,(iy+offset)
     set  2,a
     set  2,b
     set  2,c
     set  2,d
     set  2,e
     set  2,h
     set  2,l

     set  3,(hl)
     set  3,(ix+offset)
     set  3,(iy+offset)
     set  3,a
     set  3,b
     set  3,c
     set  3,d
     set  3,e
     set  3,h
     set  3,l

     set  4,(hl)
     set  4,(ix+offset)
     set  4,(iy+offset)
     set  4,a
     set  4,b
     set  4,c
     set  4,d
     set  4,e
     set  4,h
     set  4,l

     set  5,(hl)
     set  5,(ix+offset)
     set  5,(iy+offset)
     set  5,a
     set  5,b
     set  5,c
     set  5,d
     set  5,e
     set  5,h
     set  5,l

     set  6,(hl)
     set  6,(ix+offset)
     set  6,(iy+offset)
     set  6,a
     set  6,b
     set  6,c
     set  6,d
     set  6,e
     set  6,h
     set  6,l

     set  7,(hl)
     set  7,(ix+offset)
     set  7,(iy+offset)
     set  7,a
     set  7,b
     set  7,c
     set  7,d
     set  7,e
     set  7,h
     set  7,l

     SLA  (hl)		; illegal
     sla  (ix+offset)	; illegal
     sla  (iy+offset)	; illegal
     sla  a		; illegal
     sla  b		; illegal
     sla  c		; illegal
     sla  d		; illegal
     sla  e		; illegal
     sla  h		; illegal
     sla  l		; illegal
!    sla  xh
!    sla  yl

!    slp		; ed 76		hd 64180

     SRA  (hl)
     sra  (ix+offset)
     sra  (iy+offset)
     sra  a
     sra  b
     sra  c
     sra  d
     sra  e
     sra  h
     sra  l
!    sra  xh
!    sra  yl

     SRL  (hl)
     srl  (ix+offset)
     srl  (iy+offset)
     srl  a
     srl  b
     srl  c
     srl  d
     srl  e
     srl  h
     srl  l
!    srl  xh
!    srl  yl
     
     SUB  (hl)
     sub  (ix+offset)
     sub  (iy+offset)
     sub  a
     sub  b
     sub  c
     sub  d
     sub  e
     sub  h
     sub  l
     sub  n

     SUB  A,(HL)
     sub  A,(IX+offset)
     sub  a,(iy+offset)
     sub  a,a
     sub  a,b
     sub  a,c
     sub  a,d
     sub  a,e
     sub  a,h
     sub  a,l
     sub  a,xh		; illegal
     sub  a,xl		; illegal
     sub  a,yh		; illegal
     sub  a,yl		; illegal
     sub  a,n

!     tst  b		; ed 04		hd 64180
!     tst  c		; ed 0c		hd 64180
!     tst  d		; ed 14		hd 64180
!     tst  e		; ed 1c		hd 64180
!     tst  h		; ed 24		hd 64180
!     tst  l		; ed 2c		hd 64180
!     tst  (hl)		; ed 34		hd 64180
!     tst  a		; ed 3c		hd 64180
!     tst  n		; ed 64		hd 64180
!     tstio n		; ed 76		hd 64180

     xor  (hl)
     xor  (ix+offset)
     xor  (iy+offset)
     xor  a
     xor  b
     xor  c
     xor  d
     xor  e
     xor  h
     xor  l
     xor  n

     XOR  A,(HL)
     xor  A,(IX+offset)
     xor  A,(IY+offset)
     xor  A,A
     xor  A,B
     xor  A,C
     xor  A,D
     xor  A,E
     xor  A,H
     xor  A,L
     xor  A,XH		; illegal
     xor  A,XL		; illegal
     xor  A,XH		; illegal
     xor  A,XL		; illegal
     xor  A,n
     
