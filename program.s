;==============================================================================================
; PROGRAM.S
; using precomp
;
; tokens :
; keywords     : KWSTT .. KWEND
; functions    : FNSTT .. FNEND
; operators    : OPRSTT.. OPREND
; vars         : $fb oo       = fastvar ('A'..'Z') , oo = offset (0,2,4..50)
;                $fc n1 n2 tp = var (name1,name2,type) 
;                               type : 00= int,01 = byte,02 = $; bit 7 set = array
;                note that the array-opener, '[' is NOT stored, bit 7 of tp implies it's there.
; numbers      : $fd bl bh    = hexnum, value stored l-h
;              : $fe bl bh    = decnum
;
; NOTE         : excess chars in variable names are discarded; 'ADDRESS' is stored as 'A','D'. 
;==============================================================================================


;-------------------------------------------------------------------------------
; FindLine (number in linnum)
; return CLC : line not found, linptr points to higher line (or txtend)
;        SEC : line found, linptr points to starting byte (length)
;-------------------------------------------------------------------------------

FindLine:      lda     txttab
               sta     linptr
               lda     txttab+1
               sta     linptr+1
tryline:       ldy     #0
               lda     (linptr),y      ; get first char (len) of line
               beq     clcrts          ; at end of program, not found
               iny
               iny                     ; point to hi byte of linenr
               lda     linnum+1
               cmp     (linptr),y
               bcc     flrts           ; linnum smaller than this line, not found
               beq     flhieq          ; hi bytes equal, go cmp lo byte
flnextl:       ldy     #0              ; linnum higher than this line, try next line
               lda     (linptr),y      ; get length byte..
               clc
               adc     linptr          ; ..add to current
               sta     linptr
               bcc     tryline
               inc     linptr+1
               bne     tryline         ; always
flhieq:        dey                     ; point to lo byte
               lda     linnum
               cmp     (linptr),y
;              bcc     flrts           ; linnum smaller than this line, not found
               beq     flrts           ; equal, found it (CY is set)
               bcs     flnextl         ; go check next line
clcrts:        clc
flrts:         rts


;-------------------------------------------------------------------------------
; InsLine : insert 'crunchbuf' at (linptr).
;-------------------------------------------------------------------------------

InsLine:       lda     linptr
               sta     from
               clc
               adc     crunchbuf
               sta     to
               lda     linptr+1
               sta     from+1
               adc     #0
               sta     to+1
               jsr     MoveUpT
;
; now copy crunchbuf to (linptr) and adjust txtend
;
               ldy     crunchbuf       ; length
               dey
-:             lda     crunchbuf,y     ; insert new line
               sta     (linptr),y
               dey
               bpl     -               ; can use bpl, y always < 128
               lda     txtend          ; adjust txtend
               clc
               adc     crunchbuf
               sta     txtend
               bcc     ilrts
               inc     txtend+1
ilrts:         rts

;-------------------------------------------------------------------------------
; DelLine (at linptr, don't change it, may be followed by an InsLine)
;-------------------------------------------------------------------------------

DelLine:       ldy     #0
               lda     (linptr),y      ; get length of line to del
               sta     tmp             ; save
               lda     linptr
               sta     to
               clc
               adc     (linptr),y      ; length of line to delete
               sta     from
               lda     linptr+1
               sta     to+1
               adc     #0
               sta     from+1
               jsr     MoveDnT
               lda     txtend          ; adjust txtend
               sec
               sbc     tmp
               sta     txtend
               bcs     +
               dec     txtend+1
+:             rts

;-------------------------------------------------------------------------------
; helper funcs PreComp
; StoZ, StoA both finish wit Z=0 due to the iny
;-------------------------------------------------------------------------------

StoNext:       sta     crunchbuf,y
               inx
               lda     INBUF,x         ; get next char of inbuf
               jmp     sa4
               
StoZ:          lda     #0
StoA:          sta     crunchbuf,y
sa4:           iny
               bpl     sa7
               jmp     LLErr           ; line too long !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
sa7:           rts
               
slist:         DB      " ():]',",95  ; 95 = backarrow
slistlen:      =       $-slist

;-------------------------------------------------------------------------------
; PreComp
; x = index into INBUF, y = index into crunchbuf
;-------------------------------------------------------------------------------

PreComp:       ldx     #$ff
               stx     crunchbuf+2     ; assume direct mode
;
; check if line starts with linenum, store it if so
;
               inx                     ; x <- 0
               lda     INBUF,x
               jsr     IsDigit
               bcs     prc020
               jsr     ReadDec         ; reads from INBUF,x, output at 'number'. x points to 1st non-digit
               lda     number+1
               cmp     #$ff            ; check hi byte <> $ff, valid linenrs 0..65279
               bne     prc010
               jmp     SynErr          ; linenr too high
prc010:        sta     crunchbuf+2     ; store linenumber
               lda     number
               sta     crunchbuf+1
;              
; init precomp
        
prc020:        ldy     #3              ; starting offset in compbuf
               dex                     ; prepare for 'inx'
;              
;   ********** loop start **********
;
; comment
;
prc100:        inx
               lda     INBUF,x
               cmp     #39             ; comment ?  (single quote)
               bne     prc200
               lda     #TK_REM         ; artificial token
prc130:        jsr     StoNext         ; just store everything until EOL
               cmp     #0              ; check EOL
               bne     prc130          ; always
prc150:        jmp     prc900          ; put terminating 00, set length of crunchbuf
;              
; quoted string
;              
prc200         cmp     #'"'            ; opening quote ?
               bne     prc300
prc230:        jsr     StoNext     
               cmp     #0
               beq     prc250          ; EOL, unterminated $
               cmp     #'"'
               bne     prc230
               jsr     StoZ            ; end of $ : store 00 iso quote
               bne     prc100          ; always, continue mainloop
prc250:        jsr     StoZ            ; unterminated, store an extra 00
               bne     prc150          ; always, go finish
;               
; char's that are always stored
;
prc300:        cmp     #0              ; is it EOL ?
               beq     prc150
               stx     resxsav
               ldx     #slistlen-1
prc310:        cmp     slist,x
               beq     prc320
               dex
               bpl     prc310
               ldx     resxsav
               jmp     prc400
prc320:        ldx     resxsav
prc325:        jsr     StoA
               bne     prc100
;              
; decimal number
;
prc400:        jsr     IsDigit
               bcs     prc500
;              inx                     ; ??
               jsr     ReadDec
               lda     #TK_DECNUM
prc430:        jsr     StoA  
               lda     number
               jsr     StoA
               lda     number+1
               jsr     StoA
               dex                     ; we're past last char in number, prepare for 'inx' at prc100
               jmp     prc100          ; back mainloop
;              
; keyword or function
;
prc500:        cmp     #'?'            ; is it print ?
               bne     prc510
               lda     #TK_PRINT
               jsr     StoA            ; yes, store printtoken
               bne     prc100          ; continue
               stx     resxsav
prc510:        jsr     IsAlpha         ; 'A'..'Z' possible keyword
               bcc     prc520
               and     #$7f            ; also check 'A'..'Z' with bit 7 set. this allows
               jsr     IsAlpha         ; keyword abbrevations
               bcc     prc520
               jmp     prc700          ; not alpha, can skip variable check as well
prc520:        lda     #<keywords
               sta     resptr
               lda     #>keywords
               sta     resptr+1
               jsr     FindToken
               cmp     #0              ; found it ?
               bne     prc325          ; go store and continue
               lda     #<functions
               sta     resptr
               lda     #>functions
               sta     resptr+1
               jsr     FindToken
               cmp     #0
               bne     prc325
               ldx     resxsav
;              
; check variable
;
               lda     INBUF,x         ; reload
               jsr     IsAlpha         ; vars start with 'A'..'Z'
               bcc     prc610
               jmp     prc700
prc610:        sta     varn1
               lda     #0
               sta     varn2           ; assume no 2nd char
               sta     typflag         ; type = scalar, int
               inx
               lda     INBUF,x         ; get 2nd char
               jsr     IsAlpha
               bcc     prc620          ; it's a letter
               jsr     IsDigit
               bcc     prc620          ; it's a digit
               bcs     prc640          ; always, go check type
prc620:        sta     varn2           ; digit or letter, store
prc630:        inx                     ; swallow further digits/letters
               lda     INBUF,x
               jsr     IsAlpha
               bcc     prc630
               jsr     IsDigit
               bcc     prc630
prc640:        cmp     #$25            ; '%'
               beq     prc655          ; bump typeflag
               cmp     #'$'
               bne     prc660          ; not % or $, go check array
               inc     typflag
prc655:        inc     typflag         ; string bumps 2
               inx
               lda     INBUF,x
prc660:        cmp     #'['
               beq     prc670
               dex                     ; read one too much
               bpl     prc680          ; always
prc670:        lda     #$80
               ora     typflag         ; $80 = array
               sta     typflag
;               
; determine fast/normal var & store
;
prc680:        lda     varn2           ; is there a 2nd char ?
               bne     prc690          ; yes, normal var
               lda     typflag         ; got a single letter  ar, is it an int ?
               bne     prc690          ; no, normal var
               lda     #TK_FVAR        ; yes, a fastvar
               jsr     StoA
               lda     varn1           ; get 1st char. to calc the offset into fastvars
               sec
               sbc     #'A'            ; 0..25
               asl                     ; 0..50
prc688:        jsr     StoA
               jmp     prc100          ; back mainloop
;      
; it's a normal var
;
prc690:        lda     #TK_VAR         ; no, it's a var. store 1st 2 chars and type 
               jsr     StoA
               lda     varn1           ; first char
               jsr     StoA
               lda     varn2          
               jsr     StoA
               lda     typflag
prc693:        jsr     StoA
               jmp     prc100          ; back mainloop
;              
; Hex number
;
prc700:        lda     INBUF,x         ; must reload, char may have been ANDED with $7f
               cmp     #'$'
               bne     prc800
               inx
               jsr     ReadHex         ; read the number
               bne     prc730          ; OK, go store
               dex
               lda     #'$'            ; no digits, store single '$'
               bne     prc688          ; trampoline
prc730:        lda     #TK_HEXNUM
               jmp     prc430          ; go store TK and number
;              
; Operators
;
prc800:        cmp     #221            ; OR character
               bne     prc805
               lda     #125            ; same character, bit 7 clear
               sta     INBUF,x
               lda     INBUF+1,x       ; dead ugly, but must convert.
               cmp     #221 
               bne     prc805
               lda     #125
               sta     INBUF+1,x
prc805:        lda     #<operators
               sta     resptr
               lda     #>operators
               sta     resptr+1
               jsr     FindToken
               cmp     #0              ; found it ?
               bne     prc693          ; yes,go store and continue
               lda     INBUF,x         ; reload char
;              
; none of the above, just store
;
               jmp     prc693
;              
; end of line
;
prc900:        lda     #0
               jsr     StoA
               sty     crunchbuf       ; set length of line
               rts

;-------------------------------------------------------------------------------
; FindToken    
;              Need more than 256 chars for all keywords, functions and
;              operators, so there are 3 separate tables for them, each < 256.
;              resptr has table address
;              return a = tokennr or 0 (not found)
;-------------------------------------------------------------------------------

FindToken:     stx     resxsav
               sty     resysav
               ldy     #0
               lda     (resptr),y      ; first byte in table is starting tokennumber
               sta     restok
;              dey                     ; prepare for 'iny'
ft090:         dex              
ft100:         inx      
               iny
               lda     INBUF,x
               sec
               sbc     (resptr),y
               beq     ft100
               cmp     #$80
               beq     ft910           ; match
               inc     restok          ; no match, bump tokennr
               dey
ft150:         iny
               lda     (resptr),y      ; find end of keyword 
               bpl     ft150
               iny                     ; one further
               lda     (resptr),y      ; check end of reslist
               beq     ft900           ; EOL, return with a = 0
               dey                     ; prepare for 'iny'
               ldx     resxsav         ; set x back to start of INBUF
               jmp     ft090           ; try next keyword
ft900:         ldx     resxsav
               ldy     resysav
               rts
ft910:         lda     restok
               ldy     resysav
               rts
               
;-------------------------------------------------------------------------------
; List
;-------------------------------------------------------------------------------

modsa:         DB      ' '             ; keep consecutive
mods:          DB      "%$"            ;  ..      ..

tbytesl:       DB      <keywords,<functions,<operators
tbytesh:       DB      >keywords,>functions,>operators

PrNotZ:        iny                     ; print everything until $00 found
               lda     (wptr),y
               beq     sz99 
               jsr     CHROUT
               jmp     PrNotZ
sz99:          rts                     ; returns with Z=1

LoadTab:       lda     tbytesl,x
               sta     resptr
               lda     tbytesh,x
               sta     resptr+1
               rts
               
List:          jsr     CROut
;
; start of program to wptr
;
               lda     txttab
               sta     wptr 
               lda     txttab+1
               sta     wptr+1
;               
; next line
;
lst100:        jsr     ChkStop
               bne     lst108
               jmp     $c003           ; !!!!! 
lst108:        ldy     #0
               lda     (wptr),y        ; get lengthbyte
               bne     lst112
               jmp     lst900          ; 00, end of program
lst112:        iny
               lda     (wptr),y        ; linenum lo
               sta     number
               iny
               lda     (wptr),y        ; linenum hi
               sta     number+1
               sty     resysav
               jsr     PrtDecU         ; print it
               ldy     resysav
;              
; next char
;
lst110:        iny
               lda     (wptr),y        ; get char
               bne     lst130
               jmp     lst800          ; EOL
lst130:        cmp     #TK_REM         ; comment ?
               bne     lst200
               lda     #'''
               jsr     CHROUT
               jsr     PrNotZ          ; print everything until 0x00 (EOL)
               beq     lst800          ; always
               
lst200:        cmp     #'"'            ; quoted $ ?
               bne     lst300
               dey                     ; make it print opening quote as well
               jsr     PrNotZ          ; print everything until closing 00 ($-terminator)
               lda     #'"'
               jsr     CHROUT
               jmp     lst110          ; always
               
lst300:        cmp     #$80            ; is the char positive ?
               bcs     lst310          ; no, check further     
               jsr     CHROUT          ; yes, print
               jmp     lst110          ; and go back for more
;              
; tokens
;
lst310:        cmp     #TK_DECNUM      ; a number ?
               bne     lst320
               clc                     ; CY clear indicates decimal
lst315:        jsr     lstprnum        ; print it
               jmp     lst110
lst320:        cmp     #TK_HEXNUM      ; a hex number ?
               bne     lst330
               sec                     ; CY set indicates hex
               bcs     lst315          ; go print
lst330:        cmp     #TK_FVAR        ; a fast var ?
               bne     lst340
               iny
               lda     (wptr),y        ; get offset
               lsr                     ; make index
               clc
               adc     #'A'
               jsr     CHROUT
               jmp     lst110
lst340:        cmp     #TK_VAR         ; a normal var ?
               bne     lst400
               iny
               lda     (wptr),y        ; get 1st char of name
               jsr     CHROUT
               iny
               lda     (wptr),y        ; get 2nd char
               beq     lst350          ; if it's zero, single char var
               jsr     CHROUT
lst350:        iny
               lda     (wptr),y        ; get type byte
               pha
               and     #$7f            ; mask off array opener
               beq     lst360          ; it's a normal int, no modifier
               tax
               dex
               lda     mods,x          ; load "%" or "$"
               jsr     CHROUT
lst360:        pla                     ; get type again
               bmi     lst365          ; an array
               jmp     lst110
lst365:        lda     #'['
               jsr     CHROUT
               jmp     lst110
;              
; a token in one of the 3 tables
;                   
lst400:        sta     restok
               ldx     #2              ; table 2,1,0   
lst410:        jsr     LoadTab         ; set resptr
               jsr     PrintToken
               bcc     lst420          ; CC = found
               dex
               bpl     lst410
lst420:        jmp     lst110
;               
; EOL 
;
lst800:        jsr     CROut
               ldy     #0
               lda     (wptr),y        ; length byte
               clc
               adc     wptr
               sta     wptr
               lda     wptr+1
               adc     #0
               sta     wptr+1
               jmp     lst100
;               
; End of prog
;
lst900:        ; jsr     CROut  
               jmp     NewStt

;-------------------------------------------------------------------------------
; PrintToken
; resptr has the address of a token table (keywords.s), token is in restok
; if the token is in this list, print it and return CY clear
;-------------------------------------------------------------------------------

PrintToken:    stx     resxsav
               sty     resysav
               ldy     #0
               lda     (resptr),y      ; 1st char in table is starting tokennr
               cmp     restok          ; token in this table ?
               beq     pt010           ; yes, it's the first
               bcs     pt900           ; no, starting token > token searched, exit with CY=1
pt010:         sec
               lda     restok
               sbc     (resptr),y      ; subtract 1st token in list, to make an index
               tax
               inx
               iny                     ; point to first keyword
pt050:         dex
               beq     pt300           ; it's 0, y points to keyword, go print
               dey
pt100:         iny
               lda     (resptr),y      ; find byte with bit 7 set
               bpl     pt100
               iny                     ; one further to start of next keyw
               lda     (resptr),y      ; end of table ?
               sec
               beq     pt900           ; return with CY set
               bne     pt050           ; always
pt300:         lda     (resptr),y
               php
               and     #$7f
               jsr     CHROUT
               iny
               plp
               bpl     pt300
               clc                     ; done, CY clear, OK exit
pt900:         ldx     resxsav
               ldy     resysav
               rts
               
;------------------------------------------------------------------------------              
; lstprnum     CY=1 : print hex, else dec. Updates Y !
;------------------------------------------------------------------------------              
                
lstprnum:      iny
               lda     (wptr),y        ; line LO
               sta     number          ; print linenr
               iny
               lda     (wptr),y
               sta     number+1
               tya
               pha                     ; save y
               bcs     lprn100         ; CY set : do hexprint
               jsr     PrtDecU
               pla                     ; and restore
               tay
               rts
lprn100:       lda     #'$'
               jsr     CHROUT
               lda     number+1        ; hi byte
               beq     lprn120
               jsr     PrtHex2
lprn120:       lda     number
               jsr     PrtHex2
               pla
               tay
               rts
               
               rts
