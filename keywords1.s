;==============================================================================================
; Keywords1.s
; LET,VARS,PRINT,MON,DIM,RUN,GOTO,NEW,CLR
;==============================================================================================


;-------------------------------------------------------------------------------
; GetToken
; leaves txtptr at start of next token (skip strings,nrs etc)
; ! returns with flags set corresponding a
;-------------------------------------------------------------------------------

GetToken:      jsr     GetChr          ; get non-spavce char
               pha                     ; save it
               cmp     #TK_REM
               bne     gtk050
gtk030:        jsr     GetChrAll       ; loop : find $00, either EOL (REM) or $-terminator
               cmp     #0
               bne     gtk030
               beq     gtk090
gtk050:        cmp     #'"'
               beq     gtk030
               ldy     #4              ; 3 bytes, but gtk080 starts with dey
               cmp     #TK_VAR
               beq     gtk080
               dey                     ; y=3
               cmp     #TK_HEXNUM
               beq     gtk080
               cmp     #TK_DECNUM
               beq     gtk080
               dey                     ; y=2
               cmp     #TK_FVAR
               beq     gtk080
               dey                     ; y=1, so no getchar for 1 byte tokens
gtk080:        dey
               beq     gtk090 
               jsr     GetChrAll
               jmp     gtk080
gtk090:        pla              
               rts

;-------------------------------------------------------------------------------
; InitProgPtrs
; set start/end of BASIC program
;-------------------------------------------------------------------------------

InitProgPtrs:  lda     membot
               sta     txttab
               sta     txtend
               lda     membot+1
               sta     txttab+1
               sta     txtend+1
               ldy     #$00  
               tya
               sta     (txttab),y      ; set length byte to 00 -> end of prgram
               rts
               
;-------------------------------------------------------------------------------
; InitVarPtrs
;-------------------------------------------------------------------------------

InitVarPtrs:   lda     memtop
               sta     vartab
               lda     memtop+1
               sta     vartab+1
               rts
               
;-------------------------------------------------------------------------------
; ClrFastvars
;-------------------------------------------------------------------------------

ClrFastvars:   lda     #0              ; init fastvars
               ldx     #51             ; 26 fastvars, 52 bytes
cfv020:        sta     fastvars,x
               dex
               bpl     cfv020
               rts
               
;-------------------------------------------------------------------------------
; StackInit
; reset stack, all context is lost.
; leave one $00 entry on stack to terminate FNDFOR etc
; warm start, NEW, CLR, error call this
;-------------------------------------------------------------------------------

StackInit:     pla                     ; pop return address
               tay
               pla
               ldx     stksav          ; use sp saved on entry
               txs
               tax                     ; ret addr hi
               lda     #0
               pha
               txa                     ; push return address
               pha
               tya
               pha
               rts
               
;-------------------------------------------------------------------------------
; NEW
;-------------------------------------------------------------------------------

New:           lda     #0
               jsr     SynChk          ; check further line empty
               jsr     InitProgPtrs    ; clear the program
               jmp     Clr2            ; continue with CLR
               
;-------------------------------------------------------------------------------
; CLR
;-------------------------------------------------------------------------------

Clr:           lda     #0              ; check further line empty
               jsr     SynChk
Clr2:          jsr     DoClr
               jsr     StackInit
               jmp     Ready
DoClr:         jsr     InitVarPtrs
               jsr     ClrFastvars
               jsr     CLALL
               rts
               
;-------------------------------------------------------------------------------
; STOP
;-------------------------------------------------------------------------------

SaveContext:   lda     txtptr
               sta     oldtxt
               lda     txtptr+1
               sta     oldtxt+1
               lda     pcurlin
               sta     pcurlinsav
               lda     pcurlin+1
               sta     pcurlinsav+1
               lda     curlin
               sta     curlinsav
               lda     curlin+1
               sta     curlinsav+1
               rts
End:               
Stop:          jsr     SaveContext
               jmp     Ready
               
;-------------------------------------------------------------------------------
; Cont
;-------------------------------------------------------------------------------

Cont:          lda     oldtxt+1
               bne     cnt030
               jmp     ContErr
cnt030:        sta     txtptr+1
               lda     oldtxt
               sta     txtptr
               lda     pcurlinsav
               sta     pcurlin
               lda     pcurlinsav+1
               sta     pcurlin+1
               lda     curlinsav
               sta     curlin
               lda     curlinsav+1
               sta     curlin+1
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; LET
; leaves targettyp and varptr (target) for 'FOR' statement
;-------------------------------------------------------------------------------

Let:           jsr     LetCode
               jmp     NewStt
LetCode:       jsr     PtrGet
               lda     varptr
               sta     target          ; target = receiving varaddress
               lda     varptr+1
               sta     target+1
               lda     vartyp
               sta     targettyp
               lda     #$3d            ; '='
               jsr     SynChk
               jsr     Expression  
               lda     estacktype      ; what did expr. return ?
               cmp     #T_STR          ; a string ?
               bne     let200
               lda     targettyp       ; yes, was a string expected ?
               cmp     #T_STR
               bne     leterrtyp       ; no, type error
               lda     estacklo        ; yes, copy estack to wptr
               sta     wptr
               lda     estackhi
               sta     wptr+1
               ldy     #$ff
let100:        iny                     ; copy the string to target
               lda     (wptr),y
               sta     (target),y
               bne     let100
               lda     #0
               sta     tempstrix       ; clear temp$ index
               rts                  
let200:        ldx     targettyp       ; 0 (word) or 1 (byte)
               cpx     #T_STR          ; expression returned int, check targettype != T_STR
               beq     leterrtyp
               ldy     #0              ; expr. returned word, store it, but check target = byte%
               lda     estacklo
               sta     (target),y
               dex                     ; -1 or 0
               bmi     let400          ; type was int, go store hi byte
               lda     estackhi
               bne     letillerr
let400:        lda     estackhi        ; int, store hi byte
               iny
               sta     (target),y
let900:        rts
leterrtyp:     jmp     TypeErr
letillerr:     jmp     IllErr

;-------------------------------------------------------------------------------
; Mon
;-------------------------------------------------------------------------------
               
Mon:           jmp     $c003

;-------------------------------------------------------------------------------
; DIM
;-------------------------------------------------------------------------------
               
Dim:           jsr     GetChr          ; get name (TK_VAR)
               cmp     #TK_VAR
               beq     dim020
dim010:        jmp     SynErr
dim020:        jsr     GetChr          ; first char 

               sta     varn1
               jsr     GetChr
               sta     varn2
               jsr     GetChr
               sta     vartyp
               jsr     FindVar         ; check already defined
               bcc     dim040
               ldx     #E_REDIM
               jmp     ErrHnd
dim040:        jsr     ConstIntExpr
               lda     estacklo
               sta     ndim
               lda     estackhi
               bmi     letillerr       ; negative dimension
               sta     ndim+1
               lda     #']'
               jsr     SynChk
               jsr     EnterVar
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; VARS
;-------------------------------------------------------------------------------

Vars:          jsr     InitVarptr
vars100:       jsr     ChkVarend
               beq     vars900
               lda     #'$'
               jsr     CHROUT
               lda     varptr
               sta     number
               lda     varptr+1
               sta     number+1
               jsr     PrtHex4
               jsr     SPCOut
               ldy     #0
               lda     (varptr),y      ; first char name
               jsr     CHROUT
               iny
               lda     (varptr),y      ; 2nd char name
               bne     vars130
               lda     #$20
vars130:       jsr     CHROUT
               iny
               lda     (varptr),y      ; get type
               pha
               and     #$7f            ; mask off array bit
               tax
               lda     modsa,x         ; " %$"
               jsr     CHROUT
               jsr     SPCOut
               pla                     ; type again; array ?
               bpl     vars180         ; no, done
               lda     #'['
               jsr     CHROUT
               ldy     #5              ; point to dim
               lda     (varptr),y
               sta     number
               iny
               lda     (varptr),y
               sta     number+1
               jsr     PrtDecU
               lda     #']'
               jsr     CHROUT
vars180:       jsr     CROut
               jsr     NextVar
               jmp     vars100
vars900:       jmp     NewStt

;-------------------------------------------------------------------------------
; RUN
;-------------------------------------------------------------------------------

Run:           clc                     ; no error if no number following RUN
               jsr     DoClr
               clc
               jsr     ReadLineNr
               bcs     ru050           ; no linenum
               jsr     FindLine
ru018:         bcs     ru020           ; found, address in linptr
ru019:         jmp     LineErr
ru020:         lda     linptr 
               sta     pcurlin
               lda     linptr+1
               sta     pcurlin+1
               bne     ru090           ; always
ru050:         lda     txttab          ; start at first line
               sta     pcurlin
               lda     txttab+1
               sta     pcurlin+1
ru090:         jmp     GoLine

;-------------------------------------------------------------------------------
; GOTO
;-------------------------------------------------------------------------------

Goto:          sec                     ; request error if no (decimal) linenum found
               jsr     ReadLineNr      ; result in 'linnum' if CY clear
goto050:       jsr     FindLine        ; result is OK (ReadLineNr jumps to SynErr if not)
               jmp     ru018
               
;-------------------------------------------------------------------------------
; GOSUB
;-------------------------------------------------------------------------------

Gosub:         sec
               jsr     ReadLineNr      ; number->linnum 
gosub050:      jsr     FindLine
               bcc     ru019           ; line not found
               lda     pcurlin  
               pha
               lda     pcurlin+1
               pha
               lda     txtptr
               pha
               lda     txtptr+1
               pha
               lda     #TK_GOSUB
               pha
               jmp     ru018           ; Find & goto new line, it's still in linptr
               
               
;-------------------------------------------------------------------------------
; ON
;-------------------------------------------------------------------------------

On:            jsr     IntExpression
               lda     #0              ; assume GOTO
               sta     tosubflg
               jsr     GetChr
               cmp     #TK_GOTO
               beq     on040
               inc     tosubflg        ; then it must be GOSUB
               cmp     #TK_GOSUB
               beq     on010
               jmp     SynErr
on010:         lda     estackhi        ; check hi byte
               beq     on030
on020:         jmp     IndErr          ; index error,checks <0, >255
on030:         lda     estacklo        ; check lo byte
               bmi     on020           ; neg = error
on040:         lda     estacklo        ; get 'index'
               bmi     on080           ; done, go skip rest of statement
               sec
               jsr     ReadLineNr      ; must have linenr, read at least once
               dec     estacklo
               bmi     on060
               lda     #','            ; while index >= 0, a comma must follow
               jsr     SynChk
on060:         jmp     on040
on080:         jsr     GetToken        ; must skip all further linenrs and commas
               beq     on090           ; EOL
               cmp     #':'
               bne     on080
on090:         jsr     UngetChr        ; !! txtptr is already past $00/':'
               lda     tosubflg
               bne     gosub050
               beq     goto050
               
;-------------------------------------------------------------------------------
; RETURN
;-------------------------------------------------------------------------------

Return:        tsx
               lda     $101,x
               beq     reterr
               cmp     #TK_GOSUB
               bne     reterr
               txs
               pla                     ; pull TK_GOSUB
               pla
               sta     txtptr+1
               pla
               sta     txtptr
               pla
               sta     pcurlin+1
               pla
               sta     pcurlin
ret100:        ldy     #1
               lda     (pcurlin),y
               sta     curlin
               iny
               lda     (pcurlin),y
               sta     curlin+1
ret900:        jmp     NewStt
reterr:        ldx     #E_RWG   
               jmp     ErrHnd
               
;-------------------------------------------------------------------------------
; IF
;-------------------------------------------------------------------------------

DoIf:          jsr     IntExpression
               lda     estacklo
               ora     estackhi        ; checkexpr result == 0
               bne     if200           ; no, expr = TRUE, go find THEN or GOTO
               
if150:         jsr     GetToken        ; expr. was FALSE. see if there's an ELSE 
               beq     if180           ; EOL, no ELSE
               cmp     #TK_ELSE
               beq     ret900
               bne     if150
if180:               
DoElse:        jmp     StmEol          ; yes, skip rest of line.Also entry for ELSE if IF was TRUE
if200:         jsr     GetChr          ; expression was TRUE
               cmp     #TK_THEN
               beq     ret900
               cmp     #TK_GOTO
               bne     if240
               jmp     Goto            ; !! was beq
if240:         jmp     SynErr

               
;-------------------------------------------------------------------------------
; FOR
; loop var must be int, array allowed
; PUSH : pcurlin varptr upper step txtptr TK_FOR (11 bytes)
;-------------------------------------------------------------------------------
               
For:           jsr     GetChr
               jsr     IsVar
               beq     for010
               jmp     SynErr
for010:        jsr     UngetChr        ; PtrGet needs token
               jsr     LetCode         ; let gets var and initialises it. var in 'target', targettype
               lda     targettyp       ; check if var was int
               beq     for020
               jmp     TypeErr         ; $ or % not allowed
for020:        lda     pcurlin         ; PUSH : pcurlin
               pha
               lda     pcurlin+1
               pha
               lda     target          ; PUSH : varptr
               pha
               lda     target+1
               pha
               lda     #TK_TO
               jsr     SynChk
               jsr     IntExpression   ; get upper limit
               lda     estacklo        ; PUSH : upper
               pha
               lda     estackhi
               pha
               jsr     GetChr
               cmp     #TK_STEP
               beq     for100
               jsr     UngetChr
               lda     #1              ; default stepvalue 
               ldy     #0
               beq     for120
for100:        jsr     IntExpression   ; get step value
               lda     estacklo
               ldy     estackhi
for120:        pha                     ; PUSH : step
               tya
               pha
               lda     txtptr          ; PUSH : txtptr
               pha
               lda     txtptr+1
               pha
               lda     #TK_FOR
               pha
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; Next
; $101,x       TK_FOR
; $102,x       txtptr hi
; $103,x       txtptr lo
; $104,x       step   hi
; $105,x       step   lo
; $106,x       upper  hi
; $107,x       upper  lo
; $108,x       varptr hi
; $109,x       varptr lo
; $10a,x       pcurln hi
; $10b,x       pcurln lo
;-------------------------------------------------------------------------------

Next:          tsx
               lda     $0101,x
               cmp     #TK_FOR
               beq     nxt050
               ldx     #E_NWF
               jmp     ErrHnd
nxt050:        lda     $0108,x
               sta     varptr+1
               lda     $0109,x
               sta     varptr
               ldy     #0
               clc
               lda     (varptr),y      ; current value of loopvar
               adc     $0105,x         ; add step
               sta     (varptr),y
               iny
               lda     (varptr),y
               adc     $0104,x
               sta     (varptr),y
               lda     $0104,x         ; is step positive or negative
               bmi     nxt070
               jsr     NxtCmpS         ; positive, compare var - upper
               bmi     nxt100          ; < , continue
               ldy     #0              ; check eql, must continue if so
               lda     (varptr),y
               cmp     $0107,x
               bne     nxt200
               iny
               lda     (varptr),y
               cmp     $0106,x
               bne     nxt200
               beq     nxt100
nxt070:        jsr     NxtCmpS         ; step is negative
               bpl     nxt100          ; >=, continue
               bmi     nxt200
nxt100:        lda     $010b,x         ; continue loop
               sta     pcurlin
               lda     $010a,x
               sta     pcurlin+1
               lda     $0103,x
               sta     txtptr
               lda     $0102,x
               sta     txtptr+1
               jmp     ret100
nxt200:        txa                     ; end loop
               clc
               adc     #11
               tax
               txs
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; NxtCmpS
; N=1 : (varptr) < upper/lower
;-------------------------------------------------------------------------------

NxtCmpS:       ldy     #0
               lda     (varptr),y      ; var
               cmp     $0107,x         ; upper/lower
               iny
               lda     (varptr),y
               sbc     $0106,x
               bvc     +
               eor     #$80
+:             rts
               
