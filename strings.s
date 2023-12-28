;==============================================================================================
; SFUNCS.S
; remember operators return with RTS, functions with JMP EXPFRET
;==============================================================================================

;-------------------------------------------------------------------------------
; ChkComma
;-------------------------------------------------------------------------------

ChkComma:      lda     #','            ; ','
               jmp     SynChk
               
;-------------------------------------------------------------------------------
; GetTempStr   Assign a temporary string, return pointer in sptr1
;-------------------------------------------------------------------------------

NTMPSTR        =       4
tmpslo         DB      <tempstr1,<tempstr2,<tempstr3,<tempstr4
tmpshi         DB      >tempstr1,>tempstr2,>tempstr3,>tempstr4

GetTempStr:    txa
               pha
               ldx     tempstrix
               cpx     #NTMPSTR
               bcc     gts20
               ldx     #0              ; !! rotate tempstrings !!
gts20:         
               lda     tmpslo,x
               sta     sptr1
               lda     tmpshi,x
               sta     sptr1+1
               inx
               stx     tempstrix
               pla
               tax
               rts

;-------------------------------------------------------------------------------
; MoveStr      sptr1 has target address, sptr2 source, mmax maximum chars to move
;              y has position in target where the move should start.
;              On exit, y points to terminator in target (terminator is set)
;              sptr2 points to last char moved from source. if (*sptr2) != 0,
;              source couldn't be moved completely
;-------------------------------------------------------------------------------

MoveStr:       txa
               pha
               ldx     #0
               stx     mcnt            ; movecount
ms010:         lda     mcnt            ; check if max. chars done
               cmp     mmax
               bcs     ms080
               cpy     strlen          ; strlen is one less than actually allocated
               bcs     ms080 
               lda     (sptr2,x)
               sta     (sptr1),y
               beq     ms090
               inc     mcnt
               inc     sptr2
               bne     ms050
               inc     sptr2+1
ms050:         iny
               bne     ms010           ; always !! watch out y=-1 at entry !!
ms080:         txa                     ; 0
               sta     (sptr1),y
ms090:         pla      
               tax
               rts

;-------------------------------------------------------------------------------
; DoStrCat
;-------------------------------------------------------------------------------

DoStrCat:      jsr     GetTempStr      ; set sptr1
               lda     estacklo-2,x
               sta     sptr2
               lda     estackhi-2,x
               sta     sptr2+1         ; setup to copy 1st string to Temp
               lda     strlen
               clc
               adc     #1
               sta     mmax
               jsr     Zero1stChar     ; store a terminator in (sptr1), in case 0 char's to move
               jsr     MoveStr         ; do the 1st copy
               lda     estacklo-1,x
               sta     sptr2
               lda     estackhi-1,x
               sta     sptr2+1         ; setup to concat 2nd string to Temp
               jsr     MoveStr         ; do the concat
               dex
dsc080:        lda     sptr1
               sta     estacklo-1,x
               lda     sptr1+1
               sta     estackhi-1,x
               lda     #T_STR
               sta     estacktype-1,x
               rts                     ; this is an operator, RTS

;-------------------------------------------------------------------------------
; Len1
;-------------------------------------------------------------------------------

Len1:          ldy     #0
-:             lda     (sptr1),y
               beq     l190
               iny
               bne     -
l190:          tya
               rts
               
;-------------------------------------------------------------------------------
; DoLenStr
;-------------------------------------------------------------------------------

DoLenStr:      jsr     ChkLparen
               jsr     StrExpression2
               jsr     ChkRparen
               lda     estacklo-1,x
               sta     sptr1
               lda     estackhi-1,x
               sta     sptr1+1
               jsr     Len1
               sta     estacklo-1,x
               lda     #0
               sta     estackhi-1,x
               lda     #T_INT
               sta     estacktype-1,x
               jmp     expfret

;-------------------------------------------------------------------------------
; DoStrPrep    common code for LEFT$,RIGHT$,MID$
;-------------------------------------------------------------------------------

DoStrPrep:     jsr     ChkLparen
               jsr     StrExpression2  ; get string
               jsr     ChkComma
               jmp     IntExpression2  ; get int param
               
;-------------------------------------------------------------------------------
; GetStrParam
;-------------------------------------------------------------------------------

GetStrParam:   jsr     ChkLparen
               jsr     StrExpression2
               jmp     ChkRparen
               
;-------------------------------------------------------------------------------
; Zero1stChar
; returns y=0
;-------------------------------------------------------------------------------

Zero1stChar:   ldy     #0
               tya
               sta     (sptr1),y
               rts

               
;-------------------------------------------------------------------------------
; DoChrStr     CHR$(asc)
;-------------------------------------------------------------------------------

DoChrStr:      jsr     Entry1int       ; in ifuncs, calls expression
               jsr     GetTempStr
               lda     estackhi-1,x    ; hi byte<>0 -> overflow
;              bne     dsp030
               lda     estacklo-1,x
               ldy     #0
               sta     (sptr1),y
               tya
               iny
               sta     (sptr1),y       ; terminating zero
               jmp     lfts080
               
               
;-------------------------------------------------------------------------------
; DoLeftStr    LEFT$, return empty $ if argument is 0
;-------------------------------------------------------------------------------

DoLeftStr:     jsr     DoStrPrep
               jsr     ChkRparen
               jsr     GetTempStr      ; sptr1. do AFTER all calls to Expression, may be recursive
               lda     estacklo-2,x
               sta     sptr2
               lda     estackhi-2,x
               sta     sptr2+1
               lda     estacklo-1,x    ; int param
               sta     mmax
               jsr     Zero1stChar     ; store a terminator in (sptr1), in case 0 char's to move
               jsr     MoveStr         ; mmax has been setup, copy that much characters
               dex
lfts080:       lda     sptr1           ; ! common exit for LEFT$,MID$,RIGHT$
               sta     estacklo-1,x
               lda     sptr1+1
               sta     estackhi-1,x
               lda     #T_STR          ; tempstrings : store T_STR and index of etmpstring
               sta     estacktype-1,x
               jmp     expfret
               
;-------------------------------------------------------------------------------
; DoMidStr     ! no errorchecks yet
;-------------------------------------------------------------------------------

DoMidStr:      jsr     DoStrPrep
               jsr     ChkComma
               jsr     IntExpression2  ; get int param2
               jsr     ChkRparen
               jsr     GetTempStr      ; sptr1. do AFTER all calls to Expression, may be recursive
               lda     estacklo-3,x
               sta     sptr2
               lda     estackhi-3,x
               sta     sptr2+1
               ldy     #0              ; target pos
               ldy     estacklo-2,x    ; starting pos in sourcestring, 1 based
               dey                     ; make 0-based
               tya
               clc
               adc     sptr2
               sta     sptr2
               lda     sptr2+1
               adc     #0
               sta     sptr2+1
               lda     estacklo-1,x    ; nr. chars to copy
               sta     mmax
               jsr     Zero1stChar     ; store a terminator in (sptr1), in case 0 char's to move
               jsr     MoveStr
               dex
               dex
               jmp     lfts080
               
;-------------------------------------------------------------------------------
; DoRightStr
;-------------------------------------------------------------------------------

DoRightStr:    jsr     DoStrPrep
               jsr     ChkRparen
               lda     estacklo-2,x    ; get string to sptr1 for LEN
               sta     sptr1
               sta     sptr2
               lda     estackhi-2,x
               sta     sptr1+1
               sta     sptr2+1
               jsr     Len1
               sta     length
               sec
               sbc     estacklo-1,x
               clc
               adc     sptr2
               sta     sptr2
               jsr     GetTempStr      ; sptr1. do AFTER all calls to Expression, may be recursive
               lda     estacklo-1,x    ; int param
               cmp     length
               bcc     drst50
               lda     length
drst50:        sta     mmax
               jsr     Zero1stChar     ; store a terminator in (sptr1), in case 0 char's to move
               jsr     MoveStr
               dex
               jmp     lfts080
               
;-------------------------------------------------------------------------------
; DoStrStr     number->string
; DoHexStr
;-------------------------------------------------------------------------------

DoHexStr:      sec
               php
               bcs     dostr010
DoStrStr:      clc
               php
dostr010:      jsr     ChkLparen
               jsr     IntExpression2
               jsr     ChkRparen
               lda     estacklo-1,x
               sta     number
               lda     estackhi-1,x
               sta     number+1
               txa
               plp
               bcc     dostr040
               pha
               jsr     PrtBHex4
               pla
               jmp     dostr060
dostr040:      pha
               jsr     PrtBDecS
               pla
dostr060:      tax
               jsr     GetTempStr
               lda     #<prtbuf
               sta     sptr2
               lda     #>prtbuf
               sta     sptr2+1
               jsr     Zero1stChar     ; store a terminator in (sptr1), in case 0 char's to move
               lda     strlen
               sta     mmax
               jsr     MoveStr
               jmp     lfts080
               
;-------------------------------------------------------------------------------
; DoAsc        string 1st char
;-------------------------------------------------------------------------------

DoAsc:         jsr     GetStrParam     ; (str)
               lda     estacklo-1,x    ; low byte strpointer
               sta     strptr
               lda     estackhi-1,x    ; and hi
               sta     strptr+1
               tya
               pha
               ldy     #0
               lda     (strptr),y      ; get 1st char of string
               sta     estacklo-1,x
               tya
doa090:        sta     estackhi-1,x
               lda     #T_INT
               sta     estacktype-1,x
               pla
               tay
               jmp     expfret

;-------------------------------------------------------------------------------
; DoVal        dec only
;-------------------------------------------------------------------------------

DoVal:         jsr     GetStrParam     ; (str)
               lda     estacklo-1,x    ; low byte strpointer
               sta     strptr
               lda     estackhi-1,x    ; and hi
               sta     strptr+1
               tya
               pha
               txa
               pha
               ldy     #0              ; copy num to INBUF, for ReadDec
dov040:        lda     (strptr),y
               sta     INBUF,y
               beq     dov080          ; end of string
               iny
               bne     dov040
dov080:        ldx     #0
               jsr     ReadDec
               pla
               tax
               lda     number
               sta     estacklo-1,x
               lda     number+1
               jmp     doa090          ; finish stack setup, pull y and return
               
;-------------------------------------------------------------------------------
; StrCmp       estack has two strings, return 0/ffff
; return   a : ff = (1 < 2), 00 = (1 == 2), 01 = (1 > 2). flags set accordingly
;-------------------------------------------------------------------------------

StrCmp:        lda     estacklo-2,x    ; first string
               sta     scp1
               lda     estackhi-2,x
               sta     scp1+1
               lda     estacklo-1,x
               sta     scp2
               lda     estackhi-1,x
               sta     scp2+1
               ldy     #0
               sty     cmpres
scmp030:       lda     (scp1),y
               cmp     (scp2),y 
               bcc     scmp070         ; 1 < 2
               beq     scmp040         ; go check 00
               inc     cmpres          ; 1 > 2
               bne     scmp090         ; always
scmp040:       lda     (scp1),y        ; reload
               beq     scmp090         ; if 00, end of strings and no differences, cmpres is still 0
               iny                     ; no, continue compare
               bne     scmp030         ; branch always
scmp070:       dec     cmpres          ; set 0xff
scmp090:       lda     cmpres
               rts
               
;-------------------------------------------------------------------------------
; DoStrEqu
;-------------------------------------------------------------------------------
               
DoStrEqu:      jsr     StrCmp    
               beq     exitt
               bne     exitf
DoStrNeq:      jsr     StrCmp
               bne     exitt
               beq     exitf
DoStrGtr:      jsr     StrCmp    
               cmp     #1
               beq     exitt           ; 1 < 2
               bne     exitf
DoStrGeq:      jsr     StrCmp    
               bpl     exitt
               bmi     exitf
DoStrLeq:      jsr     StrCmp    
               bmi     exitt
               beq     exitt
               bne     exitf
DoStrLss:      jsr     StrCmp
               bpl     exitf
               bmi     exitt
               
exitt:         lda     #$ff 
               DB      $2c
exitf:         lda     #0
               sta     estacklo-2,x
               sta     estackhi-2,x
               lda     #0
               sta     estacktype-2,x
               dex
               rts
               
;-------------------------------------------------------------------------------
; DoTimeStr
;-------------------------------------------------------------------------------

DoTimeStr:     nop




