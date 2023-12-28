;-------------------------------------------------------------------------------
; screenio.s
;-------------------------------------------------------------------------------

; GOXY         x,y
; CLRLIN
; CLREOL
; CLRSCR
; XPOS()
; YPOS()
; CURON
; CUROFF
; GTKEY()
; PRINT        (.)expression , (.)expression...
; TAB          x
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; GOXY         !! PLOT uses y as x-coordinate and v.v.
;-------------------------------------------------------------------------------

GoXY:          jsr     IntExpression
               lda     estacklo
               pha
               lda     #','
               jsr     SynChk
               jsr     IntExpression
               ldx     estacklo
               pla
               tay
goxy100:       clc
               jsr     PLOT
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; CLRLIN       current line
;-------------------------------------------------------------------------------

ClrLin:        ldx     TBLX            ; current y-pos
               jsr     $e9ff           ; clears line (x)
nwst100:       jmp     NewStt
               
;-------------------------------------------------------------------------------
; CLREOL       current line
;-------------------------------------------------------------------------------

ClrEol:        lda     PNTR
               pha                     ; save X
               lda     #39
               sec
               sbc     PNTR            ; current x-pos
               tax
cle030:        lda     #$20            ; clear to EOL
               jsr     CHROUT
               dex
               bne     cle030
               pla
               tay
               ldx     TBLX
               jmp     goxy100
               
;-------------------------------------------------------------------------------
; CLRSCR
;-------------------------------------------------------------------------------

ClrScr:        lda     #147
               jsr     CHROUT
               jmp     NewStt

               
;-------------------------------------------------------------------------------
; XPOS()
; YPOS()
;-------------------------------------------------------------------------------

DoYpos:        clc
               bcc     +
DoXpos:        sec
+:             php
               jsr     ChkLparen
               jsr     ChkRparen       ; intfuncs : EmptyParams
               plp
               bcc     +               ; ypos ?
               lda     PNTR
               DB      $2c
+:             lda     TBLX
expfr100:      sta     estacklo,x
               lda     #0
               sta     estackhi,x
               sta     estacktype,x
               inx                     ; no argument, so we add to stack
               jmp     expfret
               
;-------------------------------------------------------------------------------
; CURON
;-------------------------------------------------------------------------------

CurOn:         lda     #$00
               sta     BLNSW
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; CUROFF
;-------------------------------------------------------------------------------

CurOff:        php
               sei                     ; disable cursor flash so it won't interfere
               lda     GNDBL           ; character under cursor
               jsr     CHROUT          ; restores character, not color (MON doesn't change it)
               lda     #1
               sta     BLNSW           ; stop cursor flash
               plp
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; GETKEY()
;-------------------------------------------------------------------------------

DoGetKey:      
               jsr     ChkLparen
               jsr     ChkRparen       ; intfuncs : EmptyParams
dgk020:        lda     NDX
               beq     dgk020
               txa
               pha
               jsr     GETIN
               tay
               pla
               tax
               tya
;              cmp     #3
;              beq     dgk020
               bne     expfr100        ; common exit

;-------------------------------------------------------------------------------
; PRINT        (.)expression ,(.)expression..
; PRINT alone prints a CR
;-------------------------------------------------------------------------------

Print:         jsr     PeekChr
               cmp     #0
               beq     pri900
               cmp     #':'
               beq     pri900
pri050:        lda     #0
               sta     printflg
               jsr     PeekChr
pri065:        cmp     #'.'            ; unsigned ?
               bne     pri100
               lda     #$80            ; unsigned flag
               ora     printflg
               sta     printflg
               jsr     GetChr          ; swallow the '.'
pri100:        jsr     Expression
               lda     estacktype
               beq     pri200          ; int ?    
               lda     estacklo        ; no, print string
               ldy     estackhi
               jsr     PrtStrAY
               jmp     pri300
pri200:        lda     estacklo
               sta     number
               lda     estackhi
               sta     number+1 
               bit     printflg
               bpl     pri230          ; test unsigned
               jsr     PrtDecU
               jmp     pri300
pri230:        jsr     PrtDecS
pri300:        jsr     GetChr
               cmp     #','            ; more to print ?
               beq     pri050          ; yes, go do it
               bne     pri920
pri900:        jsr     CROut           ; print without arguments, CR
               jmp     NewStt
pri920:        jsr     UngetChr
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; TAB
; TAB is a keyword, no parenthesis, not part of PRINT
;-------------------------------------------------------------------------------

Tab:           jsr     IntExpression   ; get new popsition
               lda     estacklo        ; get new pos
               sec
               sbc     PNTR            ; subtract current pos (ignore hi byte)
               bcc     dot090          ; current is greater than new, no action
               beq     dot090          ; already at required pos
               tax
dot060:        jsr     SPCOut
               dex
               bne     dot060
dot090:        jmp     NewStt
               
