
;==============================================================================================
; Intops.s     int operators
;              x is the current estackptr, growing up and pointing to the
;              first free loc
;              y may be used freely
; operators return with 'rts'
;==============================================================================================

;-------------------------------------------------------------------------------
; DoXor
;-------------------------------------------------------------------------------

DoXor:         lda     estacklo-2,x
               eor     estacklo-1,x
               sta     estacklo-2,x
               lda     estackhi-2,x
               eor     estackhi-1,x
               jmp     retpop
               
;-------------------------------------------------------------------------------
; DoLor
;-------------------------------------------------------------------------------

DoLor:         lda     estacklo-2,x
               ora     estacklo-1,x
               bne     ExitTrue
               lda     estackhi-2,x
               ora     estackhi-1,x
               bne     ExitTrue
               beq     ExitFalse
               
;-------------------------------------------------------------------------------
; DoOr 
;-------------------------------------------------------------------------------

DoOr:          lda     estacklo-2,x
               ora     estacklo-1,x
               sta     estacklo-2,x
               lda     estackhi-2,x
               ora     estackhi-1,x
               jmp     retpop
               
;-------------------------------------------------------------------------------
; DoLand
;-------------------------------------------------------------------------------

DoLand:        lda     estacklo-2,x
               ora     estackhi-2,x
               beq     ExitFalse
               lda     estacklo-1,x
               ora     estackhi-1,x
               beq     ExitFalse
               bne     ExitTrue
               
;-------------------------------------------------------------------------------
; DoAnd
;-------------------------------------------------------------------------------

DoAnd:         lda     estacklo-2,x
               and     estacklo-1,x
               sta     estacklo-2,x
               lda     estackhi-2,x
               and     estackhi-1,x
               jmp     retpop
               
;-------------------------------------------------------------------------------
; ExitTrue/ExitFalse
;-------------------------------------------------------------------------------
               
ExitFalse:     dex                     ; pop TOS
ef005:         lda     #0
ef010:         sta     estacklo-1,x
               sta     estackhi-1,x
               rts
ExitTrue:      dex
et005:         lda     #$ff
               bne     ef010
               
;-------------------------------------------------------------------------------
; CmpU/CmpS    unsigned/signed compare    a = TRUE mask
; first check 1 < 2, if not, check 1 == 2
; return       exit to TRUE/FALSE depending on outcome and TRUE mask
;-------------------------------------------------------------------------------

CmpU:          tay
               jsr     Subtract
               bcc     cmp070          ; 1 < 2
               bcs     cmp050          ; 1 >= 2
CmpS:          tay                     ; save TRUE mask
               jsr     Subtract
               bvc     cmp040          ; 1 >= 2
               eor     #$80
cmp040:        bmi     cmp070          ; 1 < 2
cmp050:        lda     estacklo-2,x    ; 1 >= 2, check EQU
               bne     cmp080          ; 1 > 2
               lda     estackhi-2,x
               bne     cmp080          ; 1 > 2
               lda     #%00000010      ; EQL
               bne     cmp090
cmp070:        lda     #%00000001      ; LSS
               bne     cmp090
cmp080:        lda     #%00000100      ; GTR
cmp090:        sta     cmpres
               tya                     ; retrieve mask
               and     cmpres
               beq     ExitFalse
               bne     ExitTrue
               
;-------------------------------------------------------------------------------
; comparisons
;-------------------------------------------------------------------------------

DoEql:         lda     #%00000010      ; EQL
               bne     CmpU
DoNeq:         lda     #%00000101      ; GTR or LSS
               bne     CmpU
DoUGeq:        lda     #%00000110      ; GTR or EQL
               bne     CmpU
DoGeq:         lda     #%00000110      ; GTR or EQL
               bne     CmpS
DoUGtr:        lda     #%00000100      ; GTR
               bne     CmpU
DoGtr:         lda     #%00000100      ; GTR
               bne     CmpS
DoULeq:        lda     #%00000011      ; EQL or LSS
               bne     CmpU
DoLeq:         lda     #%00000011      ; EQL or LSS
               bne     CmpS
DoULss:        lda     #%00000001      ; LSS
               bne     CmpU
DoLss:         lda     #%00000001      ; LSS
               bne     CmpS
               
               
               
;-------------------------------------------------------------------------------
; DoLnot       0 -> -1, else 0
;-------------------------------------------------------------------------------

DoLnot:        lda     estacklo-1,x    ; both 0 ?
               ora     estackhi-1,x
               bne     ef005           ; no, return 0
               beq     et005
               
;-------------------------------------------------------------------------------
; DoNot
;-------------------------------------------------------------------------------

DoNot:         lda     estacklo-1,x
               eor     #$ff
               sta     estacklo-1,x
               lda     estackhi-1,x
               eor     #$ff
               sta     estackhi-1,x
               rts
               
;-------------------------------------------------------------------------------
; DoPlus
; retpop is the common exitpoint for many routines
;-------------------------------------------------------------------------------

DoPlus:        lda     estacklo-2,x
               clc
               adc     estacklo-1,x
               sta     estacklo-2,x
               lda     estackhi-2,x
               adc     estackhi-1,x
retpop:        sta     estackhi-2,x
               dex
               rts
               
;-------------------------------------------------------------------------------
; Subtract     helper for Minus,CmpU,CmpS
;-------------------------------------------------------------------------------

Subtract:      sec
               lda     estacklo-2,x
               sbc     estacklo-1,x
               sta     estacklo-2,x
               lda     estackhi-2,x
               sbc     estackhi-1,x
               sta     estackhi-2,x
               rts
               
;-------------------------------------------------------------------------------
; DoMinus
;-------------------------------------------------------------------------------
;
DoMinus:       jsr     Subtract
               jmp     retpop
               
;-------------------------------------------------------------------------------
; DoUmul
; 16*16, 32 bit result to mulres..mulres+3, then copy low 16 bits to estack
; multiplies mnum1 by mnum2
; copy stack to mnum1,mnum2
;-------------------------------------------------------------------------------

DoUmul:        
DoUmul2:       lda     estacklo-2,x
               sta     mnum1
               lda     estackhi-2,x
               sta     mnum1+1
               lda     estacklo-1,x
               sta     mnum2
               lda     estackhi-1,x
               sta     mnum2+1
               jsr     Umul
;              
; res is in mulres, copy to estack
;
               jsr     CopyMul
               dex
               rts
               
Umul:          tya                     ; save y
               pha
               lda     #0
               sta     mulres+2
               ldy     #16             ; 16 bit count
               sty     mulcnt
umul1:         lsr     mnum2+1         ; Get low bit of NUM2
               ror     mnum2        
               bcc     umul2           ; 0 or 1?
               tay                     ; If 1, add NUM1 (hi byte of RESULT is in A)
               clc
               lda     mnum1       
               adc     mulres+2
               sta     mulres+2
               tya
               adc     mnum1+1       
umul2:         ror      
               ror     mulres+2
               ror     mulres+1
               ror     mulres
               dec     mulcnt
               bne     umul1
               sta     mulres+3
               pla
               tay
               rts
               
;mul:          tya
;              pha
;              lda     #0
;              sta     num3   
;              sta     num3+1  
;              ldy     #16
;mul1:         lsr     num2+1
;              ror     num2           ; rightmost bit of num2
;              bcc     umul2
;              jsr     Add1to3        ; add num1 to result
;mul2:         asl     num1           ; num1 << 1
;              rol     num1+1
;              dey
;              bne     umul1
;              pla
;              tay
;              rts
               
;-------------------------------------------------------------------------------
; CopyMul      copy lower16 bits from mulres 
;-------------------------------------------------------------------------------
               
CopyMul:       lda     mulres
               sta     estacklo-2,x
               lda     mulres+1
               sta     estackhi-2,x
               rts
               
;-------------------------------------------------------------------------------
; DoMul
; Determine sign of result, make both positive and Umul, then apply sign
;-------------------------------------------------------------------------------
               
DoMul:         
               lda     estackhi-2,x    ; determine sign of result
               eor     estackhi-1,x
               php                     ; sign is now in N
               lda     estackhi-2,x    ; MSB lhs
               bpl     mul020          ; negative ?
               dex                     ; !! Uminus works on stackptr-1 ! temp dec stackptr..
               jsr     DoUminus        ; negate lhs
               inx                     ; .. and restore it
mul020:        lda     estackhi-1,x    ; MSB rhs
               bpl     mul030
               jsr     DoUminus        ; negate rhs
mul030:        jsr     DoUmul2         ; !! x = decremented, result now on TOS
               plp                     ; get sign of result
               bpl     mul050
               sec
               lda     #0              ; if the result is negative, negate the full 4 bytes in mulres
               sbc     mulres
               sta     mulres
               lda     #0
               sbc     mulres+1
               sta     mulres+1
               lda     #0
               sbc     mulres+2
               sta     mulres+2
               lda     #0
               sbc     mulres+3
               sta     mulres+3
               inx                     ; !! Umul has already decr. x, inc temp for CopyMul
               jsr     CopyMul
               dex
mul050:        rts

               
;-------------------------------------------------------------------------------
; ShftEntry    Common entry for shifts - exits with shftcnt in a, flags set
;-------------------------------------------------------------------------------

ShftEntry:     
               lda     estacklo-1,x
               and     #15
               sta     shftcnt
               rts
               
;-------------------------------------------------------------------------------
; DoLsl        logical shift left
;-------------------------------------------------------------------------------

DoLsl:         jsr     ShftEntry
               beq     lsl090          ; don't shift 0 times
lsl030:        asl     estacklo-2,x    ; zero in, carry out
               rol     estackhi-2,x    ; carry in, carry out
               dec     shftcnt
               bne     lsl030
lsl090:        dex
               rts
               
;-------------------------------------------------------------------------------
; DoLsr        logical shift right
;-------------------------------------------------------------------------------

DoLsr:         jsr     ShftEntry
               beq     lsr090          ; don't shift 0 times
lsr030:        lsr     estackhi-2,x    ; zero in, carry out
               ror     estacklo-2,x    ; carry in, carry out
               dec     shftcnt
               bne     lsr030
lsr090:        dex
               rts
               
;-------------------------------------------------------------------------------
; DoUdiv
;-------------------------------------------------------------------------------

DoUdiv:        
Udiv:          lda     estacklo-1,x
               ora     estackhi-1,x
               bne     udiv005
               ldx     #E_DV0
               jmp     ErrHnd
udiv005:       tya                     ; save y
               pha
               lda     #0
               sta     remainder
               sta     remainder+1
               lda     #16
               sta     divcnt
udiv010:       asl     estacklo-2,x
               rol     estackhi-2,x
               rol     remainder
               rol     remainder+1
               lda     remainder
               sec
               sbc     estacklo-1,x
               tay
               lda     remainder+1
               sbc     estackhi-1,x
               bcc     udiv020
               sta     remainder+1
               sty     remainder
               inc     estacklo-2,x
udiv020:       dec     divcnt   
               bne     udiv010
               dex
               pla
               tay
               rts
               
;-------------------------------------------------------------------------------
; DoUmod
;-------------------------------------------------------------------------------

DoUmod:        jsr     DoUdiv
umod10:        lda     remainder       ; ! DoUdiv already decremented estackptr
               sta     estacklo-1,x
               lda     remainder+1
               sta     estackhi-1,x
               rts
               
;-------------------------------------------------------------------------------
; DoDiv
;-------------------------------------------------------------------------------

DoDiv:         
               lda     estackhi-2,x    ; determine sign of result
               eor     estackhi-1,x
               php                     ; sign is now in N
               lda     estackhi-2,x    ; MSB lhs
               bpl     div020          ; negative ?
               dex                     ; !! Uminus works on stackptr-1 ! temp dec stackptr..
               jsr     DoUminus        ; negate lhs
               inx                     ; .. and restore it
div020:        lda     estackhi-1,x    ; MSB rhs
               bpl     div030
               jsr     DoUminus        ; negate rhs
div030:        jsr     Udiv            ; !! x = decremented, result now on TOS
               plp                     ; get sign of result
               bpl     div050
               jsr     DoUminus
div050:        rts

;-------------------------------------------------------------------------------
; DoMod        neg number(s) : such that Q * divisor + remainder = dividend
;-------------------------------------------------------------------------------

DoMod:         lda     estackhi-2,x    ; get sign of dividend
               php
               jsr     DoDiv
               jsr     umod10
               plp                     ; if dividend was < 0, mod also <0
               bpl     mod090
               jsr     DoUminus
mod090:        rts

;-------------------------------------------------------------------------------
; DoUminus
;-------------------------------------------------------------------------------
               
DoUminus:      lda     #0              ; do 0-num
               sec
               sbc     estacklo-1,x
               sta     estacklo-1,x
               lda     #0
               sbc     estackhi-1,x
               sta     estackhi-1,x
               rts
               
