;-------------------------------------------------------------------------------
; Keywords2.s
;
; POKE,POKEW
; SYS
; BRKON,BRKOFF
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; POKE, POKEW
;-------------------------------------------------------------------------------

PokeCommon:    jsr     IntExpression   ; get address
               lda     estacklo
               sta     poker
               lda     estackhi
               sta     poker+1
               jsr     ChkComma
               jmp     IntExpression   ; get value to poke
               
Poke:          clc                     ; CY clr -> byte
               php                     ; save CY
               bcc     pok060
PokeW:         sec                     ; CY set -> word
               php                     ; save CY
pok060:        jsr     PokeCommon  
               ldy     #0
               lda     estacklo
               sta     (poker),y
               plp                     ; retrieve CY
               lda     estackhi
               bcs     pok070          ; word, go store hi byte
               beq     pok090          ; byte, a (hi byte) == 0, OK, exit
               jmp     IllErr          ; byte, value > 255, error
pok070:        iny       
               sta     (poker),y
pok090:        jmp     NewStt

;-------------------------------------------------------------------------------
; SYS
;-------------------------------------------------------------------------------

Sys:           jsr     IntExpression   ; get sys address
               lda     estacklo        ; setup indirect jmp
               sta     sysvec
               lda     estackhi
               sta     sysvec+1
               lda     #>(Sysret-1)    ; provide return address
               pha
               lda     #<(Sysret-1)
               pha
               lda     sysflags        ; get regs from user
               pha
               lda     sysareg
               ldx     sysxreg
               ldy     sysyreg
               plp
               jmp     (sysvec)        ; go
               
Sysret:        php       
               sta     sysareg
               stx     sysxreg
               sty     sysyreg
               pla
               sta     sysflags
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; BRKON
; BRKOFF       control RUN/STOP check at NewStt
;-------------------------------------------------------------------------------

BrkOn:         lda     #$ff
               DB      $2c
BrkOff:        lda     $00
               sta     chkbrkflg
               jmp     NewStt
               
               

               
               
