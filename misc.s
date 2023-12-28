;==============================================================================================
; MISC.S
;
; strout,decimal and hex print. ReadDec,IsDigit,IsAlpha
;==============================================================================================

;-------------------------------------------------------------------------------
; CROut,SPCOut
;-------------------------------------------------------------------------------

CROut:         lda     #$0d
-              jmp     CHROUT
SPCOut:        lda     #$20
               bne     -
               
;-------------------------------------------------------------------------------
; PrtBHex2(a),PrtBHex4(number,number+1)
; !! trashes x
; returns with Z=1
;-------------------------------------------------------------------------------

PrtBHex4:      lda     number+1
               jsr     PrtBHex2
               lda     number
               jmp     pbh020
PrtBHex2:      ldx     #0
pbh020:        pha
               lsr                                  
               lsr
               lsr
               lsr
               jsr     prnyb
               pla    
               and     #$0f
prnyb:         clc
               adc     #$30 
               cmp     #$3a
               bcc     prn034
               adc     #$06
prn034:        sta     prtbuf,x
               inx
               lda     #0
               sta     prtbuf,x
               rts

;-------------------------------------------------------------------------------
; PrtStrAY. a = low, y = high byte of address of zero-terminated string
; on exit y = length of string (not counting the 00-terminator)
;-------------------------------------------------------------------------------

PrtStrAY:
               sty     strptr+1
               sta     strptr
               ldy     #0
-              lda     (strptr),y
               beq     +
               jsr     CHROUT
               iny
               bne     -               ; !! string must be shorter than 255 bytes 
+              rts

 
PrtBDecS:      bit     number+1
               bpl     PrtBDecU 
               sec
               lda     #0
               pha
               sbc     number
               sta     number
               pla
               sbc     number+1
               sta     number+1
               lda     #$2d            ; ascii -
               sta     prtbuf
               ldx     #1              ; prt index
               bne     +   
PrtBDecU:      ldx     #0
+:             lda     #$80            ; leading zero flag
               sta     prtmp
               ldy     #6
bpd10:         lda     #$ff            ; digit starts at -1
               sta     digit
               sec
bpd20:         lda     number          ; subtract power of 10..
               sbc     table10,y
               sta     number
               lda     number+1
               sbc     table10+1,y
               bcc     bpd30           ; don't store hi byte if subtracted 1 too much, so no need to add it back
               sta     number+1        
bpd30:         inc     digit
               bcs     bpd20           ; while result > 0
               lda     number          ; add back in, lo byte only, hi wasn't changed last time
               adc     table10,y
               sta     number
               lda     digit           ; digit
               bne     bpd50           ; not zero, print
               bit     prtmp           ; leading zero ?
               bmi     bpd80           ; yes, don't print
bpd50:         asl     prtmp           ; kill leading-Z flag
               jsr     bpdout
bpd80:         dey     
               dey   
               bpl     bpd10
               lda     number          ; number now in 0..9, store it
               jsr     bpdout
               lda     #0              ; terminating 00
               beq     bpdout2
bpdout:        ora     #'0'
bpdout2:       sta     prtbuf,x
               inx
               rts
               
table10:       .word   10,100,1000,10000     
               
PrtDecS:       jsr     PrtBDecS
               jmp     prtdo
PrtDecU:       jsr     PrtBDecU
prtdo:         ldx     #0
prtdo2:        lda     prtbuf,x
               bne     +
               rts
+              jsr     CHROUT
               inx
               bne     prtdo2          ; always
               
PrtHex2:       
               jsr     PrtBHex2
               beq     prh040          ; always
               
PrtHex4: 
               jsr     PrtBHex4    
prh040:        jsr     prtdo
               rts

;-------------------------------------------------------------------------------
; IsDigit(a) : return CC if in '0'..'9', a unchanged
;-------------------------------------------------------------------------------
               
IsDigit:       cmp     #'9' + 1
               bcs     + 
               sec
               sbc     #$30
               sec
               sbc     #$d0
+              rts

;-------------------------------------------------------------------------------
; IsVar(a)     return EQU if a = vartoken
;-------------------------------------------------------------------------------

IsVar:         cmp     #TK_VAR
               beq     +
               cmp     #TK_FVAR
+              rts
               
;-------------------------------------------------------------------------------
; IsAlpha(a) : return CC if in 'A'..'Z', a unchanged
;-------------------------------------------------------------------------------
               
IsAlpha:       cmp     #'Z' + 1
               bcs     + 
               sec
               sbc     #'A' 
               sec
               sbc     #256-'A'
+              rts

;-------------------------------------------------------------------------------
; IsNum:       EQ if (a) == TK_DECNUM or TK_HEXNUM
;-------------------------------------------------------------------------------

IsNum:         cmp     #TK_DECNUM
               beq     +
               cmp     #TK_HEXNUM
+:             rts

;-------------------------------------------------------------------------------
; initreadhd
; setup for readdec/readhex
;-------------------------------------------------------------------------------

initreadhd:    lda     #0
               sta     number
               sta     number+1
               sta     prtmp
               rts

;-------------------------------------------------------------------------------
; ReadDec. from INBUF,x  result to 'number', x points to non-digit
; first char is guaranteed to be a decimal digit
; !! save Y !!
;-------------------------------------------------------------------------------

ReadDec:       jsr     initreadhd
               tya
               pha
rdd030:        lda     INBUF,x
               jsr     IsDigit
               bcs     rddec99
               sbc     #('0' - 1)      ; Cy = clear
               sta     digit
               lda     number+1
               sta     prtmp
               lda     number          ; * 4
               asl
               rol     prtmp
               asl
               rol     prtmp
               clc
               adc     number
               sta     number
               lda     prtmp
               adc     number+1        ; + 1
               sta     number+1
               asl     number          ; * 2
               rol     number+1
               lda     number
               clc
               adc     digit           ; add in new digit
               sta     number
               lda     #0
               adc     number+1
               sta     number+1
               bcs     rh065           ; overflow
               inx
               bne     rdd030          ; always
rddec99:       pla
               tay
               rts
;
;-------------------------------------------------------------------------------
; ReadHex      read hex.number from INBUF,x. return EQ if nothing read
; save Y !     '$' has already been read
;-------------------------------------------------------------------------------

ReadHex:       jsr     initreadhd   
               tya                     ; save y
               pha
rh020:         lda     INBUF,x
               sec
               sbc     #$30
               cmp     #$0a
               bcc     gotdig          ; 0..9
               adc     #$e8            ; e9, but carry is set
               cmp     #$fa
               bcc     nodig
               and     #$0f            ; fa..ff, drop hi nybble
gotdig:        ldy     #3
rh060:         asl     number
               rol     number+1
               dey
               bpl     rh060
               bcc     rh070
rh065:         jmp     OvrErr          ; Cy out of number+1 : overflow
rh070:         ora     number          ; OR in new digit
               sta     number
               inc     prtmp
               inx
               bne     rh020           ; branch always
nodig:         pla
               tay
               lda     prtmp           ; <>0 if anything read
               rts
               
;-------------------------------------------------------------------------------
; MoveUp  (from,to,size)
; MoveUpA (from,to,last byte to move)
; MoveUpT (from,to)                   sets size to 'txtend' for ins/del line
; from = source start address
; to   = destination start address
; size = number of bytes to move/last byte to move
;-------------------------------------------------------------------------------
;
MoveUpT:       lda     txtend          ;     some bug here (line 50 at end of prog didn't disappear)
               sta     size
               lda     txtend+1
               sta     size+1
MoveUpA:       lda     size            ; &last byte - &first byte
               sec
               sbc     from
               sta     size
               lda     from+1
               sbc     size+1
               sta     size+1
               inc     size            ; + 1, size.
               bne     MoveUp
               inc     size+1
MoveUp:        ldx     size+1          ; the last byte must be moved first
               clc                     ; start at the final pages of FROM and TO
               txa                     
               adc     from+1          
               sta     from+1          
               clc                     
               txa                     
               adc     to+1            
               sta     to+1            
               inx                     ; allows the use of BNE after the DEX below
               ldy     size           
               beq     mu3             
               dey                     ; move bytes on the last page first
               beq     mu2             
mu1:           lda     (from),y        
               sta     (to),y          
               dey                     
               bne     mu1            
mu2:           lda     (from),y        ; handle Y = 0 separately
               sta     (to),y          
mu3:           dey                     
               dec     from+1          ; move the next page (if any)
               dec     to+1            
               dex                     
               bne     mu1             
               rts
               
;-------------------------------------------------------------------------------
; MoveDn  (from,to,size)
; MoveDnA (from,to,last byte to move)
; MoveDnT (from,to)                   sets size to 'txtend' for ins/del line
; from = source start address
; to   = destination start address
; size = number of bytes to move/last byte to move
;-------------------------------------------------------------------------------
;
MoveDnT:       lda     txtend
               sta     size
               lda     txtend+1
               sta     size+1
MoveDnA:       lda     size            ; &last byte - &first byte
               sec
               sbc     from
               sta     size
               lda     from+1
               sbc     size+1
               sta     size+1
               inc     size            ; + 1, size.
               bne     MoveDn
               inc     size+1
MoveDn:        ldy     #0
               ldx     size+1
               beq     md2
md1:           lda     (from),y        ; move a page at a time
               sta     (to),y          
               iny                     
               bne     md1             
               inc     from+1          
               inc     to+1            
               dex                     
               bne     md1             
md2:           ldx     size          
               beq     md4             
md3:           lda     (from),y        ; move the remaining bytes
               sta     (to),y
               iny     
               dex     
               bne     md3
md4:           rts

;-------------------------------------------------------------------------------
; ZeroMem      from,size
;-------------------------------------------------------------------------------

ZeroMem:       ldy     #0
               tya
               ldx     size+1
               beq     zm2
zm1:           sta     (from),y
               iny
               bne     zm1
               inc     from+1
               dex
               bne     zm1
zm2:           ldx     size
               beq     zm4
zm3:           sta     (from),y
               iny
               dex
               bne     zm3
zm4:           rts

               
;-------------------------------------------------------------------------------
; LoadMon
;-------------------------------------------------------------------------------

monfilename:   BYTE    "mon"
LoadMon:
               lda     #0              ; Logical filenum for LOAD
               ldx     #8              ; devicenum
               tay                     ; SA : use alternative load address
               jsr     SETLFS
               lda     #3              ; length filename
               ldx     #<monfilename
               ldy     #>monfilename
               jsr     SETNAM
               lda     #0              ; LOAD (1= VERIFY)
               ldx     #$00            ; set alternative load address
               ldy     #$c0            ; (not used if sa = 1)
               jmp     LOAD
               
;-------------------------------------------------------------------------------
; CopyRom
;-------------------------------------------------------------------------------

CopyRom:       lda     #$00            ; copy ROM to ram
               sta     wptr
               lda     #$a0
               sta     wptr+1
cro2:          ldy     #0
cro3:          lda     (wptr),y
               sta     (wptr),y
               iny
               bne     cro3  
               inc     wptr+1
               lda     wptr+1
               cmp     #$c0
               bne     cro2  
               lda     $01
               and     #$7e            ; switch in ram
               sta     $01
               rts

