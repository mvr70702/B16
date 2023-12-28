;==============================================================================================
; Vars.s
;          
; vartab : n1 n2 0x00 v1 v2         for integer var.
;          n1 n2 0x01 v1            for byte var.
;          n1 n2 0x02 ...           for strings (STRLEN).
;          n1 n2 0x8x pp pp dd dd ..bit 7 set : array, pp pp is *nextvar, dd = dimension.
;
; all vars are inited to $00
;==============================================================================================

               rts
               
;-------------------------------------------------------------------------------
; GetSize(a)   get size of one element
;-------------------------------------------------------------------------------

GetSize:       and     #$7f            ; mask off array bit
               cmp     #2              ; 2-> strlen
               bne     gs20
               lda     strlen          ; !! strlen is one less than actual allocation
               clc
               adc     #1
               bne     gs90 
gs20:          cmp     #1
               beq     gs90            ; 1 -> 1
               lda     #2
gs90:          rts
                                  
;-------------------------------------------------------------------------------
; VarSize(vartyp,mnum2,mnum2+1)
; calc the size of scalar/array ->varsize
; if vartyp is an array, store dimension in mnum2,mnum2+1 before calling
; returns address of 1st item (array refs must add (index * GetSize))
;-------------------------------------------------------------------------------

VsAdd:         clc                     ; helper : add a to varsize
               adc     varsize
               sta     varsize
               lda     varsize+1
               adc     #0
               sta     varsize+1
               rts

VarSize:       lda     #0              ; total size of scalar/array, including header
               sta     varsize+1
               sta     mnum1+1
               lda     vartyp
               bmi     vsarray
               jsr     GetSize         ; size of one element
               sta     varsize
               lda     #3              ; n1,n2,type
               bne     VsAdd
vsarray:       and     #$7f            ; mask off array bit
               jsr     GetSize         ; size of one element
               sta     mnum1           ; mnum1 hi already set to 0
               jsr     Umul
               lda     mulres+1
               sta     varsize+1
               lda     mulres
               sta     varsize
               lda     #7              ; n1,n2,type,pointer to next,diml,h
               bne     VsAdd
               
;-------------------------------------------------------------------------------
; FindVar (varn1,varn2,vartyp)
; search table (fastvars have already been dealt with)
; return address of entry in varptr, CY set if found, else CY clear
;-------------------------------------------------------------------------------

InitVarptr:    lda     vartab          ; helper : init varptr
               sta     varptr
               lda     vartab+1
               sta     varptr+1
               rts
               
AddVptr3:      lda     #3    
AddVptr:       clc        
               adc     varptr
               sta     varptr
               lda     #0
               adc     varptr+1
               sta     varptr+1
               rts
               
ChkVarend:     lda     varptr          ; helper : check if varptr == memtop
               cmp     memtop
               bne     chv90
               lda     varptr+1
               cmp     memtop+1        ; returns EQ if at end
chv90:         rts
               
NextVar:       ldy     #2              ; helper : set varptr to next var.
               lda     (varptr),y      ; reload type in vartab
               bmi     nv100           ; go handle array
               pha
               jsr     AddVptr3        ; add 3 to varptr
               pla                     ; type again
               jsr     GetSize         ; get size of 1 element
               clc
               adc     varptr
               sta     varptr
               lda     #0
               adc     varptr+1
               sta     varptr+1
               rts
nv100:         ldy     #3              ; array, has pointer to next var
               lda     (varptr),y
               pha
               iny
               lda     (varptr),y
               sta     varptr+1 
               pla
               sta     varptr
               rts 

FindVar:       tya
               pha
;               
; loop
;
               jsr     InitVarptr
fv120:         jsr     ChkVarend
               clc                     ; assume end, not found
               bne     fv150
               beq     fv900           ; always
fv150:         ldy     #0
fv160:         lda     (varptr),y      ; check name & type
               cmp     varn1,y
               bne     fv200
               iny
               cpy     #3
               bne     fv160
               sec                     ; good return
               bcs     fv900           ; exit
fv200:         jsr     NextVar
               jmp     fv120           ; loop
fv900:         pla
               tay
               rts

;-------------------------------------------------------------------------------
; EnterVar (varn1,varn2,vartyp,ndim,ndim+1)
; enter at vartab, set varptr to start of entry (not value)
; May cause an OUT OF MEMORY error
;-------------------------------------------------------------------------------

EnterVar:      tya
               pha
               lda     vartyp
               bpl     ev100
               lda     ndim
               sta     mnum2
               lda     ndim+1
               sta     mnum2+1
ev100:         jsr     VarSize         ; size to varsize,varsize+1
               lda     varsize
               ldx     varsize+1
               jsr     ChkRoom         ; check aloc possible . uses wptr !
               sec
               lda     vartab          ; new vartab = old-size
               sta     wptr            ; save old vartab, needed below
               sbc     varsize
               sta     vartab
               lda     vartab+1
               sta     wptr+1
               sbc     varsize+1
               sta     vartab+1
;              
; vartab now points to the start of the new entry
;
ev150:         lda     vartab
               sta     varptr          ; to varptr
               sta     from            ; prepare for ZeroMem
               lda     vartab+1
               sta     varptr+1
               sta     from+1
               lda     varsize         ; set size for ZeroMem
               sta     size
               lda     varsize+1
               sta     size+1
               jsr     ZeroMem         ; init the entire var
               ldy     #0              ; enter first 3 bytes (n1,n2,type)
ev160:         lda     varn1,y
               sta     (vartab),y      ; store n1,n2,type
               iny
               cpy     #3
               bne     ev160
               dey                     ; further code expects y=2
               lda     vartyp
               bmi     ev180           ; if array, go fill in next pointer & dim
               tax                     ; x now 0 (word),1 (byte), 2 (string)
               lda     #0              ; initialise scalars
               iny
               sta     (vartab),y      ; zero first byte (y already incremented to 3)
               dex 
               beq     ev200           ; if byte var, done
               iny
               sta     (vartab),y      ; zero 2nd byte
               bne     ev200           ; always, y isn't 0 here 
               
ev180:         lda     wptr            ; array pointer : to old vartab
               iny
               sta     (vartab),y
               iny
               lda     wptr+1
               sta     (vartab),y
               iny
               lda     ndim            ; also enter dim
               sta     (vartab),y
               iny
               lda     ndim+1
               sta     (vartab),y
               
ev200:         pla
               tay
               rts

;-------------------------------------------------------------------------------
; PtrGet
; result in varptr,vartyp - vartyp WITHOUT array-bit7
; It's already established (by Expression) that the first token is
; either TK_FVAR or TK_VAR
; Arrays may call Expression(), which may call PtrGet !
; if var is an array, check for closing ']'
;-------------------------------------------------------------------------------

PtrGet:        txa
               pha
               tya
               pha
               jsr     GetChr          ; get the token again (there was an UngetChr)
               cmp     #TK_FVAR
               bne     pg100
               jsr     GetChrAll       ; get offset (no $20-check !)
               clc
               adc     #<fastvars
               sta     varptr
               lda     #>fastvars
               sta     varptr+1
               lda     #0
               sta     vartyp
               jmp     pg900           ; done, exit
pg100:         jsr     GetChr          ; get name1
               sta     varn1
               jsr     GetChr          ; name2
               sta     varn2
               jsr     GetChr          ; and type
               sta     vartyp
               jsr     FindVar         ; does it already exist ?
               bcs     pg130           ; Findvar sets varptr, leaves vartyp
               lda     #10
               sta     ndim
               lda     #0
               sta     ndim+1          ; if entering array from here, dim = 10
               jsr     EnterVar        ; no, enter in table
;               
; if vartyp is an array, need to evaluate dimension
; varptr now points to entry
;
pg130:         bit     vartyp
               bmi     pg135           ; array        
               jsr     AddVptr3        ; value is 3 bytes after entry
               jmp     pg900
pg135:         lda     vartyp
               pha
               lda     varptr+1
               pha
               lda     varptr
               pha
               jsr     IntExpression   ; this may call GetPtr recursively
               pla
               sta     varptr
               pla
               sta     varptr+1
               pla
               sta     vartyp
               lda     #']'
               jsr     SynChk
               lda     vartyp
               and     #$7f            ; make sure array-bit off
               sta     vartyp          ; !! mask off array bit !! From here on, only 0,1,2 important
               
               pha                     ; save vartype
               ldy     #6              ; point to ndim+1
               lda     estackhi        ; check dimension here (estack vs dd in arrayentry) also check constant, <0
               cmp     (varptr),y
               bcc     pg138           ; hi byte index < hi byte dim, OK
               bne     pginderr        ; bigger, index error
               dey
               lda     estacklo
               cmp     (varptr),y
               bcc     pg138
pginderr:      jmp     IndErr

pg138:         pla                     ; retrieve vartype
               jsr     GetSize         ; size of one element
               cmp     #1              ; byte variable ?
               bne     pg150
pg140:         clc    
               lda     varptr          ; yes, add expression result to varptr
               adc     estacklo
               sta     varptr
               lda     varptr+1
               adc     estackhi
               sta     varptr+1
               lda     #7              ; add the header 
               jsr     AddVptr
               jmp     pg900           ; always, done
pg150:         cmp     #2              ; integer variable ?
               bne     pg170
               asl     estacklo        ; yes, double expression result
               rol     estackhi
               jmp     pg140           ; go add to varptr
pg170:         sta     mnum2           ; it's a stringarray, multiply expression result by strlen
               lda     #0
               sta     mnum2+1         ; strlen -> mnum2
               lda     estacklo
               sta     mnum1
               lda     estackhi
               sta     mnum1+1
               jsr     Umul
               lda     mulres
               sta     estacklo
               lda     mulres+1
               sta     estackhi
               jmp     pg140           ; add result to varptr
pg900:         pla
               tay
               pla
               tax
               rts
               
               
