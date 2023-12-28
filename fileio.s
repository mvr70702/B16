;==============================================================================================
; fileio.s
;
; DIR cmd by DMANTIONE, from lemon64
;==============================================================================================

;-------------------------------------------------------------------------------
; KernalIOMsg
; set/clear MSGFLAG
;-------------------------------------------------------------------------------

KernalIOMsgOff:lda     #$00
               beq     +      
KernalIOMsgOn: lda     #$ff
+:             sta     MSGFLG
               rts
               

;-------------------------------------------------------------------------------
; GetName
; common code - read string to filename, set namelen, set fnptr to filename
;-------------------------------------------------------------------------------

GetName:       jsr     StrExpression
               lda     estacklo
               sta     fnptr
               lda     estackhi
               sta     fnptr+1
               ldy     #$ff
gna020:        iny
               cpy     #31             ; filename can hold 32 chars max.
               bcc     gna040
               lda     #0
               beq     gna050
gna040:        lda     (fnptr),y       ; copy name to safe place.
gna050:        sta     filename,y
               bne     gna020
               sty     namelen
               lda     #<filename
               sta     fnptr
               lda     #>filename
               sta     fnptr+1
               rts

;-------------------------------------------------------------------------------
; GetNameDevAdr
; common code for LOAD/SAVE 
; read filename and devnr, exit if not present
; if a comma is following, read first address and set carry
;-------------------------------------------------------------------------------

GetNameDevAdr: jsr     GetName
               jsr     ChkComma
               jsr     IntExpression   ; dev.nr
               lda     estacklo
               sta     devnum
               jsr     GetChr
               cmp     #','
               beq     gnd050
               clc
               jmp     UngetChr        ; exit
gnd050:        jsr     IntExpression
               sec
               rts

;-------------------------------------------------------------------------------
; SetNamLFS
; a must have logical filenr (0 for LOAD, 1 for SAVE)
; y must have SA 
;-------------------------------------------------------------------------------

SetNamLFS:     ldx     devnum
               jsr     SETLFS
               ldx     fnptr
               ldy     fnptr+1
               lda     namelen
               jmp     SETNAM

;-------------------------------------------------------------------------------
; SAVE   ".." ,dev <,start,end> if start is present, end is also required
;-------------------------------------------------------------------------------

Save:          jsr     KernalIOMsgOn
               lda     txttab          ; assume B16 prog
               sta     from            ; use 'from' as ZP holder for startaddress
               lda     txttab+1
               sta     from+1
               jsr     GetNameDevAdr
               php                     ; save CY
               lda     #1              ; LFN (1 = reserved for SAVE)
               ldy     #0              ; SA
               jsr     SetNamLFS
               plp                     ; restore CY
               bcc     save050         ; was there an optional startaddress ?
               lda     estacklo        ; yes, set it..
               sta     from
               lda     estackhi
               sta     from+1
               jsr     ChkComma        ; .. get required endaddress..
               jsr     IntExpression
               ldx     estacklo        ; .. and put it in x,y for call to SAVE
               ldy     estackhi
               jmp     save070
save050:       lda     txtend          ; need endaddress + 1 !
               clc
               adc     #1
               tax
               lda     txtend+1
               adc     #0
               tay
save070:       lda     #<from          ; ZP offset of startaddress
               jsr     SAVE
               jsr     KernalIOMsgOff
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; LOAD   ".." ,dev <,address> if address==1, use header, else load at address
; in direct mode, on a normal B16 LOAD, 'CLR' is called, txtend set
; in program mode, there's no CLR and execution is forced, starting with
; the first program line.
;
; ! so whenever a SA or an alternative loadaddress is given, this is
; considered to NOT being a B16 load, and nothing is adjusted
;-------------------------------------------------------------------------------
               
Load:    
               jsr     KernalIOMsgOn
               lda     txttab          ; assume normal B16 LOAD
               sta     to
               lda     txttab+1
               sta     to+1
               lda     #0              ; normal B16 LOAD
               sta     ldb16
               jsr     GetNameDevAdr
               bcc     load030         ; No SA, go do normal B16 load
               ldy     estacklo        ; check if SA == 1
               cpy     #1
               bne     load010         ; no go setup user address
               lda     estackhi        ; (hi byte must be 0)
               beq     load040         ; SA=1, go load from header address
               
load010:       lda     estacklo        ; setup user address
               sta     to
               lda     estackhi
               sta     to+1
               inc     ldb16
               
load030:       ldy     #0              ; SA != 1, user loadaddress
load040:       lda     #0              ; LFN (0 is reserved for LOAD)
               jsr     SetNamLFS
               
load070:       ldx     to              ; (not needed if SA=1)
               ldy     to+1            ; load address
               lda     #0              ; LOAD (1 = VFY)
load080:       
               jsr     LOAD
               lda     ldb16           ; a normal B16 load ?
               bne     load090
;              save x,y 
               sty     txtend+1
               stx     txtend
;               lda     curlin+1        ; direct mode ?
;               cmp     #$ff
;               bne     load090
;               jmp     Clr
load090:       jsr     KernalIOMsgOff
               jmp     NewStt

;-------------------------------------------------------------------------------
; DIR          DIR devnum [,"wildcard string"]
;-------------------------------------------------------------------------------

deverr:        ldx     #E_DEV
               jmp     ErrHnd
skip:          nop
-:             jsr     ACPTR
               dex
               bne     -
               rts

Dir:           lda     #0              ; ! must init STATUS, kernal doesn't
               sta     STATUS
               jsr     KernalIOMsgOn
               jsr     IntExpression
               lda     estacklo
               sta     devnum
               jsr     LISTN
               lda     #$f0            ; open channel 0
               jsr     SECND
               lda     STATUS          ; check device present
               bne     deverr
               lda     #'$'
               jsr     CIOUT
               
               lda     #'*'
               jsr     CIOUT
               lda     #'.'
               jsr     CIOUT
               lda     #'P'
               jsr     CIOUT
               lda     #'R'
               jsr     CIOUT
               lda     #'G'
               jsr     CIOUT
               
               
               jsr     UNLSN
               lda     devnum
               jsr     TALK
               lda     #$60
               jsr     TKSA
               ldx     #2
               jsr     skip
dir020:        ldx     #2
               jsr     skip
               lda     STATUS
               bne     dir090
               jsr     ACPTR           ; size (blocks) lo
               sta     number
               jsr     ACPTR           ; size (blocks) hi
               sta     number+1
               jsr     PrtDecU
               lda     STATUS
               bne     dir090
dir040:        jsr     ACPTR           ; get the filename
               tax
               cmp     #0
               beq     dir060
               lda     STATUS
               bne     dir090
               txa
               jsr     CHROUT          ; print it
               jmp     dir040
dir060:        jsr     CROut
               lda     STATUS
               beq     dir020
dir090:        
               jsr     UNTLK
               lda     devnum
               jsr     LISTN
               lda     #$e0            ; CLOSE
               ora     devnum
               jsr     SECND
               jsr     UNLSN
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; OPEN
; LFN,DEV,SA(channelnr) [,"NAME"]
; LFN: 1..255
; DEV: 0..15
; SA : 0..15, channels 0/1 are for LOAD/SAVE resp.
;-------------------------------------------------------------------------------
               
Open:          jsr     KernalIOMsgOff  ; important ! 
               jsr     IntExpression   ; read LFN
               lda     estacklo
               sta     lfn
               jsr     ChkComma
               jsr     IntExpression   ; read dev
               lda     estacklo
               sta     devnum
               jsr     ChkComma
               jsr     IntExpression   ; read SA (channel)
               lda     estacklo
               sta     chnl
               jsr     ChkComma
               jsr     GetName
               lda     lfn
               ldy     chnl
               jsr     SetNamLFS
               jsr     OPEN
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; CLOSE lfn
;-------------------------------------------------------------------------------

Close:         jsr     IntExpression
               lda     estacklo
               jsr     CLOSE
               jmp     NewStt
              
;-------------------------------------------------------------------------------
; PRINT#       lfn [,expr,expr]
;              without arguments : print a CR
;-------------------------------------------------------------------------------

PrintN:        jsr     IntExpression   ; get lfn
               ldx     estacklo
               stx     lfn
               jsr     PeekChr
               cmp     #':'            ; end of statement ?
               beq     pn0900          ; go print CR
               cmp     #0
               beq     pn0900
               jsr     ChkComma
pn0100:        jsr     Expression
               ldx     lfn
               jsr     CHKOUT          ; set as output channel, AFTER Expression (there may be an EXPR. ERRPR)
               lda     estacktype
               beq     pn0200          ; int ?    
               lda     estacklo        ; no, output string to channel
               sta     strptr
               lda     estackhi
               sta     strptr+1
               ldy     #0
pn0120:        lda     (strptr),y
               php
               iny
               jsr     CHROUT
               plp
               bne     pn0120
               beq     pn0300
pn0200:        lda     estacklo        ; output int to channel
               jsr     CHROUT
               lda     estackhi
               jsr     CHROUT 
pn0300:        jsr     CLRCHN          ; restore default I/O
               jsr     GetChr
               cmp     #','            ; more to print ?
               beq     pn0100          ; yes, go do it
               jsr     UngetChr        ; no,exit
               jmp     pn0910
pn0900:        jsr     CROut           ; print without arguments, CR
pn0910:        jsr     CLRCHN
               jmp     NewStt
               
;-------------------------------------------------------------------------------
; GET#         x=GET#(lfn)
;-------------------------------------------------------------------------------
               
;-------------------------------------------------------------------------------
; INPUT#       lfn,var[,var]
;-------------------------------------------------------------------------------

InputN:        jsr     IntExpression
               ldx     estacklo
               stx     lfn
               jsr     PeekChr
               jsr     IsVar
;              bne     in0990
               jsr     PtrGet
               
;ST / STATUS ?()

               
               
