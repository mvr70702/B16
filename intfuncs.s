;==============================================================================================
; ifuncs.s
; funcs return to 'expfret' in expression.s
;==============================================================================================

;-------------------------------------------------------------------------------
; ChkLparen
;-------------------------------------------------------------------------------

ChkLparen:     lda     #'('
               jsr     SynChk
               rts

;-------------------------------------------------------------------------------
; ChkRparen
;-------------------------------------------------------------------------------

ChkRparen:     lda     #')'
               jmp     SynChk
               
;-------------------------------------------------------------------------------
; Entry1int    check '(',expr==int,')'
;-------------------------------------------------------------------------------

Entry1int:     jsr     ChkLparen
               jsr     IntExpression2
               jmp     ChkRparen

;-------------------------------------------------------------------------------
; DoSgn
;-------------------------------------------------------------------------------

DoSgn:         jsr     Entry1int
               lda     estackhi-1,x      ; get sign
               bpl     dosgn060
               lda     #$ff              ; it's negative
dosgn050:      sta     estacklo-1,x
dosgn055:      sta     estackhi-1,x
               lda     #0
               sta     estacktype-1,x
               jmp     expfret
dosgn060:      lda     estackhi-1    
               ora     estacklo-1,x
               bne     dosgn070
               lda     #0
               beq     dosgn050
dosgn070:      lda     #1
               sta     estacklo-1,x
dosgn080:      lda     #0
               beq     dosgn055
               
;-------------------------------------------------------------------------------
; PeekEntry    preparations for PEEK/PEEKW
;-------------------------------------------------------------------------------

PeekEntry:     jsr     Entry1int
               lda     estacklo-1,x
               sta     peeker
               lda     estackhi-1,x
               sta     peeker+1
               sty     ysavpeek
               ldy     #0
               lda     (peeker),y
               sta     estacklo-1,x
               rts
               
;-------------------------------------------------------------------------------
; DoPeek
;-------------------------------------------------------------------------------

DoPeek:        jsr     PeekEntry
               ldy     ysavpeek
               jmp     dosgn080
               
;-------------------------------------------------------------------------------
; DoPeekW
;-------------------------------------------------------------------------------

DoPeekW:       jsr     PeekEntry
               iny
               lda     (peeker),y
               ldy     ysavpeek
               jmp     dosgn055
               
;-------------------------------------------------------------------------------
; DoFre
;-------------------------------------------------------------------------------

DoFre:         jsr     Entry1int
               sec
               lda     vartab
               sbc     txtend
               sta     estacklo-1,x
               lda     vartab+1
               sbc     txtend+1
               sta     estackhi-1,x
               jmp     Dovp10          ; store inttype & return
               
;-------------------------------------------------------------------------------
; DoAbs
;-------------------------------------------------------------------------------

DoAbs:         jsr     Entry1int
               lda     estackhi-1,x
               bpl     +
               jsr     DoUminus
+:             jmp     expfret

;-------------------------------------------------------------------------------
; DoHi
;-------------------------------------------------------------------------------

DoHi:          jsr     Entry1int
               lda     estackhi-1,x
               sta     estacklo-1,x
               lda     #0
               sta     estackhi-1,x
               jmp     Dovp10          ; store inttype & return
               
;-------------------------------------------------------------------------------
; DoVarptr
;-------------------------------------------------------------------------------

DoVarptr:      jsr     ChkLparen
               jsr     PtrGet
               jsr     ChkRparen
               lda     varptr
               sta     estacklo,x
               lda     varptr+1
               sta     estackhi,x
Dovp10:        lda     #T_INT
               sta     estacktype,x
               jmp     expfret
               
;-------------------------------------------------------------------------------
; DoTime
;-------------------------------------------------------------------------------

DoTime:        jsr     ChkLparen
               jsr     IntExpression2
               jsr     ChkRparen
               lda     $a2
               sta     estacklo-1,x
               lda     $a1
               sta     estackhi-1,x
               jmp     Dovp10

;-------------------------------------------------------------------------------
; RND(x)
;-------------------------------------------------------------------------------

DoRnd:
