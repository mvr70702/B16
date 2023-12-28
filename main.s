;==============================================================================================
; MAIN.S
;
; 1st file to be assembled, includes the other modules.
;
; (Un)GetChr, cold/warm start, error, input, mainloop, dispatch commands
;==============================================================================================

               .include                "kernel.inc"
               .include                "ram.s"
               
MAXSTR         =       32              ; maximum fixed stringlen (31 chars+terminator)
               
               ORG     $a000
               WORD    cold
               WORD    warm
               
;-------------------------------------------------------------------------------
; getchr/ungetchr. _GetChr is copied to ram @GetChr, txtptr & GetChr are
;                  defined in ram.s
; ! On return from GetChr, flags do not reflect the value in a !
;-------------------------------------------------------------------------------

UngetChr:      lda     txtptr          ; DO NOT change x,y or CY flag
               bne     +
               dec     txtptr+1
+:             dec     txtptr
               rts

_GetChr:
_gc5:          lda     $0a00           ; _gc5+1,+2 = txtptr
               inc     txtptr          ; postincrement
               bne     _gc8
               inc     txtptr+1
_gc8:          bit     skipflg         ; if a number is read, must skip $20 check. 5 extra cycles (or 6 if minus)
               bmi     _gc9
               cmp     #$20
               beq     _gc5
_gc9:          sta     csave
               rts
nbgc           =       $-_GetChr       ; length of _GetChr

;-------------------------------------------------------------------------------
; GetChrAll
; don't do $20-check
;-------------------------------------------------------------------------------

GetChrAll:     lda     #$80
               sta     skipflg
               jsr     GetChr
               asl     skipflg         ; clear
               rts
               
;-------------------------------------------------------------------------------
; PeekChr
;-------------------------------------------------------------------------------

PeekChr:       jsr     GetChr
               jsr     UngetChr
               lda     csave
               rts

;-------------------------------------------------------------------------------
; PrintHello
;-------------------------------------------------------------------------------

welcome:       BYTE    147,$0d,"    ***** B16 BASIC 1.0 BY MVR *****",$0d,$0d,$00

PrintHello:    lda     #<welcome
               ldy     #>welcome
               jmp     PrtStrAY
readymess:     BYTE    $0d,"OK",$0d,$00

;-------------------------------------------------------------------------------
; InitBasic
; - copy getchr to ram
; - set memtop,membot (memtop lowered 2 pages)
; - set txttab,vartab
; - clr fastvars
; - set various flags and values
;-------------------------------------------------------------------------------

InitBasic:     ldx     #0              ; copy _GetChr to ram
inb020:        lda     _GetChr,x
               sta     GetChr,x
               inx
               cpx     #nbgc
               bne     inb020
               sec                     ; read memtop
               jsr     MEMTOP
               stx     memtop
               dey
               dey                     ; ! kernel uses top of basic memory !
               sty     memtop+1
               lda     #0
               sta     membot
               lda     #$0a
               sta     membot+1
               jsr     InitProgPtrs
               jsr     InitVarPtrs
               jsr     ClrFastvars
               lda     #15             ; $-len, 1 is added in allocation for terminating $00
               sta     strlen
               jsr     InitBasicVars
               rts
               
;-------------------------------------------------------------------------------
; InitBasicVars
; warm start, error all call this
; ! Does NOT change strlen
;-------------------------------------------------------------------------------

InitBasicVars:
               lda     #0
               sta     skipflg
               sta     brkflg
               sta     tempstrix
               sta     oldtxt+1        ; prevent CONT
               lda     #$ff
               sta     chkbrkflg       ; check RUN/STOP at NewStt
               rts

;-------------------------------------------------------------------------------
; Cold start
;-------------------------------------------------------------------------------

cold:          jsr     InitBasic
               tsx                     ; save stackpointer on entry
               stx     stksav
               jsr     CopyRom         ; misc.s
               jsr     LoadMon         ; load mon for now
               jsr     PrintHello      ; print BASIC startup message
                                       ; fall into 'warm'
               
;-------------------------------------------------------------------------------
; Warm start (RUN/STOP)
;-------------------------------------------------------------------------------

warm:          jsr     CLRCHN          ; set default channels
               jsr     StackInit
               jsr     InitBasicVars
               jsr     $c000
               jmp     Ready
               
;-------------------------------------------------------------------------------
; SynChk       (a)
; check if next char equals a 
;-------------------------------------------------------------------------------

SynChk:        pha
               jsr     GetChr
               pla
               cmp     csave
               bne     ExpErr
Synrts:        rts
               
;-------------------------------------------------------------------------------
; ChkRoom
; Before a line is entered in the prog, check if it won't force txtend beyond
; vartab. Also used by 'vars' to check if there's room to enter a var/array
; acc has low, x has high byte of room needed.
; Exit to MemErr if not enough room.
;-------------------------------------------------------------------------------

ChkRoom:       clc                     ; set wptr to (txtend + a)
               adc     txtend
               sta     wptr
               txa                     ; get hi byte
               adc     txtend+1        ; no need to store wptr+1, a has high byte
               bcs     MemErr          ; overflow (request + txtend exceeds 64), error
               cmp     vartab+1        ; compare high bytes
               beq     cro80           ; equal, go check low bytes
               bcs     MemErr          ; txtend > vartab, error
               bcc     cro90           ; hi bytes OK
cro80:         lda     wptr            ; low byte new txtend
               cmp     vartab
               bcs     MemErr          ; new txtend >= vartab, Error
cro90:         rts

;-------------------------------------------------------------------------------
; Break        RUN/STOP pressed while running
;-------------------------------------------------------------------------------

Break:         inc     brkflg
               jsr     SaveContext     ; save txtptr etc for CONT
               ldx     #E_BREAK
               jsr     PrintErr
               jmp     Ready
               
;-------------------------------------------------------------------------------
; Error handler. x is index into table of messages
;-------------------------------------------------------------------------------

PrintErr:      dex                     ; print errormessage
errprt:        inx       
               lda     ERRSTT,x
               php
               and     #$7f
               jsr     CHROUT
               plp
               bpl     errprt
               jsr     SPCOut
               lda     brkflg
               bne     ep040           ; if BREAK, skip "ERROR"
               lda     #<errormsg
               ldy     #>errormsg
               jsr     PrtStrAY        ; print "ERROR"
ep040:         lda     curlin+1
               cmp     #$ff            ; direct mode ? skip "IN", linenr
               beq     ep080
               lda     #<inmsg         ; print "IN"
               ldy     #>inmsg
               jsr     PrtStrAY
               lda     curlin
               sta     number
               lda     curlin+1
               sta     number+1
               jsr     PrtDecU         ; print linenr.
               
ep080:         lda     #0
               sta     brkflg
               jmp     CROut
               
IllErr:        ldx     #E_ILL  
               bne     ErrHnd
LLErr:         ldx     #E_LL
               bne     ErrHnd
TypeErr:       ldx     #E_TYP
               bne     ErrHnd
ExpErr:        jsr     CHROUT          ; xxx expected
               ldx     #E_EXP
               bne     ErrHnd2
FrmErr:        ldx     #E_FRM
               bne     ErrHnd
OvrErr:        ldx     #E_OVF
               bne     ErrHnd
MemErr:        ldx     #E_MEM
               bne     ErrHnd
IndErr:        ldx     #E_IND  
               bne     ErrHnd
ContErr:       ldx     #E_CCONT   
               bne     ErrHnd
LineErr:       ldx     #E_LINE
               bne     ErrHnd
SynErr:        ldx     #E_SYN
ErrHnd:        jsr     CROut
ErrHnd2:       jsr     CLRCHN
               jsr     PrintErr
               jsr     StackInit
               jsr     InitBasicVars
;               jmp     Main
               
;-------------------------------------------------------------------------------
; print readymessage, fall into main
;-------------------------------------------------------------------------------
;
Ready:         lda     #<readymess
               ldy     #>readymess
               jsr     PrtStrAY
               
;-------------------------------------------------------------------------------
; Main : get input from screeneditor
;-------------------------------------------------------------------------------

Main:          ldx     #0
               jsr     CHRIN
               cmp     #$20            ; ignore leading spaces, don't bother PreComp with them
               beq     Main
               bne     +
main1:         jsr     CHRIN
+              cmp     #$0d
               bne     main20
               lda     #0
main20:        sta     INBUF,x
               beq     main30 
               inx
               bne     main1           ; always
main30:        jsr     CROut

;-------------------------------------------------------------------------------
; Got line. Precompile. If direct mode go execute else insert/delete line in prog
;-------------------------------------------------------------------------------

               jsr     PreComp
               lda     crunchbuf+2
               cmp     #$ff            ; direct mode ?
               beq     execute
               sta     linnum+1        ; store hi byte of linenumber
               lda     crunchbuf+1
               sta     linnum          ; and lo
               jsr     FindLine        ; does this line exist ? linptr gets address
               bcc     main040         ; no
               jsr     DelLine         ; delete current
               lda     #0
               sta     oldtxt+1        ; can't continue
main040:       lda     crunchbuf       ; get length of new line
               cmp     #4              ; 4 bytes is just len-linel-lineh-0x00 (empty line)
               beq     Main            ; nothing further to do
;               
;-------------------------------------------------------------------------------
; Insert line in prog. First check space available
;-------------------------------------------------------------------------------
 
               ldx     #0              ; x = hi byte lebgth of line
               jsr     ChkRoom         ; a has length of line (crunchbuf[0])
               jsr     InsLine         ; insert new line
               lda     #0
               sta     oldtxt+1        ; can't continue
               jmp     Main     

;-------------------------------------------------------------------------------
; Execute direct mode
;-------------------------------------------------------------------------------

execute:       lda     #<crunchbuf
               sta     pcurlin
               lda     #>crunchbuf
               sta     pcurlin+1
               jmp     GoLine
               
;-------------------------------------------------------------------------------
; new statement
; all keywords return here, except when a new line must be executed (GOTO,..)
;-------------------------------------------------------------------------------
 
NewStt:        lda     #0
               sta     tempstrix
               bit     chkbrkflg       ; must check RUN/STOP ?
               bpl     stm050          ; no
               jsr     ChkStop         ; check RUN/STOP 
               bne     stm050
               jmp     Break
stm050:        jsr     GetChr    
               cmp     #0              ; EOL ?
               beq     StmEol
               cmp     #':'            ; colon
               beq     NewStt
               jsr     IsVar           ; assignment ?
               bne     stm100
               jsr     UngetChr        ; backup to start of var
               lda     #TK_LET         ; oldfashioned LET..
               bne     stm150          ; always
stm100:        cmp     #TK_REM         ; REM (') isn't in the list of keywords, process here
               bne     stm110
               jmp     StmEol
stm110:        cmp     #STMSTT         ; in range STMSTT..STMEND, can execute
               bcc     stmerr
               cmp     #STMEND
               bcs     stmerr          ; not a statement or assignment, error
               
;-------------------------------------------------------------------------------
; dispatch
;-------------------------------------------------------------------------------

stm150:        sec
               sbc     #STMSTT         ; make index into STMDSP table
               asl
               tax
               lda     STMDSP+1,x      ; hi byte of dispatch addrbess
               pha
               lda     STMDSP,x
               pha
               rts                     ; go execute token
               
;-------------------------------------------------------------------------------
; EOL found. Check direct mode, end of program. Setup pcurlin
;-------------------------------------------------------------------------------
;
StmEol:        lda     curlin+1        ; EOL found, if in direct mode, back to input mode
               cmp     #$ff
               bne     NextLine
               jmp     Ready
NextLine:      clc
               ldy     #0
               lda     (pcurlin),y     ; length current line
               adc     pcurlin         ; length + current start -> newline address
               sta     pcurlin
               lda     pcurlin+1
               adc     #0
               sta     pcurlin+1
               ldy     #0
               lda     (pcurlin),y     ; length byte new line
               cmp     #0              ; end of program ?
               bne     GoLine
               jmp     Ready
               
;-------------------------------------------------------------------------------
; pcurlin is set up for next line, set txtptr, curlin and continue execution
; GOTO/GOSUB also come here
;-------------------------------------------------------------------------------

GoLine:        ldy     #1
               lda     (pcurlin),y     ; linenumber lo
               sta     curlin
               iny
               lda     (pcurlin),y     ; linenumber hi
               sta     curlin+1        ; !! Trace can be executed here
               clc
               lda     pcurlin
               adc     #3              ; text starts 3 byte away from linestart
               sta     txtptr
               lda     pcurlin+1
               adc     #0
               sta     txtptr+1
               jmp     NewStt          ; continue executing
stmerr:        jmp     SynErr
               
;-------------------------------------------------------------------------------
; ChkStop      see if RUN/STOP pressed. return EQ if so
;-------------------------------------------------------------------------------

ChkStop:       lda     STKEY
               cmp     #$7f
               bne     chks90
               lda     #0
               sta     NDX             ; remove STOP from keybuf, set Z
chks90:        rts

;-------------------------------------------------------------------------------
; ReadLineNr   Get a (decimal) linenumber from (txtptr) (GOTO, RUN, etc)
;              if there's a linenumber, set in in 'linnum', return CY = 0
;              if not : if CY set by caller, jump to syntax error, else just
;              return CY set. 
;-------------------------------------------------------------------------------

ReadLineNr:    php
               jsr     GetChr
               cmp     #TK_DECNUM
               bne     rln050
               jsr     GetChrAll
               sta     linnum
               jsr     GetChrAll
               sta     linnum+1
               plp
               clc                     ; OK return
               rts
rln050:        plp       
               bcc     rln070          ; error requested ?
               jmp     SynErr          ; yes
rln070:        sec                     ; BAD return
               rts
               
;-------------------------------------------------------------------------------
; Assemble rest of program
;-------------------------------------------------------------------------------

               INCLUDE                 "program.s"
               INCLUDE                 "expression.s"
               INCLUDE                 "intops.s"
               INCLUDE                 "intfuncs.s"
               INCLUDE                 "strings.s"
               INCLUDE                 "misc.s" 
               INCLUDE                 "vars.s"
               INCLUDE                 "keywlist.s"
               INCLUDE                 "keywords1.s"
               INCLUDE                 "keywords2.s"
               INCLUDE                 "errors.s"
               INCLUDE                 "fileio.s"
               INCLUDE                 "screenio.s"
               
               PAD     $c000
               
