;==============================================================================================
; expression.s
;
; uses x as estackptr (evaluation stack).x must be preserved at all times.
; y = operatorstack ptr(index)
;
; result is at estacklo,estackhi,estacktype
;                                             prio (1=lowest, 11 is highest)
; precedence : ||                                1
;              &&                                2
;              |                                 3
;              ^                                 4
;              &                                 5
;              ==,!=                             6                $ or int
;              <=,.<=,<,.<,>,.>,>=,.>=           7                $ or int
;              <<,>>                             8
;              +,-                               9                $ or int (+)
;              *,/,%,.*,./,.%                   10
;              +,-,!,!!                         11         unary
;
; operators, functions are done in intops.s, intfuncs.s, strings.s
;==============================================================================================
;
T_INT          =       $00             ; !! T_INT must be $00
T_BYTE         =       $01
T_STR          =       $02
;
; PrioTab 
;
F_RLA          =       $80             ; right-to-left associated
F_UN           =       $40             ; unary operator
PRIOMASK       =       $0f             ; priority bits only mask

PrioTab:       DB      1               ; ||          
               DB      2               ; AND logical 
               DB      3               ; |           
               DB      4               ; XOR         
               DB      5               ; AND binary  
               DB      6               ; EQL         
               DB      6               ; NEQ         
               DB      8               ; SHR         
               DB      8               ; SHL         
               DB      7               ; GEQ unsigned
               DB      7               ; GEQ signed  
               DB      7               ; LEQ unsigned
               DB      7               ; LEQ signed  
               DB      7               ; GTR unsigned
               DB      7               ; GTR signed  
               DB      7               ; LSS unsigned
               DB      7               ; LSS signed  
               DB      9               ; PLUS        
               DB      9               ; MINUS       
               DB     10               ; MULT unsigned
               DB     10               ; MULT signed 
               DB     10               ; DIV unsigned
               DB     10               ; DIV signed  
               DB     10               ; MOD unsigned
               DB     10               ; MOD signed  
               DB     11|F_RLA|F_UN    ; NOT logical 
               DB     11|F_RLA|F_UN    ; NOT binary  
               DB     11|F_RLA|F_UN    ; unary MINUS

;-------------------------------------------------------------------------------
; OprTab       the dispatch adresses, for int and string operators
;-------------------------------------------------------------------------------
               
OprTablo       DB      <(DoLor-1)      ; ||           
               DB      <(DoLand-1)     ; AND logical  
               DB      <(DoOr-1)       ; |            
               DB      <(DoXor-1)      ; XOR          
               DB      <(DoAnd-1)      ; AND binary   
               DB      <(DoEql-1)      ; EQL          
               DB      <(DoNeq-1)      ; NEQ          
               DB      <(DoLsr-1)      ; SHR          
               DB      <(DoLsl-1)      ; SHL          
               DB      <(DoUGeq-1)     ; GEQ unsigned 
               DB      <(DoGeq-1)      ; GEQ signed   
               DB      <(DoULeq-1)     ; LEQ unsigned 
               DB      <(DoLeq-1)      ; LEQ signed   
               DB      <(DoUGtr-1)     ; GTR unsigned 
               DB      <(DoGtr-1)      ; GTR signed   
               DB      <(DoULss-1)     ; LSS unsigned 
               DB      <(DoLss-1)      ; LSS signed   
               DB      <(DoPlus-1)     ; PLUS         
               DB      <(DoMinus-1)    ; MINUS        
               DB      <(DoUmul-1)     ; MULT unsigned
               DB      <(DoMul-1)      ; MULT signed  
               DB      <(DoUdiv-1)     ; DIV unsigned 
               DB      <(DoDiv-1)      ; DIV signed   
               DB      <(DoUmod-1)     ; MOD unsigned 
               DB      <(DoMod-1)      ; MOD signed   
               DB      <(DoLnot-1)     ; NOT logical  
               DB      <(DoNot-1)      ; NOT binary   
               DB      <(DoUminus-1)   ; unary MINUS  
               
OprTabloS 
;              DB      <(SynErr-1)   
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
               DB      <(DoStrEqu-1)
               DB      <(DoStrNeq-1)
               DB      <(SynErr-1)
               DB      <(SynErr-1)
               DB      <(DoStrGeq-1)
               DB      <(DoStrGeq-1)
               DB      <(DoStrLeq-1)
               DB      <(DoStrLeq-1)
               DB      <(DoStrGtr-1)
               DB      <(DoStrGtr-1)
               DB      <(DoStrLss-1)
               DB      <(DoStrLss-1)
               DB      <(DoStrCat-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
;              DB      <(SynErr-1)
               
OprTabhi       DB      >(DoLor-1)      ; ||           
               DB      >(DoLand-1)     ; AND logical  
               DB      >(DoOr-1)       ; |            
               DB      >(DoXor-1)      ; XOR          
               DB      >(DoAnd-1)      ; AND binary   
               DB      >(DoEql-1)      ; EQL          
               DB      >(DoNeq-1)      ; NEQ          
               DB      >(DoLsr-1)      ; SHR          
               DB      >(DoLsl-1)      ; SHL          
               DB      >(DoUGeq-1)     ; GEQ unsigned 
               DB      >(DoGeq-1)      ; GEQ signed   
               DB      >(DoULeq-1)     ; LEQ unsigned 
               DB      >(DoLeq-1)      ; LEQ signed   
               DB      >(DoUGtr-1)     ; GTR unsigned 
               DB      >(DoGtr-1)      ; GTR signed   
               DB      >(DoULss-1)     ; LSS unsigned 
               DB      >(DoLss-1)      ; LSS signed   
               DB      >(DoPlus-1)     ; PLUS         
               DB      >(DoMinus-1)    ; MINUS        
               DB      >(DoUmul-1)     ; MULT unsigned
               DB      >(DoMul-1)      ; MULT signed  
               DB      >(DoUdiv-1)     ; DIV unsigned 
               DB      >(DoDiv-1)      ; DIV signed   
               DB      >(DoUmod-1)     ; MOD unsigned 
               DB      >(DoMod-1)      ; MOD signed   
               DB      >(DoLnot-1)     ; NOT logical  
               DB      >(DoNot-1)      ; NOT binary   
               DB      >(DoUminus-1)   ; unary MINUS  
               
OprTabhiS 
;              DB      >(SynErr-1)   
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
               DB      >(DoStrEqu-1)
               DB      >(DoStrNeq-1)
               DB      >(SynErr-1)
               DB      >(SynErr-1)
               DB      >(DoStrGeq-1)
               DB      >(DoStrGeq-1)
               DB      >(DoStrLeq-1)
               DB      >(DoStrLeq-1)
               DB      >(DoStrGtr-1)
               DB      >(DoStrGtr-1)
               DB      >(DoStrLss-1)
               DB      >(DoStrLss-1)
               DB      >(DoStrCat-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
;              DB      >(SynErr-1)
                                       

;-------------------------------------------------------------------------------
; GetPrio(a) 
; does NOT return the RLA or UN flags
;-------------------------------------------------------------------------------

GetPrio:       sec
               sbc     #OPRSTT         ; make index 
               stx     xsavexpr
               tax
               lda     PrioTab,x
               and     #PRIOMASK       ; mask off unary/rla mask
               ldx     xsavexpr
               rts

;-------------------------------------------------------------------------------
; StackNum     put number at 'txtptr' on evaluation stack.
;              update 'txtptr'
;-------------------------------------------------------------------------------

StackNum:      jsr     GetChrAll       ; unfortunately need this when reading numbers,
               sta     estacklo,x
               jsr     GetChrAll
               sta     estackhi,x
               lda     #T_INT
               sta     estacktype,x
               inx                     ; inc estackptr
               rts
               
;-------------------------------------------------------------------------------
; StackStrlit  put address of string at 'txtptr' on evaluation stack.
;              update 'txtptr'
;-------------------------------------------------------------------------------
               
StackStrlit:   lda     txtptr          ; point to behind '"', to 1st char of string
               sta     estacklo,x
               lda     txtptr+1
               sta     estackhi,x
               lda     #T_STR
               sta     estacktype,x
               inx                     ; inc estackptr
sstrl20:       jsr     GetChr          ; skip rest of string
               cmp     #0              ; terminating zero skipped, done, txtptr now behind string
               bne     sstrl20
               rts
               
;-------------------------------------------------------------------------------
; StackVar
; varptr has address, vartyp = type
;-------------------------------------------------------------------------------

StackVar:      tya
               pha
               lda     vartyp          ; string ?
               cmp     #T_STR
               bne     sv200
               sta     estacktype,x
               lda     varptr
               sta     estacklo,x
               lda     varptr+1
               sta     estackhi,x
               jmp     sv260
               
sv200:         pha                     ; push vartyp
               ldy     #0              ; vartyp = 0 (int) or 1 (byte)
               lda     (varptr),y
               sta     estacklo,x
               iny
               pla                     ; vartyp
               beq     sv230           ; it's int
               lda     #0              ; byte var. set hi byte to 0
               beq     sv240
sv230:         lda     (varptr),y
sv240:         sta     estackhi,x
               lda     vartyp
               and     #$7e            ; remove array (has this been done already ?) and promote byte to int
               sta     estacktype,x
sv260:         inx
               pla
               tay
               rts
               
;-------------------------------------------------------------------------------
; ExecOpr(a)
; Sort out string/integer/unary/binary operators and dispatch
; destroys y, but caller(s) pushed it and will restore it
; ! estacktype in expression can only be T_INT or T_STR
;-------------------------------------------------------------------------------

ExecOpr:       sec
               sbc     #OPRSTT
               tay
               lda     PrioTab,y
               and     #F_UN           ; check if operator is unary
               beq     exo020          ; no
               lda     estacktype-1,x  ; check TOS type
               beq     exo040          ; it's INT, go dispatch int func
               jmp     TypeErr         ; string, error, no unary string operators
exo020:        lda     estacktype-2,x  ; left-side type
               pha                     ; save 
               cmp     estacktype-1,x  ; right-side type, same as left-side ?
               beq     exo030
               jmp     TypeErr         ; no, error
exo030:        pla                     ; re-get type
               bne     exo060          ; T_STR
exo040:        lda     OprTabhi,y      ; int operator, push execfunction
               pha
               lda     OprTablo,y
               pha
               rts                     ; go exec, returns to caller of this func
exo060:        tya
               sec
               sbc     #(TK_EQL-OPRSTT)  ; string dispatch : reduced table TK_EQL..TK_PLUS
               tay
               lda     OprTabhiS,y     ; string operator
               pha
               lda     OprTabloS,y
               pha
               rts                     ; go exec, returns to caller of this func
               
               
               
;-------------------------------------------------------------------------------
; StackOpr (a)
; put operator on stack, but first empty stack while in-stack priority
; >= incoming priority
;
; right-to left assoc. : push the operator if in-stack priority == incoming.
; There's a 'RLA'-flag in PrioTable, but the only RLA operators are
; all on level 11, so just check for that.
;
; pushes and restores y. it's destroyed in ExecOpr.
;-------------------------------------------------------------------------------
               
StackOpr:      sta     incopr          ; incoming operator
               jsr     GetPrio
;              and     #PRIOMASK
               sta     incprio         ; incoming priority
sto100:        dey
               lda     oprstack,y
               beq     sto200          ; dummy operator, just go stack opr
               jsr     GetPrio         ; in-stack-priority
;              and     #PRIOMASK
               cmp     incprio         ; in-stack >= incoming ?
               bcc     sto200          ; no, go stack opr
               bne     sto150          ; in-stack == incoming ?
               cmp     #11             ; yes, check -,!,!!  THIS IGNORES THE PRIOTABLE and FLAGS
               beq     sto200          ; all level 11 operators are RL-assoc, so just push them
sto150:        tya
               pha
               lda     oprstack,y      ; yes, must pop in-stack opr and execute. reload in-stack opr..
               jsr     ExecOpr
               pla
               tay
               jmp     sto100
sto200:        iny                     ; leave the dummy 0x00
               lda     incopr
               sta     oprstack,y      ; and stack the incoming operator
               iny
               rts
               
;-------------------------------------------------------------------------------
; EmptyStack   end of expression, evaluate stack if anything left.
;              pushes y. it's destroyed in ExecOpr.
;-------------------------------------------------------------------------------

EmptyStack:    dey
               tya
               pha
               lda     oprstack,y
               bne     ems050
               pla
               tay
               rts                     ; don't inx, drop the dummy 0x00
ems050:        jsr     ExecOpr
               pla
               tay
               jmp     EmptyStack

;-------------------------------------------------------------------------------
; Expression, Expression2
; call Expression from program, Expression2 from within Expression
; leaves answer on estack. Expression2 needs to use estack,x !
;
; error if nothing processed (empty expression)
; 'constant' : if all primaries are constants, this is 0 after expression
;-------------------------------------------------------------------------------

expfret:       pla                     ; common returnpoint for functions
               tay
               jmp     expr100
Expression:    ldx     #0              ; estack ptr
               ldy     #0              ; optrstack ptr
               sty     tempstrix       ; temp string index, rotating at the moment, but init anyway
               sty     constant        ; assume constant expression
               sty     anything        ; nothing processed yet
Expression2:   lda     #0              ; push dummy operator 0x00
               sta     oprstack,y
               iny
expr090:       lda     #$ff            ; set bit 7, 'inc' will set it to 0
               sta     primexp         ; start condition : primary expected
;               
; main loop
;
expr100:    
               jsr     GetChr          ; main loop - get something
;
; number
;
               jsr     IsNum           ; test dec/hexnum TK
               bne     expr210
               inc     anything
               bit     primexp         ; expected a primary ?
               bmi     expr120         ; yes
               jmp     FrmErr
expr120:       jsr     StackNum        ; go push num on estack
               inc     primexp         ; primexp = 0
               jmp     expr100         ; back mainloop
;              
; string literal 
;
expr210:       cmp     #'"'            ; string literal ?
               bne     expr310
               inc     anything
               bit     primexp
               bmi     expr220
               jmp     FrmErr
expr220:       jsr     StackStrlit
               inc     primexp         ; primexp = 0
expr230:       jmp     expr100         ; back mainloop
;               
; functions
;
;              
expr310:       cmp     #FNSTT
               bcc     expr410
               cmp     #FNEND
               bcs     expr410
               inc     anything
               inc     constant
               sta     xfnsav
               tya                     ; !!!!! push y here, functions must return to 'expfret', where it's popped2
               pha
               lda     xfnsav
               stx     xfnsav
               sec
               sbc     #FNSTT
               asl
               tax
               lda     FNDSP+1,x
               pha
               lda     FNDSP,x
               pha
               ldx     xfnsav
               rts                     ; dispatch, functions must return to expr100
;              
; Variable
;
expr410:       cmp     #TK_FVAR        ; !! primexp ?
               beq     expr420
               cmp     #TK_VAR
               bne     expr810
expr420:       jsr     UngetChr
               jsr     PtrGet
               jsr     StackVar
               inc     anything
               inc     constant
               inc     primexp         ; primexp = 0
expr430:       jmp     expr100
               
; operators
;
expr810:       cmp     #OPRSTT
               bcc     expr910
               cmp     #OPREND
               bcs     expr910
;               inc     anything
               bit     primexp         ; check if primary expected
               bpl     expr850         ; no, go check normal operator
               cmp     #TK_NOT         ; primary was expected, so this must be a unary operator
               beq     expr850
               cmp     #TK_LNOT
               beq     expr850
               cmp     #TK_PLUS        ; unary + : ignore
               beq     expr230         ; trampoline to expr100
               cmp     #TK_MINUS       ; it's unary minus : change token
               beq     expr830
expr820:       jmp     FrmErr
expr830:       lda     #TK_UMINUS
expr850:       jsr     StackOpr
               jmp     expr090         ; if unary, primexp was already $ff, but doesn't hurt set it again
;              
; ( or end
;
expr910:       cmp     #$28            ; '('
               bne     expr930
               jsr     Expression2
               lda     #$29            ; ')'
               jsr     SynChk
               beq     expr430         ; always, trampoline to expr100
;              
expr930:       jsr     EmptyStack      ; check if operators left, go pop and execute
               lda     anything        ; did we process anything ?
               beq     expr820         ; no, error
               jmp     UngetChr        ; unget what we couldn't process, exit
               
;-------------------------------------------------------------------------------
; IntExpression , StrExpression, '2' versions of both
;-------------------------------------------------------------------------------

IntExpression: jsr     Expression
               lda     estacktype
ie88:          beq     ie99            ; T_INT == 0
ie90:          jmp     TypeErr
ie99:          rts

StrExpression: jsr     Expression
               lda     estacktype
se88:          cmp     #T_STR
               jmp     ie88
               
IntExpression2: jsr    Expression2
                lda    estacktype-1,x
                jmp    ie88
                
StrExpression2: jsr    Expression2
                lda    estacktype-1,x
                jmp    se88
                
ConstIntExpr:  jsr     Expression         
               lda     estacktype      ; check int
               bne     ie90
               lda     constant
               bne     expr820
               rts
               

