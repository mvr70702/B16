;-------------------------------------------------------------------------------
; keywlist.s2
;-------------------------------------------------------------------------------

Q              =       $80
               MACRO   TOK c1
               DCI     c1
Q              =       Q+1
               ENDM
               
;-------------------------------------------------------------------------------
; the lists of reserved words etc. and definitions for tokens
; (including TK_DECNUM,TK_STRING,TK_HEXNUM,TK_VAR,TK_FVAR)
;
; since more than 256 bytes are needed, there are 3 tables :
;
; keywords   tokens that can start a statement,
;            tokens like THEN, ELSE
; functions
; operators  these consist of non-alpha char's
;
; Tables start with a one byte tokenvalue, the value of the first token in
; the table, and end with $00
;-------------------------------------------------------------------------------
;
;-------------------------------------------------------------------------------
; keywords
;-------------------------------------------------------------------------------

STMSTT:        =       Q

keywords:      DB      STMSTT          ; first tokenvalue
               TOK     "BRKON"
               TOK     "BRKOFF"
               TOK     "CLREOL"        ; $80
               TOK     "CLRLIN"        ; $81
               TOK     "CLRSCR"        ; $82
               TOK     "CLR"
TK_REM         =       Q
               TOK     "CMD"
               TOK     "CONT"
               TOK     "CLOSE"
               TOK     "CURON"
               TOK     "CUROFF"
               TOK     "DATA"
               TOK     "DIM"
               TOK     "DIR"
               TOK     "END"
TK_FOR         =        Q
               TOK     "FOR"
               TOK     "GET#"
               TOK     "GET"
TK_GOTO        =       Q
               TOK     "GOTO"
TK_GOSUB       =       Q 
               TOK     "GOSUB"
               TOK     "GOXY"
               TOK     "IF"
               TOK     "INPUT#"
TK_LET         =       Q
               TOK     "LET"
               TOK     "LIST"
               TOK     "LOAD"
               TOK     "MON"
               TOK     "NEXT"
               TOK     "NEW"
               TOK     "ON"
               TOK     "OPEN"
               TOK     "POKEW"
               TOK     "POKE"
               TOK     "PRINT#"
TK_PRINT       =       Q 
               TOK     "PRINT"
               TOK     "READ"
               TOK     "RESTORE"
               TOK     "RETURN"
               TOK     "RUN"
               TOK     "SAVE"
               TOK     "SYS"
               TOK     "STOP"
               TOK     "TAB"
               TOK     "VARS"
               TOK     "VFY"
TK_ELSE        =       Q
               TOK     "ELSE"
STMEND:        =       Q


TK_STEP        =       Q
               TOK     "STEP"
TK_THEN        =       Q
               TOK     "THEN"
TK_TO          =       Q
               TOK     "TO"
STM2END        =       Q
               DB      0               ; table end2

;-------------------------------------------------------------------------------
; functions
;-------------------------------------------------------------------------------

FNSTT          =       Q
functions:     DB      FNSTT           ; first tokenvalue in this table
               TOK     "PEEKW"
               TOK     "PEEK"
               TOK     "ABS"
               TOK     "VARPTR"
               TOK     "SGN"
               TOK     "RND"
               TOK     "VAL"
               TOK     "LEN"
               TOK     "FRE"
               TOK     "POS"
               TOK     "ASC"
               TOK     "HI"
               TOK     "CHR$"
               TOK     "RIGHT$"
               TOK     "MID$"
               TOK     "LEFT$"
               TOK     "STR$"
               TOK     "TIME"
               TOK     "XPOS"
               TOK     "YPOS"
               TOK     "HEX$"
               TOK     "GTKEY"
FNEND          =       Q
               BYTE    0               ; end of table

;-------------------------------------------------------------------------------
; operators
;-------------------------------------------------------------------------------

OPRSTT         =       Q
operators:     DB      OPRSTT          ; starting tokenvalue
               
               TOK     "}}"            ; || OR logical ( '}' = SHFT-minus )
               TOK     "&&"            ; AND logical
               TOK     "}"             ; |  OR binary
               TOK     "^"             ; XOR               Up arrow on CBM
               TOK     "&"             ; AND binary
TK_EQL         =       Q
               TOK     "=="            ; EQL
               TOK     "!="            ; NEQ
               TOK     ">>"            ; SHR
               TOK     "<<"            ; SHL
               TOK     ".>="           ; GEQ unsigned
               TOK     ">="            ; GEQ signed
               TOK     ".<="           ; LEQ unsigned
               TOK     "<="            ; LEQ signed
               TOK     ".>"            ; GTR unsigned
               TOK     ">"             ; GTR signed
               TOK     ".<"            ; LSS unsigned
TK_LSS         =       Q
               TOK     "<"             ; LSS signed
TK_PLUS        =       Q
               TOK     "+"             ; PLUS
TK_MINUS       =       Q
               TOK     "-"             ; MINUS
               TOK     ".*"            ; MULT unsigned
               TOK     "*"             ; MULT signed
               TOK     "./"            ; DIV unsigned
               TOK     "/"             ; DIV signed
               TOK     ".%"            ; MOD unsigned
               TOK     "%"             ; MOD signed
TK_LNOT        =       Q
               TOK     "!!"            ; NOT logical
TK_NOT         =       Q
               TOK     "!"             ; NOT binary
               BYTE    0               ; end of table
OPREND         =       Q        
TK_UMINUS      =       OPREND          ; artificial, used in Expression only. tokenvalue can be reused.

               
TK_REM         =       $fa
TK_FVAR        =       $fb             ; fast var TK_FVAR offset (2 bytes)
TK_VAR         =       $fc             ; var TK_VAR n1 n2 type   (4 bytes)
TK_HEXNUM      =       $fd             ; TK_HEXNUM ll hh         (3 bytes)
TK_DECNUM      =       $fe             ; TK_DECNUM 11 hh         (3 bytes)
TK_PI          =       $ff             ; don't change. CBM code for pi = 255, AND no other token may be equal to $ff
;
; STMDSP : dispatch addresses. address-1 for rts-dispatch
;
STMDSP:        
               DW      BrkOn-1         ; BRKON
               DW      BrkOff-1        ; BRKOFF
               DW      ClrEol-1        ; CLREOL
               DW      ClrLin-1        ; CLRLIN
               DW      ClrScr-1        ; CLRSCR
               DW      Clr-1           ; CLR      $80
               DW      NotYet-1        ; CMD      $81
               DW      Cont-1          ; CONT     $82
               DW      Close-1         ; CLOSE    $83
               DW      CurOn-1         ; CURON
               DW      CurOff-1        ; CUROFF
               DW      NotYet-1        ; DATA     $84
               DW      Dim-1           ; DIM      $85
               DW      Dir-1           ; DIR
               DW      End-1           ; END      $86
               DW      For-1           ; FOR
               DW      NotYet-1        ; GET#     $87
               DW      NotYet-1        ; GET      $88
               DW      Goto-1          ; GOTO     $89
               DW      Gosub-1         ; GOSUB    $8a
               DW      GoXY-1
               DW      DoIf-1          ; IF       $8b
               DW      InputN-1        ; INPUT#   $8c
               DW      Let-1           ; LET      $8d
               DW      List-1          ; LIST     $8e
               DW      Load-1          ; LOAD     $8f
               DW      Mon-1           ; MON      $90
               DW      Next-1          ; NEXT     $91
               DW      New-1           ; NEW      $92
               DW      On-1            ; ON       $93
               DW      Open-1          ; OPEN     $94
               DW      PokeW-1         ; POKEW    $95
               DW      Poke-1          ; POKE     $96
               DW      PrintN-1        ; PRINT#   $97
               DW      Print-1         ; PRINT    $98 
               DW      NotYet-1        ; READ     $9a
               DW      NotYet-1        ; RESTORE  $9b
               DW      Return-1        ; RETURN   $9c
               DW      Run-1           ; RUN      $9d
               DW      Save-1          ; SAVE     $9e
               DW      Sys-1           ; SYS      $9f
               DW      Stop-1          ; STOP     $a0
               DW      Tab-1           ; TAB
               DW      Vars-1          ; VARS     $a1
               DW      NotYet-1        ; VFY      $a2         INC,DEC !
               DW      DoElse-1        ; ELSE     $a3
               DW      NotYet-1        ; THEN     $a4
               
;-------------------------------------------------------------------------------
; Function dispatch table
;-------------------------------------------------------------------------------

FNDSP:         DW      DoPeekW-1       ; "PEEKW"
               DW      DoPeek-1        ; "PEEK" 
               DW      DoAbs-1         ; "ABS"  
               DW      DoVarptr-1      ; "VARPTR"
               DW      DoSgn-1         ; "SGN"  
               DW      NotYet-1        ; "RND"  
               DW      DoVal-1         ; "VAL"  
               DW      DoLenStr-1      ; "LEN"  
               DW      DoFre-1         ; "FRE"  
               DW      NotYet-1        ; "POS"  
               DW      DoAsc-1         ; "ASC"  
               DW      DoHi-1          ; "HI"   
               DW      DoChrStr-1      ; "CHR$" 
               DW      DoRightStr-1    ; "RIGHT$
               DW      DoMidStr-1      ; "MID$" 
               DW      DoLeftStr-1     ; "LEFT$"
               DW      DoStrStr-1      ; "STR$" 
               DW      DoTime-1        ; "TIME"
               DW      DoXpos-1        ; "XPOS"
               DW      DoYpos-1        ; "YPOS"
               DW      DoHexStr-1      ; "HEX$"
               DW      DoGetKey-1      ; "GTKEY"
               
;-------------------------------------------------------------------------------
; NotYet (implemented)
;-------------------------------------------------------------------------------

nystring:      DB      "NOT YET IMPLEMENTED",$0d,$00

NotYet:        lda     #<nystring
               ldy     #>nystring
               jsr     PrtStrAY
               jmp     NewStt
               

