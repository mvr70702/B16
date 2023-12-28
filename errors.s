;==============================================================================================
; errors.s
;==============================================================================================
;
errormsg:      DB      "ERROR ",0
inmsg:         DB      "IN ",0
;
E              =       0
;
               MACRO   EMSG c1 
               DCI     c1
E              =       ($-ERRSTT)
               ENDM
;
; !! no more than 256 chars errmessages !!
;
ERRSTT         =       $
E_SYN          =       E
               EMSG    "SYNTAX"
E_MEM          =       E
               EMSG    "MEM"
E_RWG          =       E
               EMSG    "RET/GOSUB"
E_OVF          =       E
               EMSG    "OVF"
E_FRM          =       E
               EMSG    "EXPR"
E_DV0          =       E
               EMSG    "ZDIV"
E_EXP          =       E
               EMSG    " EXPECTED"
E_TYP          =       E
               EMSG    "TYPE"
E_LL           =       E
               EMSG    "LINE LONG"
E_LINE         =       E
               EMSG    "LINENR"
E_BREAK        =       E
               EMSG    "BREAK"
E_REDIM        =       E
               EMSG    "REDIM"
E_IND          =       E
               EMSG    "INDEX"
E_CCONT        =       E
               EMSG    "CAN'T CONTINUE"
E_ILL          =       E
               EMSG    "ILLEGAL QTY"
E_NWF          =       E
               EMSG    "NEXT WO FOR" 
E_DEV          =       E
               EMSG    "NO DEV"
