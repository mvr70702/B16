;==============================================================================================
; RAM.s
; 
;==============================================================================================


;-------------------------------------------------------------------------------
; ZEROPAGE memory
;-------------------------------------------------------------------------------

               DATA    $0000
D6510:         DSB     1
R6510:         DSB     1
csave:         DSB     1               ; last character read .used in 'SynChk' and others
skipflg:       DSB     1               ; GetChr : skip $20 test
number:        DSB     2               ; decimal/hex value to be printed, result from ReadDec/Hex 
prtmp:         DSB     1               ; used by dec. print                                       
digit:         DSB     1               ; used by dec. print, ReadDec/Hex                          
strptr:        DSW     1               ; used by PrtStrAY and crunch/list (as resptr)                   
resptr:        =       strptr
wptr:          DSW     1               ; work, used by insline/delline/list etc. 
sptr1          =       wptr
sptr2          =       strptr
from:          DSW     1               ; moveup/movedn/SAVE
to:            DSW     1               ; moveup/movedn/LOAD  
restok:        DSB     1               ; precomp/list
membot:        DSW     1               ; bottom of basic memory avail ($0a00), set by cold start
txttab:        DSW     1               ; start of basic                            
txtend:        DSW     1               ; end of basic                              
vartab:        DSW     1               ; end of vars (starts at memtop, goes down)
memtop:        DSW     1               ; top of basic memory
linptr:        DSW     1               ; used by FindLine etc.
linnum:        DSW     1               ; used by FindLine etc.
sysvec         =       linnum          ; indirect SYS jmp2
curlin:        DSW     1               ; linenumber currently being interpreted
pcurlin:       DSW     1               ; address of line ..     ..      ..
peeker:        DSW     1               ; pointers for PEEK/POKE2
poker:         DSW     1
varptr:        DSW     1               ; result from ptrget
target:        DSW     1               ; lvalue in assignment
oldtxt:        DSW     1               ; txtptr save for END, STOP, BREAK. Hi byte == 0 -> invalid2
fnptr:         DSW     1               ; LOAD/SAVE : pointer to filename
scp1:          DSW     1               ; sfuncs : compare pointer
scp2:          DSW     1               ;    ..      ..     ..
cmpres:        DSB     1
GetChr         DSB     nbgc            ; room for GetChr, it's copied to this loc
txtptr         =       GetChr+1                  ; rfr to main.s. GetChr and txtptr are completely relocatable.

               CHECKPC $07f
               DEND
               
;-------------------------------------------------------------------------------
; PAGE 2 memory        misc
; 
;-------------------------------------------------------------------------------
               
               DATA    $0200
fastvars:      DSW     26              ; vars 'A'..'Z'. Must all be on same page
               CHECKPC $0258           ; at $0258, kernal memory begins
unused2        =       $02a7           ; $02a7..$02ff : user memory
               DEND
               
;-------------------------------------------------------------------------------
; PAGE 3 memory        Basic jmps etc
;-------------------------------------------------------------------------------
               
               DATA    $0300           ; B16 jmptable etc, keep same as BASICv2.0
ierror:        DSW     1               ; error
imain:         DSW     1               ; main
icrnch:        DSW     1               ; precompile
iqplop:        DSW     1               ; list
igone:         DSW     1               ; newstt
ieval:         DSW     1               ; expression
sysareg:       DSB     1
sysxreg:       DSB     1
sysyreg:       DSB     1
sysflags:      DSB     1
               CHECKPC $033c
unused3        =       $033c           ; $033c..$03fb : tape buffer
               DEND
               
;-------------------------------------------------------------------------------
; PAGE 8/9 memory      evaluation stacks, precompile/input buffers etc
;-------------------------------------------------------------------------------
               
               DATA    $0800
estacklo:      DSB     32              ; expression split stack. must all be on page $0800, starting here
estackhi:      DSB     32              ; they will also be used in the B16 compiler
estacktype:    DSB     32              ; T_STR or T_INT, byte variables are for storage only
crunchbuf:     DSB     128             ; INBUF gets precompiled here, source line in direct mode                         
INBUF          =       crunchbuf+48    ; 80 char's inputbuf, starting 48 bytes from the start of crunchbuf
strlen         DSB     1               ; length of all strings

prtbuf         DSB     7               ; printbuffer for prtdec/str$  
printflg       DSB     1
stksav         DSB     1               ; sp on entry basic
size           DSW     1               ; moveup/dn
tmp            DSB     1               ; delline
resxsav:       DSB     1               ; precomp
resysav:       DSB     1               ; precomp
typflag:       DSB     1               ; precomp
incopr:        DSB     1               ; expression : incoming operator
incprio:       DSB     1               ; expression : incoming operator priority
xsavexpr:      DSB     1               ; expression : x save loc
oprstack:      DSB     16              ; expression : operatorstack
primexp:       DSB     1               ; expression : primary expected
mulres:        DSB     4               ; expression : 32 bit result of multiplication
mulcnt:        DSB     1               ; expression : temp for mult.
mnum1:         DSW     1               ; calc.s : holds lhs for Umul
mnum2:         DSW     1               ; calc.s : holds rhs for Umul
shftcnt:       DSB     1               ; expression : shift count
remainder:     DSW     1               ; expression : remainder for DIV
divcnt:        DSB     1               ; expression : shift count
xfnsav:        DSB     1               ; expression : x save for dispatch function
ysavpeek:      DSB     1               ; expression
anything       DSB     1               ; expression : something was processed (to check if a required field is actually there)
constant       DSB     1               ; expression : this is zero if no vars/functions were processed
;
; FILE I/O
;
filename:      DSB     32              ; LOAD,SAVE,OPEN
lfn:           DSB     1               ; LFN for fileIO..
devnum:        DSB     1               ; .. device nr,
namelen:       DSB     1               ; .. length of filename
chnl:          DSB     1               ; .. SA (channel)
ldb16:         DSB     1               ; LOAD : B16 program load = 0, other  = 1

varn1:         DSB     1               ; precomp,vars : variable name
varn2:         DSB     1               ; keep varn1,varn2,vartyp sequentially
vartyp:        DSB     1               ; ; ..   ..
varsize:       DSB     2               ; vars : temp holding size for EnterVar
ndim:          DSB     2               ; vars : set this up forEnterVar
targettyp      DSB     1               ; LET : type of receiving var
tempstr1:      DSB     MAXSTR          ; expression : temp strings (string.s, NTMPSTR=4)
tempstr2:      DSB     MAXSTR
tempstr3:      DSB     MAXSTR
tempstr4:      DSB     MAXSTR
tempstrix      DSB     1               ; temp string index
mmax:          DSB     1               ; sfuncs : max chars to move in MoveStr
mcnt:          DSB     1               ; sfuncs : nr. chars moved in MoveStr
length:        DSB     1               ; sfuncs : temp for RIGHT$
pcurlinsav:    DSB     2               ; save/restore context for ^break/STOP/CONT
curlinsav:     DSB     2               ;   ..         ..            ..      ..
tosubflg:      DSB     1               ; ON : goto/gosub
chkbrkflg:     DSB     1               ; if bit 7 set, B16 checks for RUN/STOP at NewStt
brkflg:        DSB     1               ; main : flag if RUN/STOP breaks program

               CHECKPC $09ff
               DEND
