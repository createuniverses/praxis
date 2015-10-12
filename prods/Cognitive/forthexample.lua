os.execute("cd p4th")
os.execute("dir")


print2(setCurrentDir(".."))

print2(forth("loadsys"))
loadsys Load system.fth
AUTO.INIT redefined.
Include loadp4th.fth
Include forget.fth
    include added 740 bytes,293632 left.
Include numberio.fth
. redefined.
    include added 1280 bytes,292352 left.
Include misc1.fth
    include added 1280 bytes,291072 left.
Include case.fth
    include added 408 bytes,290664 left.
Include strings.fth
    include added 756 bytes,289908 left.
Include private.fth
    include added 340 bytes,289568 left.
Include ansilocs.fth
: redefined.
; redefined.
EXIT redefined.
DOES> redefined.
    include added 2516 bytes,287052 left.
Include locals.fth
    include added 700 bytes,286352 left.
Include math.fth
/MOD redefined.
MOD redefined.
    include added 596 bytes,285756 left.
Include misc2.fth
    include added 2076 bytes,283680 left.
Include condcomp.fth
    include added 296 bytes,283384 left.
Include member.fth
    include added 796 bytes,282588 left.
Include c_struct.fth
    include added 1824 bytes,280764 left.
Include smart_if.fth
IF redefined.
DO redefined.
?DO redefined.
BEGIN redefined.
THEN redefined.
REPEAT redefined.
UNTIL redefined.
LOOP redefined.
+LOOP redefined.
    include added 344 bytes,280420 left.
Include filefind.fth
    include added 1060 bytes,279360 left.
Include see.fth
    include added 1956 bytes,277404 left.
Include wordslik.fth
    include added 148 bytes,277256 left.
Include trace.fth
    include added 6920 bytes,270336 left.
Include termio.fth
CLS redefined.
    include added 340 bytes,269996 left.
Include history.fth
AUTO.INIT redefined.
AUTO.TERM redefined.
    include added 5844 bytes,264152 left.
Code Segment
   CODEBASE           = 2946C30 
   HERE               = 294F838 
   CODELIMIT          = 2990010 
   Compiled Code Size = 35848 
   CODE-SIZE          = 300000 
   Code Room UNUSED   = 264152 
Name Segment
   NAMEBASE           = 2929730 
   HEADERS-PTR @      = 292D258 
   NAMELIMIT          = 2946BF0 
   CONTEXT @          = 292D250 
   LATEST             = 292D250  = ;;;;
   Compiled Name size = 15144 
   HEADERS-SIZE       = 120000 
   Name Room Left     = 104856 
    include added 30220 bytes,264152 left.
;;;; redefined.
Dictionary compiled, save in "pforth.dic".
   ok





print2(forth("words"))
words ;;;;      ;;;;    ;;;;    AUTO.TERM       
AUTO.INIT       HISTORY.OFF     HISTORY.ON      
HISTORY.RESET   XX      HISTORY HISTORY#        
TEST.HISTORY    KH.ACCEPT       KH.GETLINE      
EOL?    KH.INSCHAR      KH.SMART.KEY    KH.SPECIAL.KEY  
KH.HANDLE.ANSI.KEY      KH.HANDLE.WINDOWS.KEY   
KH.DELETE       KH.BACKSPACE    KH.REFRESH      
KH.GO.LEFT      KH.GO.RIGHT     KH.CLEAR.LINE   
KH.GET.NEWER    KH.GET.OLDER    KH.FAR.LEFT     
KH.FAR.RIGHT    KH.GET.MATCH    KH.REPLACE.LINE 
KH.RETURN       KH-BUFFER       KH.FIND.LINE    
KH.OLDEST.LINE  KH.FORWARD.LINE KH.BACKUP.LINE  
KH.ADD.LINE     KH.ENDCOUNT.ADDR        KH.ADDR--       
KH.ADDR++       KH.CURRENT.NUM  KH.NUM.ADDR     
KH.COMPARE      KH.CURRENT.LINE KH.CURRENT.ADDR 
KH.REWIND       KH.NEWEST.LINE  KH.MAKE.ROOM    
KH-INSIDE       KH-ADDRESS      KH-CURSOR       
KH-MATCH-SPAN   KH-SPAN KH-COUNTER      KH-MAX  
KH-LOOK LINENUM!        LINENUM@        KH-END  
KH_LINE_EXTRA_SIZE      KH-HISTORY      KH_HISTORY_SIZE 
TASK-HISTORY.FTH        ::::history.fth ;;;;    
BACKSPACE       BELL    TIO.ERASE.EOL   TIO.FORWARDS    
TIO.BACKWARDS   CLS     ESC[    ASCII_CTRL_X    
ASCII_CTRL_E    ASCII_CTRL_A    ASCII_ESCAPE    
ASCII_DELETE    ASCII_BACKSPACE TASK-TERMIO.FTH 
::::termio.fth  ;;;;    TRACE.HELP      G       
GD      TRACE.USER      SM      SD      S       
TRACE   TRACE.NEXT      TRACE.DO.NEXT   TRACE.DO.PRIMITIVE      
TRACE.SHOW.NEXT TRACE.SHOW.STACK        TRACE.SHOW.IP   
TRACE.CHECK.IP  TRACE.(+LOOP)   TRACE.(LOOP)    
TRACE.(?DO)     TRACE.(LOCAL+!) TRACE.(8_LOCAL!)        
TRACE.(7_LOCAL!)        TRACE.(6_LOCAL!)        
TRACE.(5_LOCAL!)        TRACE.(4_LOCAL!)        
TRACE.(3_LOCAL!)        TRACE.(2_LOCAL!)        
TRACE.(1_LOCAL!)        TRACE.(LOCAL!)  TRACE.(8_LOCAL@)        
TRACE.(7_LOCAL@)        TRACE.(6_LOCAL@)        
TRACE.(5_LOCAL@)        TRACE.(4_LOCAL@)        
TRACE.(3_LOCAL@)        TRACE.(2_LOCAL@)        
TRACE.(1_LOCAL@)        TRACE.(LOCAL@)  TRACE.(LOCAL.EXIT)      
TRACE.(LOCAL.ENTRY)     TRACE-LOCALS-PTR        
TRACE.RESTORE.STATE2    TRACE.RESTORE.STATE1    
TRACE.RESTORE.STATE     TRACE.RESTORE++ TRACE.SAVE.STATE2       
TRACE.SAVE.STATE1       TRACE.SAVE.STATE        
TRACE.SAVE++    TRACE-STATE-PTR TRACE-STATE-2   
TRACE-STATE-1   TRACE_STATE_SIZE        TRACE.RCHECK    
TRACE.RDROP     TRACE.0RP       TRACE.RPICK     
TRACE.R@        TRACE.R>        TRACE.>R        
TRACE-RSP       TRACE-RETURN-STACK      TRACE_RETURN_SIZE       
TRACE_LEVEL_MAX TRACE_LEVEL     TRACE_IP        
IS.PRIMITIVE?   SPACE.TO.COLUMN TASK-TRACE.FTH  
::::trace.fth   ;;;;    WORDS.LIKE      PARTIAL.MATCH.NAME      
TASK-WORDSLIK.FTH       ::::wordslik.fth        
;;;;    SEE     (SEE)   SEE.XT  SEE.0BRANCH     
SEE.BRANCH      SEE.SHOW.TARGET SEE.SHOW.STRING 
SEE.SHOW.ALIT   SEE.SHOW.LIT    SEE.GET.TARGET  
SEE.GET.INLINE  SEE.ADVANCE     SEE.OUT+        
SEE.CR? SEE.NEWLINE     SEE.CR  SEE.INDENT.BY   
SEE_OUT SEE_ADDR        SEE_LEVEL       CODE_CELL       
CODE@   BYTE_CODE       .XT     TASK-SEE.FTH    
::::see.fth     ;;;;    FILE?   FINDNFA.FROM    
F?.SEARCH.NFA   BEW!    BEW@    BE!     BE@     
TASK-FILEFIND.FTH       ::::filefind.fth        
;;;;    +LOOP   LOOP    UNTIL   REPEAT  THEN    
BEGIN   ?DO     DO      IF      }SMIF   SMIF{   
SMIF-DEPTH      SMIF-XT TASK-SMART_IF.FTH       
::::smart_if.fth        ;;;;    S@      (S@)    
COMPILE+@BYTES  (S+W@)  (S+C@)  (S+REL@)        
(S+@)   (S+UW@) (S+UC@) @BYTES  S!      (S!)    
!BYTES  COMPILE+!BYTES  (S+REL!)        (S+!)   
(S+W!)  (S+C!)  ..      ;STRUCT :STRUCT <:STRUCT>       
TASK-C_STRUCT   ::::c_struct.fth        ;;;;    
STRUCT  ULONG   RPTR    APTR    USHORT  UBYTE   
LONG    SHORT   BYTE    BYTES   SIZEOF()        
OB.STATS?       OB.STATS        OB.FINDIT       
OB.MEMBER       }UNION  }UNION{ UNION{  OB.MAKE.MEMBER  
OB.SIZE,        OB.SIZE@        OB.OFFSET,      
OB.OFFSET@      OB_OFFSET_SIZE  OB_DEF_STRUCT   
OB_DEF_CLASS    OB-CURRENT-CLASS        OB-STATE        
FIND.BODY       TASK-MEMBER.FTH ::::member.fth  
;;;;    EXISTS? [THEN]  [IF]    [ELSE]  TASK-CONDCOMP.FTH       
::::condcomp.fth        ;;;;    SEARCH  MAP     
UNUSED  TIB     EXPECT  SPAN    QUERY   BLANK   
ERASE   MOVE    WITHIN  W->S    B->S    .HEX    
.DEC    .BIN    ARRAY   WARRAY  BARRAY  -2SORT  
2SORT   WCHOOSE CHOOSE  RANDOM  RAND-SEED       
SHIFT   MSEC    (MSEC)  MSEC-DELAY      IF-DEBUG        
'C      ?LITERAL        'N      TASK-MISC2.FTH  
::::misc2.fth   ;;;;    */      */MOD   MOD     
/MOD    SM/REM  FM/MOD  TASK-MATH.FTH   ::::math.fth    
;;;;    TLV2    TLV1    {       LOC-DONE        
LOC-COMMENT-MODE        LOC-TEMP-MODE   TASK-LOCALS.FTH 
::::locals.fth  ;;;;    DOES>   EXIT    ;       
:       +->     ->      TO      VALUE   (LOCAL) 
LV.TERM LV.SETUP        LV.FINISH       LV.CLEANUP      
LV.COMPILE.LOCAL        LV.COMPILE.STORE        
LV.COMPILE.FETCH        LV.MATCH        LV-#NAMES       
LV-NAMES        LV_MAX_CHARS    LV_MAX_VARS     
TASK-ANSILOCS.FTH       ::::ansilocs.fth        
;;;;    PRIVATIZE       }PRIVATE        PRIVATE{        
FLAG_SMUDGE     PRIVATE-STOP    PRIVATE-START   
TASK-PRIVATE.FTH        ::::private.fth ;;;;    
TEXTROM $ROM    ($ROM)  $APPEND.CHAR    INDEX   
$MATCH? TEXT=?  TEXT=   $=      $ARRAY  -TRAILING       
TASK-STRINGS.FTH        ::::strings.fth ;;;;    
ENDCASE ENDOF   RANGEOF (RANGEOF?)      OF      
?OF     CASE    OF-DEPTH        CASE-DEPTH      
TASK-CASE       ::::case.fth    ;;;;    \S      
EVALUATE        TOLOWER @EXECUTE        >NAME   
CLOSEST-XT      CLOSEST-NFA     VLIST   WORDS   
TAB     TAB-WIDTH       .HX     $       PAGE    
CLS     CR?     #COLS   ?PAUSE  ABORT"  (ABORT")        
WARNING"        (WARNING")      <<      >>      
TASK-MISC1.FTH  ::::misc1.fth   ;;;;    .R      
.       (.)     U.R     U.      (U.)    D.R     
D.      (D.)    UD.R    UD.     (UD.)   #S      
#       SIGN    #>      <#      HOLD    HLD     
(NUMBER?)       ((NUMBER?))     NUM_TYPE_DOUBLE 
NUM_TYPE_SINGLE NUM_TYPE_BAD    CONVERT >NUMBER 
DIGIT   TASK-NUMBERIO.FTH       ::::numberio.fth        
;;;;    MARKER  ANEW    FORGET  [FORGET]        
IF.FORGOTTEN    LAST-FORGET     (FORGET)        
VERIFY.FORGET   FORGET.NFA      FREEZE  RFENCE  
::::forget.fth  ::::loadp4th.fth        TURNKEY 
SAVE-FORTH      AUTO.INIT       CODE-SIZE       
HEADERS-SIZE    INCLUDE?        RI      INCLUDE 
INCLUDE-SAVE-NAME       $INCLUDE        INCLUDE.MARK.END        
INCLUDE.MARK.START      TRACE-INCLUDE   AUTO.TERM       
AUTO.INIT       POSTPONE        $APPEND SLITERAL        
""      P"      "       S"      C"      .'      
."      .(      ,"      ",      (.")    (S")    
(C")    EVEN    'WORD   $TYPE   [CHAR]  CHAR    
ASCII   LWORD   PARSE   PARSE-WORD      PLACE   
/STRING WHAT'S  (WHAT'S)        IS      (IS)    
>IS     CHECK.DEFER     >NEWLINE        0SP     
SPACES  SPACE   RECURSE UNLOOP  +LOOP   LOOP    
LOOP-BACK       LOOP-FORWARD    LEAVE   ?DO     
DO      ?DO_FLAG        LEAVE_FLAG      DO_FLAG 
0USP    US@     US>     >US     USTACK  0STACKP 
STACKP  STACK.PICK      STACK@  STACK>  >STACK  
:STACK  A,      A@      A!      IF.REL->USE     
IF.USE->REL     BL      TRUE    FALSE   D2*     
2/      2*      MOD     /MOD    D>S     S>D     
DABS    ABS     2@      2!      -2      -1      
CONSTANT        2VARIABLE       VARIABLE        
DOES>   (DOES>) [']     REPEAT  WHILE   ELSE    
AHEAD   UNTIL   AGAIN   BEGIN   THEN    IF      
?PAIRS  ?COMP   <RESOLVE        <MARK   >RESOLVE        
>MARK   ?CONDITION      CONDITIONAL_KEY ABORT   
ERR_DEFER       ERR_PAIRS       ERR_EXECUTING   
ERR_ABORTQ      ERR_ABORT       :NONAME COMPILE 
(COMPILE)       [COMPILE]       COMPILE,        
X!      X@      REL->USE        USE->REL        
BODY>   >BODY   N>LINK  CODE>   >CODE   NAMEBASE+       
CODELIMIT       NAMELIMIT       CODEBASE        
NAMEBASE        N>NEXTLINK      ,       W,      
C,      ALLOT   ALIGN   ALIGNED EVEN-UP ]       
[       BETWEEN $MOVE   PAD     BINARY  HEX     
OCTAL   DECIMAL ID.     DNEGATE NEGATE  NOT     
INVERT  >=      <=      TUCK    NIP     2DROP   
3DUP    -ROT    CHARS   CHAR+   CELL*   CELL-   
CELL+   OFF     ON      COUNT   \       (       
IMMEDIATE       FLAG_IMMEDIATE  LATEST  FIRST_COLON     
CTEST1  CTEST0  0BRANCH XOR     W!      W@      
WORD    >IN     STATE   OUT     TRACE-STACK     
TRACE-LEVEL     TRACE-FLAGS     RETURN-CODE     
#TIB    HEADERS-LIMIT   HEADERS-BASE    HEADERS-PTR     
ECHO    DP      CONTEXT CODE-LIMIT      CODE-BASE       
BASE    TYPE    >R      THROW   *       '       
TEST2   TEST1   SWAP    POP-SOURCE-ID   PUSH-SOURCE-ID  
SOURCE-ID       SET-SOURCE      SOURCE  SKIP    
SCAN    (SAVE-FORTH)    !       SP!     SP@     
;       RP!     RP@     R>      R@      RDROP   
RSHIFT  ROT     ROLL    RESIZE  REFILL  KEY?    
?TERMINAL       ?DUP    (?DO)   QUIT    (QUIT)  
+!      (+LOOP) +       PICK    OVER    OR      
NUMBER? NOOP    PREVNAME        NAME>   -       
MIN     MAX     LSHIFT  (LOOP)  (LOCAL+!)       
(8_LOCAL!)      (7_LOCAL!)      (6_LOCAL!)      
(5_LOCAL!)      (4_LOCAL!)      (3_LOCAL!)      
(2_LOCAL!)      (1_LOCAL!)      (LOCAL!)        
(8_LOCAL@)      (7_LOCAL@)      (6_LOCAL@)      
(5_LOCAL@)      (4_LOCAL@)      (3_LOCAL@)      
(2_LOCAL@)      (1_LOCAL@)      (LOCAL@)        
(LOCAL.EXIT)    (LOCAL.ENTRY)   LOCAL-COMPILER  
LOADSYS (LITERAL)       LITERAL (LEAVE) KEY     
INCLUDE-FILE    J       INTERPRET       I       
(SNUMBER?)      HERE    FREE    FLUSHEMIT       
FINDNFA BIN     W/O     R/W     R/O     REPOSITION-FILE 
FILE-POSITION   WRITE-FILE      FILE-SIZE       
READ-FILE       CLOSE-FILE      OPEN-FILE       
DELETE-FILE     CREATE-FILE     FIND    FILL    
@       EXECUTE ?ERROR  (?ERROR)        EOL     
EMIT    (EMIT)  DUP     DUMP    DROP    (DO)    
.S      .       /       DEPTH   C!      DEFER   
UM*     M*      MU/MOD  UM/MOD  D-      D+      
(CREATE)        CREATE  CR      0<      0>      
0<>     0=      U<      <       U>      >       
<>      =       COMPARE (:)     :       CMOVE>  
CMOVE   C@      CELLS   CELL    CATCH   BYE     
BODY_OFFSET     BRANCH  BAIL    AND     ARSHIFT 
ALLOCATE        (ALITERAL)      ALITERAL        
ACCEPT  (ACCEPT)        2SWAP   2OVER   2+      
2-      (2LITERAL)      2LITERAL        2DUP    
2>R     2R>     2R@     1+      1-      EXIT    

841  words
   ok

setBufferName("forthexample.lua")

