\ ============================================================
\ Object-oriented extensions from Yerk/Mops
\ Version 1.0, 4 Feb 1997
\ Andrew McKewan
\ mckewan@austin.finnigan.com

cr .( Loading Class )
DECIMAL

0 [if]
selectors on

: assert-isClass  ( ^class -- ^class )
    dup ?isClass not abort" is not a class" ;

:class object
\    :m addr:    ^base@         ;m  \ get object address

;class


: @+  ( a -- a' n ) dup cell+ swap @ ;

: count-link  ( addr -- n )
    0 swap begin dup while swap 1+ swap @ repeat drop ;

: .list  ( link -- )
    begin dup while dup .name @ repeat drop ;

: .classname ( ^class -- )
    assert-isClass
    body> >name .name ;

: .class ( ^class -- )
    assert-isClass
    cr ." Class:   "  dup .classname
    cr ." Methods: "  8 0 do  @+ .list  loop
    cr ." Ivars:   "  @+ .list
    cr ." Length:  "  @+ .
    cr ." Width:   "  @+ .
    cr ." Flags:   "  @+ .
    cr ." Super:   "  begin @+ dup while body> >name .name repeat 2drop ;

: showclass  ' >body .class ;
\S

[then]

\ =====================================================================
\ Misc words. You may already have some of these.

: HAVE  ( -- f )  SYSWORD FIND NIP ;

: C+!  ( char addr -- )   DUP C@  ROT +  SWAP C! ;

: RESERVE  ( n -- )  HERE OVER ERASE ALLOT ;

\ Build link to list head at addr
: LINK  ( addr -- )   HERE  OVER @ ,  SWAP ! ;

: NOOP ;

: BOUNDS  ( a n -- limit index )  OVER + SWAP ;

: S=  COMPARE 0= ;

: (,str)  ( addr len --)
    TUCK HERE PLACE
    1+ ALIGNED ALLOT  ;


: ,str  ( c -- )
    \ c is delimiter.  Adds the following text until delimiter
    \ to the dictionary as a counted string.
    
    WORD COUNT  (,str)  ;



\ =====================================================================
\ Class Structure.

0       VALUE   ^Class              \ pointer to class being defined
0       VALUE   newObject           \ object being created
0       VALUE   (ClassInit:)        \ selector for ClassInit: message
FALSE   VALUE   method?             \ true if we're compiling a method
0       VALUE   ^methFlags          \ addr of flags cell of this method
FALSE   VALUE   compInline?         \ true when we're compiling an
                                    \  inline method call
0       VALUE   ^obj_for_bind       \ if nonzero, addr of obj we're
                                    \  early-binding to - used for inline
                                    \  methods.

\ The following offsets refer to the ^Class, or Pfa of the class.
\ The first 8 cells are the hashed method dictionary.

: IFA_offset    8 CELLS ;

: IFA    8 CELLS + ;    \ ivar dict Latest field
: DFA    9 CELLS + ;    \ datalen of named ivars
: XFA   10 CELLS + ;    \ width of indexed area, <= 0 if not indexed
: SFA   11 CELLS + ;    \ superclass ptr field
: TAG   12 CELLS + ;    \ class tag field

13 CELLS CONSTANT classSize     \ size of class pfa

12345678 constant classTag     \ contents of tag field for valid class

: ?isClass  ( pfa -- f )  \ is this a valid class?
    TAG @ classTag = ;

: ?isObj  ( pfa -- )  \ is this a valid object?
    @ DUP IF ?isClass THEN ;

: classAllot  ( n -- )  \ Allot space in the current class
    ^Class DFA +! ;

: classAlign  ( -- )  \ Align class data size (optional)
    ^Class DFA @  ALIGNED  ^Class DFA ! ;

: @width  ( ^class -- elWidth )  \ return the indexed element width for a class
    XFA @  0 MAX ;

\ Error if not compiling a new class definition
: ?Class   ^Class   0= ABORT" Not inside a class definition" ;


\ =====================================================================

\ Public and private ivars and methods, as in Mops

0   VALUE   pub/priv

: private       -1 -> pub/priv  ;   \ following methods and ivars will be private
: public         1 -> pub/priv  ;   \ following methods and ivars will be public

: end_private   0 -> pub/priv  ;    \ restore default (methods public, ivars private)
: end_public    0 -> pub/priv  ;    \ ditto



\ =====================================================================
\ Objects have a pointer to their class stored in the first cell of
\ their pfa. When they execute, they return the address of the cell
\ following the class pointer, which is location of the first named
\ instance variable.
\
\ Object structure: | ^class | named ivars |
\
\ If an object is embedded...
\
\ | class-offset | -1 | named ivars |
\ | -class-offset | named ivars
\

\ If an object is indexed, an indexed header appears after the data area.
\ This header consists of a cell containing the number of elements.
\ The indexed data follows this header.
\
\ Indexed object:   | ^class | named ivars | #elems | indexed ivars |
\ =====================================================================

: (Obj)  ( -- )  CREATE  DOES> CELL+ ;


: >obj  ( xt -- ^obj )  \ get the object address from the execution token
    >BODY CELL+ ;

: >class  ( ^obj -- ^class )  \ get the class of an object
    CELL - @
    DUP -1 <>  IF  EXIT  THEN   \ if not an embedded obj, we have ^class
    DROP CELL -             \ previous cell is offset to ^class
    DUP @ +                 \ get addr of ^class
    @
;


\ =====================================================================
\ Methods are stored in an 8-way linked-list from the MFA field.
\ Each method is identified by a 32-bit selector which is the parameter
\ field of the selector. Offsets are in cells.
\
\ Method Structure:
\   0   link to next method
\   1   selector
\   2   method execution token (called mcfa below)
\   3   flags
\
\           flag masks: 1   - 1 public, 0 private
\                       4   - 1 inline, 0 normal
\
\ =====================================================================

\ MFA finds the top of the method link for a given selector.
\ The "2/ 2/" below is to get a better distribution if the selectors
\ are aligned values.

: MFA_offset  ( SelID ^Class -- SelID ^Class MFA_offset )
    OVER 2/ 2/ 7 AND CELLS  ;

: MFA  ( SelID ^Class -- SelID MFA )  MFA_offset  + ;

\ ((findM))  is the lowest-level routine to search through the linked-list of
\  methods/ivars for the given selector.

0   VALUE   methSearch?
0   VALUE   selfRef?
0   VALUE   sch_offset
0   VALUE   sch_selID
0   VALUE   sch_in_class_xt
0   VALUE   supOffs

: search_superclasses  ( ^n-way -- offs ^mcfa true | false )
    0  TO supOffs           \ initial base offset
    BEGIN
        CELL+               \ look at next class pointer in Nway
        DUP @ DUP
        IF
            DUP sch_in_class_xt EXECUTE
                                        \ search in that class
            IF                          \ found!
                ROT DROP ROT DROP       \ drop ^class and ^n-way
                SWAP supOffs + SWAP
                TRUE  EXIT
            THEN
            DFA @ ALIGNED  2 CELLS +    \ not found - update offset by
            supOffs +  TO supOffs       \  ivar len of that superclass
                                        \  plus alignment and the 2 cells
                                        \  of info we put between the 
                                        \  embedded objects
        ELSE
            NIP  EXIT       \ search failed - return the zero
        THEN
    AGAIN  ;


: sch_in_class ( ^class -- offs ^mcfa true | false )

    sch_offset +        \ addr of initial link for linked search
    BEGIN
        DUP @           \ follow link - addr of meth/ivar dic entry
        DUP -1 =
        IF  DROP  ( ^n-way ) search_superclasses  EXIT
        THEN

        NIP DUP
    WHILE
        DUP CELL+ @  sch_selID =
        IF                  \ may be a match - but if a method,
                            \  we have to check if it's private
            methSearch?
            IF  DUP 3 CELLS + @     \ get the flags
                1 AND               \ it's a match if it's public
                selfRef? OR         \ or a reference to SELF or SUPER
            ELSE
                TRUE                \ ivar search - it's a match
            THEN
        ( match? )
            IF                      \ found!
                2 CELLS +           \ ^mfca
                0 SWAP              \ push zero offset under ^mcfa
                TRUE  EXIT
            THEN
        THEN

    REPEAT  ;

' sch_in_class  TO sch_in_class_xt


: ((findM))  ( selID ^class search_offset methSearch? -- offs ^mcfa true | false )
    TO methSearch?
    TO sch_offset
    SWAP TO sch_selID
  ( ^class )  sch_in_class  ;


\ clFindM  returns the addr of a method's mcfa field, given a selID
\  and a class address.

: clFindM   ( selID ^class -- offs ^mcfa )
    MFA_offset TRUE ((findM)) IF  EXIT  THEN
    TRUE ABORT" Message not understood by class" ;


\ objFindM finds a method in an object's class, given a selID and the
\  addr of the object.

: objFindM   ( cSelID ^obj -- offs ^mcfa )

    DUP 0= ABORT" null object pointer" ( debugging )
    >class clFindM  ;


\ ivFindM finds a method in an ivar, given a selID and a pointer to the
\  class field in the ivar dictionary info.

: ivFindM  ( selID ^iclass -- offs ^mcfa )

    @               \ addr of ivar's class
    clFindM  ;      \ ( offs ^mcfa )


\ =====================================================================
\ Method execution.
\ (This has to come before the object initialization code, since at
\ initialization time we have to send ClassInit: to all our ivars.)
\
\ The current object address is stored in the value ^base@.  To get the
\ current obj addr in a method, use ^base.  This is "smart", and will
\ normally give ^base@, or if it's an inline method, will compile the
\ object's addr as a literal.
\
\ You can only use ^base inside a method definition.  When we call a
\ method, we save the old object pointer and set it to the current object.
\ When the method returns, we restore the object pointer.


\ 0 VALUE (^base)     \ pointer to current object
\ ^base is in edi register
\ kernel provides these primatives:
\ ^base@
\ ^base!

: ^base
    compInline?
    IF      ^obj_for_bind  POSTPONE LITERAL
    ELSE    POSTPONE ^base@
    THEN  ;         IMMEDIATE

: EXECUTE-METHOD  ( ^obj offs ^mcfa -- )    \ execute method, saving object pointer
    ^base@ >R  ROT ROT + ^base!  @  EXECUTE   R> ^base! ;

: EXECUTE-IVAR  ( ^mcfa offset -- )     \ execute ivar method at given offset
    ^base@ >R  ^base@ + ^base!  @  EXECUTE  R> ^base! ;


\ =====================================================================
\ For late-bound method calls, we compile code to look up the method
\ at runtime and execute it. The syntax is:
\
\   Selector: [ object ]
\
\ The code between [ and ] must return an object reference. If the method
\ is not found in the class of the object, a runtime error occurs.
\ Because we use PARSE, [ and ] must be on the save source line.
\ =====================================================================

: (Defer)  ( ^obj selID -- )  OVER >class clFindM  EXECUTE-METHOD  ;
: Defer,   ( SelId -- )  POSTPONE LITERAL  POSTPONE (Defer) ;

: Defered  ( SelId -- )     \ Compile or execute a defered message send
    DUP 0= ABORT" late-binding to public ivars not yet implemented"
    >R  [CHAR] ] PARSE  EVALUATE  R>
    STATE @ IF  Defer,  ELSE  (Defer)  THEN ;

\ True if string is "[" to start defered message send
: ?isParen  ( str -- f )   1+ C@ [CHAR] [ = ;


\ =====================================================================
\ Hash function for instance variable names. The "32 OR" is for
\ case-insensitive names. The compiler will warn you if you have
\ a hash collision.

\ : HASH  ( addr len -- n ) 
\   TUCK BOUNDS ?DO  5 LSHIFT  I C@ 32 OR XOR  LOOP
\   DUP 0< IF EXIT THEN  INVERT ;

: hash>  ( -- n )  SYSWORD COUNT HASH ;


\ =====================================================================
\ Instance variables consist of six cell-sized fields. The fifth field
\ is used for indexed ivars only. Offsets are in cells.
\ 
\     Offset   Name      Description
\     ------   ----      ---------------------------------------
\        0     link      points to link of next ivar in chain
\        1     name      32-bit hash value of name
\        2     class     pointer to class
\        3     offset    offset in object to start of ivar data
\        4     #elem     number of elements (indexed ivars only)
\        5     flags     flags
\
\     Offset   Name      Description
\     ------   ----      ---------------------------------------
\        0     link      points to link of next ivar in chain
\             class     pointer to class
\             offset    offset in object to start of ivar data
\             #elem     number of elements (indexed ivars only)
\             flags     flags
\             name      string
\
\           flag masks: 1   - 1 public, 0 private
\                       2   - 1 self or super, 0 normal ivar
\ 
\ In the stack diagrams, "ivar" refers to the starting address of this
\ structure.  The IFA field of a class points to the first ivar.
\ =====================================================================

: iclass     ( ivar -- 'class )   2 CELLS +   ;
: @IvarOffs  ( ivar -- offset )   3 CELLS + @ ;
: @IvarElems ( ivar -- #elems )   4 CELLS + @ ;


\ =====================================================================
\ Build SUPER and SELF pseudo ivars. These are always the last
\ two ivars in a class. When we define a class, we will patch the
\ class fields to the appropriate class and superclass.

\ In the case of SUPER, there's a change with multiple inheritance -
\ the superclasses are represented by an n-way (set of class addresses),
\ but the class pointer in SUPER has to be a real class address, not
\ the address of an n-way (since ((findM)) must have a class address
\ to start with).
\ So we set the "class" of SUPER to a dummy class SUPCL, which has no
\ ivars or methods (so the search will pass right on by), and plug the
\ superclass pointer of SUPCL to point to the current n-way for the
\ superclasses of the class we're defining.


CREATE supcl  classSize RESERVE
        \ dummy superclass - no methods, ivar link patched at :Class time
classTag supcl TAG !        \ give it a legal class tag
                

CREATE ^Self
    0 ,             \ link
    hash> self ,    \ name
    0 ,             \ class
    0 ,             \ offset
    0 ,             \ #elem = 0 (not indexed)
    2 ,             \ flags = self/super, private

CREATE ^Super
    ^Self ,         \ link
    hash> super ,   \ name
    supcl ,         \ class = dummy supcl
    0 ,             \ offset
    0 ,             \ #elem = 0 (not indexed)
    2 ,             \ flags = self/super, private


\ Create a dummy class that "object" inherits from.

CREATE Meta classSize RESERVE

^Super   Meta IFA ! \ latest ivar
classTag Meta TAG ! \ class tag


\ =====================================================================
\ Determine if next word is an instance var.
\ Return pointer to class field in ivar structure.

: vFind  ( str -- str false | offs ^iclass true )
    ^Class
    IF  DUP COUNT HASH ^Class IFA_offset FALSE ((findM))
        IF                  \ found - ( str offs ^iclass )
                ROT DROP            \ get rid of str
                DUP 3 CELLS + @     \ look at flags
                2 AND  TO selfRef?  \ true if this is self or super
                                    \ - allows private methods to be found.
                TRUE
        ELSE    FALSE
        THEN
    ELSE    FALSE
    THEN ;


\ init_obj sends the ClassInit: message to an object.

: init_obj  ( theClass theObj -- )
    SWAP
    (ClassInit:) SWAP clFindM       \ ( ^obj offs mcfa )
    EXECUTE-METHOD  ;

\ =====================================================================

\ IVsetup recursively traverses the tree of nested ivar definitions in a
\ class, building necessary class pointers and indexed area headers, and
\ sending the ClassInit: method to each ivar.
\
\ On entry infa is the nfa of the first ivar in the ivar dictionary of the
\ object/ivar whose (sub)ivars we are to set up. The dictionary chain is
\ followed to the end, the last link pointing to the Nway superclass pointer.
\ baseOffs is the position of the current object/ivar's data space relative
\ to newObject, the current top-level object being created.
\ EOoffs is non-zero if the ivar whose subivars we are to set up is part
\ of an "embedded object", ie. is inherited from a superclass, and this
\ superclass is not the first super of the current top-level object.
\ This is passed on unmodified in any recursive call and used only by
\ nw_IVsetup to calculate the offset to the class pointer.
\ When this word is called, if the object/ivar's class is in a module,
\ the module will be held. In some circumstances the caller still needs it.
\ The recursive call might require another module to be held, so we have to
\ save and restore any module held on entry.

0   VALUE   prevIvar



\ nw_IVsetup sets up the groups of ivars for each superclass of the current object/ivar
\ being processed. One group for each super of a multiply inherited object.
\ Each group we call an "embedded object", which sort of describes what it is.
\ On entry ^nway points to the first superclass pointer in the n-way defining
\ the multiple inheritance. We repeat the procedure for each superclass until
\ the zero marking the end of the n-way is encountered. If the superclass
\ is the pseudoclass Meta we don't do anything since it does not have any ivars.
\ baseOffs is the position of the current object/ivar's data space relative
\ to newObject, the current top-level object being created.
\ EOoffs is the offset from newObj at which the current Embedded Object
\ starts. When an embedded object starts at a non-zero EOoffs, we put in
\ front of it a 2-byte offset to the class pointer. Note that if the
\ multiply inherited object is an ivar, there may not be a class pointer!
\ This doesn't matter, since it's better for multiply inherited
\ objects to always have the same format, wherever they are, and any attempt
\ to use the class pointer offset to get the (nonexistent) class pointer
\ will most probably be caught by our checks.

0   VALUE   EOoffs
0   VALUE   initEOoffs
0   VALUE   baseOffs
0   VALUE   supOffs
0   VALUE   IVsetup_xt


: nw_IVsetup  ( ^nway baseOffs EOoffs \ initEOoffs supClass supOffs -- )

    DUP TO EOoffs  TO initEOoffs
    TO baseOffs
    CELL+                   \ skip the -1
    BEGIN
        DUP @               \ addr of next class
        baseOffs EOoffs +  initEOoffs -     \ Start of dataspace of this
        TO supOffs                          \  superclass
        DUP IFA @                           \ ^ivar for its first ivar
        supOffs  EOoffs
        IVsetup_xt EXECUTE
        ( ^class )  newObject supOffs +  init_obj
        CELL+                           \ look at next class ptr in list
        DUP @
    WHILE       \ another class coming up - store offset back to class pointer
        DUP @ ( DUP )  DFA @            \ ( ^nway ^class dlen9 )
        ALIGNED  EOoffs +  TO EOoffs    \ update EOoffs
        EOoffs NEGATE CELL -            \ ^class offset for store

        EOoffs initEOoffs -             \ offset not already included in baseOffs
        baseOffs + newObject +          \ final addr for store
        TUCK !                          \ store offs to class ptr
        CELL+  -1 SWAP !                \ followed by -1 as marker
        EOoffs 2 CELLS +  TO EOoffs     \ update EOoffs by those 2 cells
    REPEAT
    DROP
;

0   VALUE   this_iclass

: IVsetup   ( ^ivar offset EOoffs -- )
    >R >R  ( ^ivar -- )

    BEGIN   
        DUP ^Super <>
        OVER -1 <>  AND
    WHILE
        DUP iclass @
        IFA @                       \ ( ^ivar ^ivar-in-class )
        OVER  @IvarOffs R@ +  0  RECURSE
                                    \ initialize ivar's own ivars

        DUP iclass @                \ get ivar class again
        DUP TO this_iclass          \ can save it in a Value now
        DUP XFA @                   \ needs class pointer?
        IF  OVER @IvarOffs R@ + newObject +
        ( ivar ^Class ivarAddr )
            2DUP CELL - !           \ store class pointer
            OVER @width  ( indexed? )
            IF  SWAP DFA @ +        \ addr of indexed area
                OVER @IvarElems     \ #elems
                SWAP !              \ upper array limit
            ELSE    2DROP
            THEN
        ELSE    DROP
        THEN

    ( ^ivar ) this_iclass R@ init_obj   \ send ClassInit: to this ivar
        DUP  TO prevIvar            \ in case needed by nw_IVsetup
        @                           \ next ivar in chain
    REPEAT

    -1 =
    IF                      \ we've hit an n-way - prevIvar has the addr
        prevIvar R> R> nw_IVsetup  EXIT
    THEN
    R> R> 2DROP  ;

' IVsetup  TO IVsetup_xt


\ =====================================================================
\ Compile an instance variable dictionary entry.

0   VALUE   flags

: <Var  ( #elems ^Class | ^Class -- )
    pub/priv 1 =  1 AND   TO flags      \ initial flags
    SYSWORD vFind ABORT" Duplicate instance variable"
    COUNT HASH                          \ get hash value of name
    ALIGN ^Class IFA LINK ,             \ link & name
    DUP ,                               \ class
    DUP XFA @ IF  CELL classAllot  THEN \ if indexed or general, allow for class ptr
    ^Class DFA @ ,                      \ offset to ivar data
    DUP @width DUP
    IF  ROT DUP ,  * CELL+              \ #elems, cell for idx-hdr
    ELSE    0 ,                         \ not indexed - #elem field = 0
    THEN
    flags ,                             \ flags
    SWAP DFA @ + classAllot ;           \ Account for named ivar lengths


\ =====================================================================
\ Build an instance of a class. If we are inside a class definition, 
\ build an instance variable. Otherwise build a global object.

\ Compile the indexed data header into an object
: IDX-HDR   ( #elems ^Class | ^Class -- indlen )
    @width DUP IF  OVER , ( limit )  *  THEN ;

: (Build)   ( #elems ^Class | ^Class -- )
    ^Class
    IF  <Var    \ build an ivar if we are inside a class
    ELSE
        (Obj)                   \ create object
        DUP >R ,                \ store class pointer
        HERE TO newObject       \ remember current object
        R@ DFA @ RESERVE        \ allot space for ivars
        R@ IDX-HDR RESERVE      \ allot space for indexed data
        R@ IFA @ 0 0 IVsetup    \ init instance variables
        R> newObject init_obj   \ send ClassInit: message
    THEN ;


\ =====================================================================

\ These values are used in processing the superclass list.

0   VALUE   ^sup
0   VALUE   #sup
0   VALUE   ivlen
0   VALUE   wid
0   VALUE   ^nway

\ :Class builds a class header with its superclass pointer..

: :Class  ( -- )
    CREATE  0 TO ^Class
    DOES>  (Build)  ;


\ We retain <Super for doing single inheritance - using it bypasses
\  the multiple inheritance stuff, and saves a little bit of overhead.

: <Super  ( -- )
    HERE TO ^Class              \ save current class
    classSize ALLOT             \ reserve rest of class data
    ' >BODY                     \ pfa of superclass
    DUP ^Class classSize MOVE   \ copy class data
    DUP ^Class SFA !            \ store pointer to superclass
\   ^Super iclass !             \ store superclass in SUPER
    DUP  supcl iclass !         \ store superclass in dummy class supcl,
                                \  so SUPER will use it
    supcl 9 CELLS  MOVE         \ copy method and ivar chain links to
                                \  supcl
    ^Class ^Self iclass !       \ store my class in SELF
    0 TO pub/priv               \ set public & private to default
;


\ Multiple inheritance support:

: merge_info  ( ^sup -- )
    TO ^sup
    ^sup XFA @  TO wid          \ indexed width of this superclass
    wid  0= IF EXIT THEN        \ If this superclass not indexed, we're done
    
\ This class is indexed - we need to check if prev classes were indexed
\  and make sure the widths are compatible.

    wid
    ^Class XFA      \ Addr of indexed wid field of class we're building
    2DUP @
    DUP IF          \ there's a previous width - it must be the same
        <>  ABORT" Incompatible indexed widths"
    ELSE            
        !           \ No prev width - set it
    THEN
;

: next_super  ( ^sup -- )
    DUP ?isClass 0= ABORT" not a class"
    TO ^sup
    ^sup ,                          \ Add ^class to n-way
    ^sup merge_info
    #sup IF                         \ If this is a subsequent class,
        ivlen ALIGNED  2 CELLS +    \  align and allow for ^class offset
        TO ivlen                    \  and -1 marker
    THEN
    ^sup DFA @  ivlen +  TO ivlen   \ And add ivar length of new class
    #sup 1+  TO #sup  ;


: }     ABORT" Unmatched }"  ;  IMMEDIATE

: supers_loop
    BEGIN                       \ Loop over superclasses:
        >IN @
        SYSWORD                 \ See if } follows - we don't use  ['] }
                                \  since we don't want to prevent } from
                                \  being redefined
        COUNT 1 =
        IF  C@ [CHAR] } =
            IF  DROP EXIT  THEN
        ELSE DROP
        THEN
        >IN !
        '                       \ cfa of next item on list
        >BODY  next_super       \ handle next superclass
    AGAIN  ;


: Super{
    HERE TO ^Class              \ save current class
    classSize RESERVE           \ reserve rest of class data
    classTag  ^Class TAG !      \ tag it as a class
    HERE DUP TO ^nway           \ n-way for superclasses will start here
    DUP ^Class SFA !            \ store pointer to n-way as superclass ptr
    ^Class DUP 9 CELLS +        \  and in the 8 method links and the
    SWAP                        \  ivar link
    DO  DUP I !  CELL +LOOP

    supcl DUP 9 CELLS +         \ also we point the method and ivar links
                                \  in supcl to the n-way
    SWAP
    DO  DUP I !  CELL +LOOP
    DROP

    ^Class ^Self iclass !       \ store my class in SELF
    0 TO pub/priv               \ set public & private to default

    0 TO ivlen  0 -> #sup
\    ^nway  ^Class MFA !         \ Point methods link to n-way
\    ^nway  ^Class IFA !         \ Likewise ivars link
    -1 ,                        \ Mark start of n-way with -1 (never a
                                \  legal address, we hope)
    supers_loop                 \ Loop over superclasses
    0 ,                         \ Terminate n-way
    ivlen  ^Class DFA !         \ Set total ivar length in this class
;


: ;Class  ( -- )
\    classAlign          \ align class data size (optional)
\   0 ^Super iclass !   \ clear out super and self class pointers
    0 supcl  iclass !
    0 ^Self  iclass !
    0 TO ^Class ;       \ clear class compiling flag



\ =====================================================================
\ Object Compiler. We rely on being able to classify the type of
\ object from it's execution token. There is no general way to 
\ do this in ANS forth for builtin types such as VALUEs. So we
\ only allow message sends to objects and classes. In Yerk, the
\ following will send a late-bound message to a object pointer:
\
\   0 VALUE theObject  ' myObject TO theObject
\
\   Message: theObject
\
\ Here we will have to use the following syntax (which does the same
\ this and is also allowed in Yerk):
\
\   Message: [ theObject ]
\
\ Key to instantiation actions (types 3 and 4 not supported)
\ 1 = objType       defined as an object
\ 2 = classType     as a class
\ 3 = vecType       as an object vector (value or vector)
\ 4 = parmType      as a named parm (local variable)
\ 5 = parenType     open paren for defer group
\ 6 = pubIvarType   reference to a public ivar          - **mh
\
\ =====================================================================


\ ========== public ivars - lifted from Mops, and ANSIfied ===========

0   VALUE   pivar       \ hashed name of any public ivar we're accessing
0   VALUE   pivSel      \ selector for any msg being sent to
                        \  to a public ivar

: ivar>     ABORT" Must be preceded by a selector"  ;

\ fix_pivar does the housekeeping for accessing a public ivar.  When we
\ encounter  msg: ivar>  then we store the selector in pivSel, and the
\ hashed ivar name in pivar.  We then continue with a zero "selector",
\ which signals that it's a public ivar access, and leads to us being
\ called back here to fix everything up once we've got the class in which
\ the ivar lives.

: fix_pivar  ( ^class -- ^mcfa offset )

    pivar SWAP ( selctr ^class )
    IFA_offset FALSE ((findM))   ( offs ^iclass true  OR  false )
    0= ABORT" ivar not found"
    
        \ we now have ( base-offs ^iclass ). ^iclass is the addr of
        \  the class pointer field of the ivar - the following cells
        \  have offset, #elem, flags.

    DUP 3 CELLS + @ 1 AND
    0= ABORT" public reference to an ivar which isn't public"

    DUP CELL+ @         \ ( base-offs ^iclass offset )
    ROT +               \ ( ^iclass offs ) - offs is updated offset
    pivSel              \ ( ^iclass offs selector )
    ROT                 \ ( offs selector ^iclass )
    ivFindM             \ find method - ( offs offs' ^mcfa )
    ROT ROT +           \ ( ^mcfa final-offset )
;


\ ( str -- xt tokenID )  Determine type of token referenced by string.
: refToken
    DUP ?isParen        IF  5 EXIT          THEN
\   pFind               IF  4 EXIT          THEN
    FIND 0= ABORT" undefined object"      \ 0= IF -29 THROW THEN
    DUP ['] ivar> =     IF  DROP 6 EXIT     THEN        \ **mh
    DUP >BODY ?isObj    IF  1 EXIT          THEN
    DUP >BODY ?isClass  IF  2 EXIT          THEN
\   DUP ?isVect         IF  3 EXIT          THEN
    TRUE ABORT" Invalid object type" ;

: (ivarRef)   ( xt offset -- )      \ compile ivar reference
    SWAP  POSTPONE LITERAL  POSTPONE LITERAL  POSTPONE EXECUTE-IVAR ;


: ivarRef   ( selID offs ^iclass -- )
            \ compile ivar reference.  ^iclass is the addr of the
            \  class pointer in the ivar dictionary entry.  offs is
            \  the base offset of the group of ivars representing
            \  a superclass.
    SWAP >R >R          \ push offs and ^iclass on Rstack
    ?DUP
    IF                  \ normal ivar reference
        R@              \ ( selID ^iclass )
        ivFindM         \ find method - ( offs ^mcfa ).
                        \ offs can be nonzero if the ivar has a
                        \ multiply-inherited class.  Now we have to 
                        \  add the offsets:
        SWAP
        R> CELL+ @      \ ( ^mcfa offs ivar-local-offset )
        +
        R> +            \ ( ^mcfa final-offset )
    ELSE                \ it's a public ivar reference within the referenced ivar
        R> @            \ class of ivar
        fix_pivar       \ ( ^mcfa offs )
        R> +            \ add base offset, giving final offset
    THEN
    ?DUP
    IF      (ivarRef)
    ELSE    @ ( xt )  COMPILE,      \ optimize for offset zero )
    THEN
    FALSE TO selfRef?               \ **mh - restore normal default
;


: callMethod  ( xt -- )   \ compile method call
\   POSTPONE LITERAL  POSTPONE EXECUTE-METHOD       \ **mh - slightly optimizing
    @               \ get xt of method
    POSTPONE ^base@  POSTPONE >R   
    S" ^base! " EVALUATE  COMPILE,
    S" R> ^base! "  EVALUATE  ;


: ((objRef))  ( SelID objCfa -- ^obj offs ^mcfa )
        \ common code used in compiling or executing an object reference
    >obj                            \ ( selID ^obj )
    OVER 0=
    IF                              \ public ivar reference
        NIP DUP >class  fix_pivar   \ ( ^obj ^mcfa offs )
        SWAP                        \ ( ^obj offs ^mcfa )
    ELSE
        TUCK objFindM               \ ( ^obj offs ^mcfa )
    THEN
;


: (objRef)   ( SelID objCfa -- )    \ compiles object reference

    ((objRef))                  \ ( ^obj offs ^mcfa )
    DUP CELL+ @                 \ get method flags
    4 AND                       \ check "inline" bit
    IF                          \ we'll do an inline bind
        ROT ROT +               \ ( ^mcfa ^obj' )
        ^obj_for_bind >R        \ save in case there's a nested inline
        TO ^obj_for_bind
        2 CELLS + COUNT         \ ( addr len ) - string to EVALUATE
        TRUE  TO compInline?
        EVALUATE
        R> TO ^obj_for_bind
        FALSE TO compInline?
    ELSE                        \ normal early bind
        ROT ROT +               \ add offs to ^obj - ( ^mcfa ^obj' )
        POSTPONE LITERAL        \ compile push of obj addr
        callMethod
    THEN  ;


0   VALUE   message_xt          \ ANSI doesn't have FORWARD definitions!


\ PubIvarRef handles the  msg: ivar> someIvar IN someObj  construct, to
\  send a message directly to a public ivar in an object.  At this point
\  we've just read "ivar>".

: pubIvarRef  ( selID -- )
    TO pivSel                       \ save selector being sent to the ivar
    SYSWORD COUNT HASH  TO pivar    \ parse ivar name
    SYSWORD COUNT 2DUP UPPER " IN" S=
    IF  0                   \ dummy "selector" for message (not a legal selector)
        message_xt EXECUTE  \ handle whatever object comes after IN.  The
                            \  zero selector signals that a public ivar in the
                            \  indicated object is to be accessed - real selectors
                            \  can't ever be zero.  This will lead to fix_pivar
                            \  being called to complete the job.
    ELSE
        ABORT" wrong syntax for public ivar"
    THEN
;


\ ( selID $str -- )  Build a reference to an object or vector

: objRef
    refToken CASE
      1 ( object ) OF   (objRef)                    ENDOF
      2 ( class  ) OF   >BODY  clFindM  callMethod  ENDOF
\     3 ( vector ) OF   COMPILE, Defer,             ENDOF
\     4 ( parm   ) OF   EXECUTE  Defer,             ENDOF
      5 ( paren  ) OF   DROP Defered                ENDOF
      6 ( ivar>  ) OF   pubIvarRef                  ENDOF
    ENDCASE ;


\ ( selID $str -- )  Execute using token in stream

: runRef
    refToken CASE
      1 ( object ) OF  ((objRef))               ENDOF
      2 ( class  ) OF  >BODY clFindM            ENDOF
\     3 ( vector ) OF  EXECUTE TUCK objFindM    ENDOF
\     4 ( parm   ) OF  1 ABORT" param?"         ENDOF
      5 ( paren  ) OF  DROP Defered EXIT        ENDOF
      6 ( ivar>  ) OF  pubIvarRef  EXIT         ENDOF       \ **mh
    ENDCASE  EXECUTE-METHOD ;


\ =====================================================================
\ Selectors are immediate words that send a message to the object
\ that follows. The Yerk requirement that selectors end in ":" is
\ enforced here but not otherwise required by the implementation.

\ This is the message compiler invoked by using a selector.

: message   ( SelID -- )
    SYSWORD  STATE @
    IF  vFind   \ instance variable?
        IF      ivarRef     \ ivar reference
        ELSE    objRef      \ compile object/vector reference
        THEN
    ELSE    runRef          \ run state - execute object/vector ref
    THEN ;


' message  TO message_xt


: ?isSel  ( str -- flag )  \ true if word at addr is a selector xxx:
    DUP DUP C@ CHARS +  C@ [CHAR] :  =  SWAP C@ 1 > AND ;

: ?Selector ( -- )  \ Verify that following word is valid selector
    >IN @  SYSWORD ?isSel 0= ABORT" Not a selector"  >IN ! ;

\ Create a selector that sends a message when executed.

: Selector  ( n -- )  ?Selector
    CREATE IMMEDIATE  DOES> message ;

\ If the selector already exists, just return the existing selector,
\ otherwise create a new selector.

: getSelect  ( -- n )
    >IN @  SYSWORD FIND
    IF  >BODY NIP  ( already defined )
    ELSE    DROP >IN ! Selector
        HERE
    THEN ;

Selector ClassInit:    getSelect ClassInit: TO (ClassInit:)


\ =====================================================================
\ Build a methods dictionary entry. :M starts a method definition
\ by adding to the class method list and starting the compiler with
\ :NONAME. ;M ends a method and saves the method xt.

: :M    ( -- )
    ?Class
    getSelect
    DUP ^Class MFA_offset TRUE ((findM))    \ is method already defined?
    IF  NIP
\       CR ." Method redefined "
\       HERE COUNT TYPE SPACE
\       ^Class BODY> >NAME .ID      \ print class name

        ^Class U> ABORT" Method redefined in same class"
    THEN
    ALIGN           \ align method
    ^Class MFA LINK     \ link into method chain
    ( SelID ) ,         \ name is selector's hashed value
    HERE 0 ,                    \ save location for method xt
    HERE  TO ^methFlags         \ remember location of flags cell
    pub/priv -1 <>  1 AND ,     \ flags - default is public for methods
    :NONAME                     \ start compiling nameless definition
    TRUE TO method?             \ we're now compiling a method
;


: ;M    ( addr xt -- )      \ ends a method definition
    ?Class  POSTPONE ;  SWAP ! ( save xt )
    FALSE TO method?  ;             IMMEDIATE


: inline{  ( --< inline source text> )
    method? 0= ABORT" inline{ can only be used in a method"
    ^methFlags @ 4 OR  ^methFlags ! \ flag this method as an inline
    POSTPONE ;  DROP                \ undo :M
    ^methFlags CELL+  HERE - ALLOT  \ remove anything :M put at HERE
                                    \  - we'll redo it shortly
    HERE  >R                        \ save location of start of string
                                    \  for EVALUATE below
    [CHAR] }  ,str                  \ store inline text, terminated by }
    :NONAME                         \ restart compiling nameless definition
    R> COUNT EVALUATE               \ compile out-of-line code
;       IMMEDIATE


\ =====================================================================
\ Build a new object on the heap for class. Use: Heap> className
\ gets heap, and returns ptr. Throws an error if not enough memory

HAVE ALLOCATE [IF]

: ?MEMERR  ( ior -- )  ABORT" Memory allocation error" ;

Selector Release:   \ sent to an object before it is freed

: allocObj  ( size class -- )  \ allocate object and store in newObject
    OVER CELL+      \ allow for class ptr
    ALLOCATE ?MEMERR    \ ( size class addr -- )
    DUP CELL+ TO newObject  \ object address
    !           \ create the class ptr
    newObject SWAP ERASE ;  \ clear to zero

: (heapObj)  ( #elems class | class -- ^obj )
    >R  ( save class on return stack )
    R@ DFA @  ( ivar data size )
    R@ @width ?DUP
    IF  \ indexed object, add size of indexed area
        \ ( #elems size width -- )
        2 PICK * + ( indexed data )  CELL+ ( idxHdr )
        R@ allocObj
        newObject R@ DFA @ + !  ( store #elems in idxHdr )
    ELSE
        R@ allocObj \ non-indexed object
    THEN
    R> IFA @ 0 0 IVsetup        \ initialize instance variables
    ClassInit                   \ send ClassInit: message to new object
    newObject  ;                \ return object address


: HEAP>   ( -- addr )
    ' >BODY  DUP ?isClass 0= ABORT" Not a class"
    STATE @
    IF  POSTPONE LITERAL  POSTPONE (heapObj)
    ELSE    (heapObj)
    THEN ; IMMEDIATE

: RELEASE  ( ^obj -- )      \ free heap object
    Release: [ DUP ]        \ send Release: message to object
    CELL - FREE ?MEMERR ;   \ deallocate it

[THEN]


\ =====================================================================
\ Support for indexed instance variables. When object of these classes
\ are defined, the number of elements should be on the stack.

\ Set a class and its subclasses to indexed

: <Indexed  ( width -- )  ?Class  ^Class XFA !  ;

\ For some classes, we always want the class pointer to be stored whenever
\ the object is an instance variable, so that the object can receive late-
\ bound messages. This is already the case for indexed classes. Here we
\ fake it out by storing -1 into the indexed field. This is the reason
\ for the "0 MAX" in the definition of @width.

: <General  ( -- )  -1 <Indexed ;

\ self returns the same address as ^base, but is used for late bound calls
\ to the object, i.e.   Selector: [ self ]

: self  ( -- )  ?Class
    ^Class XFA @ 0= ABORT" Class must be <General or <Indexed"
    POSTPONE ^base ; IMMEDIATE

\ =====================================================================
\ Indexed primatives. These should be in code for best performance.

0 [if]
: idxBase  ( -- addr )      \ get base of idx data area
    ^base  DUP >class DFA @ +  CELL+ ;

: limit    ( -- n )     \ get idx limit (#elems)
    ^base  DUP >class DFA @ +  @ ;

: width    ( -- n )     \ width of an idx element
    ^base >class XFA @ ;

: ^elem   ( index -- addr ) \ get addr of idx element
    width *  idxBase + ;

\ Fast access to byte and cell arrays.
: At1  ( index -- char )   idxBase + C@ ;
: At4  ( index -- cell )   CELLS idxBase + @ ;

: To1  ( char index -- )   idxBase + C! ;
: To4  ( cell index -- )   CELLS idxBase + ! ;

: ++1  ( char index -- )   idxBase + C+! ;
: ++4  ( cell index -- )   CELLS idxBase + +! ;
[then]

\ objlen computes the total length of the object.
\ The length does not include the class pointer.

: objlen  ( -- objlen )
    ^base@ >class DUP DFA @  ( non-indexed data )
    SWAP @width ?DUP
    IF  idxBase CELL - @ ( #elems ) * +  CELL+  THEN ;

\ =====================================================================
\ Runtime indexed range checking. Use +range and -range to turn range
\ checking on and off.
0 [if]
: ?range  ( index -- index )  \ range check
    DUP idxBase CELL - @ ( #elems )  U< IF EXIT THEN
    TRUE ABORT" Index out of range" ;

0 VALUE (?idx)      \ execution vector for range checking

: ?idx   (?idx) EXECUTE ;

: +range  ['] ?range TO (?idx) ;  +range
: -range  ['] NOOP   TO (?idx) ;
[then]

: ?idx ; immediate

\ =====================================================================
\ Primitives for cell-sized objects.

: M@  ( -- n )  POSTPONE ^base@  POSTPONE @ ; IMMEDIATE
: M!  ( n -- )  POSTPONE ^base@  POSTPONE ! ; IMMEDIATE

\ =====================================================================
\ Define base class "object" from which all other classes inherit
\ Some of the common indexed methods are defined here.

:Class Object  <Super Meta

    :M Class:   ^base@ >class  ;M  \ non-IX - leave class ptr
    :M Addr:    ^base@         ;M  \ get object address

    \ ( -- len )  Return total length of object
    :M  Length:    objlen      ;M

HAVE DUMP [IF]
    :M  Dump:     ^base objlen DUMP  ;M
[THEN]
    :M  Print:     ." Object@" ^base U.  ;M

    :M ClassInit:   ;M  \ null method for object init
    :M Release:     ;M  \ null method for object release

;Class



\ Bytes is used as the allocation primitive for basic classes
\ It creates an object of class Object that is n bytes long.
\ You can get the address by sending it an addr: message.

: bytes  ( n -- )
    ?Class  ['] Object >BODY <Var  classAllot ;

\ =====================================================================
\ debug helpers

: m'  ( <sel> <class> -- method-xt )
    getSelect
    sysword refToken 2 <> abort" is not a class"
    >body clFindM @ nip ;

have disasm [if]

: see ( <sel> <class> -- )
    >in @ sysword swap >in !
    ?isSel if  m' @ dis  else  see  then ;

[then]

\ =====================================================================
\ Load primitive classes and test routines.

\ fload classM/var
\ fload classM/array
\ fload classM/test

\ CR .( Classes loaded )
