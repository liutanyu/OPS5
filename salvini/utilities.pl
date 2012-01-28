/*  UTILITIES.PL  */
/*  Shelved on the 3rd of October 1988  */


/*****************************************************************************/
/************ PROGRAM: utilities *********************************************/
/*****************************************************************************

    This program may be used and amended for non-profit making
    purposes on condition that this header be left intact and in situ.

    Copyright: Steven Salvini 1986, 1987, 1988

        Amended:
                Steven Salvini: 1987
                Steven Salvini: 1988

 *****************************************************************************/

/*****************************************************************************

    write_caller:
    writes name of calling program/level on screen

 *****************************************************************************/

write_caller(Caller):-
    ttyflush,
    video(reverse),
    write('                                                    called by '),
    write(Caller),
    write(' '),
    video(normal),
    nl.

/*****************************************************************************
 
    reverse:
    reverses a list
 
 *****************************************************************************/

reverse(L,R):-
    reverse(L,[],R).

reverse([X|L1],L2,L3):-
    reverse(L1,[X|L2],L3).

reverse([],L,L).

/*****************************************************************************

    pstring/1:
    prints a string

    pstring/2:
    prints a string (arg1) up to a maximum number of characters (arg2)
 
 *****************************************************************************/

pstring([]).
pstring([Char|Chars]):-
    put(Char),
    pstring(Chars).

pstring(String,Max):-
    pstring(String,1,Max).

pstring([],Index,Max):-
    Spaces is Max - Index,
    tab(Spaces).

pstring([],_,_).

pstring([Char|Chars],Max,Max):-
    put(Char).

pstring([Char|Chars],Index,Max):-
    put(Char),
    Next is Index + 1,
    pstring(Chars,Next,Max).

/****************************************************************************
 
    pause:
    halts execution of program until <CR> pressed
 
 ****************************************************************************/

pause(Message):-
    check_pause_message(Message),
    nl,
    video(reverse),
    write(' Press <RETURN> key'),
    write(Message),
    write(' '),
    video(normal),
    get0(_),
    !.

check_pause_message(' to continue...'). /* default message */
check_pause_message(_).         /* message already instantiated */
    
member(X,[X|_]).
member(X,[_|Y]):-
    member(X,Y).

/**************************************************************************
  
    input_number:
    inputs a number between 'Max' & 'Min'
 
 **************************************************************************/

input_number(Min,Max,Number):-
    repeat,
    nl,
    read_word(Reply),
    ok_number(Reply,Min,Max,Number),
    !.

ok_number(Reply,_,_,Validated_Reply):-
    means(Reply,quit,Validated_Reply),
    !.
    
ok_number(Reply,Min,Max,Reply):-
    means(Reply,valid_replies,Validated_Reply),
    nl,
    write('Valid replies are...'),
    nl,nl,
    write('v,V,valid,VALID:   gives this message'),nl,
    write('q,Q,quit,Quit:     quit'),nl,
    write('OR a number in the range '),
    write(Min),
    write(' - '),
    write(Max),
    write(' inclusive'),
    nl,
    !,
    fail.



ok_number(Reply,Min,Max,Reply):-
    number(Reply),
    Reply =< Max,
    Reply >= Min.

/**************************************************************************
  
    input_integer:
    inputs an integer
 
 **************************************************************************/

input_integer(I):-
    repeat,
    nl,
    read_word(Reply),
    ok_integer(Reply,I),
    !.

ok_integer(Reply,Validated_Reply):-
    means(Reply,quit,Validated_Reply).

ok_integer(Reply,Validated_Reply):-
    means(Reply,valid_replies,Validated_Reply),
    nl,
    write('Valid replies are...'),
    nl,nl,
    write('v,V,valid,VALID:   gives this message'),nl,
    write('q,Q,quit,Quit:     quit'),nl,
    write('OR an integer value '),
    nl,
    !,
    fail.

ok_integer(I,I):-
    number(I).

/**************************************************************************

    input_yes_no:
    returns "yes" or "no" in answer to a question

 **************************************************************************/

input_yes_no(Validated_Reply):-
    repeat,
    nl,
    read_word(Reply),
    ok_reply_yn(Reply,Validated_Reply),
    do_ok_reply_yn(Reply,Validated_Reply),
    !.

do_ok_reply_yn(Reply,valid_replies):-
    nl,write('Valid replies are...'),nl,nl,
    write('v,V,valid,VALID:   gives this message'),nl,
    write('q,Q,quit,QUIT:     exit current option making no changes'),nl,
    write('y,Y,yes,YES:       yes'),nl,
    write('n,N,no,NO:         no'),nl,
    !,
    fail.

do_ok_reply_yn(_,_).

ok_reply_yn(Reply,Validated_Reply):-
    means(Reply,yes,Validated_Reply);
    means(Reply,no,Validated_Reply);
    means(Reply,valid_replies,Validated_Reply);
    means(Reply,quit,Validated_Reply).

/****************************************************************************

    means:
    these clauses standardise a user response to a fixed value for the 
    system.
 
    eg.
        w,W,why,WHY are all acceptable synonyms for 'why'
 
 ****************************************************************************/

means(Number,threshold,Number):-
    number(Number),
    Number =< 10,
    Number >= 5.        /* arbitrary choice!!!!!!! */

means(Number,certainty,Number):-
    number(Number),
    Number =< 10,
    Number >= -10.

/* means('',default,default).       <CR> gives default option */

means(q,quit,quit).
means('Q',quit,quit).
means(quit,quit,quit).
means('QUIT',quit,quit).

means(v,valid_replies,valid_replies).
means('V',valid_replies,valid_replies).
means(valid,valid_replies,valid_replies).
means('VALID',valid_replies,valid_replies).

means(w,why,why).
means('W',why,why).
means(why,why,why).
means('WHY',why,why).

means(or,or,or).
means('OR',or,or).

means(and,and,and).
means('AND',and,and).


means(y,yes,yes).
means('Y',yes,yes).
means(yes,yes,yes).
means('YES',yes,yes).

means(n,no,no).
means('N',no,no).
means(no,no,no).
means('NO',no,no).

/**************************************************************************
 
    remove_duplicates:
    removes any duplicate entries in a list (arg1) and returns a 
    second list consisting of unique elements
 
 **************************************************************************/

remove_duplicates(I,O):-
remove_duplicates(I, [], O).

remove_duplicates([],L,L).

remove_duplicates([H|T],L,M):-
        member(H,L),
    remove_duplicates(T,L,M).

remove_duplicates([H|T],L,M):-
    remove_duplicates(T,[H|L],M).

/**************************************************************************
 
    min, max, invert, modulus:
    miscellaneous arithmetic procedures
 
 **************************************************************************/

min(A,B,Min):-
    A < B,
    Min = A,!.
min(_,Min,Min).

max(A,B,Max):-
    A > B,
    Max = A,!.
max(_,Max,Max).

invert(CA,C_notA):-
    C_notA is - CA.

modulus(X,Modulus):-
    X < 0,
    Modulus is - X.
modulus(Modulus,Modulus).

/**************************************************************************
 
    clause_is_a:
    determines whether or not a given atom (Ctype) begins with a second
    atom (Type).
 
 **************************************************************************/

clause_is_a(Type,Ctype):-
    name(Ctype,Ctlist),
    name(Type,Tlist),
    conc(Tlist,_,Ctlist).

/**************************************************************************
 
    conc:
    joins two lists giving a list as result (i.e. "append")
 
 **************************************************************************/

conc([],L,L).
conc([A|L1],L2,[A|L3]):-
    conc(L1,L2,L3).

/**************************************************************************
 
    conc_atom:
    joins two atoms giving an atom as result
 
 **************************************************************************/

conc_atom(A1,A2,A3):-
    name(A1,A1list),
    name(A2,A2list),
    conc(A1list,A2list,A3list),
    name(A3,A3list).

/**************************************************************************
 
    check_rb_loaded:
    checks that rules are present in knowledge base
    (loading a rulebase if necessary)
 
 **************************************************************************/

check_rb_loaded(Rulebase):-
    do_get_base(Rulebase, rule),
    pause(Default_Message).

check_any_rb_loaded:-
    Type:Rest,
    clause_is_a(rule,Type),
    write('Do you want to load a new rule base? (yes/no)...'),
    nl,
    input_yes_no(Reply),
    do_check_rb_loaded(Reply).

check_any_rb_loaded:-
    write('No rule base has been loaded... '), nl,
    write('Do you want to load a new rule base? (yes/no)...'),
    nl,
    input_yes_no(Reply),
    do_check_rb_loaded(Reply).

do_check_rb_loaded(yes):-
    get_base(rule,old).

do_check_rb_loaded(no).

do_check_rb_loaded(quit).

/**************************************************************************
 
    get_base:
    Inputs the rules/facts from a given rule/fact base
 
 **************************************************************************/

get_base(Type,Status):-
    name(Type,Type_list),
    conc("Please enter the name of the ",Type_list,Temp_list),
    conc(Temp_list," base to be used...",Message),
    get_file_name(Filename,Message,Status),
    do_get_base(Filename,Type).

do_get_base(quit,_):-
    write('No action taken!!!'),
    nl,
    !,
    fail.

do_get_base(Filename,Type):-
    fileerrors,
    reconsult(Filename),
    write('The '),
    write(Type),
    write('`s from '),
    write(Filename),
    write(' have been loaded.'),
    nl.

/**************************************************************************
 
    get_file_name:
    iputs a valid filename.
    arg3 specifies whether the file should:
 
        already exist...........................old
        already exist & "quit"-ing is allowed...old_quit
        not already exist.......................new
 
 **************************************************************************/

get_file_name(Newfilename,Message,Status):-
    repeat,
    nl,
    pstring(Message),
    nl,
    read_word(Filename),
    check_file_name(Filename,Newfilename,Status),
    !.

check_file_name(Filename,Newfilename,new):-
    atom(Filename),
    nofileerrors,
    exists(Filename),
    write('File: '),
    write(Filename),
    write(' already exists!!!'),
    nl,
    write('Do you still wish to use it? (yes/no) '),
    nl,
    input_yes_no(Reply),
    !,
    use_file(Reply,Filename,Newfilename).

check_file_name(Filename,Filename,new).

check_file_name(Filename,Filename,old):-
    atom(Filename),
    nofileerrors,
    exists(Filename).

check_file_name(Filename,Newfilename,old):-
    write(Filename),
    write(' not found.'),
    nl,
    write('Please re-enter...'),
    nl,
    !,
    fail.

check_file_name(quit,quit,old_quit).

check_file_name(Filename,Filename,old_quit):-
    atom(Filename),
    nofileerrors,
    exists(Filename).

check_file_name(Filename,_,old_quit):-
    write(Filename),
    write(' not found.'),
    nl,
    write('Please re-enter... (or quit to exit making no changes) '),
    nl,
    !,
    fail.

use_file(yes,Filename,Filename).

use_file(no,_,_):-
    fail.

use_file(quit,_,_):-
    fail.

/**************************************************************************

    export:
    used to write clauses to a file in a format suitable for later
    use by the 'read' primitive.

 **************************************************************************/

export(X):-
    clause(X,Y),
    export_clause(X,Y),
    write('.'),
    nl,
    fail.

export(_).

export_clause(X,true):-
    !,
    write(X).

export_clause(X,Y):-
    write((X:-Y)).

/**************************************************************************
 
    retract_unknowns:
    removes all facts & tolds with certainty = 0
 
 **************************************************************************/

retract_unknowns:-
    Type:O/A/V with certainty(0),
    (Type = fact; Type = told),
    retract(Type:O/A/V with certainty(0)),
    fail.

retract_unknowns.

/**************************************************************************
 
    read_string:
    reads in a string term from the keyboard
 
 **************************************************************************/

read_string(String):-
    get0(Char),
    get_more_chars(Char,[34],String).   /* 34 is ASCII "." */

get_more_chars(10,String,[34|String]).      /* 10 is ASCII <CR> */
get_more_chars(Char,S,String):-
    get0(Next_Char),
    get_more_chars(Next_Char,[Char|S],String).

/**************************************************************************

    read_word:
    reads input from keyboard and returns it as an atom.
    (may include blanks, periods, back-slashes, underlines & hyphens)
    (adapted from p.104 'Clocksin & Mellish, 1985')
 
 **************************************************************************/

read_word(W):- 
        get0(C1),readword(C1,W,C2,no_blanks).

read_word(W,Blanks_allowed):- 
        get0(C1),readword(C1,W,C2,Blanks_allowed).

readword(10,'',_,_):-       /* accept <CR> as word (10 is ASCII <CR>) */
    !.

readword(C,W,C2,B):-
    in_word(C,NewC,B),
    !,
    get0(C1),
    restword(C1,Cs,C2,B),
        name(W,[NewC|Cs]).

readword(C,W,C2,B):-    
    get0(C1),
    readword(C1,W,C2,B).

restword(C,[NewC|Cs],C2,B):-
    in_word(C,NewC,B),
    !,
    get0(C1),
    restword(C1,Cs,C2,B).

restword(C,[],C,_).

in_word(C,C,_):-C>96,C<123.         /* a - z */
in_word(C,C,_):-C>64,C<91.              /* A - Z */
in_word(C,C,_):-C>47,C<58.          /* 0 - 9 */
in_word(47,47,_).               /* '/' */
in_word(46,46,_).               /* '.'   period */
in_word(45,45,_).               /* '-'   hyphen */
in_word(96,96,_).               /* '_'   underline */
in_word(32,32,blanks).              /* ' '  blank space */


/**************************************************************************

    banner:
    prints its argument using a special character set of "characters"
    7x7 normal size characters & centering this on the screen.
    (assumes an 80 column screen)
 
 **************************************************************************/

banner(Title):-
    name(Title,T_list),
    centre_title(T_list,C_list),
    get_bits(C_list,C_out),
    find_line_units(C_out,1,7).

centre_title(T,C):-
    length(T,L),
    Offset is (10 - L)/2,
    pad_left(T,Offset,C).

pad_left(L,N,L):-
    N < 1.

pad_left(L,N,C):-
    Next is N - 1,
    pad_left([32|L],Next,C).    /* 32 is ASCII 'space' */

get_bits([],[]).
get_bits([H|T],[U|Tbits]):-
    name(Ha,[H]),
    U =.. [Ha,HA,HB,HC,HD,HE,HF,HG],
    call(bc(U)),
    get_bits(T,Tbits).

find_line_units(List,Line,Line):-
    print_line_unit(List,Line).

find_line_units(List,Line,Lines):-
    print_line_unit(List,Line),
    Next_line is Line + 1,
    find_line_units(List,Next_line,Lines).

print_line_unit([],_):-
    nl.

print_line_unit([H|T],I):-
    arg(I,H,Arg),
    write(Arg),
    tab(1),
    print_line_unit(T,I).

/**************************************************************************
 
    bc:
    special "character set" for 'banner'
 
 **************************************************************************/

bc(' '('       ','       ','       ','       ','       ','       ','       ')).
bc('`'('   #   ','    #  ','       ','       ','       ','       ','       ')).
bc('.'('       ','       ','       ','       ','       ','       ','  ##   ')).
bc('!'('   #   ','   #   ','   #   ','   #   ','   #   ','       ','   #   ')).
bc(a('   #   ',' #   # ','#     #','#######','#     #','#     #','#     #')).
bc(b('###### ','#     #','#     #','###### ','#     #','#     #','###### ')).
bc(c('  #### ',' #    #','#      ','#      ','#      ',' #    #','  #### ')).
bc(d('#####  ','#    # ','#     #','#     #','#     #','#    # ','#####  ')).
bc(e('#######','#      ','#      ','#######','#      ','#      ','#######')).
bc(f('#######','#      ','#      ','####   ','#      ','#      ','#      ')).
bc(g('  #### ',' #    #','#      ','#      ','#   ###',' #   # ','  ###  ')).
bc(h('#     #','#     #','#     #','#######','#     #','#     #','#     #')).
bc(i(' ##### ','   #   ','   #   ','   #   ','   #   ','   #   ',' ##### ')).
bc(j('#######','   #   ','   #   ','   #   ','   #   ','#  #   ',' ##    ')).
bc(k('#    # ','#   #  ','#  #   ','###    ','#  #   ','#   #  ','#    # ')).
bc(l('#      ','#      ','#      ','#      ','#      ','#      ','#######')).
bc(m('#     #','##   ##','# # # #','#  #  #','#     #','#     #','#     #')).
bc(n('#     #','##    #','# #   #','#  #  #','#   # #','#    ##','#     #')).
bc(o('  ##   ',' #  #  ','#    # ','#    # ','#    # ',' #  #  ','  ##   ')).
bc(p('###### ','#     #','#     #','###### ','#      ','#      ','#      ')).
bc(q('  ##   ',' #  #  ','#    # ','#    # ','#  # # ',' #  #  ','  ## # ')).
bc(r('###### ','#     #','#     #','###### ','#  #   ','#   #  ','#    # ')).
bc(s(' ##### ','#     #','#      ',' ##### ','      #','#     #',' ##### ')).
bc(t('#######','   #   ','   #   ','   #   ','   #   ','   #   ','   #   ')).
bc(u('#     #','#     #','#     #','#     #','#     #','#     #',' ##### ')).
bc(v('#     #','#     #','#     #','#     #',' #   # ','  # #  ','   #   ')).  
bc(w('#     #','#     #','#     #','#     #','#  #  #','# # # #','#     #')).
bc(x('#     #',' #   # ','  # #  ','   #   ','  # #  ',' #   # ','#     #')).
bc(y('#     #',' #   # ','   #   ','   #   ','   #   ','   #   ','   #   ')).
bc(z('#######','     # ','    #  ','   #   ','  #    ',' #     ','#######')).

bc(X):- /*** not foolproof! ***/
    X =.. [Y, '       ','       ','       ','       ','       ','       ','       '].


/**************************************************************************
 
    htab/0:
    outputs an ASCII <HT> (horizontal tabulation) character
 
    htab/1:
    outputs 'argument' number of ASCII <HT> characters
 
 **************************************************************************/

htab:-
    htab(1).

htab(N):-
    htab(0,N).

htab(Max,Max).

htab(N,Max):-
    put(9),     /* ASCII <HT>   (horizontal tabulation character) */
    Next is N + 1,
    htab(Next,Max).

/******************************************************************************/
/**************  END OF PROGRAM: utilities ************************************/
/******************************************************************************/
