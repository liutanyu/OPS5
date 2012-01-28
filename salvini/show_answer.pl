/*  SHOW_ANSWER.PL  */
/*  Shelved on the 3rd of October 1988  */


/****************************************************************************/
/*********** PROGRAM: show_answer *******************************************/
/****************************************************************************
 
    This program may be used and amended for non-profit making
    purposes on condition that this header be left intact and in situ.
 
    Copyright: Steven Salvini 1986, 1987, 1988
                 
      Amended Graeme Sandieson 1987
 
        Nota Bene:  This module was extensively rewritten (or should
                that be "written"!?!?!?!) by GS and provides a 
                much clearer presentation of the results of a
                consultation with the system. - SS: April '88.
 
      Amended Steven Salvini: April 1988
 
****************************************************************************/

/****************************************************************************
 
    explain:
    Asks whether the user wishes to see the solution tree for the 
    Answer/Differential Diagnosis given and acts accordingly
 
****************************************************************************/

explain([]).

explain([CF - _ | _]):-
    CF =< 0.

explain(Answer):-
    nl,
    write('Would you like to see how this was deduced? (yes/no) '),
    input_yes_no(Reply),
    do_explain(Reply,Answer).

do_explain(quit,_).

do_explain(no,_).

do_explain(yes,Answer):-
    clear_screen,
    show(Answer).

/****************************************************************************
 
    show:
    'shows' complete solution tree produced by 'explore'
    formatting it for ease of reading
 
 ****************************************************************************/

show(Solution):-
    nl,
    remove_keys(Solution,[],Clean_Solution),
    select_full_diagnoses(Clean_Solution),
    pause(Default_Message).

/****************************************************************************
 
    remove_keys:
    removes the 'certainties' used as the key by "keysort"
 
****************************************************************************/

remove_keys([0 - Tree|More],CS,CS).    /* excludes all '0' diagnoses */

remove_keys([],CS,CS).

remove_keys([C - Tree|More],Temp,CS):-
    remove_keys(More,[Tree|Temp],CS).

/****************************************************************************
 
    select_full_diagnoses (& following procedures):
    allows the selection (by pseudo of a menu) of diagnoses to be explained 
 
****************************************************************************/

select_full_diagnoses(Solns):-
    repeat,
    clear_screen,
    write(' EXPLANATION FACILITY '),nl,
    write(' ~~~~~~~~~~~~~~~~~~~~ '),nl,nl,nl,
    show_diagnosis(Solns,1,Last),nl,
    write('Please enter the number of the diagnosis to be explained... '),
    write(' (0 to quit)'),
    nl,
    Max is Last - 1,
    input_number(0,Max,Choice),
    do_sdc(Choice,1,Solns).

show_diagnosis([],Last,Last).

show_diagnosis([tree(L,O/A/V # C was _,R)|More],Index,Last):-
    write('# '),
    write(Index),
    tab(5),
    write(V),
    write(' with certainty '),
    write(C),
    nl,
    Next is Index + 1,
    show_diagnosis(More,Next,Last).


do_sdc(quit,_,_).   /* to quit */

do_sdc(0,_,_).      /* to quit */

do_sdc(Choice,Index,Solutions):-
    select_explanation(Choice,Index,Solutions),
    !,
    fail.

select_explanation(Choice,Choice,[S|Ss]):-
    show_tree_explain(S).

select_explanation(Choice,Index,[S|Ss]):-
    Next is Index + 1,
    select_explanation(Choice,Next,Ss).

/****************************************************************************
 
                show_tree_explain
       Top routine for drawing the solution tree and moving up
      down it to enable explanation of each node.
 
 ****************************************************************************/

show_tree_explain(Tree):-
         show_tree1(Tree),
         write_prompts,
         read_word(Reply),
         ok_request(Reply,Valid_reply),
       ( do_command(Valid_reply,Tree,Tree);
         !,show_tree_explain(Tree)).


/***********************************************************************
 
                  show_tree1(2 & 3)
       routines for drawing the graph with each routine drawing
       its respective level.
 
 **********************************************************************/

show_tree1(nil):-
         nl,nl,
         write(' CANNOT TRAVERSE EMPTY BRANCH '),
         nl,
         pause(' to return to explaiation graph.... '),
         !,fail.

show_tree1(tree(Left,O/A/V # Cert was Explained,Right)):-
        clear_screen,
    tab(20),
        video(reverse),
    write(O),write('/'),write(A),write('/'),write(V),
        video(normal),
        nl,
        tab(24),
    write(' with certainty '),
    write(Cert),
        nl,
        tab(26),
    write('  was '),
    write(Explained),
        nl,
        show_tree2(Left,Right).

show_tree1(tree(Left,Goal # Cert was Explained,Right)):-
        clear_screen,
    tab(27),
        video(reverse),
    write(Goal),
         video(normal),
        nl,
        tab(24),
    write(' with certainty '),
    write(Cert),
        nl,
        tab(26),
    write('  was '),
    write(Explained),
        nl,
        show_tree2(Left,Right).

show_tree1(' ' # ' ' was Explained):-
        clear_screen,
        tab(25),
        video(reverse),
        write(Explained),
        video(normal),
        nl,nl,
        show_tree2(nil,nil).

show_tree2(nil,nil):-nl,nl.

show_tree2(NodeL,NodeR):-
    assign_sym_cert(NodeL,SymbolL),
    assign_sym_cert(NodeR,SymbolR),
    write_branches(SymbolL,SymbolR,' ' # ' ',' ' # ' ',34,5),
    create_list(NodeL,LL),
    create_list(NodeR,LR),
    write_nodes(LL,LR,[],[],4),
    siblings(NodeL,Left1,Right1),
    siblings(NodeR,Left2,Right2),
    show_tree3(Left1,Right1,Left2,Right2).

show_tree3(nil,nil,nil,nil):-nl,nl.

show_tree3(NodeL1,NodeR1,NodeL2,NodeR2):-
    assign_sym_cert(NodeL1,Sym1),assign_sym_cert(NodeR1,Sym2),
    assign_sym_cert(NodeL2,Sym3),assign_sym_cert(NodeR2,Sym4),
    write_branches(Sym1,Sym2,Sym3,Sym4,16,5),
    create_list(NodeL1,L1),create_list(NodeR1,L2),
    create_list(NodeL2,L3),create_list(NodeR2,L4),
    write_nodes(L1,L2,L3,L4,4).

/******************************************************************
 
     write_branches : routine to draw branches and inlude certainty
                      value along the branch.
 
 ******************************************************************/

write_branches(L1,R1,L2,R2,Indent,0).

write_branches(L1 # C1,R1 # C2,L2 # C3,R2 # C4,Firstindent,3):- /* write certainty value */
    Indent is Firstindent -6,
         tab(Indent),pnumber(C1),
        pnumber(C2),
        tab(21),pnumber(C3),
        pnumber(C4),nl,
        write_branches(L1 # C1,R1 # C2,L2 # C3,R2 # C4,Firstindent,2),!.

write_branches(Left1 # C1,Right1 # C2,Left2 # C3,Right2 # C4,Firstindent,Number):-
    Newno is Number - 1,
    Deviate is Number - 5,
    modulus(Deviate,Deviation),
    Indent is Firstindent - Deviation,
    tab(Indent),write(Left1),
    Dev is 2 * Deviation,
    tab(Dev),write(Right1),
        Next is 32 - Dev,
    tab(Next),write(Left2),
    tab(Dev),write(Right2),nl,
        write_branches(Left1 # C1,Right1 # C2,Left2 # C3,Right2 # C4,Firstindent,Newno),!.

/**********************************************************************
 
     write_nodes : writes nodes of tree in afixed size 'window'
 
 ***********************************************************************/

write_nodes(L1,R1,L2,R2,0).

write_nodes([Left1|L1],[Right1|R1],[],[],Number):-
        Newno is Number - 1,
    tab(18),pstring(Left1,20),
    tab(4),pstring(Right1,20),nl,
    write_nodes(L1,R1,[],[],Newno),!.

write_nodes([Left1|L1],[Right1|R1],[Left2|L2],[Right2|R2],Number):-
    Newno is Number - 1,
    tab(1),pstring(Left1,18),
    tab(2),pstring(Right1,18),
    tab(2),pstring(Left2,18),
    tab(2),pstring(Right2,18),nl,
    write_nodes(L1,R1,L2,R2,Newno),!.

/***************************************************************************/
/*          create_list altered to cope with structures: March '88: SS     */
/***************************************************************************/

create_list(nil,[[],[],[],[]]).

create_list(tree(_,O/A/V # C was Explained,_),[Obj, Att, Val, Expl]):-
    do_graemes_bit_about_creating_lists(O, Obj),
    do_graemes_bit_about_creating_lists(A, Att),
    do_graemes_bit_about_creating_lists(V, Val),
     name(Explained,Expl).

create_list(tree(_,Goal # C was Explained,_),[GoalList,Expl,[],[]]):-
        name(Goal,GoalList),
        name(Explained,Expl).

create_list(' ' # ' ' was Explained,[Explist,[],[],[]]):-
        name(Explained,Explist).

do_graemes_bit_about_creating_lists(V, Val):-
       atomic(V),
        name(V,Val).

do_graemes_bit_about_creating_lists(V, Val):-
    V =.. Univ_list,
        name_all(Univ_list, Messy_list),
    flatten(Messy_list, Val).

name_all([], []).

name_all([H|T], [X|Y]):-
    name_all_1(H, X),
    name_all(T, Y).

name_all_1(Vin, [Vout, 32]):-
    nonvar(Vin),
    name(Vin, Vout).

name_all_1(Vin, [86,32]):- /*** [86] = "V" - a first attempt to tidy vars ***/
    var(Vin).

flatten(Xs, Ys):-
    flatten1(Xs, Ys-[]).

flatten1([X|Xs], Ys-Zs):-
    flatten1(X, Ys-Ys1), 
    flatten1(Xs, Ys1-Zs).

flatten1(X, [X|Xs]-Xs):-
    atomic(X),
    X \== [].

flatten1([], Xs-Xs).

/***************************************************************************/
/*            end of alterations area: March '88: SS                       */
/***************************************************************************/

assign_sym_cert(nil,' ' # ' ').

assign_sym_cert(Tree,'*' # C):-
        get_certainty(Tree,C).

assign_sym_cert(_,'*' # ' ').

siblings(tree(Left,_,Right),Left,Right).

siblings(_,nil,nil).

/********************************************************************
 
            pnumber : prints certainty value in a fixed length
                      'window' of 5 chars.
                     restricts output to 2 d.p.
 
********************************************************************/

pnumber(' '):-
        tab(5).

pnumber(Number):-
        video(reverse),
        name(Number,ASCnum),
        width_out(ASCnum,0,5),
        video(normal).

width_out(_,Max,Max).

width_out([],2,Max):-
        put(46),
        width_out([],3,Max).

width_out([],Index,Max):-
        put(48),
        Next is Index+1,
        width_out([],Next,Max).

width_out([Char|Chars],0,Max):-
        ( Char==45,
          put(Char),
          width_out(Chars,1,Max);
          put(32),
          put(Char),
          width_out(Chars,2,Max)).

width_out([Char|Chars],1,Max):-
          put(Char),
          width_out(Chars,2,Max).

width_out([Char|Chars],Index,Max):-
          put(Char),
          Next is Index+1,
          width_out(Chars,Next,Max).

/**********************************************************************
 
      do_command:
        routine to handle the traversing of the tree
        Also copes with the explanation of the current top node
 
*********************************************************************/

do_command(parent,Tree,Original):-
         !,fail.

do_command(left,tree(Left,Goal # C was Exp,Right),Original):-
           show_tree1(Left),
           write_prompts,
           read_word(Reply),
           ok_request(Reply,Valid_reply),
         ( do_command(Valid_reply,Left,Original);
          !,do_command(default,tree(Left,Goal # C was Exp,Right),Original)).
          
do_command(right,tree(Left,Goal # C was Exp,Right),Original):-
           show_tree1(Right),
           write_prompts,
           read_word(Reply),
           ok_request(Reply,Valid_reply),
         ( do_command(Valid_reply,Right,Original);
           !,do_command(default,tree(Left,Goal # C was Exp,Right),Original)).

do_command(explain,tree(L,O/A/V # C was Exp,R),tree(L,O/A/V # C was Exp,R)):-
           clear_screen,
           nl,nl,
           write(' The '),write(O),write('''s '),write(A),write(' is '),nl,
           write(V),write(' with a certainty factor of '),write(C),write(' .'),nl,
           write(' It is also the top goal to this solution.'),nl,
           nl,
           pause(' to return to the explanation graph.... '),
           !,fail.

do_command(explain,tree(nil,O/A/V # C was 'found as fact',nil),Original):-
           clear_screen,
           nl,nl,
           write(' The '),write(O),write('''s '),write(A),write(' is '),nl,
           write(V),write(' with a certainty factor of '),write(C),write(' .'),nl,
           write(' This was found as a fact in the rule base, i.e. it was derived'),nl,
           write('from the answer''s given by the user'),
           nl,
           pause(' to return to the explanation graph.... '),
           !,fail.

do_command(explain,tree(nil,O/A/V # C was 'told by user',nil),Original):-
           clear_screen,
           nl,nl,
           write(' The '),write(O),write('''s '),write(A),write(' is '),nl,
           write(V),write(' with a certainty factor of '),write(C),write(' .'),nl,
           write(' This is information answered directly by the user.'),
           nl,
           pause(' to return to the explanation graph.... '),
           !,fail.

do_command(explain,tree(L,O/A/V # C was Exp,R),Original):-
           clear_screen,
           nl,nl,
           write(' The '),write(O),write('''s '),write(A),write(' is '),nl,
           write(V),write(' with a certainty factor of '),write(C),write(' .'),nl,
           write(' It is combined with another goalto give the final solution.'),
           nl,
           pause(' to return to the explanation graph.... '),
           !,fail.

do_command(explain,tree(L,'AND' # C was Exp,R),Original):-
           clear_screen,
           nl,nl,
           write(' This is the head of a conjunction of two branches and has '),nl,
           write('a certainty factor of '),write(C),write(' .'),nl,
           write(' This factor is taken as the minimum of the two branches.'),
           nl,
           pause(' to return to the explanation graph.... '),
           !,fail.

do_command(explain,tree(L,'OR' # C was Exp,R),Original):-
           clear_screen,
           nl,nl,
           write(' This is the head of a disjunction of two branches and has '),nl,
           write('a certainty factor of '),write(C),write(' .'),nl,
           write(' This factor is taken as the maximum of the two branches.'),
           nl,
           pause(' to return to the explanation graph.... '),
           !,fail.

do_command(explain,tree(L,'false ' # C was Exp,R),Original):-
           clear_screen,
           nl,nl,
           write(' This is the head of a negation of a branch and has '),nl,
           write('a certainty factor of '),write(C),write(' .'),nl,
           write(' This factor is taken as the negimuation of the invert'),
            nl,
           pause(' to return to the explanation graph.... '),
           !,fail.

do_command(explain,tree(L,Rule # C was Exp,R),Original):-
           clear_screen,
           nl,nl,
           do_show_specified_rule(Rule),
           nl,
           write(' This is how the rule is represented in the rule base.'),nl,
           write(' It has a certainty factor of '),write(C),write(' .'),nl,
           write(' This is calculated as a function of the antecedent''s certainty'),nl,
           write('and the consequent''s certainty.'),
            nl,
           pause(' to return to the explanation graph.... '),
            !,fail.

do_command(explain,' ' # ' ' was 'insignificant to goal',Original):-
           clear_screen,
           nl,nl,
           write(' This branch did not need to be followed since the certainty'),nl,
           write('factor of the other could not be changed by any value.'),
           pause(' to return to the explanation graph.... '),
           !,fail.

do_command(quit,Tree,Original).

do_command(default,Tree,Original):-
          show_tree1(Tree),
          write_prompts,
          read_word(Reply),
          ok_request(Reply,Valid_reply),!,
          do_command(Valid_reply,Tree,Original).

write_prompts:-
          nl,
          video(reverse),
          write('  {P}arent , {L}eft , {R}ight , {E}xplain or {Q}uit ...? '),
          video(normal).

ok_request(Reply,Valid_reply):-
          pseudo(Reply,parent,Valid_reply);
          pseudo(Reply,left,Valid_reply);
          pseudo(Reply,right,Valid_reply);
          pseudo(Reply,explain,Valid_reply);
          pseudo(Reply,quit,Valid_reply);
          pseudo(Reply,other,Valid_reply).

pseudo('P',parent,parent).
pseudo(p,parent,parent).
pseudo(parent,parent,parent).
pseudo('PARENT',parent,parent).

pseudo('L',left,left).
pseudo(l,left,left).
pseudo('LEFT',left,left).
pseudo(left,left,left).

pseudo('R',right,right).
pseudo(r,right,right).
pseudo('RIGHT',right,right).
pseudo(right,right,right).

pseudo('E',explain,explain).
pseudo(e,explain,explain).
pseudo('EXPLAIN',explain,explain).
pseudo(explain,explain,explain).

pseudo('Q',quit,quit).
pseudo(q,quit,quit).
pseudo(quit,quit,quit).
pseudo('QUIT',quit,quit).

pseudo(_,other,default).

/****************************************************************************/
/*********** END OF PROGRAM: show_answer ************************************/
/****************************************************************************/
