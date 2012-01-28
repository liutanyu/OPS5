/*  MAIN.PL  */
/*  Shelved on the 3rd of October 1988  */


/**************************************************************************/
/************************ PROGRAM: main ***********************************/
/**************************************************************************
 
    This program may be used and amended for non-profit making
    purposes on condition that this header be left intact and in situ.
 
    Copyright: Steven Salvini 1986, 1987, 1988
 
    Amended:
            Steven Salvini: 1988
 
 **************************************************************************/

threshold(10).      /* The value to be accepted as "100% true" */

/************************************************************************** 
 
    go:
    program entry point
 
 **************************************************************************/

go:-
    new_prompt,
    title_page(first, 'protest'),
    nl, nl, nl, nl, nl, nl,
    pause(Default_Message),
    clear_screen,
    main_menu('Son of Protest'),
    !,
    fail.

/************************************************************************** 
 
    main_menu:
    Prints appropriate menu and inputs the user's selection
 
 **************************************************************************/

main_menu(Caller):-
    repeat,
    clear_screen,
    write_caller(Caller),
    nl,
    write(' MAIN MENU  '),nl,
    write(' ~~~~~~~~~  '),nl,
    write(' #1   Consult Expert System in backward chaining mode'),nl,
    write(' #2   Consult Expert System in mixed mode'),nl,
    write(' #3   Knowledge Base Examination Menu'),nl,
    write(' #4   QUIT'),nl,
    write(' #0   HELP'),nl,nl,
    write('Choose an option by entering its code number...'),nl,
    input_number(0,4,Choice),
    do_main_choice(Choice, Caller),
    !.

/**************************************************************************
 
    do_main_choice:
    Acts on user's selection from main_menu
 
 **************************************************************************/

do_main_choice(0, _):-  /* help/instructions */
    clear_screen,
    write('Please select the option required by entering the associated number'),
    nl,
    pause(' to return to main menu ...'),
    !,
    fail.

/**************************************************************************/

do_main_choice(X, _):-  /* quit */
    (X = quit; X = 4),
    clear_screen,
    !.

/**************************************************************************/

do_main_choice(1, _):-  /* backward chaining mode only */
    clear_screen,
    check_any_rb_loaded,
    tidy_dynamic_kb,
    investigate(Answer_list,noforward),
    sort_answer(Answer_list,Sorted_Answers),
    clear_screen,
    show_diff_diag(Sorted_Answers),
    explain(Sorted_Answers),
    !,
    fail.

/**************************************************************************/

do_main_choice(2, _):-  /* input findings then process them in */
    clear_screen,       /* silent mode (and then backward chaining mode */
    check_any_rb_loaded,    /* if no firm diagnosis yet reached) */
    tidy_dynamic_kb,
    input_history,  
    clear_screen,
    investigate(Answer_list,forward),
    sort_answer(Answer_list,Sorted_Answers),
    clear_screen,
    show_diff_diag(Sorted_Answers),
    explain(Sorted_Answers),
    !,
    fail.

/**************************************************************************/

tidy_dynamic_kb:-       /* begin input of history with no facts in db */
    clear_screen,
    retract_all(fact),
    retract_all(told),
    retract_all(derived).

/**************************************************************************
 
    do_main_choice(3):
    Prints KB Examination menu and inputs the user's selection
 
 **************************************************************************/

do_main_choice(3, Caller):- /* KB Examination menu */
    repeat,
    clear_screen,
    write_caller(Caller),
    write(' KNOWLEDGE BASE EXAMINATION MENU  '),nl,
    write(' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  '),nl,
    write(' #1   Show all facts'),nl,
    write(' #2   Show top_level_goals'),nl,
    write(' #3   Show all rules'),nl,
    write(' #4   Show rule by number'),nl,
    write(' #5   Show rules containing X'),nl,
    write(' #6   Show all questions'),nl,
    write(' #7   Show question by number'),nl,
    write(' #8   Show questions containing X'),nl,
    write(' #9   EXIT TO MAIN MENU'),nl,
    write(' #0   HELP'),nl,nl,
    write('Choose an option by entering its code number...'),nl,
    input_number(0,9,Choice),
    do_te_choice(Choice),
    !,
    fail.

/**************************************************************************
 
    do_te_choice:
    Acts on user's selection from case_menu
 
 **************************************************************************/

do_te_choice(0):-   /* help */
    clear_screen,
    write('Please select the option required by entering the associated number'),
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

/**************************************************************************/

do_te_choice(1):-   /* show all facts */
    clear_screen,
    show_all_facts,
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

/**************************************************************************/

do_te_choice(2):-   /* show top_level_goals */
    clear_screen,
    show_all_goals,
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

/**************************************************************************/

do_te_choice(3):-   /* show all rules */
    clear_screen,
    show_all_rules,
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

/**************************************************************************/

do_te_choice(4):-   /* show rule X */
    repeat,
    clear_screen,
    show_specified_rule(Rule, rule),
    nl,nl,nl,
    write('Do you want to examine another rule? (yes/no) '),
    nl,
    input_yes_no(Reply),
    do_te_another(Reply),
    !,
    fail.

/**************************************************************************/

do_te_another(quit).
do_te_another(no).
do_te_another(yes):-
    fail.

/**************************************************************************/

do_te_choice(5):-   /* What rules contain X */
    clear_screen,
    nl,
    write('Please enter the word for which to search... '),
    nl,
    read_word(Target,blanks),
    bagof(Rule:if Ants then Cond with Certainty,
          Rule:if Ants then Cond with Certainty,Rules),
    hunt_target_in_rules(Rules,Target),
    nl,
    write('No (more) instances found '),
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

do_te_choice(5):-   /* What rules contain X - no rules found! */
    nl,
    write('No rules found '),
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

/**************************************************************************/

hunt_target_in_rules([],_).

hunt_target_in_rules([R|Rs],T):-
    aux_htr(R,T),
    hunt_target_in_rules(Rs,T).

aux_htr(Rule:if Ants then Cond with Certainty,T):-
    (
        hunt_t_in_triples(Ants,T);
        hunt_t_in_triples(Cond,T)
    ),
    report_result_aht(Rule:if Ants then Cond with Certainty,T).

aux_htr(_,_).

report_result_aht(Rule,_):-
    clear_screen,
    nl,
    show_rule(Rule),
    nl,nl,
    pause(' to continue searching...').

hunt_t_in_triples(Triple and Triples,T):-
    aux_htit(Triple,T);
    hunt_t_in_triples(Triples,T).

hunt_t_in_triples(Triple or Triples,T):-
    aux_htit(Triple,T);
    hunt_t_in_triples(Triples,T).

hunt_t_in_triples(Triple or false Triples,T):-
    aux_htit(Triple,T);
    hunt_t_in_triples(Triples,T).

hunt_t_in_triples(Triple and false Triples,T):-
    aux_htit(Triple,T);
    hunt_t_in_triples(Triples,T).

hunt_t_in_triples(Triple,T):-
    aux_htit(Triple,T).

aux_htit(O/A/V,T):- O == T.

aux_htit(O/A/V,T):- A == T.

aux_htit(O/A/V,T):- V == T.

/**************************************************************************/

do_te_choice(6):-   /* show all questions */
    clear_screen,
    show_all_questions,
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

/**************************************************************************/

do_te_choice(7):-   /* show question X */
    repeat,
    clear_screen,
    show_specified_question(Question),
    nl,
    write('Do you want to try another? '),
    input_yes_no(Reply),
    do_te_another(Reply),
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

/**************************************************************************/

do_te_choice(8):-   /* What question contains X */
    clear_screen,
    nl,
    write('Search for what? '),
    nl,
    read_word(Target,blanks),
    bagof(Q:O/A/V-M-Tt,Q:O/A/V-M-Tt,Questions),
    hunt_target_in_questions(Questions,Target),
    nl,
    write('No (more) instances found '),
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

do_te_choice(8):-   /* What question contains X - no questions found! */
    nl,
    write('No questions found '),
    nl,
    pause(' to return to knowledge base examination menu ...'),
    !,
    fail.

hunt_target_in_questions([],_).

hunt_target_in_questions([Q|Qs],T):-
    aux_htq(Q,T),
    hunt_target_in_questions(Qs,T).

aux_htq(Q:Triple-M-Tt,T):-
    aux_htitq(Triple,T),
    show_question(Q:Triple-M-Tt),
    nl,
    pause(' to continue searching...').

aux_htq(_,_).

aux_htitq(T/A/Vs,T).
aux_htitq(O/T/Vs,T).
aux_htitq(O/A/Values,T):-
    member(T,Values).

/**************************************************************************/

do_te_choice(9).    /* exit to main menu */
do_te_choice(quit). /* exit to main menu */

/**************************************************************************
 
    show_***:
    The following procedures display rules, questions, goals & facts
 
 **************************************************************************/

show_all_goals:-
    top_level_goals: O - A - Goals,
    print_menu_lines(Goals,1,Last).

show_all_goals:-
    nl,
    write('There are no goals in the current knowledge base'),
    nl.

/**************************************************************************/

show_specified_rule(Rule, Type):-
    write('Please enter the number of the rule required...'),
    nl,
    input_integer(I),
    !,
    do_ssr(I,Rule, Type),
    !.

do_ssr(quit,_, _).

do_ssr(I,Rule, Type):-
    conc_atom(Type,I,Rule),
    do_show_specified_rule(Rule),
    !.

do_show_specified_rule(Rule):-
    clear_screen,
    Rule:Body,
    show_rule(Rule:Body).

do_show_specified_rule(Rule):-
    write(Rule),
    write(' not found!!!'),
    nl.

/**************************************************************************/

show_all_facts:-
    told:Fact,
    show_fact(told:Fact),
    fail.

show_all_facts:-
    fact:Fact,
    show_fact(fact:Fact),
    fail.

show_all_facts:-
    nl,
    write('No (more) facts').

show_fact(Type:Triple with certainty(Certainty)):-
    clear_screen,
    nl,
    write(Type),
    write(':'),
    nl,
    tab(6),
    write(Triple),
    nl,
    tab(6),
    write('with certainty('),
    write(Certainty),
    write(')'),
    nl,nl,nl,
    pause(' to show next fact...'),
    !.

/**************************************************************************/

show_all_rules:-
    Type:Body,
    clause_is_a(rule,Type),
    show_rule(Type:Body),
    nl,nl,
    pause(' to show next rule...'),
    fail.

show_all_rules:-
    nl,
    write('No (more) rules').

show_rule(Rule:if Conds then Goal with certainty(Certainty)):-
    write(Rule),
    write(':'),
    nl,
    tab(6),
    write(' if   '),
    show_conds(Conds),
    nl,
    tab(6),
    write(' then '),
    nl,
    tab(12),
    write(Goal),
    nl,
    tab(18),
    write('with certainty('),
    write(Certainty),
    write(')'),
    nl,
    !.

show_rule(_):-
    write('The rule found is of an unknown format which cannot be shown.'),
    nl,
    !.

show_conds(Cond1 and Conds):-   
    show_conds(Cond1),
    nl,
    tab(12),
    write(and),
    show_conds(Conds).

show_conds(Cond1 or Conds):-    
    show_conds(Cond1),
    nl,
    tab(12),
    write(or),
    show_conds(Conds).

show_conds(false Cond):-
    nl,
    tab(15),
    write('NOT '),
    write(Cond).

show_conds(Cond):-
    nl,
    tab(15),
    write(Cond).

/**************************************************************************/

show_all_questions:-
    Type:Body,
    clause_is_a(question,Type),
    show_question(Type:Body),
    pause(' to show next question...'),
    fail.

show_all_questions:-
    nl,
    write('No (more) questions').

show_specified_question(Question):-
    write('# of question required...'),
    nl,
    input_integer(I),
    !,
    do_ssq(I,Question),
    !.

do_ssq(quit,_).

do_ssq(I,Question):-
    conc_atom(question,I,Question),
    do_show_specified_question(Question),
    !.

do_show_specified_question(Question):-
    Question:Body,
    show_question(Question:Body).

do_show_specified_question(Question):-
    write(Question),
    write(' not found!!!'),
    nl,nl.

show_question(Question:O/A/Values - Message - Type):-
    clear_screen,
    nl,
    write(Question),
    write(':'),
    nl,
    tab(9),
    write('The '),
    write(A),
    write(' for '),
    write(O),
    write(' may be -> '),
    show_values(Values,12),
    nl,nl,
    tab(9),
    show_rule_type(Type),
    nl,nl,
    tab(9),
    write('The associated text is...'),
    nl,
    tab(12),
    write('"'),
    pstring(Message),
    write('"'),
    nl,nl,nl.

show_values([],_).
show_values([V|Vs],Indent):-
    nl,
    tab(Indent),
    write(V),
    show_values(Vs,Indent).

show_rule_type(opposite):-
    write('These are treated as opposites.'),
    nl.

show_rule_type(single):-
    write('These are treated as mutually exclusive.'),
    nl.

show_rule_type(multiple):-
    write('Any, all or none of these may be true in a particular case.'),
    nl.

/**************************************************************************/
/***************** END OF PROGRAM: main ***********************************/
/**************************************************************************/
