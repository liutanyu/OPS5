/*  EXPLORE.PL  */
/*  Shelved on the 3rd of October 1988  */



/**************************************************************************/
/************************ PROGRAM: explore ********************************/
/**************************************************************************
 
    This program may be used and amended for non-profit making
    purposes on condition that this header be left intact and in situ.
 
        Copyright: Steven Salvini   1986, 1987, 1988
 
            Amended -
 
                Graeme Sandieson    1987
                Steven Salvini      1987, 1988
 
 **************************************************************************/
 
/**************************************************************************
 
    explore:
    The main part of the 'Inference Engine'
    Tries to deduce the system's top level goals & at the same time
    builds up a trace of its actions. (from an idea in 'Bratko, 1986')
 
 **************************************************************************/

explore(Goal1 and Goal2,Trace,
        tree(Answer1,'AND' # C was 'derived from',Answer2),Mode):-
    !,
    explore(Goal1,Trace,Answer1,Mode),
        get_certainty(Answer1,C1),!,
      ( found(C1,and,Answer2,C);
    explore(Goal2,Trace,Answer2,Mode),
    get_certainty(Answer2,C2),
        min(C1,C2,C) ).

explore(Goal1 or Goal2,Trace,
        tree(Answer1,'OR' # C was 'derived from',Answer2),Mode):-
    !,
    explore(Goal1,Trace,Answer1,Mode),
        get_certainty(Answer1,C1),!,
      ( found(C1,or,Answer2,C);
    explore(Goal2,Trace,Answer2,Mode),
    get_certainty(Answer2,C2),
    max(C1,C2,C) ).

explore(false Goal,Trace,tree(nil,'false ' # Certainty was 'derived from',Answer),Mode):-
    !,
    explore(Goal,Trace,Answer,Mode),
    get_certainty(Answer,C),
    invert(C,Certainty).

explore(O/A/V,Trace,tree(nil,O/A/V # Certainty was 'found as fact',nil),Mode):-
    fact:O/A/V with certainty(Certainty).

explore(O/A/V,Trace,tree(nil,O/A/V # Certainty was 'told by user',nil),Mode):-
    told:O/A/V with certainty(Certainty).

explore(O/A/V,Trace,Tree,Mode):-
    derived:O/A/V-Tree.

explore(O/A/V,Trace,tree(nil,O/A/V # 1 was 'found in model',nil),Mode):-
    model_fact:O/A/V.

explore(O/A/Value,
    Trace, tree(nil, O/A/Function # Certainty_out was 'derived from', tree(nil, O/A/V # 1 was Found, nil)), Mode):-
            (fact:O/A/V with Certainty_in, Found = 'found as fact';
             told:O/A/V with Certainty_in, Found = 'told by user';
             model_fact:O/A/V, Found = 'found in model', Certainty_in=1),
         Function =.. List,
         [Value, Certainty_in, Certainty_out|List] =.. Full_function,
         call(Full_function).

explore(O/A/V,Trace,tree(Left,O/A/V # Certainty was 'derived by',Right),Mode):-
    bagof(Rule:if Condition then O/A/V with Strength,
        (Rule:if Condition then O/A/V with Strength, clause_is_a(rule, Rule)), Rules),
    modify(Rules,Trace,Certainty,Left,Right,Mode),
    asserta(derived:O/A/V-tree(Left,O/A/V # Certainty was 'derived by',Right)).

explore(O/A/V,Trace,tree(Left,O/A/V # Certainty was 'derived by',Right),Mode):-
    bagof(Rule:if Condition then O/A/V with Strength,
        (Rule:if Condition then O/A/V with Strength, clause_is_a(model_rule, Rule)), Rules),
    modify(Rules,Trace,Certainty,Left,Right,Mode),
    asserta(derived:O/A/V-tree(Left,O/A/V # Certainty was 'derived by',Right)).

explore(O/A/V,Trace,tree(nil,O/A/V # Certainty was 'told by user',nil),Mode):-
    not( not_askable(O, A, V, _) ),
    user_answer(O/A/V,Trace,Certainty).

explore(O/A/V,Trace,tree(nil,O/A/V # C was 'assumed as unknown',nil),Mode):-
    not_askable(O, A, V, C).

/* These are the "functions" that rules MAY use - possibly still pretty buggy */

between(X, C, C, A, B):-
    X >= A,
    X =< B.

between(X, Cin, Cout, A, B):-
    (X < A;
     X > B),
    invert(Cin, Cout).

greater_than(X, C, C, A):-
    X > A.

greater_than(X, Cin, Cout, A):-
    X =< A,
    invert(Cin, Cout).

less_than(X, C, C, A):-
    X < A.

less_than(X, Cin, Cout, A):-
    X >= A,
    invert(Cin, Cout).

/**************************************************************************
 
    found : checks if certainty is in state that no further processing
            could change it.
 
 **************************************************************************/

  found(-1,and,' ' # ' ' was 'insignificant to goal',-1).

  found(1,or,' ' # ' ' was 'insignificant to goal',1).

/**************************************************************************
 
    investigate:
                solves in three versions
 
    "backward" or interactive - 
        true backward-chaining inference.
 
    "forward" - 
                forward chains through every rule in the K.B.
 
    "verbose" - 
        as for "backward" plus system shows on-screen the rules being
        used to try to deduce the current goal, outputing the cetainty
        value thus produced for each instance of a rule firing.
 
 **************************************************************************/

investigate(Answer_list, Mode):-    /*** MEGA-HACK!!! to avoid "threshold" ***/
    investigate(10, Answer_list, Mode). /* Bratko uses a threshold, this */
                                /* version of PROTEST does not! */

investigate(Threshold,Answer_list,verbose):-
        clear_screen,
    write('Processing in "tracing" mode, please be patient...'),
    nl,
    get_top_level_goals(Goal_list),
        goal_in_mind(Goal_list,Ordered),
    solve(Ordered,Threshold,[],Answer_list,verbose).

investigate(Threshold,Answer_list,noforward):-
    clear_screen,
    write('Processing in backward chaining mode, please be patient...'),
    nl,
    get_top_level_goals(Goal_list),
        goal_in_mind(Goal_list,Ordered),
    solve(Ordered,Threshold,[],Answer_list,backward).

investigate(Threshold,Answer_list,forward):-
    clear_screen,
    write('Processing in mixed mode, please be patient...'),
    nl,
    get_top_level_goals(Top_level_goals),
        forward_solve(Forward_Answer_list,Top_level_goals),
        sort_answer(Forward_Answer_list,F_A_list),
    more_investigation(F_A_list,Threshold,Top_level_goals,Answer_list).

/**************************************************************************
 
    get_top_level_goals & make_OAV_triples:
    these two procedures construct the list of goals the system
    tries to deduce
 
 **************************************************************************/

get_top_level_goals(Goal_triples):-
    top_level_goals:Object-Attribute-Top_level_goals,
    make_OAV_triples(Top_level_goals,Object,Attribute,Goal_triples).

get_top_level_goals(_):-        /*** Should never reach this point!!! ***/
    write('There are NO goals to try to prove!!!'), nl,
    abort.

make_OAV_triples([],_,_,[]).
make_OAV_triples([H|T],Obj,Att,[Obj/Att/H|Rest]):-
    make_OAV_triples(T,Obj,Att,Rest).

make_OAV_triples([],_,_,All_goals, All_goals).
make_OAV_triples([F|Fs], Gs, Obj, Goals_in, Goals_out):-
    aux_make_OAV_triples(F, Gs, Obj, [], Goals_SF),
    conc(Goals_SF, Goals_in, All_goals_SF),
    make_OAV_triples(Fs, Gs, Obj, All_goals_SF, Goals_out).

aux_make_OAV_triples(_, [], _, Gsf, Gsf).
aux_make_OAV_triples(F, [G|Gs], Obj, Gsf_in, Gsf_out):-
    aux_make_OAV_triples(F, Gs, Obj, [Obj/F/G|Gsf_in], Gsf_out).
    
/************************************************************************
 
       goal_in_mind : asks user if there is a goal that would be most
                      likely to succeed and re-orders the goal-list
                      so that this can be tested first.
 
 ***********************************************************************/

goal_in_mind(Top_level_goals,Revised_order):-
        nl,nl,
        write('  Do you have any suggestions for a possible answer ? ---'),
        write('(y/n).'),nl,
        input_yes_no(Reply),
       ( Reply == yes,
         get_most_likely(Answer,Top_level_goals),
        most_likely_first(Answer,Top_level_goals,Revised_order);
        fail ),!.

goal_in_mind(Top_level_goals,Top_level_goals).

get_most_likely(Answer,Top_level_goals):-
        nl,nl,nl,
        write('  Enter the correct spelling and punctuation of your'),
        write(' suggestion .'),nl,
        input_answer(Answer,Top_level_goals).

input_answer(O/A/Reply,Top_level_goals):-
        repeat,
        read_word(Reply,blanks),
      ( member(O/A/Reply,Top_level_goals);
        nl,
        write(' Your input is incorrect !'),nl,
        write(' It has to be one of :- '),nl,
        writeall(Top_level_goals),
        fail ),!.

writeall([]):-
        nl.

writeall([O/A/X|Y]):-
        nl,
        tab(4),
        write(X),
        writeall(Y),!.

most_likely_first(O/A/V,Top_level_goals,Revised_order):-
        add(O/A/V,Top_level_goals,Big_list),
        remove_duplicates(Big_list,[],Right_size),
        reverse(Right_size,Revised_order).

most_likely_first(Answers,Top_level_goals,Revised_order):-
        remove_goal(Answers,[],Clean_list),
        check_if_OAV(Clean_list,[],Sorted_clean),
        conc(Sorted_clean,Top_level_goals,Big_list),
        remove_duplicates(Big_list,[],Right_size),
        reverse(Right_size,Revised_order).

add(X,Y,[X|Y]).

remove_goal([],[],[]).

remove_goal([],List,List).

remove_goal([Cert - tree(_,Goal # C was _,_)|Rest],List,Clean_list):-
        remove_goal(Rest,[Goal|List],Clean_list),!.

check_if_OAV([],List,List).

check_if_OAV([O/A/V|Rest],Temp,List):-
    check_if_OAV(Rest,[O/A/V|Temp],List),!.

check_if_OAV([Rule|Rest],Temp,List):-
    Rule:if Cond then Goal with Strength,
    check_if_OAV(Rest,[Goal|Temp],List),!.

/*********************************************************************
 
       forward_solve:
       forward chains through the entire rule base and solves
       every rule using default 'unknown'-assumption then
       returns the top level solutions.
 
        Added by Graeme Sandieson 1987
 
        Nota Bene:  This is NOT true forward chaining as it does
                not "restart" each time a new datum is added 
                to the database THUS the rules must be in the
                correct order (hierarchically) in the rulebase.
                (This is, however, a BETTER attempt than the
                 previous version!) - SS: April 1988.
 
        Amended by Steven Salvini: April 1988
 
 *********************************************************************/

forward_solve(Forward_Answer_list,Top_Goals):- 
        forward_group,
        get_top_level_derives(Forward_Answer_list,[],Top_Goals).

get_top_level_derives(F_A_list,F_A_list,[]):-!.

get_top_level_derives(Temp,Answers,[Goal|Goals]):-
          derived:Goal-tree(L,Goal # C was Exp,R),
        ( C > 0,
          get_top_level_derives(Temp,[C - tree(L,Goal # C was Exp,R)|Answers],Goals),!;
          get_top_level_derives(Temp,Answers,Goals),!).

get_top_level_derives(Temp,Answers,[Goal|Goals]):-
          derived:Goal-Tree,
          get_certainty(Tree,Cert),
        ( Cert > 0,
          get_top_level_derives(Temp,[Cert - tree(Tree,Goal # Cert was 'derived by',nil)|Answers],Goals),!;
          get_top_level_derives(Temp,Answers,Goals),!).

get_top_level_derives(Temp,Answers,[Goal|Goals]):-
          not( derived:Goal-Tree ),
          get_top_level_derives(Temp,Answers,Goals),!.

forward_group:-
        bagof(Rule:if Antecedent then Goal with certainty(Rcert),
        Rule:if Antecedent then Goal with certainty(Rcert), Rulelist),
        forward(Rulelist).

/************************************************************************
 
     forward : main forward chaining engine
 
 ************************************************************************/

forward([]).

forward([Rule:if Cond then Goal with certainty(Rcert)|Rules]):-
       (ante_solve(Cond,Answer),
        get_certainty(Answer,Antecert),
        modify_rule_cert(Rcert,Antecert,Rulecert),
        add_to_answers(Answer,Goal,Rule,Rulecert);
        true),
        forward(Rules),!.

add_to_answers(Answer,Goal,Rule,Rulecert):-
        retract_goal(Goal,tree(L1,Goal # Gcert was Exp,R1)),
        ascertain(Rulecert,Gcert,Finalcert),
        label_tree(Goal,Finalcert,tree(L1,Goal # Gcert was Exp,R1),
                      tree(Answer,Rule # Rulecert was 'derived by',nil),Tree),
        asserta(derived:Goal-Tree).

add_to_answers(Answer,Goal,Rule,Rulecert):-
        label_tree(Rule,Rulecert,Answer,nil,Tree),
        asserta(derived:Goal-Tree).

retract_goal(Goal,Tree):-
        derived:Goal-Tree,
        retract(derived:Goal-Tree).

ante_solve(false Antecedent,tree(nil,'false ' # Invcert was 'derived from',Answer)):-
        !,
        ante_solve(Antecedent,Answer),
        get_certainty(Answer,Antecert),
        invert(Antecert,Invcert).

ante_solve(Ante1 and Ante2,tree(Answer1,'AND' # Anscert was 'derived from',Answer2)):-
        !,
        ante_solve(Ante1,Answer1),
        get_certainty(Answer1,Acert1),
       (found(Acert1,and,Answer2,Anscert);
        ante_solve(Ante2,Answer2),
        get_certainty(Answer2,Acert2),
        min(Acert1,Acert2,Anscert)).

ante_solve(Ante1 or Ante2,tree(Answer1,'OR' # Anscert was 'derived from',Answer2)):-
        !,
        ante_solve(Ante1,Answer1),
        get_certainty(Answer1,Acert1),
       (found(Acert1,or,Answer2,Anscert);
        ante_solve(Ante2,Answer2),
        get_certainty(Answer2,Acert2),
        max(Acert1,Acert2,Anscert)).

ante_solve(Antecedent,tree(nil,Antecedent # Antecert was 'found as fact',nil)):-
        fact:Antecedent with certainty(Antecert).

ante_solve(Antecedent,tree(nil,Antecedent # Antecert was 'told by user',nil)):-
        told:Antecedent with certainty(Antecert).

ante_solve(Antecedent,Tree):-
        derived:Antecedent-Tree.

ante_solve(Antecedent, tree(nil, Antecedent # 1 was 'found in model',nil)):-
        model_fact:Antecedent.

ante_solve(O/A/Function, tree(nil, O/A/Function # Certainty_out was 'derived from', tree(nil, O/A/V # 1 was Found, nil))):-
            (fact:O/A/V with Certainty_in, Found = 'found as fact';
             told:O/A/V with Certainty_in, Found = 'told by user';
             model_fact:O/A/V, Found = 'found in model', Certainty_in=1),
         Function =.. List,
         [Value, Certainty_in, Certainty_out|List] =.. Full_function,
         call(Full_function).

ante_solve(Antecedent,tree(nil,Antecedent # 0 was 'assumed as unknown',nil)).

/**************************************************************************
 
    more_investigation:
    continue after forward-chaining only if answer not already
    deduced
 
 **************************************************************************/

more_investigation(Forward_Answer_list,Threshold,Top_level_goals,Answer_list):-
    write('Do you want to see the current differential diagnosis? (y/n)'),
    input_yes_no(Reply),
    show_dd_mi(Reply,Forward_Answer_list),
        nl,nl,
        write('Would you like to continue try to narrow down the diagnosis '),
        write('by providing more '),nl,write('   information.... (y/n) ?'),
        input_yes_no(Rep2),
      ( Rep2==no,
        Forward_Answer_list = Answer_list;
        clear_screen,
    write('Processing in backward chaining mode, please be patient...'),
    nl,
        most_likely_first(Forward_Answer_list,Top_level_goals,Revised_order),
        retract_all(derived),
       retract_unknowns,
    solve(Revised_order,Threshold,[],Answer_list,backward)).

/**************************************************************************
 
    show_dd_mi:
    shows differential diagnosis list if requested
 
 **************************************************************************/

show_dd_mi(quit,_).
show_dd_mi(no,_).
show_dd_mi(yes,Answer):-
    show_diff_diag(Answer).

/**************************************************************************
 
    solve:
    'explores' all the 'Goals' in a 'Goal_list'
 
 **************************************************************************/

solve([],_,[],_,_):-        /* ERROR in user's input */
    write('Your goal-list was originally empty!!!'),
    nl.

solve([],_,Answer,Answer,_).

solve([Goal|Goals],Threshold,Answer_list,Final_Answer,Mode):-
    explore(Goal,Trace,Answer,Mode),
    get_certainty(Answer,Certainty),
    more_solve(Goal,Goals,Threshold,[Certainty - Answer|Answer_list],
         Final_Answer,Mode).

/**************************************************************************
 
    sort_answer:
    sorts answers into descending order of certainty
 
 **************************************************************************/

sort_answer(Answer,Descending_Sorted):-
    keysort(Answer,Ascending_Sorted),
    reverse(Ascending_Sorted,Descending_Sorted).

/**************************************************************************
 
    show_diff_diag:
    shows differential diagnosis list
 
 **************************************************************************/

show_diff_diag([]):-
    write('No differential diagnosis is possible from '),
    write('the information given '),
    nl,nl,nl,nl,nl,nl,
    pause(Default_Message).

show_diff_diag([CF - _ | _]):-
    CF =< 0,
    write('No differential diagnosis is possible from '),
    write('the information given '),
    nl,nl,nl,nl,nl,nl,
    pause(Default_Message).

show_diff_diag([Answer]):-
    write('The Diagnosis is...'),
    nl,nl,nl,
    show_solve([Answer]),
    nl,nl,nl,nl,nl,nl,
    pause(Default_Message).

show_diff_diag(Answer):-
    nl,nl,nl,nl,nl,nl,
    write('Differential Diagnosis'),nl,
    write('~~~~~~~~~~~~~~~~~~~~~~'),
    nl,nl,
    show_solve(Answer),
    nl,nl,nl,nl,nl,nl,
    pause(Default_Message).

/**************************************************************************

    more_solve:
    stops processing of 'Goal_list' by 'solve' if an answer with
    certainty > threshold is found

**************************************************************************/

more_solve(Goal,Goals,Threshold,[Certainty - Answer|Answers],[Certainty - Answer],_):-
    Certainty >= Threshold,
        nl,nl,nl,
         write('  A solution hes been found -- carry on and find more ? (y/n)'),
        input_yes_no(Reply),
      ( Reply==no;
        fail).

more_solve(_,Goals,Threshold,Answers,Final_Answer,Mode):-
    solve(Goals,Threshold,Answers,Final_Answer,Mode).

/**************************************************************************
 
    show_solve:
    shows differential diagnosis list only as far as the last 
    'Goal' with a non-zero certainty
 
 **************************************************************************/

show_solve([]).
show_solve([CF - _|_]):-    /* 'Short-circuits' showing diagnoses with */
    CF =< 0.        /* certainties of 0.  (ie still 'unknown') */

show_solve([Certaintykey - tree(_,O/A/V # Certaintykey was _,_)|More]):-
    write(O),write('/'),write(A),write('/'),write(V),
    htab,
    write(' with certainty '),
    htab,
    write(Certaintykey),
    nl,
    show_solve(More).

show_solve([Certaintykey - tree(_,Rule # Certaintykey was _,_)|More]):-
    Rule:if Cond then Goal with Strength,
    write(Goal),
    htab,
    write(' with certainty '),
    htab,
    write(Certaintykey),
    nl,
    show_solve(More).

/**************************************************************************
 
    modify:
    processes the certainty of current 'Goal'
 
 **************************************************************************/

modify([],_,0,nil,nil,Mode).

modify([Rule:if Cond then Goal with certainty(Rcert)|Rules],Trace,Fincert, Left,Right,Mode):-
    solve_and_mod(Cond,Goal,Rule,Rcert,Trace,Answer,Newcert,Mode),
        label_tree(Rule,Newcert,Answer,nil,Rule_tree),
    create_soln_tree(Newcert,Rules,Trace,Rule_tree,nil,Fincert,Left,Right,Mode).

/***************************************************************************
 
      create_soln_tree :  routine to create the solution tree after
                          a rule has been solved.
 
 ***************************************************************************/

create_soln_tree(Prior,[],_,Left,Right,Prior,Left,Right,_).

create_soln_tree(Prior,[Rule:if Cond then Goal with certainty(Rcert)|Rules],
        Trace,L_tree,nil,Fcert,Left,Right,Mode):-
    solve_and_mod(Cond,Goal,Rule,Rcert,Trace,Answer,Newcert,Mode),
    label_tree(Rule,Newcert,Answer,nil,R_tree),
    ascertain(Prior,Newcert,Post),
    create_soln_tree(Post,Rules,Trace,L_tree,R_tree,Fcert,Left,Right,Mode).

create_soln_tree(Prior,[Rule:if Cond then Goal with certainty(Rcert)|Rules],
        Trace,tree(LL,R1 # C1 was ExL,nil),tree(LR,R2 # C2 was ExR,nil),Fcert,Left,Right,Mode):-
    solve_and_mod(Cond,Goal,Rule,Rcert,Trace,Answer,Newcert,Mode),
        label_tree(Rule,Newcert,Answer,nil,R_tree),
    label_tree(Goal,Prior,tree(LL,R1 # C1 was ExL,nil),tree(LR,R2 # C2 was ExR,nil),L_tree),
    ascertain(Prior,Newcert,Post),
    create_soln_tree(Post,Rules,Trace,L_tree,R_tree,Fcert,Left,Right,Mode).

create_soln_tree(Prior,[Rule:if Cond then Goal with certainty(Rcert)|Rules],
        Trace,tree(LL,Goal # C was ExL,RL),tree(LR,R1 # C1 was ExR,nil),Fcert,Left,Right,Mode):-
    solve_and_mod(Cond,Goal,Rule,Rcert,Trace,Answer,Newcert,Mode),
    label_tree(Rule,Newcert,Answer,nil,R_temp),
    ascertain(Newcert,C1,Cert),
    label_tree(Goal,Cert,tree(LR,R1 # C1 was ExR,nil),R_temp,R_tree),
    ascertain(Cert,C,Post),
    create_soln_tree(Post,Rules,Trace,tree(LL,Goal # C was ExL,RL),R_tree,Fcert,Left,Right,Mode).

create_soln_tree(Prior,[Rule:if Cond then Goal with certainty(Rcert)|Rules],
        Trace,Left_pair,Right_pair,Fcert,Left,Right,Mode):-
    solve_and_mod(Cond,Goal,Rule,Rcert,Trace,Answer,Newcert,Mode),
    label_tree(Rule,Newcert,Answer,nil,R_tree),
    label_tree(Goal,Prior,Left_pair,Right_pair,L_tree),
    ascertain(Prior,Newcert,Post),
    create_soln_tree(Post,Rules,Trace,L_tree,R_tree,Fcert,Left,Right,Mode).

label_tree(Rule,Cert,Left,Right,tree(Left,Rule # Cert was 'derived from',Right)).

/************************************************************************
  
   solve_and_mod : routine that calls the inference engine and
                   redefines the certainty of the rule.
 
 *************************************************************************/

solve_and_mod(Cond,Goal,Rule,Rcert,Trace,Answer,Newcert,Mode):-
         trace_usage(Rule:if Cond then Goal with certainty(Rcert),Mode),
         explore(Cond,[Goal by Rule|Trace],Answer,Mode),
         get_certainty(Answer,Anscert),
         modify_rule_cert(Anscert,Rcert,Newcert).

/**************************************************************************
 
    The following procedures carry out the modification of certainties
    in response to new information becoming available
 
    (cf.    Buchanan & Shortliffe)
 
 
    CCond       ->  C(E)
    CGoal       ->  C(A)    
    CRule       ->  C(F)
    NewCGoal    ->  ANSWER

 **************************************************************************/

modify_rule_cert(CF,CB,NewCF):-
    CB > 0,
    NewCF is CF * CB,!.

modify_rule_cert(_,_,0).

ascertain(-1,1,0).
ascertain(1,-1,0).

ascertain(CA,CF,CA_E):-
    CA >= 0,
    CF >= 0,
    CA_E is ((CA + CF) - (CA * CF)).


ascertain(CA,CF,CA_E):-
    CA =< 0,
    CF =< 0,
    CA_E is ((CA + CF) + (CA * CF)).

ascertain(CA,CF,CA_E):-
    modulus(CA,ModulusCA),
    modulus(CF,ModulusCF),
    min(ModulusCA,ModulusCF,Min),
    CA_E is ((CA + CF) / (1 - Min)).
    
/**************************************************************************
 
    get_certainty:
    retrieves certainty from an 'Answer' to an 'explore'
 
 **************************************************************************/

get_certainty(tree(Left,Goal # Cert was _,Right),Cert).

/**************************************************************************
 
    retract_all:
    removes all facts, rules, etc. as specified by its parameter
 
 **************************************************************************/

retract_all(Type):-
    Ctype:Rest,
    clause_is_a(Type,Ctype),
    retract(Ctype:Rest),
    fail.

retract_all(_).

trace_usage(_,backward).
trace_usage(_,forward).
trace_usage(Rule,verbose):-
    clear_screen,
    write('The rule currently firing is...'),
    nl,nl,
    show_rule(Rule),
    nl,nl,
    pause(Default_Message).

show_certainty_for_trace(_,_,backward).
show_certainty_for_trace(_,_,forward).
show_certainty_for_trace(Certainty,Rule,verbose):-
    clear_screen,
    show_rule(Rule),
    nl,nl,
    htab,
    write('has fired and returns a certainty value of: '),
    write(Certainty),
    nl,nl,
    pause(Default_Message).

/**************************************************************************
 
    title_page:
    prints out initial/final screen
 
 **************************************************************************/

title_page(Type, Title):-
    clear_screen,
    nl,nl,
    banner(Title),
    nl,nl,nl,
    video(reverse),
    title_message_start(Type),
    write(Title),
    pstring(" expert system... "),
    video(normal),
    nl,nl,nl,nl.

title_message_start(first):-
    pstring(" Welcome to the son of "). /* "son of" !??!?! */

title_message_start(last):-
    pstring(" Thank you for using the ").

/**************************************************************************/
/************   END OF PROGRAM:   explore   *******************************/
/**************************************************************************/
