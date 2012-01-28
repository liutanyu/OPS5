/*  USER_ANSWER.PL  */
/*  Shelved on the 3rd of October 1988  */


/****************************************************************************/
/************* PROGRAM: user_answer *****************************************/
/****************************************************************************
 
    This program may be used and amended for non-profit making
    purposes on condition that this header be left intact and in situ.
 
    Copyright: Steven Salvini 1986, 1987, 1988
 
****************************************************************************/

/****************************************************************************
 
    user_answer:
    gets certainties associated with Goals whose certainty is not known
    (i.e. for which no information have been asserted or can be deduced)
 
 ****************************************************************************/

user_answer(Object/Attribute/Value,Trace,Certainty):-
/* 'pretty' verion using menu-based data input - needs appropriate question */
    Question:Object/Attribute/Values - Message - Type,
    print_menu1(Object,Attribute,Values,Trace,Message,Type),
    (fact:Object/Attribute/Value with certainty(Certainty)
    ;
    told:Object/Attribute/Value with certainty(Certainty)),
    !.

user_answer(Object/Attribute/Value,Trace,Certainty):-    
/* 'fail-safe/default' version - only used if no question available */
    var(Value),
    clear_screen,
    nl,nl,nl,
    write('What value do you associate with '),
    write(Object),
    write('`s '),
    write(Attribute),
    write('?'),
    nl, nl,
    read_word(Value),
    assertz(told:Object/Attribute/Value with certainty(1)).

user_answer(Object/Attribute/Value,Trace,Certainty):-    
/* 'fail-safe/default' version - only used if no question available */
    clear_screen,
    nl,nl,nl,
    write('What certainty value do you associate with '),
    write(Object),
    write('`s '),
    write(Attribute),
    write(' being '),
    write(Value),
    write('?'),
    nl, nl,
    input_certainty(Trace,Certainty),
    assertz(told:Object/Attribute/Value with certainty(Certainty)).

/****************************************************************************
 
    input_certainty:
    Obtains certainty for current Value being investigated, allows user
    to ask "why", responding with a trace of the rules currently
    depending on this Value.
 
 ****************************************************************************/

input_certainty(Trace,Certainty):-
    write('    Valid replies are numbers between -10 to 10 or "why"'),
    repeat,
    nl,
    read_word(Reply),
    ok_reply_ic(Reply,Validated_Reply),
    process_reply_ic(Validated_Reply,Trace,Certainty),
    !.

ok_reply_ic(Reply,Validated_Reply):-    /*  regularises user's reply */
        (means(Reply,why,Validated_Reply);
         means(Reply,quit,Validated_Reply);
         means(Reply,valid_replies,Validated_Reply);
         means(Reply,certainty,Validated_Reply)).
    
process_reply_ic(why,Trace,_):-         /* in response to 'why':  */
    show_trace(Trace),          /* explains reason for asking  */
    !,                  /* the current question */
    fail.

process_reply_ic(valid_replies,_,_):-
    nl,write('Valid replies are...'),nl,
    write('v,V,valid,VALID:   gives this message'),nl,
    write('w,W,why,WHY:       shows the reasoning behind asking for this'),
    nl,write('                   certainty value'),nl,
    write('OR an integer between -10 and 10 which represents the certainty'),
    nl,write('value: -10 being FALSE, 0 NOTHING KNOWN & 10 TRUE'),nl,
    !,
    fail.

process_reply_ic(quit,_,_):-
    nl,
    write('You have indicated that you want to "quit".'),nl,
    write('This is not possible at this point as certainty values '),
    write('are mandatory'),
    write('in the interactive processing mode.'),nl,
    write('Instead, please enter a certainty of zero (0 ie. nothing '),
    write('known) when'),nl,
    write('(re-)prompted for certainties - or abort the program (^C) and '),
    nl,
    write('re-run - if you still want to exit.'),nl,
    !,
    fail.

process_reply_ic(Bigcertainty,_,Certainty):-    /* in response to a number: */
    reduce(Bigcertainty,Certainty).     /* reduces it to range of */
                        /* certainties */

/**************************************************************************
 
    show_trace:
    shows trace of current 'Goal'`s investigation when called via 
        back-chaining (explore) - clauses 1 & 2
          or "reason for asking about a symptom" when called via
        'take_history' - clause 3
 
 **************************************************************************/

show_trace([]):-
    nl,
    tab(10),
    write('which is the goal currently undergoing investigation'),
    nl.

show_trace([Goal by Rule|Trace]):-
    write('To investigate, by '),
    write(Rule),
    write(', '),
    write(Goal),
    nl,
    show_trace(Trace).

show_trace([Message]):-
    write(Message),
    nl.

/****************************************************************************
 
    reduce:
    chops a number (-10 - 10) by a factors of 10 to give an acceptable
    certainty value
 
****************************************************************************/

reduce(0,0).        /* to avoid possible division by zero */
reduce(Big,Decimated):-
    Decimated is Big / 10.

/****************************************************************************/
/************* END OF PROGRAM: user_answer **********************************/
/****************************************************************************/
