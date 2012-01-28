/*  WEE.PL  */
/*  Shelved on the 3rd of October 1988  */


/***************************************************************************/
/************************* RULE_BASE: wee **********************************/
/***************************************************************************
 
    This program may be used and amended for non-profit making
    purposes on condition that this header be left intact and in situ.
 
    Copyright: Steven Salvini 1988
 
**************************************************************************/
 
/**************************************************************************
 
    ruleX:
    Classical 'if-then' rules using Object-Attribute-Value triples
 
 **************************************************************************/

rule1:
    if
            pain/duration/constant
        and
            pain/localisation/poor
    then
        patient/diagnosis/'Acute Pulpitis'
    with
        certainty(0.9).

/**************************************************************************
 
    questionX:
    One question is provided for each O/A/V triple
 
 **************************************************************************/

question1:
    pain/localisation/[poor,accurate] 
    - "The localisation of the pain is... "
    - opposite.

question2:
    pain/duration/[constant,intermittent] 
    - "The duration of the pain is... "
    - opposite.

/**************************************************************************
 
    top_level_goals:
    These are the goals/diagnoses that the system tries to deduce
 
 **************************************************************************/

top_level_goals:
    patient -
    diagnosis -
        [
        'Acute Pulpitis'
        ].

/**************************************************************************/
/***************** END OF RULE_BASE: wee **********************************/
/**************************************************************************/
