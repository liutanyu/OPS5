/*  TERM.PL
    Shelved on the 13th of October 1988

    Minimalist terminal-control routines for a VT100.
*/


/*  '$vedoutint'( X+ ):
    Emit X (integer) as its ASCII representation.
*/
'$vedoutint'( X ) :-
    write( X ).


/*  '$vedescape'( X+ ):
    Emit atom X, preceded by an ESC.
*/
'$vedescape'( X ) :-
    put( 27 ), write( X ).  


/*  cursor( X+, Y+ ):
    Move cursor to X, Y.
*/
cursor( X, Y ) :-
    '$vedescape'( '[' ),
    Y1 is Y + 1, '$vedoutint'( Y1 ),
    write( ';' ),
    X1 is X + 1, '$vedoutint'( X1 ),
    write( 'H' ).


/*  video( State+ ):
    Inverse video on (State=reverse) or off (State=normal).
*/
video( reverse ) :-
    '$vedescape'( '[' ), write( '7m' ).
video( normal ) :-
    '$vedescape'( '[' ), write( '0m' ).


/*  clear_screen:
    Clear the screen.
*/
clear_screen :-
    '$vedescape'( '[' ), write( '2J' ),
    cursor( 0, 0 ).



/*  new_prompt:
    Set the prompt to a suitable string for this program.
*/
new_prompt.
