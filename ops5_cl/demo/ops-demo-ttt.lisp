;;; ****************************************************************
;;; OPS5 Interpreter ***********************************************
;;; ****************************************************************
;;; This Common Lisp version of OPS5 is in the public domain.  It is based
;;; in part on based on a Franz Lisp implementation done by Charles L. Forgy
;;; at Carnegie-Mellon University, which was placed in the public domain by
;;; the author in accordance with CMU policies.  Ported to Common Lisp by 
;;; George Wood and Jim Kowalski. CMU Common Lisp modifications by
;;; Dario Guise, Skef Wholey, Michael Parzen, and Dan Kuokka. 
;;; Modified to work in CLtL1, CLtL2 and X3J13 compatible lisps by 
;;; Mark Kantrowitz on 14-OCT-92.
;;; 
;;; This code is made available is, and without warranty of any kind by the
;;; authors or by Carnegie-Mellon University.
;;;

(reset-ops)

(strategy mea)
(watch 0)


(literalize task
	actor)
(literalize position
	row column value identity)
(literalize opposite
	of is)
(literalize player
	with-mark is)
(literalize move
	status whose-turn input)
(vector-attribute input)



(p start
;	 generate the wm-elements defining the "board" and find out whether
;	 the human wants his mark to be x or o
	 (ready)
	-->
	 (make task ^actor referee)
	 (make position ^row 1 ^column 1 ^value | | ^identity top-left)
	 (make position ^row 1 ^column 2 ^value | | ^identity top-middle)
	 (make position ^row 1 ^column 3 ^value | | ^identity top-right)
	 (make position ^row 2 ^column 1 ^value | | ^identity middle-left)
	 (make position ^row 2 ^column 2 ^value | | ^identity center)
	 (make position ^row 2 ^column 3 ^value | | ^identity middle-right)
	 (make position ^row 3 ^column 1 ^value | | ^identity bottom-left)
	 (make position ^row 3 ^column 2 ^value | | ^identity bottom-middle)
	 (make position ^row 3 ^column 3 ^value | | ^identity bottom-right)
	 (make opposite ^of x ^is o)
	 (make opposite ^of o ^is x)
	 (write (crlf) "Do you want to be x or o?  " )
	 (make player ^with-mark (accept) ^is human) )

(make ready)

(p pop
	 ; if there is nothing more to do in the most recently generated task,
	 ; delete the task
	 (task)
	-->
	 (remove 1) )


(p referee--display-the-board
	 ; after each move, display the board
	 (task ^actor referee)
	 (move ^status made ^whose-turn <mark>)
	 (opposite ^of <mark> ^is <opponent-mark>)
	 (position ^row 1 ^column 1 ^value <l1>)
	 (position ^row 1 ^column 2 ^value <m1>)
	 (position ^row 1 ^column 3 ^value <r1>)
	 (position ^row 2 ^column 1 ^value <l2>)
	 (position ^row 2 ^column 2 ^value <m2>)
	 (position ^row 2 ^column 3 ^value <r2>)
	 (position ^row 3 ^column 1 ^value <l3>)
	 (position ^row 3 ^column 2 ^value <m3>)
	 (position ^row 3 ^column 3 ^value <r3>)
	-->
	 (modify 2 ^status unmade ^whose-turn <opponent-mark>)
	 (write (crlf) (crlf) (crlf)
		(tabto 12) <l1> (tabto 15) "|" (tabto 18) <m1>
		(tabto 21) "|" (tabto 24) <r1>
		(tabto 10) -----------------
		(tabto 12) <l2> (tabto 15) "|" (tabto 18) <m2>
		(tabto 21) "|" (tabto 24) <r2>
		(tabto 10) -----------------
		(tabto 12) <l3> (tabto 15) "|" (tabto 18) <m3>
		(tabto 21) "|" (tabto 24) <r3>) )

(p referee--prepare-for-first-move
;	 identify the mark of the computer and create the move wm-element that
;	 will drive the game
	 (task ^actor referee)
	 (player ^with-mark <mark> ^is human)
	 (opposite ^of <mark> ^is <other-mark>)
	-->
	 (write (crlf) (crlf) 
  	   "When you are asked where you want your mark, enter two numbers."
		(crlf) 
            "The first number should be the row you want, the second number, the column.")
	 (make player ^with-mark <other-mark> ^is computer)
	 (make move ^status unmade ^whose-turn x) )

(p referee--get-a-good-mark
;	 if the human says he wants to be something other than x or o, make
;	 him x
	 (task ^actor referee)
	 (player ^with-mark <mark> ^is human)
	 - (opposite ^of <mark>)
	-->
	 (modify 2 ^with-mark x)
	 (write (crlf) (crlf) "Try to remember that you're x.") )

(p referee--next-move
;	 if it's time for the next move to be made, generate the appropriate
;	 subtask
	 (task ^actor referee)
	 (move ^status unmade ^whose-turn <mark>)
	 (player ^with-mark <mark> ^is <who>)
	-->
	 (make task ^actor <who>) )

(p referee--recognize-column-win
;	 if someone has filled a column, note that fact
	 (task ^actor referee)
	 (move ^status unmade ^whose-turn <mark>)
	 (opposite ^of <mark> ^is <other-mark>)
	 (player ^with-mark <other-mark>)
	 (position ^column <c> ^value <other-mark>)
	 - (position ^column <c> ^value <> <other-mark>)
	-->
	 (remove 2)
	 (make player ^with-mark <other-mark> ^is winner) )

(p referee--recognize-row-win
;	 if someone has filled a row, note that fact
	 (task ^actor referee)
	 (move ^status unmade ^whose-turn <mark>)
	 (opposite ^of <mark> ^is <other-mark>)
	 (player ^with-mark <other-mark>)
	 (position ^row <r> ^value <other-mark>)
	 - (position ^row <r> ^value <> <other-mark>)
	-->
	 (remove 2)
	 (make player ^with-mark <other-mark> ^is winner) )

(p referee--recognize-diagonal-win
;	 if someone has filled a diagonal, note that fact
	 (task ^actor referee)
	 (move ^status unmade ^whose-turn <mark>)
	 (opposite ^of <mark> ^is <other-mark>)
	 (player ^with-mark <other-mark>)
	 (position ^row 2 ^column 2 ^value <other-mark>)
	 (position ^row {<r> <> 2} ^column {<c> <> 2}
		   ^identity <id> ^value <other-mark>)
	 (position ^row <c> ^column <r>
		   ^identity <> <id> ^value <other-mark>)
	-->
	 (remove 2)
	 (make player ^with-mark <other-mark> ^is winner) )

(p referee--human-wins
;	 if the human won, let him know
	 (task ^actor referee)
	 (player ^with-mark <other-mark> ^is winner)
	 (player ^with-mark <other-mark> ^is human)
	-->
	 (write (crlf) (crlf) "You win." (crlf) (crlf)) )

(p referee--computer-wins
;	 if the computer won, let the human know
	 (task ^actor referee)
	 (player ^with-mark <other-mark> ^is winner)
	 (player ^with-mark <other-mark> ^is computer)
	-->
	 (write (crlf) (crlf) "I win." (crlf) (crlf)) )

(p referee--draw
;	 if there are no empty spaces, the game is a draw
	 (task ^actor referee)
	 (move ^status unmade ^whose-turn <mark>)
	 (player ^with-mark <mark>)
	 - (position ^value | |)
	-->
	 (write (crlf) (crlf) "We drew." (crlf) (crlf))
	 (remove 2) )

(p referee--cleanup
;	 if the game is over, delete all of the wm-elements
	 (task ^actor referee)
	 - (move)
	 (<> task)
	-->
	 (remove 2) )


(p human--ask-for-next-move
	 ; get the position (row and column) where the human wants his mark
	 (task ^actor human)
	 (move ^status unmade ^input nil)
	-->
 	 (write (crlf) (crlf) "Where do you want your mark?  ")
	 (modify 2 ^input (acceptline)) )

(p human--accept-move
	 ; if the move is legal, accept it
	 ; the move wm-element is remade so that the value of ^input becomes
	 ; nil (there are 2 simpler but less educational ways of achieving
	 ; this same end)
	 (task ^actor human)
	 (move ^status unmade ^whose-turn <mark>
	       ^input {<row> >= 0 <= 3} {<column> >= 0 <= 3} nil)
	 (position ^row <row> ^column <column> ^value | |)
	-->
	 (remove 2)
	 (make move (substr 2 2 input) ^status made ^input nil)
	 (modify 3 ^value <mark>) )

(p human--reject-attempt-to-overwrite
	 ; if the position specified is not empty, complain
	 ; the move condition element in this rule differs from the move
	 ; condition in the previous rule only so you can see two equivalent
	 ; ways of expressing the same condition
	 (task ^actor human)
	 (move ^status unmade
	       ^input <row> <column> nil ^input << 1 2 3 >> << 1 2 3 >>)
	 (position ^row <row> ^column <column> ^value {<mark> <> | |})
	-->
	 (write (crlf) (crlf) "There is already an " <mark> " in " <row> <column>)
	 (modify 2 ^input nil nil) )

(p human--reject-out-of-bounds-move
	 ; if the row or column specified is not within bounds or if more than
	 ; two numbers have been entered, complain
	 ; the move wm-element is remade so that the value of ^input becomes
	 ; nil (there is a simpler but less educational way of achieving this
	 ; same end)
	 (task ^actor human)
	 (move ^status unmade ^input <> nil)
	-->
	 (write (crlf) (crlf) (substr 2 input inf) "is not a legal move.")
	 (remove 2)
	 (make move (substr 2 2 input) ^input nil) )


(p computer--select-move
	 ; select any empty position
	 (task ^actor computer)
	 (move ^status unmade ^whose-turn <mark>)
	 - (position ^row 2 ^column 2 ^value | |)
	 (position ^row <r> ^column <c> ^value | |)
	-->
	 (modify 2 ^status made)
	 (modify 3 ^value <mark>) )

(p computer--select-center
	 ; select the center if it's available
	 (task ^actor computer)
	 (move ^status unmade ^whose-turn <mark>)
	 (position ^row 2 ^column 2 ^value | |)
	-->
	 (modify 2 ^status made)
	 (modify 3 ^value <mark>) )

(p computer--block-column-win
	 ; if the human has two in a column, block
	 (task ^actor computer)
	 (move ^status unmade ^whose-turn <mark>)
	 (position ^row <r> ^column <c>
		   ^value {<other-mark> <> <mark> <> | |})
	 (position ^column <c> ^value | |)
	 (position ^row <> <r> ^column <c> ^value <other-mark>)
	-->
	 (modify 2 ^status made)
	 (modify 4 ^value <mark>) )

(p computer--block-row-win
	 ; if the human has two in a row, block
	 (task ^actor computer)
	 (move ^status unmade ^whose-turn <mark>)
	 (position ^row <r> ^column <c>
		   ^value {<other-mark> <> <mark> <> | |})
	 (position ^row <r> ^value | |)
	 (position ^row <r> ^column <> <c> ^value <other-mark>)
	-->
	 (modify 2 ^status made)
	 (modify 4 ^value <mark>) )

(p computer--block-diagonal-win
	 ; if the human has two on a diagonal, block
	 (task ^actor computer)
	 (move ^status unmade ^whose-turn <mark>)
	 (position ^row 2 ^column 2
		   ^value {<other-mark> <> <mark> <> | |})
	 (position ^row {<r> <> 2} ^column {<c> <> 2} ^value | |)
	 (position ^row <c> ^column <r> ^value <other-mark>)
	-->
	 (modify 2 ^status made)
	 (modify 4 ^value <mark>) )

(p computer--possible-column
	 ; if the computer has one mark in an otherwise empty column, put
	 ; another mark in that column
	 (task ^actor computer)
	 (move ^status unmade ^whose-turn <mark>)
	 (position ^row <r> ^column <c> ^value <mark>)
	 - (position ^row <> <r> ^column <c> ^value <> | |)
	 (position ^row <> <r> ^column <c> ^value | |)
	-->
	 (modify 2 ^status made)
	 (modify 4 ^value <mark>) )

(p computer--possible-row
	 ; if the computer has one mark in an otherwise empty row, put
	 ; another mark in that row
	 (task ^actor computer)
	 (move ^status unmade ^whose-turn <mark>)
	 (position ^row <r> ^column <c> ^value <mark>)
	 - (position ^row <r> ^column <> <c> ^value <> | |)
	 (position ^row <r> ^column <> <c> ^value | |)
	-->
	 (modify 2 ^status made)
	 (modify 4 ^value <mark>) )
