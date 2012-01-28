/*  COMPAT.PL  */
/*  Shelved on the 3rd of October 1988  */


/*  Added: op declns; fileerrors, nofileerrors, exists, number
*/

:-op(1150,fx,mode).
:-op(1150,fx,public).
:-op(1200,fx,:-).
:-op(500,fx,+).
:-op(500,fx,\).
:-op(1200,fx,?-).
:-op(900,fy,\+).
:-op(500,fx,-).
:-op(900,fy,nospy).
:-op(900,fy,spy).
:-op(300,xfx,div).
:-op(400,yfx,/).
:-op(700,xfx,=).
:-op(1200,xfx,:-).
:-op(1200,xfx,-->).
:-op(1000,xfy,',').
:-op(700,xfx,@<).
:-op(500,yfx,/\).
:-op(700,xfx,is).
:-op(500,yfx,+).
:-op(700,xfx,>=).
:-op(700,xfx,>).
:-op(700,xfx,@>=).
:-op(700,xfx,=:=).
:-op(700,xfx,==).
:-op(700,xfx,\==).
:-op(700,xfx,@=<).
:-op(400,yfx,<<).
:-op(1050,xfy,->).
:-op(500,yfx,\/).
:-op(700,xfx,=\=).
:-op(400,yfx,>>).
:-op(400,yfx,*).
:-op(500,yfx,-).
:-op(700,xfx,=<).
:-op(700,xfx,@>).
:-op(700,xfx,=..).
:-op(200,xfy,^).
:-op(300,xfx,mod).
:-op(700,xfx,<).
:-op(1100,xfy,;).
:-op(40,xfx,\=).


number(X) :- integer(X).


nofileerrors.


fileerrors.


exists(_).
