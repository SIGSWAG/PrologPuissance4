%%%%%%%%%%%% tests.pl %%%%%%%%%%%%

:- use_module(jeu).
:- use_module(util).
:- use_module(miniMax).

t_gagne_colonne :-
	assert(case(1,1,rouge)),
	assert(case(1,2,rouge)),
	assert(case(1,3,rouge)),
	assert(case(1,4,rouge)),
	gagne(1,4,rouge).

t_gagne_ligne :-
	assert(case(1,1,rouge)),
	assert(case(2,1,rouge)),
	assert(case(3,1,rouge)),
	assert(case(4,1,rouge)),
	gagne(4,1,rouge).

t_gagne_diagonale1 :-
	assert(case(1,1,rouge)),
	assert(case(2,2,rouge)),
	assert(case(3,3,rouge)),
	assert(case(4,4,rouge)),
	gagne(4,4,rouge).

t_gagne_diagonale2 :-
	assert(case(4,4,rouge)),
	assert(case(3,3,rouge)),
	assert(case(2,2,rouge)),
	assert(case(1,1,rouge)),
	gagne(1,1,rouge).
	
	
t_minimax_prof1:- 
assert(evaluation(test1)),	
parcoursArbre(rouge,1,R,Value),retract(evaluation(X)),R==4,Value==10.

t_minimax_prof2:- 
assert(evaluation(test1)),	
parcoursArbre(rouge,2,R,Value),retract(evaluation(X)),R==1,Value==(-5).


