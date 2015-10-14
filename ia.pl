iaAleatoire(Coup) :- nbColonnes(NBCOLONNES), random_between(1,NBCOLONNES,Coup), coupValide(Coup).
iaAleatoire(Coup) :- iaAleatoire(Coup).