 Aquí va el código.
sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).

seCaracterizaPor(harry, [coraje, amistad, orgullo, inteligencia]).
seCaracterizaPor(draco, [inteligencia, orgullo]).
seCaracterizaPor(hermione, [inteligencia, orgullo, responsabilidad]).

odia(harry, slytherin).
odia(draco, hufflepuff).

criterioPara(gryffindor, [coraje]).
criterioPara(slytherin, [orgullo, inteligencia]).
criterioPara(ravenclaw, [inteligencia, responsabilidad]).
criterioPara(hufflepuff, [amistad]).

% Punto 1

mago(Mago):-
    sangre(Mago,_).

permiteEntrar(Casa, Mago):-
    mago(Mago),
    Casa \= slytherin.

permiteEntrar(slytherin, Mago):-
    mago(Mago),
    not(sangre(Mago, impura)).

% Punto 2

incluye(Incluyente, [X]):-
    member(X, Incluyente).

incluye(Incluyente, [X|XS]):-
    member(X, Incluyente),
    incluye(Incluyente, XS).

caracterApropiado(Mago, Casa):-
    seCaracterizaPor(Mago, Caracteres),
    criterioPara(Casa, Requeridos),
    incluye(Caracteres, Requeridos).

% Punto 3

puedeSerSeleccionado(Mago, Casa):-
    permiteEntrar(Casa, Mago),
    caracterApropiado(Mago, Casa),
    not(odia(Mago, Casa)).

puedeSerSeleccionado(hermione, gryffindor).

% Punto 4

sonAmistosos([X]):-
    seCaracterizaPor(X, Caracteres),
    member(amistad, Caracteres).

sonAmistosos([X|XS]):-
    seCaracterizaPor(X, Caracteres),
    member(amistad, Caracteres),
    sonAmistosos(XS).

cadenaDeCasas([M1,M2]):-
    puedeSerSeleccionado(M1, Casa),
    puedeSerSeleccionado(M2, Casa),
    M1 \= M2.

cadenaDeCasas([M1,M2|Magos]):-
    cadenaDeCasas([M1,M2]),
    cadenaDeCasas([M2|Magos]).

cadenaDeAmistades(Magos):-
    sonAmistosos(Magos),
    cadenaDeCasas(Magos).


% Parte 2

% Punto 1

malaAccion(andarDeNoche, 50).
malaAccion(irA(bosque), 50).
malaAccion(irA(seccionRestringida), 10).
malaAccion(irA(tercerPiso), 75).

cometio(harry, andarDeNoche).
cometio(harry, irA(bosque)).
cometio(harry, irA(tercerPiso)).
cometio(harry, buenaAccion(ganarAVoldemort, 60)).
cometio(hermione, irA(tercerPiso)).
cometio(hermione, irA(seccionRestringida)).
cometio(hermione, buenaAccion(salvarAmigos, 50)).
cometio(draco, irA(mazmorras)).
cometio(ron, buenaAccion(ganarAjedrez, 50)).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

buenAlumno(Mago):-
    cometio(Mago,_),
    forall(cometio(Mago, Accion), not(malaAccion(Accion,_))).

recurrente(Accion):-
    cometio(Mago1, Accion),
    cometio(Mago2, Accion),
    Mago1 \= Mago2.

% Punto 2

puntajeQueOtorga(buenaAccion(_, Puntaje), Puntaje).

puntajeQueOtorga(Accion, Puntaje):-
    malaAccion(Accion, PuntajeOpuesto),
    Puntaje is PuntajeOpuesto * (-1).

puntajeQueOtorga(responderPregunta(_, Dificultad, Profesor), Puntaje):-
    Profesor \= snape,
    Puntaje == Dificultad.

puntajeQueOtorga(responderPregunta(_, Dificultad, snape), Puntaje):-
    Puntaje == Dificultad / 2.

puntajeQueOtorga(Accion, 0):-
    Accion \= buenaAccion(_,_),
    not(malaAccion(Accion,_)).

puntosObtenidos(Mago, 0):-
    not(cometio(Mago,_)).

puntosObtenidos(Mago, PuntosTotales):-
    cometio(Mago,_),
    findall(Puntaje, (cometio(Mago, Accion), puntajeQueOtorga(Accion, Puntaje)), Puntos),
    sumlist(Puntos, PuntosTotales).

puntajeDeCasa(Casa, PuntosTotales):-
    esDe(_, Casa),
    findall(Puntos, (esDe(Mago, Casa), puntosObtenidos(Mago, Puntos)), Puntaje),
    sumlist(Puntaje, PuntosTotales).

% Punto 3

 casaGanadora(Casa):-
    puntajeDeCasa(Casa, Puntaje),
    forall((puntajeDeCasa(OtraCasa, OtrosPuntos), OtraCasa \= Casa), Puntaje >= OtrosPuntos).
    