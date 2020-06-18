:- use_module(library(clpfd)).
:- use_rendering(table).

adjacent(X, Y, Z, [X,Y,Z|_]).
adjacent(X, Y, Z, [_|Tail]) :-
    adjacent(X, Y, Z, Tail).

% El predicado aumentar/2 recibe una mamushka y almacena la
% mamushka del siguiente tamaño.
aumentar(r1, r2).
aumentar(r2, r3).
aumentar(r3, r3).

aumentar(v1, v2).
aumentar(v2, v3).
aumentar(v3, v3).

aumentar(a1, a2).
aumentar(a2, a3).
aumentar(a3, a3).

% El predicado desplazar es el encargado principal
% de la dinamica del juego, desplazando la fila o
% columna especificada en una cantidad de lugares
% hacia una determinada direccion.
%
% +Dir: Direccion de desplazamiento de la fila o
%       columna.
% +Num: Numero de fila o columna a desplazar.
% +Cant: Cantidad de lugares a desplazar la fila
%        o columna. Requiere que sea un entero
%        entre 1 y 4.
% +Tablero: Tablero de juego sobre el cual se
%           realiza el corrimiento.
% -EvoTablero: Parametro de salida que contiene la
%              evolucion del tablero al que se le
%              aplico el movimiento. Ver docs.
desplazar(der, Num, Cant, Tablero, EvoTablero) :-
    desplazar_fila(Num, Cant, Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrer_columnas(Aux, Num, EvoTablero).

desplazar(izq, Num, Cant, Tablero, EvoTablero) :-
    desplazar_fila(Num, (-Cant), Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrer_columnas(Aux, Num, EvoTablero).

desplazar(arriba, Num, Cant, Tablero, EvoTablero) :-
    desplazar_columna(Num, (-Cant), Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrer_filas(Aux, Num, EvoTablero).

desplazar(abajo, Num, Cant, Tablero, EvoTablero) :-
    desplazar_columna(Num, Cant, Tablero, Tablero1),
    append(Tablero1, [], Aux),
    recorrer_filas(Aux, Num, EvoTablero).

% El predicado desplazar_fila desplaza la N-esima
% fila del tablero en una determinada cantidad de
% posiciones. (Derecha o Izquierda).
%
% +N: Numero de fila a desplazar
% +Cant: Cantidad de posiciones a desplazar la fila
% +T: Tablero, dividido en Head|Tail
% -D: Fila desplazada... Creo que aca se rompe todo
desplazar_fila(N, Cant, [T|Ts], [T|D]) :-
    N > 0,
    N1 is N-1,
    desplazar_fila(N1, Cant, Ts, D).
desplazar_fila(0, Cant, [T|Ts], D) :-
    rotar_n(Cant, T, D1),
    append([D1], Ts, D).

% El predicado desplazar_columna desplaza la N-esima
% columna del tablero en una determinada cantidad de
% posiciones. (Arriba o Abajo).
% Para simplificar la operatoria, el desplazamiento
% se reduce a trasponer la matriz que representa al
% tablero, luego se desplaza la fila correspondiente,
% y finalmente, se vuelve a trasponer la matriz a su
% version original.
%
% Los parametros son los mismos que desplazar_fila
desplazar_columna(N, Cant, Tablero, D) :-
    transpose(Tablero, Trasp),
    desplazar_fila(N, Cant, Trasp, D1),
    transpose(D1, D).

% El predicado rotar_n desplaza una lista N posiciones
% a derecha o izquierda, segun el signo de N.
%
% +N: Cantidad de posiciones a desplazar. Derecha si
%     N > 0, izquierda si N < 0.
% +L1: Lista a rotar.
% -L2: Lista rotada.
rotar_n(0, L, L).
rotar_n(N, L1, L2) :-
    N < 0,
    rotar("izq", L1, L),
    N1 is N+1,
    rotar_n(N1, L, L2).
rotar_n(N, L1, L2) :-
    N > 0,
    rotar("der", L1, L),
    N1 is N-1,
    rotar_n(N1, L, L2).

rotar("der", L, [T|H]) :- append(H, [T], L).
rotar("izq", [H|T], L) :- append(T, [H], L).

% El predicado posicion_lista/2 es un predicado auxiliar que toma el
% elemento en la lista en la posicion P y lo almacena en la variable E.
posicion_lista([A | _Tail], 0, A).

posicion_lista([_A | Tail], Pos, E):-
    P1 is Pos-1,
    posicion_lista(Tail, P1, E).

% El predicado obtener_columna/3 recibe como parametro una lista de listas (un tablero),
% una posición y almacena la columna en dicha posicion en la variable F.
obtener_columna(Tablero, N, R):-
    transpose(Tablero, TableroTransp),
    obtener_fila(TableroTransp, N, R).

% El predicado obtener_fila/3 recibe como parámetro una lista de listas (un tablero),
% una posicion y almacena la fila en dicha posición en la variable F.
obtener_fila([A | _Tail], 0, A).

obtener_fila([_A | Tail], N, F):-
    N1 is N-1,
    obtener_fila(Tail, N1, F).

% El predicado intercambiar/4 intercambia el elemento en la posicion N
% en la lista por el elemento E y almacena el resultado en T.
intercambiar([_A | Tail], 0, E, [E | Tail]).

intercambiar([A | Tail], N, E, T):-
    N1 is N-1,
    intercambiar(Tail, N1, E, R),
    append([A], R, T).

% El predicado recorrer_columnas/3 recorre todas las columnas del tablero.
%  Luego, colapsa todas las mamushkas que pueda sobre el elemento Principal.
recorrer_columnas(Tablero, Principal, Res):-
    transpose(Tablero, TableroTransp),
    recorrer_filas(TableroTransp, Principal, Aux),
    transpose(Aux, Res).

% El predicado recorrer_filas/3 recorre todas las filas del tablero. Luego, colapsa todas
% las mamushkas que pueda sobre el elemento Principal.
recorrer_filas(Tablero, Principal, Res):-
    recorrer_filas_aux(Tablero, Principal, 0, R1),
    verificar_cruzadas(R1, Principal, Res).

recorrer_filas_aux(Tablero, Principal, 4, Res):-
    obtener_fila(Tablero, 4, Fila),
    verificar_consec(Fila, Principal, R),
    intercambiar(Tablero, 4, R, Res).

recorrer_filas_aux(Tablero, Principal, N, Res):-
    N1 is N+1,
    obtener_fila(Tablero, N, Fila),
    verificar_consec(Fila, Principal, R),
    intercambiar(Tablero, N, R, Aux),
    recorrer_filas_aux(Aux, Principal, N1, Res).

% El predicado verificar_cruzadas/3 verifica si se produjeron colapsos cruzados sobre la columna
% principal en el Tablero. Utiliza el predicado auxiliar verificar_cruzadas_aux/8.
% + Tablero: Tablero del juego.
% + Principal: Posición de la columna principal en el tablero.
% - Res: Tablero luego de los colapsos.
verificar_cruzadas(Tablero, Principal, Res):-
    obtener_columna(Tablero, Principal, C0),
    posicion_lista(C0, 0, P0),
    posicion_lista(C0, 1, P1),
    posicion_lista(C0, 2, P2),
    posicion_lista(C0, 3, P3),
    posicion_lista(C0, 4, P4),
	verificar_cruzadas_aux(Tablero, Principal, P0, P1, P2, P3, P4, Res).

verificar_cruzadas_aux(Tablero, Principal, P0, P1, P2, P3, P4, T4):-
    P0 == P1,
    P1 == P2,
    P2 == P3,
    P3 == P4,
    obtener_fila(Tablero, 0, L0),
    obtener_fila(Tablero, 1, L1),
    obtener_fila(Tablero, 2, L2),
    obtener_fila(Tablero, 3, L3),
    obtener_fila(Tablero, 4, L4),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    cruzar(L3, Principal, Aux4),
    cruzar(L4, Principal, Aux5),
    intercambiar(Tablero, 0, Aux1, T0),
    intercambiar(T0, 1, Aux2, T1),
    intercambiar(T1, 2, Aux3, T2),
    intercambiar(T2, 3, Aux4, T3),
    intercambiar(T3, 4, Aux5, T4).

verificar_cruzadas_aux(Tablero, Principal, P0, P1, P2, P3, _P4, T3):-
    P0 == P1,
    P1 == P2,
    P2 == P3,
    obtener_fila(Tablero, 0, L0),
    obtener_fila(Tablero, 1, L1),
    obtener_fila(Tablero, 2, L2),
    obtener_fila(Tablero, 3, L3),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    cruzar(L3, Principal, Aux4),
    intercambiar(Tablero, 0, Aux1, T0),
    intercambiar(T0, 1, Aux2, T1),
    intercambiar(T1, 2, Aux3, T2),
    intercambiar(T2, 3, Aux4, T3).

verificar_cruzadas_aux(Tablero, Principal, _P0, P1, P2, P3, P4, T3):-
    P1 == P2,
    P2 == P3,
    P3 == P4,
    obtener_fila(Tablero, 1, L0),
    obtener_fila(Tablero, 2, L1),
    obtener_fila(Tablero, 3, L2),
    obtener_fila(Tablero, 4, L3),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    cruzar(L3, Principal, Aux4),
    intercambiar(Tablero, 1, Aux1, T0),
    intercambiar(T0, 2, Aux2, T1),
    intercambiar(T1, 3, Aux3, T2),
    intercambiar(T2, 4, Aux4, T3).

verificar_cruzadas_aux(Tablero, Principal, P0, P1, P2, _P3, _P4, T2):-
    P0 == P1,
    P1 == P2,
    obtener_fila(Tablero, 0, L0),
    obtener_fila(Tablero, 1, L1),
    obtener_fila(Tablero, 2, L2),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    intercambiar(Tablero, 0, Aux1, T0),
    intercambiar(T0, 1, Aux2, T1),
    intercambiar(T1, 2, Aux3, T2).

verificar_cruzadas_aux(Tablero, Principal, _P0, P1, P2, P3, _P4, T2):-
    P1 == P2,
    P2 == P3,
    obtener_fila(Tablero, 1, L0),
    obtener_fila(Tablero, 2, L1),
    obtener_fila(Tablero, 3, L2),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    intercambiar(Tablero, 1, Aux1, T0),
    intercambiar(T0, 2, Aux2, T1),
    intercambiar(T1, 3, Aux3, T2).

verificar_cruzadas_aux(Tablero, Principal, _P0, _P1, P2, P3, P4, T2):-
    P2 == P3,
    P3 == P4,
    obtener_fila(Tablero, 2, L0),
    obtener_fila(Tablero, 3, L1),
    obtener_fila(Tablero, 4, L2),
    cruzar(L0, Principal, Aux1),
    cruzar(L1, Principal, Aux2),
    cruzar(L2, Principal, Aux3),
    intercambiar(Tablero, 2, Aux1, T0),
    intercambiar(T0, 3, Aux2, T1),
    intercambiar(T1, 4, Aux3, T2).

verificar_cruzadas_aux(Tablero, Principal, _P0, _P1, _P2, _P3, _P4, T4):-
    obtener_fila(Tablero, 0, L0),
    obtener_fila(Tablero, 1, L1),
    obtener_fila(Tablero, 2, L2),
    obtener_fila(Tablero, 3, L3),
    obtener_fila(Tablero, 4, L4),
    cruzar2(L0, Principal, Aux1),
    cruzar2(L1, Principal, Aux2),
    cruzar2(L2, Principal, Aux3),
    cruzar2(L3, Principal, Aux4),
    cruzar2(L4, Principal, Aux5),
    intercambiar(Tablero, 0, Aux1, T0),
    intercambiar(T0, 1, Aux2, T1),
    intercambiar(T1, 2, Aux3, T2),
    intercambiar(T2, 3, Aux4, T3),
    intercambiar(T3, 4, Aux5, T4).

% El predicado cruzar/3 verifica que haya en la lista L el elemento x. En caso de ser así, se verifica
% que hubo un colapso en dicha lista. Por lo tanto, se debe intercambiar la mamushka principal en la lista
% por la mamushka aumentada. Almacena la lista nueva. En el caso de que no haya colapsos en la columna principal,
% se llama al predicado cruzar2/3.
% + Lista: Lista a verificar.
% + Principal: Posición de la columna principal en la lista.
% - L2: Lista luego de los colapsos.
cruzar(Lista, Principal, L2):-
    member(x, Lista),
    posicion_lista(Lista, Principal, M),
    aumentar(M, MAum),
    intercambiar(Lista, Principal, MAum, L2).

cruzar(Lista, Principal, L2):-
    intercambiar(Lista, Principal, x, L2).

cruzar2(Lista, Principal, L2):-
    member(x, Lista),
    posicion_lista(Lista, Principal, M),
    aumentar(M, MAum),
    intercambiar(Lista, Principal, MAum, L2).

cruzar2(Lista, _Principal, Lista).

% El predicado verificar_consec/3 verifica que hayan, al menos, 3
% elementos consecutivos iguales en una lista. Luego, los colapsa
% sobre el elemento Principal. Utiliza el predicado auxiliar
% verificar_consec_aux/7
% + L: Lista a verificar.
% + Principal: Posición de la columna principal en la lista.
% - F: Lista final con los colapsos producidos.
verificar_consec(L, Principal, F):-
    posicion_lista(L, 0, P0),
    posicion_lista(L, 1, P1),
    posicion_lista(L, 2, P2),
    posicion_lista(L, 3, P3),
    posicion_lista(L, 4, P4),
    verificar_consec_aux(L, Principal, P0, P1, P2, P3, P4, F).

verificar_consec_aux(L, Principal, P0, P1, P2, P3, P4, L5):-
    P0 == P1,
    P1 == P2,
    P2 == P3,
    P3 == P4,
    posicion_lista(L, Principal, M),
    intercambiar(L, 0, x, L0),
    intercambiar(L0, 1, x, L1),
    intercambiar(L1, 2, x, L2),
    intercambiar(L2, 3, x, L3),
    intercambiar(L3, 4, x, L4),
    intercambiar(L4, Principal, M, L5).

verificar_consec_aux(L, Principal, P0, P1, P2, P3, _P4, L4):-
    P0 == P1,
    P1 == P2,
    P2 == P3,
    posicion_lista(L, Principal, M),
    intercambiar(L, 0, x, L0),
    intercambiar(L0, 1, x, L1),
    intercambiar(L1, 2, x, L2),
    intercambiar(L2, 3, x, L3),
    intercambiar(L3, Principal, M, L4).

verificar_consec_aux(L, Principal, _P0, P1, P2, P3, P4, L4):-
    P1 == P2,
    P2 == P3,
    P3 == P4,
    posicion_lista(L, Principal, M),
    intercambiar(L, 1, x, L0),
    intercambiar(L0, 2, x, L1),
    intercambiar(L1, 3, x, L2),
    intercambiar(L2, 4, x, L3),
    intercambiar(L3, Principal, M, L4).


verificar_consec_aux(L, Principal, P0, P1, P2, _P3, _P4, L3):-
    P0 == P1,
    P1 == P2,
    posicion_lista(L, Principal, M),
    intercambiar(L, 0, x, L0),
    intercambiar(L0, 1, x, L1),
    intercambiar(L1, 2, x, L2),
    intercambiar(L2, Principal, M, L3).

verificar_consec_aux(L, Principal, _P0, P1, P2, P3, _P4, L3):-
    P1 == P2,
    P2 == P3,
    posicion_lista(L, Principal, M),
    intercambiar(L, 1, x, L0),
    intercambiar(L0, 2, x, L1),
    intercambiar(L1, 3, x, L2),
    intercambiar(L2, Principal, M, L3).

verificar_consec_aux(L, Principal, _P0, _P1, P2, P3, P4, L3):-
    P2 == P3,
    P3 == P4,
    posicion_lista(L, Principal, M),
    intercambiar(L, 2, x, L0),
    intercambiar(L0, 3, x, L1),
    intercambiar(L1, 4, x, L2),
    intercambiar(L2, Principal, M, L3).

verificar_consec(L, _Principal, L),
%%%%%%%%%%%%%%##%########################################%#%%%###################%###(((//////(((((((##%%%%%%%%%%%%%%%%#%############################%%%%%%%%%%%%%%%&&%&&&&&&&&&&&&&&&&&&&&&&%%%%%%%%%%%
%%%%%%%%%%%%#%######################################################################(#((/////////(((((#####%%################################%%%%%%%%%&%&&&&&&&&&&&&&&&&&&%%%%%%%%%%#%%%##%#############
%%%%%%%%###%#############%########################################################%#((#(##%%%&%((((((####################%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##############################%%%%
%%%%%%%%%%%###################################################(#####################%&#&%&%&&&&&&@@&%%%%%%%%%%%%%%%%%%%%%%%&%%%%%%%%%%%%%%%%%%%%######################################%%%%%%%%%%%%%%%%%%
%%%%###################################################################%#%%%&@%&%&&&@&&&&&@%&&&@@@@@@@@@%%%&&&&&%%%%%%%%%%%%%%######################################%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&
%####################################################################%%&&&&@@@&@@@@&&@@%&&@@@&@@@@@@@@@@%&&%%%%%%%#####################################%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&%%%%%%%%%%%
%##################################################################&&&&&&&@@@@@@&&%&&&@@@@@&@@@@@@@@&@&&&&&@%##########################%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&%%%%%%%%%%%%%%%%%%%#############
%#############################################################%&%&&&&@@@@@@@@@&%%&&%%&@@@@@@@@@&%&&&@%&%@&&@@&%#%###%%%%%%%%%%%%%%%%%%%%%%%%%&%%%%%%%%%%%%%%%%%%%%######################################
%##########################################################%%&&#&&&@@@@@@@@@&&&@&&&&@@@@@@@&@&@@@@@@&&@@&@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%############################################%%%#%%%%%%%%%%%
%########################################################%&&@&&&%@@@@@@@@&@@@&&&%&&&@@&@@@@@@@@@@@@@@@@@@@@@@@@@@&%%%%%%%%%%%#############################################%%%%%%%%%%%%%%%%%%&%&&&&&&&&&&
%#######################################################%@@@&%%&@&@@@@@@@@&@@&&%&&@@@@@@@@@&@&&&@@@@@@@@@@@@@@@@@@@######################################%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&%%%%%
%########################################################@&&%%&%&&@@@@@@@@@@&%&&&@@@@&&&%@@&@&@@@@@@@@@@@@@@@@@@@@@@@&#################%%%%%%%%%%%%%%%%%%%%%%%%%&%&&&&&%&%%%%%%%%%%%%%%%%##%############
%######################################################%@@&&%&#&@@@@@@@@@@@&%%%%%&@@@@&@&@@@@@@@&&@@@@@@@@@@@@@@@@@@@@@&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##%#%#################################
%#####################################################%@&&&&%&&@@@@@@@@@@@@&%&&&%&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&%%%%%%%%%%%%%%%#%%##%###############################################%%%%%%%%%%%
%#####################################################&@&&&@%&&&&&@@@@@@@@@@&%%%%&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%%%%######################################%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&
%######################################################@@@@@&&&#&&&@@@@@@@@@@&@@@&@&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@######################%%%%%%%%%%%%%%%%%%&%&&&&&&&&&&&&%&&&&&&&&&&%&%%&%%%%%%%%
%####################################################%@@@@@&&%&&@@@@@@@@@@@@@&%#%&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%%#%%%%%%%%%%%%%%%&%%%%%%&&&&&&&%&&&&&%%%%%%%%%%%%%%%%%%%%####################
%#####################################################@@@@@&%%%%&&@@@@&&%&%%%#(##%%&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&%&&&&&&&&%%%%%%%%%%%%%%%####################(########((((###############
%#####################################################%@@@@@@&%&@@@@&@&&%%###(//*((###%&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&%%%%%%#%%###############(#((((((((#((((((((((##((##############%%%%%%%%%%%%%
%#####################################################%%@@@@@@@@@@@@@&%####(/**,,,,**///(#%%%&@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%###########(#((((##(#####################%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&
%#######################################################@@@@@@@@@@@&%%#(/***,,,,*/**/(#%%&%###%%%&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@##############%%%%%%%%%%%%%%%%%%&&&%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&%&&%%
%#######################################################@@@@@@@@@&&%%(((****,*,*//((%#(/***/(#%#%%%##%%%%&&@@@@@@@@@@@@@@@@@@@@&%%%%%&%%%%%%&&&&&&&&&&&&&&&&&&&&&&&%&%%%%%%%%%%%%%%%%%%#################
%########################################################@@@@@@@@@@&&%%##(((*,,/#&&%/(&&#&&&@#%%#(/*/**/(###%&@@@@@@@@@@@@@@@@@&&&&&&&&&&%%%%%%%%%%%%%%%%%###########################(#######(#((((#(###
%########################################################@@@@@@&&%#(**//((((/,.*(%%%*********/((*,,****//(##%%&@@@&&&&@@@&@@@@@@&%#######################(##########################################%%%%
%#################################(######################@@@@@((//#@%%@@///,*,,,*#%#(*,,,**/,,,,,,****///(#%%%%%&%&%%&&%%&@@@@@@#########((##(##########################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&
%#%###########################(####(######################%@@@*(//***,*//*,,*, ,/(##(/,,,,,,,,,,,,*****/(##%%%%&%%%%%%%%%%@@@&################%#%%%%%%%%%%%%%%%%%%%%%%%%&%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%#########################(#####(###########################%@**,,,,,,*,.,.,*, ,*((###(/*,,,,,,******//(##%%%%%&%%%%%%%###@@@&%%%%%%%%%%%%%%%&%&%%%&&&&&&&&&&%%&%%&&%%%&%%%%%%%%%%%%%%%%%%%%%###########
%#############################(#(#############################*/,,,,..,.,**/,,..,/////*(#&#/*****////(##%%%%%&%%%%%%%%%%&&@@@&&&%&%&&%%%%%%%%%%%%%%%%%%%%%################((((((((((((((((((((((((((((((
%##########################################################(###,,,*,,,,,*#*,,,..,**/*/#%%&%#(/////(##%%%%#&%%%%%%&%%%%%%@@@@@%###############(#(#((((((((((((((((((((((((((((((((((((((((((((###########
%############################################################(#/****,***#&//,,,,*/(%@@@&%%%####(/((#%%%%#(#%%%%%&&&&&&%&%@@%###(((((((((((((((((((((((((((((((#(#####################%%%%%%%%%%%%%%%%%%%
%###############################################################(//***/#%#/***((##&&&&%%#((#%%&%///*/(#%%/((%%%&&&&&&&&&&@@@@##############################%%%%%%%%%%%%%%%%%%%%%&&%&%&&&&&&&&&&&&&&&&&&&
%#################################################################*//*/(/*/***,,,,/#(##((##%&&@@@&%**(#%%(/(#%&&&&&&&&&&&@@@@%#@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%####
%##################################################################***/*//*(/(###%%%#((%#@@@%#%%%((/*/(#%#((&%&&@&&&&&&&@@@@@%#@@@@@@&%%%%%%%%%%%%%%%%%%%%#############(#####((((((((((#((((((((((((((((
%##################################################################******/#(/*///***,,**/(##%%%#((/(**(#%%(#&&&&&&&&@&@%@@@@@%@@@@@@@@@&&&#(((((((((((((((((((((((((((((((((((((((((((((((((((##########
%###################################################################******(/***/**/*/##%%#(#((##(//**//#&##%&&&&&&&@@@%@@@@@%@@@@@@@@@@@@@@&&&&&&((((((((((((((((((((((#######################%%%%%%%%%%
%#####################################################################****/**********/*/*//(((/(//**//(%##%&&&@&&@@@%@@@@@@%@@@@@@@@@@&@@&&&&&&&&&&&&&#######%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&
%############################################################%%%%&&&&%&/*//,,,****,,,,,,***/*///*//(#%#%%&&&&@@&&@@@@@@@@@%@@@@@@@@@@@@@@&&&&&&&&&&&%&&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%#
%###################################################%%%%%%%%%%%%%&&&&&&&&*//*,**,,,,,,,,****///*(#%&&%%&&&&&@@@@((@@@@@@%#@@@@@@@@@@@@@@@&&&&&&&&&%&&%&&&&&&&%&&&%%#############(##(##(((((#((((((((((((
%##############################################%&%%%%%%%%%%%%%%%%&&&%%%&&&&/(/*,**,******//((###&&&&&&&&&&@@@@(#%@@@@@@&@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&%%%(((((((((((((((((((####(#######
%%###########################################%&%%%%%%%%&%%%&%&%&&&&&&&&@@@@@@(/(/*//(((((#%%%%%&&%&&&&&&@&&(#&&@@@@@@%%@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&########%#%%%%%%%%%%%%%%%%%
%%%########################################%&&%&%%%&&&%&&&&&&&&&&&&&&@@@@@@@@@@&#(((%%%%%%%%%%%%%%%%%&&(((/(@@@@@@@&&@@@@@@@&@&@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@&&&&&@@@@%%%%%&%&&&&&&&&&&&&&&&&&
%#########################################%@&&&%&%%%&&&&&&&&&&&&&&&@@@@@@@@@&&&&&@@@  %###%%%%%%%%%//((/(%@@@@@@@&%@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@%%%%%%%%%%%%%%%%%%%%%
%########################################&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@&&&&&@@@@@@@ ,   ((/,////((##@@@@@@@@&%@@@@@@@&&@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@(((((((#(##(((((###
%#######################################&&@&&%&&&&&&&&&&&&&&&&&&@@@@@@@@&&&&&&@@@@@@@@(     ..,,*///@@@@@@@&&%@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@&@@@@@@@@@@@@@@@@@@@@(#((((((##########
%#######################################&&@&&&&&&&&&&&&&&&&&&&@@@@@@@@@&&&&&&@@@@@@@@@/           @@@@@@#(#&@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@######%#%%%%%%%%%
%%%%%%##################################&@&&&&&&&&&&&&&&&&&&@@@@@@@@@@&&&&&&&@@@@@@@@@*          @@@@@&(&@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@&&&%&&&&&&&&&&&&&
%%#######((((((((#####################%&&&@&&&&&&&&&&&&&&&&@@@@@@@@&&&@@&((&@@@@@@@@@#*         @@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@&&@@@@@@@@@@@@@@@@@@@@@@@@@@@&%%%%%%%%%%%##%#%
%#####(((((((((//(((#################&&&&&@&&&&&&&&&&&&&&@@@@@@@@@&#%@&@@@@@@@@@&&@@@*,.       /@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@#((((((((((((((((
%#((((((((((((///////(((############&&&&&&&&&&&&&&&&&&&@@@@@@@@&&@%@&@@/&@@@@@@@@@@@@,,.       &&@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@((((((((((######
%###((((((((///(/////////((#######%&&&&&&&&&&&&&&&&&&&@&@@@@@@&&&&%#@&&&%@@@(@@@@@%@(,.      (/@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@####%#%%%%%%%%%
%##((((((((((((////////////(((##&&&&&&&&&&&&&&&&&&&@&@@@@@@@@@&&&&@@@&%%&@@@@@@@@@%@/..    ,  @@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&%&&%&&&&&&&&&
%(((((((((((((((((//////((((#&&&&&&&@&&&&&&&&&&&&&&&@@@@@@@&&&&&&&@@@@%@@@@@@@@@@@@&*.       ,@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&%%%%%%%%%%%
%((((((((((((//((((((((##%&&&&&&&&&&&@@&&@&&&&&&&@@@@@@@@&&&&&&&&&&@@@@&#@@@@@@@@@@(,.    .   @@@@@@@@@@@@&@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@&@@@@@@@@@@@@@@@@@@&@@@@@@@@@@@@@@@@@@@@@@##########
%(((((((((((((((###%&&&&&&&&&&&&&&@@@@@@@&&&&&&&&@@@@@@&&&&&&&&&&&&@@@&%&&@@@@@@@&@/,.    ....@@@@@@@@@@@@@&@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##########

:- begin_tests(desplazar).

test(desplazar) :-
    Tablero = [ [r2, a2, r1, v1, a1],
                [r3, a2, r1, v1, r2],
                [a1, v1, v1, a2, r1],
                [r1, v2, v2, a1, a3],
                [a1, a3, a1, v1, v2] ],
    desplazar(abajo, 2, 1, Tablero, EvoTablero),
    writeln(EvoTablero).

test(desplazar) :-
    Tablero = [ [r1, r2, v2, v1, a3],
                [r1, v3, a1, a1, r3],
                [a3, r1, r1, v2, v3],
                [r1, v2, r3, v2, a2],
                [r1, a2, a3, v2, a1] ],
    desplazar(izq, 2, 1, Tablero, EvoTablero),
    writeln(EvoTablero).

:- end_tests(desplazar).
