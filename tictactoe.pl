/*
    TATETI Para SWI Prolog por Emilio Moretti.

    Para comenzar a jugar ingrese :-jugarTateti.
    para seleccionar una casilla ingrese :-tic(numeroDeCasilla).

    Por defecto la primer partida la comienza la máquina.
    La máqunia es el O y usted es el X.

    emilio.morettiATgmailDOTcom

    Copyright 2012 Emilio Moretti <emilio.morettiATgmailDOTcom>
    This program is distributed under the terms of the GNU Lesser General Public License.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU  Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


:-dynamic comienza/1. /*quien comienza 2*/
:-dynamic modo/1. /*estrategia con la que comienza 3*/
comienza(2).
modo(3).

/*
OBSOLETO, no cambiar:
para que no pierda hay que poner 2 aca, este predicado deshabilita las dos últimas estrategia
que son más debiles y que se pueden vencer con más facilidad

Default:4  (Todas las estrategias)*/
habilitar_gana_o_empata(2). 

/*
Base de datos con todos los tateti posibles

*/
iguales(X,X,X).
tateti:- box(1,X),box(2,Y),box(3,Z),iguales(X,Y,Z).
tateti:- box(4,X),box(5,Y),box(6,Z),iguales(X,Y,Z).
tateti:- box(7,X),box(8,Y),box(9,Z),iguales(X,Y,Z).
tateti:- box(1,X),box(4,Y),box(7,Z),iguales(X,Y,Z).
tateti:- box(2,X),box(5,Y),box(8,Z),iguales(X,Y,Z).
tateti:- box(3,X),box(6,Y),box(9,Z),iguales(X,Y,Z).
tateti:- box(1,X),box(5,Y),box(9,Z),iguales(X,Y,Z).
tateti:- box(3,X),box(5,Y),box(7,Z),iguales(X,Y,Z).

/*
Usado por las estrategias
*/
centro(5).
opuesta(1,9).
opuesta(9,1).
opuesta(2,8).
opuesta(8,2).
opuesta(7,3).
opuesta(3,7).
opuesta(4,6).
opuesta(6,4).
esquina(1).
esquina(3).
esquina(9).
esquina(7).
lateral(2).
lateral(4).
lateral(6).
lateral(8).
arista(1,2,3).
arista(3,2,1).
arista(1,4,7).
arista(7,4,1).
arista(3,6,9).
arista(9,6,3).
arista(7,8,9).
arista(9,8,7).



/*
Decide si insertando A se genera un tateti.
Esta es la parte utilizada por la IA para decidir
donde mover la pieza.
Primero checkea movidas del adversario.
Luego checkea los posibles ta te ti propios.
en el caso de que no haya posibilidad hace una jugada aleatoria
*/

/*
Devuelve el numero que es distinto
*/
dosIguales(X,X,Z,Z).
dosIguales(X,Z,X,Z).
dosIguales(Z,X,X,Z).
dosIguales(X,X,Z,Z,X).
dosIguales(X,Z,X,Z,X).
dosIguales(Z,X,X,Z,X).


/*devuelve la jugada que lograría un TATETI en ese momento
Si el que tiene posibilidad de hacer ta te ti soy yo entonces es una jugada ganadora!
Jugador=X(jugador humano)|O(IA)
Jugada=Numero donde debería hacerse la jugada para que se produsca TATETI
*/

winPlay(Jugador,Jugada):- box(1,X),box(2,Y),box(3,Z),dosIguales(X,Y,Z,I,J),not(I = 'X'),not(I = 'O'),J = Jugador, Jugada is I.
winPlay(Jugador,Jugada):- box(4,X),box(5,Y),box(6,Z),dosIguales(X,Y,Z,I,J),not(I = 'X'),not(I = 'O'),J = Jugador, Jugada is I.
winPlay(Jugador,Jugada):- box(7,X),box(8,Y),box(9,Z),dosIguales(X,Y,Z,I,J),not(I = 'X'),not(I = 'O'),J = Jugador, Jugada is I.
winPlay(Jugador,Jugada):- box(1,X),box(4,Y),box(7,Z),dosIguales(X,Y,Z,I,J),not(I = 'X'),not(I = 'O'),J = Jugador, Jugada is I.
winPlay(Jugador,Jugada):- box(2,X),box(5,Y),box(8,Z),dosIguales(X,Y,Z,I,J),not(I = 'X'),not(I = 'O'),J = Jugador, Jugada is I.
winPlay(Jugador,Jugada):- box(3,X),box(6,Y),box(9,Z),dosIguales(X,Y,Z,I,J),not(I = 'X'),not(I = 'O'),J = Jugador, Jugada is I.
winPlay(Jugador,Jugada):- box(1,X),box(5,Y),box(9,Z),dosIguales(X,Y,Z,I,J),not(I = 'X'),not(I = 'O'),J = Jugador, Jugada is I.
winPlay(Jugador,Jugada):- box(3,X),box(5,Y),box(7,Z),dosIguales(X,Y,Z,I,J),not(I = 'X'),not(I = 'O'),J = Jugador, Jugada is I.






tablero:- box(1,A),
		box(2,B),
		box(3,C),
		box(4,D),
		box(5,E),
		box(6,F),
		box(7,G),
		box(8,H),
		box(9,I),
		write('-------'),nl,
		write('|'),write(A),write('|'),write(B),write('|'),write(C),write('|'),nl,
		write('-------'),nl,
		write('|'),write(D),write('|'),write(E),write('|'),write(F),write('|'),nl,
		write('-------'),nl,
		write('|'),write(G),write('|'),write(H),write('|'),write(I),write('|'),nl,
		write('-------'),nl.

jugarTateti:- restartTateti,write('Bienvenido al TATETI invensible, '),nl,
write('para jugar ingrese :-tic(número).'),nl,
write('La máquina es el símbolo O.'),nl,
		(comienza(1),
		(modo(X);true),
		(retract(modo(_));true),
		( habilitar_gana_o_empata(Jota),((X+1) < Jota,(X1 is X + 1));(X1 is 1)),
		assert(modo(X1)),
		automaticPlay;true),
		tablero,!.
		

		
tic(X):- not(tateti),box(X,Y),not(Y = 'X'),not(Y = 'O'), retract(box(X,_)), assert(box(X,'X')),
		not(tateti),(automaticPlay),tablero,!.

tic(X):-tateti->(tablero,nl,write('Fin de la partida, suerte de principiante...'),nl);not(jugadaPosible),write('Fin de la partida. Empate.'),nl,tablero,!;(write('El casillero está ocupado'),nl,fail).

iaTic(X):- not(tateti),box(X,Y),not(Y = 'X'),not(Y = 'O'), retract(box(X,_)), assert(box(X,'O')),
		not(tateti),nl, jugadaPosible, write('Tu turno'),nl,!.

iaTic(X):-tateti->(nl,write('Fin de la partida, gané (para variar)... ;)'),nl);not(jugadaPosible),nl,fail.



restartTateti:-(retract(box(1,_)),retract(box(2,_)),retract(box(3,_)),retract(box(4,_)),retract(box(5,_)),retract(box(6,_)),retract(box(7,_)),retract(box(8,_)),retract(box(9,_));true),
(retract(estrategia(_,_));true),
assert(box(1,1)),
assert(box(2,2)),
assert(box(3,3)),
assert(box(4,4)),
assert(box(5,5)),
assert(box(6,6)),
assert(box(7,7)),
assert(box(8,8)),
assert(box(9,9)),
(comienza(Y);true),
(retract(comienza(_));true),
( Y = 1,(Y1 is 2);(Y1 is 1)),
assert(comienza(Y1)),
!.

/*
Chequeo si el otro está por ganar, y le arruino la jugada. En caso de que no esté por ganar chequeo si yo puedo ganar.
En caso en que no se cumpla ninguno de los dos se aplica una estrategia.
*/
automaticPlay:- winPlay('O',W)->iaTic(W);
								(winPlay('X',M)->iaTic(M)
											;jugadaEstrategica
												).


/*
4 casos:
es la primer jugada DEL PARTIDO osea que box(X,Y) es falso, para eso ponemos la ficha en cualquier lugar vacío
es la primer jugada MIA osea que box(X,'O') es falso, pero box(X,'X') no lo es

hay fichas en juego asi que deberíamos jugar algo con probabilidad de ta te ti
ya hay jugadas y no hay posibilidad de ta te ti asi que tiramos fruta
*/
jugadaEstrategica:-	(estrategia(X,Y),kill_the_duck(X,Y));

					((not(box(A,'O');box(B,'X'))),primera_jugada);
					
					((not(box(X,'O')),box(X,'X')),segunda_jugada);
					
					(jugadaAleatoria).


/*
Estrategias de ataque
modo X indica la estrategia que se utilizará, a no hacer trampa!!!!
*/

primera_jugada:-modo(X),kill_the_duck(X,1).


/*
Estrategias de defensa.
El turno empezó por el jugador humano, entonces eliminamos toda posibilidad de ganar:
*/
segunda_jugada:- box(X,'X'),
				(
				(centro(X),kill_the_duck(4,1));
				(esquina(X),kill_the_duck(5,1));
				(lateral(X),kill_the_duck(6,1,X))
				).
				
				

kill_the_duck(1,1):- (retract(estrategia(_,_));true),
					centro(X),iaTic(X),
					assert(estrategia(1,2)).

kill_the_duck(1,2):- (retract(estrategia(_,_));true),
					esquina(X),iaTic(X),
				assert(estrategia(1,3)).

kill_the_duck(1,3):- (retract(estrategia(_,_));true),
					esquina(X),
					arista(X,Y,Z),
					box(Y,Y1),
					not(Y1='X'),
					box(Z,Z1),
					Z1='O',
					
					iaTic(X).

kill_the_duck(2,1):-(retract(estrategia(_,_));true),
					esquina(X),iaTic(X),
					assert(estrategia(2,2)),
					assert(last_move(X)).
					
kill_the_duck(2,2):-(retract(estrategia(_,_));true),
					last_move(Y),
					(retract(last_move(_));true),
					opuesta(Y,X),iaTic(X),
					assert(estrategia(2,3)).

kill_the_duck(2,3):-(retract(estrategia(_,_));true),
					esquina(X),iaTic(X).					
					
kill_the_duck(3,1):-(retract(estrategia(_,_));true),
					esquina(X),iaTic(X),
					assert(estrategia(3,2)),
					assert(last_move(X)).

					
/*
En el caso de que nos arruine esta estrategia poniendo en el centro...
entonces actuamos como la estrategia 2 paso 2 y saltamos alpaso 3.
*/
kill_the_duck(3,2):-(retract(estrategia(_,_));true),
					last_move(Y),
					(retract(last_move(_));true),
					(centro(C),lateral(X),arista(Y,X,Z),jugadaPosible(C),jugadaPosible(X),jugadaPosible(Z),iaTic(X),
					assert(estrategia(3,3)));
					opuesta(Y,X),iaTic(X),
					assert(estrategia(2,3)).
					
kill_the_duck(3,3):-(retract(estrategia(_,_));true),
					centro(X),iaTic(X).
					


				
kill_the_duck(4,1):-esquina(Y),
					iaTic(Y), 
					(retract(estrategia(_,_));true),
					assert(estrategia(4,2)).

kill_the_duck(4,2):-retract(estrategia(4,2)),(esquina(Y),iaTic(Y)).

kill_the_duck(5,1):-(retract(estrategia(_,_));true),
					(centro(Y),iaTic(Y)),				
				assert(estrategia(5,2)).

kill_the_duck(5,2):-retract(estrategia(5,2)),
					(lateral(Y),iaTic(Y)).

kill_the_duck(6,1,X):-(opuesta(X,Y),iaTic(Y)).

/*
La jugada aleatoria siempre intenta obtener el centro primero
*/
jugadaAleatoria:-centro(X),iaTic(X),!.
jugadaAleatoria:-box(X,Y),not(Y='X';Y='O'),iaTic(X),!.


jugadaPosible:-box(X,Y),not(Y='X';Y='O').
jugadaPosible(X):-box(X,Y),not(Y='X';Y='O').
