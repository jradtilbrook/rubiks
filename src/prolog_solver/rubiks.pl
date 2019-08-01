% %
% Solving Core for Rubiks
% %
%   T
% L F R
%   G
%   B
% Cube format is cube(Tx9, Fx9, Lx9, Rx9, Gx9, Bx9)
% Query example - moves(Solution,  cube(g,w,w,g,w,w,r,r,r,b,b,o,y,y,o,y,y,o,g,g,r,g,g,r,y,y,b,o,b,b,o,b,b,g,b,b,w,w,w,r,r,y,r,r,y,o,o,w,o,o,w,y,g,g), C), solved(C).

% FACT 1 - A solved cube has all the faces in the right place
solved(cube(T,T,T,T,T,T,T,T,T,F,F,F,F,F,F,F,F,F,L,L,L,L,L,L,L,L,L,R,R,R,R,R,R,R,R,R,G,G,G,G,G,G,G,G,G,B,B,B,B,B,B,B,B,B)).

% FACT 2 - moves can be made twisting a face of the cube
move(up,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, G1, G2, G3, G4, G5, G6, G7, G8, G9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, F1, F2, F3, F4, F5, F6, F7, F8, F9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(T7, T4, T1, T8, T5, T2, T9, T6, T3, G1, G2, G3, G4, G5, G6, G7, G8, G9, L1, L2, F1, L4, L5, F2, L7, L8, F3, B7, R2, R3, B8, R5, R6, B9, R8, R9, R7, R4, R1, F4, F5, F6, F7, F8, F9, B1, B2, B3, B4, B5, B6, L9, L6, L3)).

move(down,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, G1, G2, G3, G4, G5, G6, G7, G8, G9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, F1, F2, F3, F4, F5, F6, F7, F8, F9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, G3, G6, G9, G2, G5, G8, G1, G4, G7, F7, L2, L3, F8, L5, L6, F9, L8, L9, R1, R2, B1, R4, R5, B2, R7, R8, B3, F1, F2, F3, F4, F5, F6, R9, R6, R3, L7, L4, L1, B4, B5, B6, B7, B8, B9)).

move(right,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, G1, G2, G3, G4, G5, G6, G7, G8, G9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, F1, F2, F3, F4, F5, F6, F7, F8, F9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(T1, T2, F3, T4, T5, F6, T7, T8, F9, G1, G2, B3, G4, G5, B6, G7, G8, B9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R7, R4, R1, R8, R5, R2, R9, R6, R3, F1, F2, G3, F4, F5, G6, F7, F8, G9, B1, B2, T3, B4, B5, T6, B7, B8, T9)).

move(left,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, G1, G2, G3, G4, G5, G6, G7, G8, G9, L7, L4, L1, L8, L5, L2, L9, L6, L3, R1, R2, R3, R4, R5, R6, R7, R8, R9, F1, F2, F3, F4, F5, F6, F7, F8, F9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(B1, T2, T3, B4, T5, T6, B7, T8, T9, F1, G2, G3, F4, G5, G6, F7, G8, G9, L7, L4, L1, L8, L5, L2, L9, L6, L3, R1, R2, R3, R4, R5, R6, R7, R8, R9, T1, F2, F3, T4, F5, F6, T7, F8, F9, G1, B2, B3, G4, B5, B6, G7, B8, B9)).

move(front,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, G1, G2, G3, G4, G5, G6, G7, G8, G9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, F1, F2, F3, F4, F5, F6, F7, F8, F9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(T1, T2, T3, T4, T5, T6, L7, L8, L9, R9, R8, R7, G4, G5, G6, G7, G8, G9, L1, L2, L3, L4, L5, L6, G3, G2, G1, R1, R2, R3, R4, R5, R6, T7, T8, T9, F7, F4, F1, F8, F5, F2, F9, F6, F3, B1, B2, B3, B4, B5, B6, B7, B8, B9)).

move(back,
    cube(T1, T2, T3, T4, T5, T6, T7, T8, T9, G1, G2, G3, G4, G5, G6, G7, G8, G9, L1, L2, L3, L4, L5, L6, L7, L8, L9, R1, R2, R3, R4, R5, R6, R7, R8, R9, F1, F2, F3, F4, F5, F6, F7, F8, F9, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    cube(L1, L2, L3, T4, T5, T6, T7, T8, T9, G1, G2, G3, G4, G5, G6, R3, R2, R1, G9, G8, G7, L4, L5, L6, L7, L8, L9, T1, T2, T3, R4, R5, R6, R7, R8, R9, F1, F2, F3, F4, F5, F6, F7, F8, F9, B3, B6, B9, B2, B5, B8, B1, B4, B7)).

% FACT 3 - Allow anticlockwise rotations
%move(+Move, From, To) :- move(Move, From, To).
%move(-Move, From, To) :- move(Move, To, From).

% FACT 4 - An empty list of moves doesn't move the cube
moves([], Cube, Cube).

% This is the actual solver. It reads as follows:
% moves(List, Cube, NewCube) is true
% IF
% There are moves from the current cube to the new cube
% AND
% there is a move NextMove from a cube to the current cube.
% It's a recursive solver
moves([NextMove | Moves], Cube, NewCube) :- moves(Moves, CurrentCube, NewCube), move(NextMove, Cube, CurrentCube).
