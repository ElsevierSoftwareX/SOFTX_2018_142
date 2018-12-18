(* ============================================================================== *)
(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* CHESSY                                                                         *)
(* v1.0                                                                           *)
(*                                                                                *)
(* Chess Graph Generation, Visualization and Analysis Toolbox                     *)
(* for Mathematica v10.0+                                                         *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* AUTHOR  :: Dr. M. Rudolph-Lilith                                               *)
(*                                                                                *)
(* ADDRESS :: Unité de Neurosciences, Information & Complexité (UNIC)             *)
(*            CNRS UPR-3293                                                       *)
(*            Bat. 33, Avenue de la Terrasse 1                                    *)
(*            91198 Gif-sur-Yvette, FRANCE                                        *)
(*                                                                                *)
(* EMAIL   :: which.lilith@gmail.com                                              *)
(*            rudolphlilith@protonmail.com                                        *)
(*                                                                                *)
(* URL     :: http://mrudolphlilith.github.io/contact.html                        *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* Copyright 2018 M. Rudolph-Lilith                                               *)
(*                                                                                *)
(* Redistribution and use in source and binary forms, with or without             *)
(* modification, are permitted provided that the following conditions are met:    *)
(*                                                                                *)
(* 1. Redistributions of source code must retain the above copyright notice,      *)
(*    this list of conditions and the following disclaimer.                       *)
(*                                                                                *)
(* 2. Redistributions in binary form must reproduce the above copyright notice,   *)
(*    this list of conditions and the following disclaimer in the documentation   *)
(*    and/or other materials provided with the distribution.                      *)
(*                                                                                *)
(* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"    *)
(* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE      *)
(* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE     *)
(* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE      *)
(* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR            *)
(* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF           *)
(* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS       *)
(* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN        *)
(* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)        *)
(* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE     *)
(* POSSIBILITY OF SUCH DAMAGE.                                                    *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)
(* ============================================================================== *)
 
Clear["Global`*"]; Clear["All`*"]

(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* AUXILIARY CONSTANTS, FUNCTIONS AND OBJECTS                                     *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

(* ------------------------------------------------------------------------------ *)
(* functions for various conversions of coordinates chessboard <-> chessgraph;    *)
(*		file[]  ::  node -> file                                                  *)
(*		rank[]  ::  node -> rank                                                  *)
(*      node[]  ::  (file,rank) -> node                                           *)
(* ------------------------------------------------------------------------------ *)

file[ node_ ] := If[ node<1 || node>64 , 0 , node - 8*Floor[(node-1)/8] ];
rank[ node_ ] := If[ node<1 || node>64 , 0 , Floor[(node-1)/8+1] ];
node[ file_ , rank_ ] := If[ file<1 || file>8 || rank<1 || rank>8 , 0 , 8*(rank-1) + (file-1) + 1 ];

f2f = <| "a"->1 , "b"->2 , "c"->3 , "d"->4 , "e"->5 , "f"->6 , "g"->7 , "h"->8 |>;
r2r = <| "1"->1 , "2"->2 , "3"->3 , "4"->4 , "5"->5 , "6"->6 , "7"->7 , "8"->8 |>;

(* ------------------------------------------------------------------------------ *)
(* lists providing the nodes in each of the cardinal and intercardinal            *)
(* directions (N,NE,E,SE,S,SW,W,NW), indexed by the node of origin;               *)
(* these lists are used to generate edges for Queen, Rook and Bishop              *)
(* ------------------------------------------------------------------------------ *)

mN = {
	{9,17,25,33,41,49,57},{10,18,26,34,42,50,58},{11,19,27,35,43,51,59},{12,20,28,36,44,52,60},{13,21,29,37,45,53,61},{14,22,30,38,46,54,62},{15,23,31,39,47,55,63},{16,24,32,40,48,56,64},
	{17,25,33,41,49,57},{18,26,34,42,50,58},{19,27,35,43,51,59},{20,28,36,44,52,60},{21,29,37,45,53,61},{22,30,38,46,54,62},{23,31,39,47,55,63},{24,32,40,48,56,64},
	{25,33,41,49,57},{26,34,42,50,58},{27,35,43,51,59},{28,36,44,52,60},{29,37,45,53,61},{30,38,46,54,62},{31,39,47,55,63},{32,40,48,56,64},
	{33,41,49,57},{34,42,50,58},{35,43,51,59},{36,44,52,60},{37,45,53,61},{38,46,54,62},{39,47,55,63},{40,48,56,64},
	{41,49,57},{42,50,58},{43,51,59},{44,52,60},{45,53,61},{46,54,62},{47,55,63},{48,56,64},
	{49,57},{50,58},{51,59},{52,60},{53,61},{54,62},{55,63},{56,64},
	{57},{58},{59},{60},{61},{62},{63},{64},
	{},{},{},{},{},{},{},{}
};

mNE = {
	{10,19,28,37,46,55,64},{11,20,29,38,47,56},{12,21,30,39,48},{13,22,31,40},{14,23,32},{15,24},{16},{},
	{18,27,36,45,54,63},{19,28,37,46,55,64},{20,29,38,47,56},{21,30,39,48},{22,31,40},{23,32},{24},{},
	{26,35,44,53,62},{27,36,45,54,63},{28,37,46,55,64},{29,38,47,56},{30,39,48},{31,40},{32},{},
	{34,43,52,61},{35,44,53,62},{36,45,54,63},{37,46,55,64},{38,47,56},{39,48},{40},{},
	{42,51,60},{43,52,61},{44,53,62},{45,54,63},{46,55,64},{47,56},{48},{},
	{50,59},{51,60},{52,61},{53,62},{54,63},{55,64},{56},{},
	{58},{59},{60},{61},{62},{63},{64},{},
	{},{},{},{},{},{},{},{}
};

mE = {
	{2,3,4,5,6,7,8},{3,4,5,6,7,8},{4,5,6,7,8},{5,6,7,8},{6,7,8},{7,8},{8},{},
	{10,11,12,13,14,15,16},{11,12,13,14,15,16},{12,13,14,15,16},{13,14,15,16},{14,15,16},{15,16},{16},{},
	{18,19,20,21,22,23,24},{19,20,21,22,23,24},{20,21,22,23,24},{21,22,23,24},{22,23,24},{23,24},{24},{},
	{26,27,28,29,30,31,32},{27,28,29,30,31,32},{28,29,30,31,32},{29,30,31,32},{30,31,32},{31,32},{32},{},
	{34,35,36,37,38,39,40},{35,36,37,38,39,40},{36,37,38,39,40},{37,38,39,40},{38,39,40},{39,40},{40},{},
	{42,43,44,45,46,47,48},{43,44,45,46,47,48},{44,45,46,47,48},{45,46,47,48},{46,47,48},{47,48},{48},{},
	{50,51,52,53,54,55,56},{51,52,53,54,55,56},{52,53,54,55,56},{53,54,55,56},{54,55,56},{55,56},{56},{},
	{58,59,60,61,62,63,64},{59,60,61,62,63,64},{60,61,62,63,64},{61,62,63,64},{62,63,64},{63,64},{64},{}
};

mSE = {
	{},{},{},{},{},{},{},{},
	{2},{3},{4},{5},{6},{7},{8},{},
	{10,3},{11,4},{12,5},{13,6},{14,7},{15,8},{16},{},
	{18,11,4},{19,12,5},{20,13,6},{21,14,7},{22,15,8},{23,16},{24},{},
	{26,19,12,5},{27,20,13,6},{28,21,14,7},{29,22,15,8},{30,23,16},{31,24},{32},{},
	{34,27,20,13,6},{35,28,21,14,7},{36,29,22,15,8},{37,30,23,16},{38,31,24},{39,32},{40},{},
	{42,35,28,21,14,7},{43,36,29,22,15,8},{44,37,30,23,16},{45,38,31,24},{46,39,32},{47,40},{48},{},
	{50,43,36,29,22,15,8},{51,44,37,30,23,16},{52,45,38,31,24},{53,46,39,32},{54,47,40},{55,48},{56},{}
};

mS = {
	{},{},{},{},{},{},{},{},
	{1},{2},{3},{4},{5},{6},{7},{8},
	{9,1},{10,2},{11,3},{12,4},{13,5},{14,6},{15,7},{16,8},
	{17,9,1},{18,10,2},{19,11,3},{20,12,4},{21,13,5},{22,14,6},{23,15,7},{24,16,8},
	{25,17,9,1},{26,18,10,2},{27,19,11,3},{28,20,12,4},{29,21,13,5},{30,22,14,6},{31,23,15,7},{32,24,16,8},
	{33,25,17,9,1},{34,26,18,10,2},{35,27,19,11,3},{36,28,20,12,4},{37,29,21,13,5},{38,30,22,14,6},{39,31,23,15,7},{40,32,24,16,8},
	{41,33,25,17,9,1},{42,34,26,18,10,2},{43,35,27,19,11,3},{44,36,28,20,12,4},{45,37,29,21,13,5},{46,38,30,22,14,6},{47,39,31,23,15,7},{48,40,32,24,16,8},
	{49,41,33,25,17,9,1},{50,42,34,26,18,10,2},{51,43,35,27,19,11,3},{52,44,36,28,20,12,4},{53,45,37,29,21,13,5},{54,46,38,30,22,14,6},{55,47,39,31,23,15,7},{56,48,40,32,24,16,8}
};

mSW = {
	{},{},{},{},{},{},{},{},
	{},{1},{2},{3},{4},{5},{6},{7},
	{},{9},{10,1},{11,2},{12,3},{13,4},{14,5},{15,6},
	{},{17},{18,9},{19,10,1},{20,11,2},{21,12,3},{22,13,4},{23,14,5},
	{},{25},{26,17},{27,18,9},{28,19,10,1},{29,20,11,2},{30,21,12,3},{31,22,13,4},
	{},{33},{34,25},{35,26,17},{36,27,18,9},{37,28,19,10,1},{38,29,20,11,2},{39,30,21,12,3},
	{},{41},{42,33},{43,34,25},{44,35,26,17},{45,36,27,18,9},{46,37,28,19,10,1},{47,38,29,20,11,2},
	{},{49},{50,41},{51,42,33},{52,43,34,25},{53,44,35,26,17},{54,45,36,27,18,9},{55,46,37,28,19,10,1}
};

mW = {
	{},{1},{2,1},{3,2,1},{4,3,2,1},{5,4,3,2,1},{6,5,4,3,2,1},{7,6,5,4,3,2,1},
	{},{9},{10,9},{11,10,9},{12,11,10,9},{13,12,11,10,9},{14,13,12,11,10,9},{15,14,13,12,11,10,9},
	{},{17},{18,17},{19,18,17},{20,19,18,17},{21,20,19,18,17},{22,21,20,19,18,17},{23,22,21,20,19,18,17},
	{},{25},{26,25},{27,26,25},{28,27,26,25},{29,28,27,26,25},{30,29,28,27,26,25},{31,30,29,28,27,26,25},
	{},{33},{34,33},{35,34,33},{36,35,34,33},{37,36,35,34,33},{38,37,36,35,34,33},{39,38,37,36,35,34,33},
	{},{41},{42,41},{43,42,41},{44,43,42,41},{45,44,43,42,41},{46,45,44,43,42,41},{47,46,45,44,43,42,41},
	{},{49},{50,49},{51,50,49},{52,51,50,49},{53,52,51,50,49},{54,53,52,51,50,49},{55,54,53,52,51,50,49},
	{},{57},{58,57},{59,58,57},{60,59,58,57},{61,60,59,58,57},{62,61,60,59,58,57},{63,62,61,60,59,58,57}
};

mNW = {
	{},{9},{10,17},{11,18,25},{12,19,26,33},{13,20,27,34,41},{14,21,28,35,42,49},{15,22,29,36,43,50,57},
	{},{17},{18,25},{19,26,33},{20,27,34,41},{21,28,35,42,49},{22,29,36,43,50,57},{23,30,37,44,51,58},
	{},{25},{26,33},{27,34,41},{28,35,42,49},{29,36,43,50,57},{30,37,44,51,58},{31,38,45,52,59},
	{},{33},{34,41},{35,42,49},{36,43,50,57},{37,44,51,58},{38,45,52,59},{39,46,53,60},
	{},{41},{42,49},{43,50,57},{44,51,58},{45,52,59},{46,53,60},{47,54,61},
	{},{49},{50,57},{51,58},{52,59},{53,60},{54,61},{55,62},
	{},{57},{58},{59},{60},{61},{62},{63},
	{},{},{},{},{},{},{},{}
};

(* ------------------------------------------------------------------------------ *)
(* lists providing the nodes for generating edges for King, Knight and Pawn,      *)
(* indexed by the node of origin;                                                 *)
(* ------------------------------------------------------------------------------ *)

(* standard moves of the white/black King *)

mKing = {
	{2,9,10},{1,3,9,10,11},{2,4,10,11,12},{3,5,11,12,13},{4,6,12,13,14},{5,7,13,14,15},{6,8,14,15,16},{7,15,16},
	{1,2,10,17,18},{1,2,3,9,11,17,18,19},{2,3,4,10,12,18,19,20},{3,4,5,11,13,19,20,21},{4,5,6,12,14,20,21,22},{5,6,7,13,15,21,22,23},{6,7,8,14,16,22,23,24},{7,8,15,23,24},
	{9,10,18,25,26},{9,10,11,17,19,25,26,27},{10,11,12,18,20,26,27,28},{11,12,13,19,21,27,28,29},{12,13,14,20,22,28,29,30},{13,14,15,21,23,29,30,31},{14,15,16,22,24,30,31,32},{15,16,23,31,32},
	{17,18,26,33,34},{17,18,19,25,27,33,34,35},{18,19,20,26,28,34,35,36},{19,20,21,27,29,35,36,37},{20,21,22,28,30,36,37,38},{21,22,23,29,31,37,38,39},{22,23,24,30,32,38,39,40},{23,24,31,39,40},
	{25,26,34,41,42},{25,26,27,33,35,41,42,43},{26,27,28,34,36,42,43,44},{27,28,29,35,37,43,44,45},{28,29,30,36,38,44,45,46},{29,30,31,37,39,45,46,47},{30,31,32,38,40,46,47,48},{31,32,39,47,48},
	{33,34,42,49,50},{33,34,35,41,43,49,50,51},{34,35,36,42,44,50,51,52},{35,36,37,43,45,51,52,53},{36,37,38,44,46,52,53,54},{37,38,39,45,47,53,54,55},{38,39,40,46,48,54,55,56},{39,40,47,55,56},
	{41,42,50,57,58},{41,42,43,49,51,57,58,59},{42,43,44,50,52,58,59,60},{43,44,45,51,53,59,60,61},{44,45,46,52,54,60,61,62},{45,46,47,53,55,61,62,63},{46,47,48,54,56,62,63,64},{47,48,55,63,64},
	{49,50,58},{49,50,51,57,59},{50,51,52,58,60},{51,52,53,59,61},{52,53,54,60,62},{53,54,55,61,63},{54,55,56,62,64},{55,56,63}
};

(* standard moves of the white/black Knight *)

mKnight = {
	{11,18},{12,17,19},{9,13,18,20},{10,14,19,21},{11,15,20,22},{12,16,21,23},{13,22,24},{14,23},
	{3,19,26},{4,20,25,27},{1,5,17,21,26,28},{2,6,18,22,27,29},{3,7,19,23,28,30},{4,8,20,24,29,31},{5,21,30,32},{6,22,31},
	{2,11,27,34},{1,3,12,28,33,35},{2,4,9,13,25,29,34,36},{3,5,10,14,26,30,35,37},{4,6,11,15,27,31,36,38},{5,7,12,16,28,32,37,39},{6,8,13,29,38,40},{7,14,30,39},
	{10,19,35,42},{9,11,20,36,41,43},{10,12,17,21,33,37,42,44},{11,13,18,22,34,38,43,45},{12,14,19,23,35,39,44,46},{13,15,20,24,36,40,45,47},{14,16,21,37,46,48},{15,22,38,47},
	{18,27,43,50},{17,19,28,44,49,51},{18,20,25,29,41,45,50,52},{19,21,26,30,42,46,51,53},{20,22,27,31,43,47,52,54},{21,23,28,32,44,48,53,55},{22,24,29,45,54,56},{23,30,46,55},
	{26,35,51,58},{25,27,36,52,57,59},{26,28,33,37,49,53,58,60},{27,29,34,38,50,54,59,61},{28,30,35,39,51,55,60,62},{29,31,36,40,52,56,61,63},{30,32,37,53,62,64},{31,38,54,63},
	{34,43,59},{33,35,44,60},{34,36,41,45,57,61},{35,37,42,46,58,62},{36,38,43,47,59,63},{37,39,44,48,60,64},{38,40,45,61},{39,46,62},
	{42,51},{41,43,52},{42,44,49,53},{43,45,50,54},{44,46,51,55},{45,47,52,56},{46,48,53},{47,54}
};

(* standard moves of the white/black Pawn *)

mwPawn = {
	{},{},{},{},{},{},{},{},
	{17},{18},{19},{20},{21},{22},{23},{24},
	{25},{26},{27},{28},{29},{30},{31},{32},
	{33},{34},{35},{36},{37},{38},{39},{40},
	{41},{42},{43},{44},{45},{46},{47},{48},
	{49},{50},{51},{52},{53},{54},{55},{56},
	{57},{58},{59},{60},{61},{62},{63},{64},
	{},{},{},{},{},{},{},{}
};

mbPawn = {
	{},{},{},{},{},{},{},{},
	{1},{2},{3},{4},{5},{6},{7},{8},
	{9},{10},{11},{12},{13},{14},{15},{16},
	{17},{18},{19},{20},{21},{22},{23},{24},
	{25},{26},{27},{28},{29},{30},{31},{32},
	{33},{34},{35},{36},{37},{38},{39},{40},
	{41},{42},{43},{44},{45},{46},{47},{48},
	{},{},{},{},{},{},{},{}
};

(* additional rank 2/7 move of the white/black Pawn *)

mwPawnR = {
	{},{},{},{},{},{},{},{},
	{25},{26},{27},{28},{29},{30},{31},{32},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{}
};

mbPawnR = {
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{33},{34},{35},{36},{37},{38},{39},{40},
	{},{},{},{},{},{},{},{}
};

(* capture moves of the white/black Pawn *)

mwPawnX = {
	{},{},{},{},{},{},{},{},
	{18},{17,19},{18,20},{19,21},{20,22},{21,23},{22,24},{23},
	{26},{25,27},{26,28},{27,29},{28,30},{29,31},{30,32},{31},
	{34},{33,35},{34,36},{35,37},{36,38},{37,39},{38,40},{39},
	{42},{41,43},{42,44},{43,45},{44,46},{45,47},{46,48},{47},
	{50},{49,51},{50,52},{51,53},{52,54},{53,55},{54,56},{55},
	{58},{57,59},{58,60},{59,61},{60,62},{61,63},{62,64},{63},
	{},{},{},{},{},{},{},{}
};

mbPawnX = {
	{},{},{},{},{},{},{},{},
	{2},{1,3},{2,4},{3,5},{4,6},{5,7},{6,8},{7},
	{10},{9,11},{10,12},{11,13},{12,14},{13,15},{14,16},{15},
	{18},{17,19},{18,20},{19,21},{20,22},{21,23},{22,24},{23},
	{26},{25,27},{26,28},{27,29},{28,30},{29,31},{30,32},{31},
	{34},{33,35},{34,36},{35,37},{36,38},{37,39},{38,40},{39},
	{42},{41,43},{42,44},{43,45},{44,46},{45,47},{46,48},{47},
	{},{},{},{},{},{},{},{}
};

(* en passant moves of the white/black Pawn *)

mwPawnEP = {
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{42},{41,43},{42,44},{43,45},{44,46},{45,47},{46,48},{47},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{}
};

mbPawnEP = {
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{18},{17,19},{18,20},{19,21},{20,22},{21,23},{22,24},{23},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{},
	{},{},{},{},{},{},{},{}
};

(* en passant target nodes, indexed by the target node of the opposing Pawn       *)
(* subject to potential en passant capture; the lists contain the allowed values  *)
(* for the en passant variable used during the evaluation of game positions, and  *)
(* must be used like enPassantXw/b[[<TARGET NODE OF WHITE/BLACK RANK 2/7 PAWN>]]  *)

enPassantXw = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,18,19,20,21,22,23,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
enPassantXb = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,42,43,44,45,46,47,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };

(* ------------------------------------------------------------------------------ *)
(* objects holding the graphical image of chess pieces;                           *)
(* ------------------------------------------------------------------------------ *)

wKg = Import["pieces/wK.eps"];
wQg = Import["pieces/wQ.eps"];
wRg = Import["pieces/wR.eps"];
wBg = Import["pieces/wB.eps"];
wNg = Import["pieces/wN.eps"];
wPg = Import["pieces/wP.eps"];
bKg = Import["pieces/bK.eps"];
bQg = Import["pieces/bQ.eps"];
bRg = Import["pieces/bR.eps"];
bBg = Import["pieces/bB.eps"];
bNg = Import["pieces/bN.eps"];
bPg = Import["pieces/bP.eps"];

(* ------------------------------------------------------------------------------ *)
(* constants and associations for node/edge states and chess pieces               *)
(* ------------------------------------------------------------------------------ *)

(* node/edge states *)
 
pw = -1;	(* square occupied by white piece / edge originating from white piece *)
pb = 1;     (* square occupied by black piece / edge originating from black piece *)

{wK,wQ,wR,wB,wN,wP} = pw*{2,3,4,5,6,7};           (* node states for white pieces *)
{bK,bQ,bR,bB,bN,bP} = pb*{2,3,4,5,6,7};           (* node states for black pieces *)

wPieces = {wK,wQ,wR,wB,wN,wP};
bPieces = {bK,bQ,bR,bB,bN,bP};

(* chess piece associations *)
 
gPieces = <| wK->wKg , wQ->wQg , wR->wRg , wB->wBg , wN->wNg , wP->wPg , bK->bKg , bQ->bQg , bR->bRg , bB->bBg , bN->bNg , bP->bPg |>;

p2wp = <| "K"->wK , "Q"->wQ , "R"->wR , "B"->wB , "N"->wN , "P"->wP |>;
p2bp = <| "K"->bK , "Q"->bQ , "R"->bR , "B"->bB , "N"->bN , "P"->bP |>;
    
(* ------------------------------------------------------------------------------ *)
(* lists holding important chess positions                                        *)
(* ------------------------------------------------------------------------------ *)

positionEmpty = {<|"enpassant"->0,"castling"->{{True,True},{True,True}},"check"->False,"checkmate"->False|>,{}};

positionStart = {<|"enpassant"->0,"castling"->{{True,True},{True,True}},"check"->False,"checkmate"->False|>,
                 {{1,wR},{2,wN},{3,wB},{4,wQ},{5,wK},{6,wB},{7,wN},{8,wR},{9,wP},{10,wP},{11,wP},{12,wP},{13,wP},{14,wP},{15,wP},{16,wP},
                 {49,bP},{50,bP},{51,bP},{52,bP},{53,bP},{54,bP},{55,bP},{56,bP},{57,bR},{58,bN},{59,bB},{60,bQ},{61,bK},{62,bB},{63,bN},{64,bR}}};

position23SpasskyFischer1972 = {<|"enpassant"->0,"castling"->{{False,False},{False,False}},"check"->False,"checkmate"->False|>,
                 {{1,wR},{3,wB},{6,wR},{7,wK},{9,wP},{10,wP},{11,wQ},{12,wN},{13,wB},{14,wP},{15,wP},{16,wP},{19,wN},{29,wP},{35,bP},
                 {36,wP},{40,bN},{44,bP},{47,bP},{49,bP},{50,bP},{52,bN},{54,bP},{55,bB},{56,bP},{57,bR},{59,bB},{60,bQ},{61,bR},{63,bK}}};
 
(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* CHESS POSITION GENERATION [MANUAL]                                             *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)
 
(* ------------------------------------------------------------------------------ *)
(* getPositionFromPieceFileRank                                                   *)
(*                                                                                *)
(* Function returning a position generated by chess piece placements on the       *)
(* chess board.                                                                   *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  p - list of chess piece placements {PieceFileRank,PieceFileRank,...}:         *)
(*      Piece = element of {wK,wQ,...wP,bK,bQ,...,wP}                             *)
(*      File  = element of {a,b,...,h}                                            *)
(*      Rank  = element of {1,2,...,8}                                            *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  EnPassant - FileRank of target node of possible en passant move               *)
(*          (D) enPassant->0                                                      *)
(*  Castling  - possible castling moves in the given position in form of          *)
(*              {{QSwhite,KSwhite},{QSblack,KSblack}}, where QS=queenside         *)
(*              and KS=kingside castling                                          *)
(*          (D) Castling->{{True,True},{True,True}}                               *)
(*  Check     - check issued in generated position (True/False)                   *)
(*          (D) Check->False                                                      *)
(*  Checkmate - checkmate issued in generated position (True/False)               *)
(*          (D) Checkmate->False                                                  *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  chess position                                                                *)
(* ------------------------------------------------------------------------------ *)

getPositionFromPieceFileRank[ p_ , OptionsPattern[{EnPassant->0,Castling->{{True,True},{True,True}},Check->False,Checkmate->False}] ] := Module[
    {
        ps,
        pos 
    },
    
    pos = { <|"enpassant"->node[f2f[StringTake[ToString[OptionValue[EnPassant]],{1}]],r2r[StringTake[ToString[OptionValue[EnPassant]],{2}]]],"castling"->OptionValue[Castling],"check"->OptionValue[Check],"checkmate"->OptionValue[Checkmate]|> , {} };
 
    For[ i=1 , i<=Length[p] , i++ ,
        ps = ToString[ p[[i]] ];
        pos[[2]] = Append[ pos[[2]] , { node[f2f[StringTake[ps,{3}]],r2r[StringTake[ps,{4}]]] , ToExpression[StringTake[ps,{1,2}]] } ];
    ];
    
    Return[pos];
];
 
(* ------------------------------------------------------------------------------ *)
(* getPositionFromPieceNode                                                       *)
(*                                                                                *)
(* Function returning a position generated by occupying graph nodes with pieces.  *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  p - list of chess piece placements {PieceNode,PieceNode,...}:                 *)
(*      Piece = element of {wK,wQ,...wP,bK,bQ,...,wP}                             *)
(*      Node  = node ID (1,2,...,64)                                              *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  EnPassant - node ID of target node of possible en passant move                *)
(*          (D) enPassant->0                                                      *)
(*  Castling  - possible castling moves in the given position in form of          *)
(*              {{QSwhite,KSwhite},{QSblack,KSblack}}, where QS=queenside         *)
(*              and KS=kingside castling                                          *)
(*          (D) Castling->{{True,True},{True,True}}                               *)
(*  Check     - check issued in generated position (True/False)                   *)
(*          (D) Check->False                                                      *)
(*  Checkmate - checkmate issued in generated position (True/False)               *)
(*          (D) Checkmate->False                                                  *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  chess position                                                                *)
(* ------------------------------------------------------------------------------ *)
 
getPositionFromPieceNode[ p_ , OptionsPattern[{EnPassant->0,Castling->{{True,True},{True,True}},Check->False,Checkmate->False}] ] := Module[
    {
        ps,
        pos 
    },
    
    pos = { <|"enpassant"->OptionValue[EnPassant],"castling"->OptionValue[Castling],"check"->OptionValue[Check],"checkmate"->OptionValue[Checkmate]|> , {} };
    
    For[ i=1 , i<=Length[p] , i++ ,
        ps = ToString[ p[[i]] ];
        pos[[2]] = Append[ pos[[2]] , { ToExpression[StringDrop[ps,2]] , ToExpression[StringTake[ps,{1,2}]] } ];
    ];
    
    Return[pos];
];
 
(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* CHESS POSITION GENERATION [PGN PARSER]                                         *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)
 
(* ------------------------------------------------------------------------------ *)
(* parsePGNMove                                                                   *)
(*                                                                                *)
(* Function parsing a single PGN chessmove string and returning a list with       *)
(* details of the move.                                                           *)
(* NOTE: This is a very baseline parser for PNG-formatted strings which might     *)
(*       not cover deviations from the FISA-recommended SAN format as well as     *)
(*       special cases. Use with care!                                            *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  move - string with move in PGN format                                         *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  list with following elements:                                                 *)
(*  {                                                                             *)
(*      piece,            - String                                                *)
(*      file,             - String                                                *)
(*      rank,             - String                                                *)
(*      capture,          - True/False                                            *)
(*      disambiguation,   - String (file/rank of departure)                       *)
(*      castling,         - String ("QS"/"KS" for queenside/kingside castling)    *)
(*      enpassant,        - True/False (en passant move possible)                 *)
(*      promotion,        - String (promotion piece)                              *)
(*      check,            - True/False                                            *)
(*      checkmate,        - True/False                                            *)
(*      result            - String (game result)                                  *)
(*  }                                                                             *)
(* ------------------------------------------------------------------------------ *)
 
parsePGNMove[ move_ ] := Module[ 
    { 
        m=StringTrim[move],   (* trimmed version of move *)
        m1,                   (* first char of move string *)
        p="",                 (* piece *)
        f="",                 (* arrival file *)
        r="",                 (* arrival rank *)
        c=False,              (* capture (True/False) *)
        da="",                (* disambiguation (file/rank of departure) *)
        ca="",                (* castling ("QS"/"KS") *)
        ep=False,             (* en passant move (True/False) *)
        pr="",                (* promotion piece *)
        ch=False,             (* check (True/False) *)
        cm=False,             (* checkmate (True/False) *)
        gr=""                 (* game result *)
    },
    
    If[ m == "O-O" , ca="KS" ; m="" ];
    If[ m == "O-O+" , ca="KS" ; ch=True ; m="" ];
    If[ m == "O-O++" , ca="KS" ; cm=True ; m="" ];
    If[ m == "O-O-O" , ca="QS" ; m="" ];
    If[ m == "O-O-O+" , ca="QS" ; ch=True ; m="" ];
    If[ m == "O-O-O++" , ca="QS" ; cm=True ; m="" ];
    If[ MemberQ[ {"1-0","0-1","1/2-1/2"} , m ] , result=m ; m="" ];

    If[ m=="" , Return[ <| "piece"->p , "file"->f , "rank"->r , "capture"->c , "disambiguation"->da , "castling"->ca , "enpassant"->ep , "promotion"->pr , "check"->ch , "checkmate"->cm , "result"->gr |> ] ];
    
    (* parse piece; if not found, set to Pawn *)
    If[ (m!="") && MemberQ[ {"K","Q","R","B","N"} , (m1=StringTake[m,1]) ] , p=m1 ; m=StringDrop[m,1] , p="P" ]; 
    
    (* parse for file marker and, if false, for rank (possible ambiguity) *)
    If[ (m!="") && MemberQ[ {"a","b","c","d","e","f","g","h"} , (m1=StringTake[m,1]) ] , f=m1 ; m=StringDrop[m,1] , If[ (m!="") && MemberQ[ {"1","2","3","4","5","6","7","8"} , (m1=StringTake[m,1]) ] , r=m1 ; m=StringDrop[m,1] ] ];
    
    (* parse for capture marker *)
    If[ (m!="") && MemberQ[ {"x"} ,(m1=StringTake[m,1]) ] , c=True ; m=StringDrop[m,1] ];
      
    (* parse again for file maker; if found, move has ambiguity which is resolved here *)
    If[ (m!="") && MemberQ[ {"a","b","c","d","e","f","g","h"} , (m1=StringTake[m,1]) ] , da=If[f!="",f,r] ; r="" ; f=m1 ; m=StringDrop[m,1] ];
    
    (* parse again for file (will be handled only if move has ambiguity) *)
    If[ (m!="") && MemberQ[ {"1","2","3","4","5","6","7","8"} , (m1=StringTake[m,1]) ] , r=m1 ; m=StringDrop[m,1] ];
    
    (* parse for promotion indicator *)
    If[ (m!="") && (StringTake[m,1]=="=") , m=StringDrop[m,1] ];
    
    (* parse for pieces indicating promotion *)
    If[ (m!="") && MemberQ[ {"Q","R","B","N"} , (m1=StringTake[m,1]) ] , pr=m1 ; m=StringDrop[m,1] ];   
    
    (* finally, parse rest of string for special markers *)
    Switch[ m 
    , "e.p." , ep=True
    , "+"    , ch=True
    , "++"   , cm=True
    , "#"    , cm=True
    ];
         
    Return[ <| "piece"->p , "file"->f , "rank"->r , "capture"->c , "disambiguation"->da , "castling"->ca , "enpassant"->ep , "promotion"->pr , "check"->ch , "checkmate"->cm , "result"->gr |> ];
];

(* ------------------------------------------------------------------------------ *)
(* getPositionsFromGamePGN                                                        *)
(*                                                                                *)
(* Function which parses all PGN-formatted moves of a chess game and returns a    *)
(* list of all successive positions of the chess game.                            *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  moves - list of PGN formatted moves                                           *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  list of positions                                                             *)
(* ------------------------------------------------------------------------------ *)
     
getPositionsFromGamePGN[ moves_ ] := Module[ 
    {
        positions,      (* list of positions to be returned *)
        position,       (* actual position being processed *)
        m,              (* PGN string with move details *)
        mw={},          (* list of details of white move *)
        mb={},          (* list of details of black move *)
        nd,na,          (* node of departure/arrival *)
        ep,             (* possible en passant move, given current move *)
        ca,             (* possible castling moves *)
        i
    },
        
    (* function which returns the disambiguated node of departure, given the node *)
    (* of arrival (ana), a list of potential departure nodes (andl), the piece    *)
    (* (ap), a move's PGN disambiguation (ada), whether a move is a capture/rank/ *)
    (* en passant move (ac/ar/aep; only used for Pawn disambiguation) and the     *)
    (* current position (apos)                                                    *)
     
    getDepartureNode[ ana_ , andl_ , ap_ , ada_ , ac_ , ar_ , aep_ , apos_ ] := Module[ { k } ,
        If[ !MemberQ[{wP,bP},ap] ,
            If[ (Length[andl]==1) , Return[ andl[[1]] ] ];
            If[ (Length[andl]>1) && (MemberQ[{"a","b","c","d","e","f","g","h"},ada]) , For[ k=1 , k<=Length[andl] , k++ , If[ f2f[ada]==file[andl[[k]]] , Return[ andl[[k]] ] ] ] ];
            If[ (Length[andl]>1) && (MemberQ[{"1","2","3","4","5","6","7","8"},ada]) , For[ k=1 , k<=Length[andl] , k++ , If[ r2r[ada]==rank[andl[[k]]] , Return[ andl[[k]] ] ] ] ];
            If[ (Length[andl]>1) && (ada=="") , Return[ Intersection[ andl , DeleteCases[ If[ (MemberQ[andl,#[[1]]]) && (#[[2]]==ana) , #[[1]] ]& /@ getEdgesFromPosition[ apos , SameColorTargets->False ] , Null ] ][[1]] ] ];
        ];
        If[ MemberQ[{wP,bP},ap] ,
            If[ (!ac) && (!aep) , If[ ap==wP , Return[ If[ ar , ana-16 , ana-8 ] ] , Return[ If[ ar , ana+16 , ana+8 ]  ] ] ];
            If[ ac , If[ ap==wP , Return[ node[f2f[ada],rank[ana]-1] ] , Return[ node[f2f[ada],rank[ana]+1] ] ] ];
        ];
        Return[ 0 ];
    ];


    (* initialize chess game with start position *)
    
    positions = { positionStart };
    position = positionStart[[2]];

    (* True/False lists indicating whether castling moves are     *)
    (* allowed; default values are True, and must be set to False *)
    (* after the first move of the King or Rook; the two entries  *)
    (* refer white/black and are of form {<QS>,<KS>}              *)
    
    ca = {{True,True},{True,True}};
     
    (* each position can have at most one valid en passant move;  *)
    (* the variable holds the target node for this move, and must *)
    (* be evaluated after each Pawn move and reset to zero after  *)
    
    ep = 0;
    
    (* process moves *)

    For[ i=1 , i<=Length[moves] , i++ ,
 
        m = StringSplit[ moves[[i]] ];

        (* get details for white/black move *)

        mw = parsePGNMove[ m[[1]] ];
        mb = If[ Length[m]>1 , parsePGNMove[ m[[2]] ] , {} ];
 
        (* process move of white piece *)

        na = 0;
        nd = 0;
        ep = 0;

        (* handle castling *)
        Switch[ mw["castling"]                
        , "QS" , position = Join[ DeleteCases[ DeleteCases[ position , {5,wK} ] , {1,wR} ] , {{3,wK},{4,wR}} ]; ca[[1]] = {False,False};
        , "KS" , position = Join[ DeleteCases[ DeleteCases[ position , {5,wK} ] , {8,wR} ] , {{7,wK},{6,wR}} ]; ca[[1]] = {False,False};
        ];

        (* get node of arrival *)
        If[ (mw["file"]!="") && (mw["rank"]!="") , na = node[f2f[mw["file"]],r2r[mw["rank"]]] ]; 
                                                                                            
        (* handle capture moves by removing first the captured piece;         *)
        (* NOTE: capturing by pawn needs to be handled separately, as as en   *)
        (* passant captures are not always indicated by "e.p." in PGN records *)
        If[ (mw["capture"]) && (!mw["enpassant"]) , position = DeleteCases[ position , Flatten[{na,DeleteCases[If[#[[1]]==na,#[[2]]]& /@ position,Null]}] ] ];
        If[ (mw["capture"]) && (mw["piece"]=="P") && (rank[na]==6) && (DeleteCases[If[#[[1]]==na,#[[2]]]& /@ position,Null]=={}) , mw["enpassant"] = True , mw["enpassant"] = False ];
        If[ mw["enpassant"] , position = DeleteCases[ position , Flatten[{na-8,DeleteCases[If[#[[1]]==(na-8),#[[2]]]& /@ position,Null]}] ] ]

        (* handle moves *)
        Switch[ mw["piece"]
        , "K" , 
            If[ #[[2]]==wK , nd=#[[1]] ]& /@ position; 
            position = Join[ DeleteCases[ position , {nd,wK} ] , {{na,wK}} ];
            ca[[1]] = {False,False};
        , "Q" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==wQ,#[[1]]]& /@ position , Null ] , wQ , mw["disambiguation"] , mw["capture"] , False , False , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,wQ} ] , {{na,wQ}} ];
        , "R" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==wR,#[[1]]]& /@ position , Null ] , wR , mw["disambiguation"] , mw["capture"] , False , False , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,wR} ] , {{na,wR}} ];
            If[ (ca[[1,1]]) || (ca[[1,2]]) , If [ nd==1 , ca[[1,1]] = False ] ; If[ nd==8 , ca[[1,2]] = False ] ];
        , "B" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==wB,#[[1]]]& /@ position , Null ] , wB , mw["disambiguation"] , mw["capture"] , False , False , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,wB} ] , {{na,wB}} ];
        , "N" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==wN,#[[1]]]& /@ position , Null ] , wN , mw["disambiguation"] , mw["capture"] , False , False , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,wN} ] , {{na,wN}} ];
        , "P" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==wP,#[[1]]]& /@ position , Null ] , wP , mw["disambiguation"] , mw["capture"] , If[ (rank[na]==4) && (DeleteCases[If[#[[1]]==(na-8),#[[2]]]& /@ position,Null]=={}) , True , False ] , mw["enpassant"] , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,wP} ] , {{na,wP}} ];
            If[ (rank[nd]==2) && (rank[na]==4) , ep = enPassantXw[[na]] ];
        ];                                                                       

        (* handle promotion *)
        If[ mw["promotion"]!="" , position = Join[ DeleteCases[ position , {na,wP} ] , {{na,p2wp[mw["promotion"]]}} ] ];
        
        (* append position to positions list *)
        positions = Append[ positions , { <|"enpassant"->ep,"castling"->ca,"check"->mw["check"],"checkmate"->mw["checkmate"]|> , position } ];
  
        (* process move of black piece *)

        If[ mb=={} , Break[] ];
        
        na = 0;
        nd = 0;
        ep = 0;
       
        (* handle castling *)
        Switch[ mb["castling"]                
        , "QS" , position = Join[ DeleteCases[ DeleteCases[ position , {61,bK} ] , {57,bR} ] , {{59,bK},{60,bR}} ]; ca[[2]] = {False,False};
        , "KS" , position = Join[ DeleteCases[ DeleteCases[ position , {61,bK} ] , {64,bR} ] , {{63,bK},{62,bR}} ]; ca[[2]] = {False,False};
        ];
                            
        (* get node of arrival *)
        If[ (mb["file"]!="") && (mb["rank"]!="") , na = node[f2f[mb["file"]],r2r[mb["rank"]]] ]; 
                                                                                         
        (* handle capture moves by removing first the captured piece;         *)
        (* NOTE: capturing by pawn needs to be handled separately, as as en   *)
        (* passant captures are not always indicated by "e.p." in PGN records *)
        If[ (mb["capture"]) && (!mb["enpassant"]) , position = DeleteCases[ position , Flatten[{na,DeleteCases[If[#[[1]]==na,#[[2]]]& /@ position,Null]}] ] ];
        If[ (mb["capture"]) && (mb["piece"]=="P") && (rank[na]==3) && (DeleteCases[If[#[[1]]==na,#[[2]]]& /@ position,Null]=={}) , mb["enpassant"] = True , mb["enpassant"] = False ];
        If[ mb["enpassant"] , position = DeleteCases[ position , Flatten[{na+8,DeleteCases[If[#[[1]]==(na+8),#[[2]]]& /@ position,Null]}] ] ]
                                                                                        
        (* handle moves *)
        Switch[ mb["piece"]
        , "K" , 
            If[ #[[2]]==bK , nd=#[[1]] ]& /@ position; 
            position = Join[ DeleteCases[ position , {nd,bK} ] , {{na,bK}} ]; 
            ca[[2]] = {False,False};
        , "Q" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==bQ,#[[1]]]& /@ position , Null ] , bQ , mb["disambiguation"] , mb["capture"] , False , False , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,bQ} ] , {{na,bQ}} ];
        , "R" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==bR,#[[1]]]& /@ position , Null ] , bR , mb["disambiguation"] , mb["capture"] , False , False , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,bR} ] , {{na,bR}} ];
            If[ (ca[[2,1]]) || (ca[[2,2]]) , If [ nd==57 , ca[[2,1]] = False ] ; If[ nd==64 , ca[[2,2]] = False ] ];
        , "B" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==bB,#[[1]]]& /@ position , Null ] , bB , mb["disambiguation"] , mb["capture"] , False , False , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,bB} ] , {{na,bB}} ];
        , "N" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==bN,#[[1]]]& /@ position , Null ] , bN , mb["disambiguation"] , mb["capture"] , False , False , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,bN} ] , {{na,bN}} ];
        , "P" , 
            nd = getDepartureNode[ na , DeleteCases[ If[#[[2]]==bP,#[[1]]]& /@ position , Null ] , bP , mb["disambiguation"] , mb["capture"] , If[ (rank[na]==5) && (DeleteCases[If[#[[1]]==(na+8),#[[2]]]& /@ position,Null]=={}) , True , False ] , mb["enpassant"] , Last[ positions ] ];
            position = Join[ DeleteCases[ position , {nd,bP} ] , {{na,bP}} ];
            If[ (rank[nd]==7) && (rank[na]==5) , ep = enPassantXb[[na]] ];
        ];  
 
        (* handle promotion *)
        If[ mb["promotion"]!="" , position = Join[ DeleteCases[ position , {na,bP} ] , {{na,p2bp[mb["promotion"]]}} ] ];
        
        (* append position to positions list *)
        positions = Append[ positions , { <|"enpassant"->ep,"castling"->ca,"check"->mb["check"],"checkmate"->mb["checkmate"]|> , position } ];
    ];

    Return[ positions ];
];

(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* CHESS GRAPH GENERATION                                                         *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

(* ------------------------------------------------------------------------------ *)
(* getNodesFromPosition                                                           *)
(*                                                                                *)
(* Function returning a 1d list of length 64 with all node states in a given      *)
(* chess position.                                                                *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  pos - chess position                                                          *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - node state:                                                           *)
(*          State->"Piece" : state indicating individual type of chess pieces     *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  list {<NODE 1 STATE>,<NODE 2 STATE>,...} of length 64 holding all node states *)
(* ------------------------------------------------------------------------------ *)

getNodesFromPosition[ pos_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        position=pos[[2]],
        nodes 
    },
    
    nodes = ConstantArray[ 0 , 64 ];
    If[ MemberQ[wPieces,#[[2]]] , nodes[[#[[1]]]]=#[[2]] , nodes[[#[[1]]]]=#[[2]] ]& /@ position;
    
    Switch[ OptionValue[State]
        , "Piece"  , Return[ nodes ];
        , "Simple" , Return[ Abs[Sign[nodes]] ];          
    ];
    
    Return[ Sign[ nodes ] ];
];
 
(* ------------------------------------------------------------------------------ *)
(* getWhiteEdgesFromPosition                                                      *)
(*                                                                                *)
(* Function returning a 2d list of all potential edges originating from white     *)
(* pieces (except edges from castling moves of the white King/Rook) in a given    *)
(* chess position.                                                                *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  pos - chess position                                                          *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  SameColorTargets - True/False indicating whether edges are included for       *)
(*                     which both source and target nodes are white;              *)
(*                 (D) sameColorTargets->False                                    *)
(*                     NOTE: this argument must be set to False (default) for     *)
(*                           generating valid chess graphs                        *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  list {{<SOURCE>,<TARGET>},{<SOURCE>,<TARGET>},...} holding the set of all     *)
(*  edges indicated by source node and target node                                *)
(* ------------------------------------------------------------------------------ *)

getWhiteEdgesFromPosition[ pos_ , OptionsPattern[{SameColorTargets->False}] ] := Module[
    { 
        supp=pos[[1]],       (* supplementary information for position *)
        position=pos[[2]],   (* chess position *)
        edges={},            (* master list of edges to be returned *)
        wn={},               (* list of white-occupied nodes *)
        bn={},               (* list of black-occupied nodes *)
        an={},               (* list of all occupied nodes *)
        n,                   (* node being processed *)
        p,                   (* piece on node being processed *)
        f,r,                 (* file/rank of node being processed *)
        tn,                  (* temporary list of target nodes of white/black pieces *)
        tni,                 (* target node included? (True/False) *)
        tnt,                 (* target node *)
        ep,                  (* target node of possible en passant move *)  
        pi,i,
        atni=OptionValue[SameColorTargets]
    },
    
    If[ Length[position]==0 , Return[ {{}} ] ];
    ep = supp["enpassant"];
    
	(* get white/black/all occupied nodes *)
	
    If[ MemberQ[wPieces,#[[2]]] , wn=Append[wn,#[[1]]] , bn=Append[bn,#[[1]]] ]& /@ position;
    an = Join[ wn , bn ];
     
	For[ pi = 1 , pi <= Length[position] , pi++ ,        
		
        n = position[[pi,1]];
		p = position[[pi,2]];
        f = file[n];
        r = rank[n];
 		tn = {};

		Switch[ p

        , wK , 
	       
            (* add target nodes for standard moves of the white King, modulo target nodes occupied with white pieces *)
            tn = Join[ tn , If[ !atni , DeleteCases[ mKing[[n]] , Alternatives @@ wn ] , mKing[[n]] ] ];
           
        , wQ ,

			(* add target nodes for N moves of the white Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mN[[n]]] , i++ , tnt=mN[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mN[[n]] , Alternatives @@ If[ (tni) || (atni) , mN[[tnt]] , Append[mN[[tnt]],tnt] ] ] ];

			(* add target nodes for NE moves of the white Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mNE[[n]]] , i++ , tnt=mNE[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mNE[[n]] , Alternatives @@ If[ (tni) || (atni) , mNE[[tnt]] , Append[mNE[[tnt]],tnt] ] ] ];

			(* add target nodes for E moves of the white Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mE[[n]]] , i++ , tnt=mE[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mE[[n]] , Alternatives @@ If[ (tni) || (atni) , mE[[tnt]] , Append[mE[[tnt]],tnt] ] ] ];

			(* add target nodes for SE moves of the white Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mSE[[n]]] , i++ , tnt=mSE[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mSE[[n]] , Alternatives @@ If[ (tni) || (atni) , mSE[[tnt]] , Append[mSE[[tnt]],tnt] ] ] ];

			(* add target nodes for S moves of the white Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mS[[n]]] , i++ , tnt=mS[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mS[[n]] , Alternatives @@ If[ (tni) || (atni) , mS[[tnt]] , Append[mS[[tnt]],tnt] ] ] ];

			(* add target nodes for SW moves of the white Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mSW[[n]]] , i++ , tnt=mSW[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mSW[[n]] , Alternatives @@ If[ (tni) || (atni) , mSW[[tnt]] , Append[mSW[[tnt]],tnt] ] ] ];

			(* add target nodes for W moves of the white Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mW[[n]]] , i++ , tnt=mW[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mW[[n]] , Alternatives @@ If[ (tni) || (atni) , mW[[tnt]] , Append[mW[[tnt]],tnt] ] ] ];

			(* add target nodes for NW moves of the white Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mNW[[n]]] , i++ , tnt=mNW[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mNW[[n]] , Alternatives @@ If[ (tni) || (atni) , mNW[[tnt]] , Append[mNW[[tnt]],tnt] ] ] ];
               
 		, wR ,

			(* add target nodes for N moves of the white Rook, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mN[[n]]] , i++ , tnt=mN[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mN[[n]] , Alternatives @@ If[ (tni) || (atni) , mN[[tnt]] , Append[mN[[tnt]],tnt] ] ] ];

			(* add target nodes for E moves of the white Rook, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mE[[n]]] , i++ , tnt=mE[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mE[[n]] , Alternatives @@ If[ (tni) || (atni) , mE[[tnt]] , Append[mE[[tnt]],tnt] ] ] ];

			(* add target nodes for S moves of the white Rook, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mS[[n]]] , i++ , tnt=mS[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mS[[n]] , Alternatives @@ If[ (tni) || (atni) , mS[[tnt]] , Append[mS[[tnt]],tnt] ] ] ];

			(* add target nodes for W moves of the white Rook, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mW[[n]]] , i++ , tnt=mW[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mW[[n]] , Alternatives @@ If[ (tni) || (atni) , mW[[tnt]] , Append[mW[[tnt]],tnt] ] ] ];
                
		, wB ,

			(* add target nodes for NE moves of the white Bishop, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mNE[[n]]] , i++ , tnt=mNE[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mNE[[n]] , Alternatives @@ If[ (tni) || (atni) , mNE[[tnt]] , Append[mNE[[tnt]],tnt] ] ] ];

			(* add target nodes for SE moves of the white Bishop, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mSE[[n]]] , i++ , tnt=mSE[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mSE[[n]] , Alternatives @@ If[ (tni) || (atni) , mSE[[tnt]] , Append[mSE[[tnt]],tnt] ] ] ];

			(* add target nodes for SW moves of the white Bishop, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mSW[[n]]] , i++ , tnt=mSW[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mSW[[n]] , Alternatives @@ If[ (tni) || (atni) , mSW[[tnt]] , Append[mSW[[tnt]],tnt] ] ] ];

			(* add target nodes for NW moves of the white Bishop, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mNW[[n]]] , i++ , tnt=mNW[[n,i]] ; If[ MemberQ[wn,tnt] , tni=False ; Break[] , If[ MemberQ[bn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mNW[[n]] , Alternatives @@ If[ (tni) || (atni) , mNW[[tnt]] , Append[mNW[[tnt]],tnt] ] ] ];
               
        , wN ,
               
            (* add target nodes for standard moves of the white Knight, modulo target nodes occupied with white pieces *)
            tn = Join[ tn , If[ !atni , DeleteCases[ mKnight[[n]] , Alternatives @@ wn ] , mKnight[[n]] ] ];
 
        , wP ,
               
            (* add target nodes for standard moves of the white Pawn, modulo target nodes occupied with white/black pieces *)
            tn = Join[ tn , DeleteCases[ mwPawn[[n]] , Alternatives @@ an ] ];

            (* add target nodes for rank 2 moves of the white Pawn, modulo target nodes occupied with white/black pieces *)
            If[ (r==2) && (Length[tn]!=0) , tn = Join[ tn , DeleteCases[ mwPawnR[[n]] , Alternatives @@ an ] ] ];
                                             
            (* add capture moves of the white Pawn *)
            tn = Join[ tn , Intersection[ mwPawnX[[n]] , If[ !atni , bn , an ] ] ];
               
            (* add en passant moves of the white Pawn *)
            If[ (r==5) && (ep!=0) , tn = Join[ tn , Intersection[ mwPawnEP[[n]] , {ep} ] ] ];
              
		];
 
		(* generate edges and add to master list *)
		edges = Append[ edges , { n , # }& /@ tn ];   
	
    ];

    Return[ DeleteDuplicates[ Flatten[ edges , 1 ] ] ];
];

(* ------------------------------------------------------------------------------ *)
(* getBlackEdgesFromPosition                                                      *)
(*                                                                                *)
(* Function returning a 2d list of all potential edges originating from black     *)
(* pieces (except edges from castling moves of the black King/Rook) in a given    *)
(* chess position.                                                                *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  pos - chess position                                                          *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  SameColorTargets - True/False indicating whether edges are included for       *)
(*                     which both source and target nodes are black;              *)
(*                 (D) sameColorTargets->False                                    *)
(*                     NOTE: this argument must be set to False (default) for     *)
(*                           generating valid chess graphs                        *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  list {{<SOURCE>,<TARGET>},{<SOURCE>,<TARGET>},...} holding the set of all     *)
(*  edges indicated by source node and target node                                *)
(* ------------------------------------------------------------------------------ *)
 
getBlackEdgesFromPosition[ pos_ , OptionsPattern[{SameColorTargets->False}] ] := Module[
    { 
        supp=pos[[1]],       (* supplementary information for position *)
        position=pos[[2]],   (* chess position *)
        edges={},            (* master list of edges to be returned *)
        wn={},               (* list of white-occupied nodes *)
        bn={},               (* list of black-occupied nodes *)
        an={},               (* list of all occupied nodes *)
        n,                   (* node being processed *)
        p,                   (* piece on node being processed *)
        f,r,                 (* file/rank of node being processed *)
        tn,                  (* temporary list of target nodes of white/black pieces *)
        tni,                 (* target node included? (True/False) *)
        tnt,                 (* target node *)
        ep,                  (* target node of possible en passant move *)
        pi,i,
        atni=OptionValue[SameColorTargets]
    },
   
    If[ Length[position]==0 , Return[ {{}} ] ];
    ep = supp["enpassant"];
    
	(* get white/black/all occupied nodes *)
	
    If[ MemberQ[wPieces,#[[2]]] , wn=Append[wn,#[[1]]] , bn=Append[bn,#[[1]]] ]& /@ position;
    an = Join[ wn , bn ];
     
	For[ pi = 1 , pi <= Length[position] , pi++ ,     
		
        n = position[[pi,1]];
		p = position[[pi,2]];
        f = file[n];
        r = rank[n];
 		tn = {};

		Switch[ p

        , bK , 
	       
            (* add target nodes for standard moves of the black King, modulo target nodes occupied with black pieces *)
            tn = Join[ tn , If[ !atni , DeleteCases[ mKing[[n]] , Alternatives @@ bn ] , mKing[[n]] ] ];
 
        , bQ ,

			(* add target nodes for N moves of the black Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mN[[n]]] , i++ , tnt=mN[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mN[[n]] , Alternatives @@ If[ (tni) || (atni) , mN[[tnt]] , Append[mN[[tnt]],tnt] ] ] ];

			(* add target nodes for NE moves of the black Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mNE[[n]]] , i++ , tnt=mNE[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mNE[[n]] , Alternatives @@ If[ (tni) || (atni) , mNE[[tnt]] , Append[mNE[[tnt]],tnt] ] ] ];

			(* add target nodes for E moves of the black Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mE[[n]]] , i++ , tnt=mE[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mE[[n]] , Alternatives @@ If[ (tni) || (atni) , mE[[tnt]] , Append[mE[[tnt]],tnt] ] ] ];

			(* add target nodes for SE moves of the black Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mSE[[n]]] , i++ , tnt=mSE[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mSE[[n]] , Alternatives @@ If[ (tni) || (atni) , mSE[[tnt]] , Append[mSE[[tnt]],tnt] ] ] ];

			(* add target nodes for S moves of the black Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mS[[n]]] , i++ , tnt=mS[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mS[[n]] , Alternatives @@ If[ (tni) || (atni) , mS[[tnt]] , Append[mS[[tnt]],tnt] ] ] ];

			(* add target nodes for SW moves of the black Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mSW[[n]]] , i++ , tnt=mSW[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mSW[[n]] , Alternatives @@ If[ (tni) || (atni) , mSW[[tnt]] , Append[mSW[[tnt]],tnt] ] ] ];

			(* add target nodes for W moves of the black Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mW[[n]]] , i++ , tnt=mW[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mW[[n]] , Alternatives @@ If[ (tni) || (atni) , mW[[tnt]] , Append[mW[[tnt]],tnt] ] ] ];

			(* add target nodes for NW moves of the black Queen, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mNW[[n]]] , i++ , tnt=mNW[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mNW[[n]] , Alternatives @@ If[ (tni) || (atni) , mNW[[tnt]] , Append[mNW[[tnt]],tnt] ] ] ];

        , bR ,

			(* add target nodes for N moves of the black Rook, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mN[[n]]] , i++ , tnt=mN[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mN[[n]] , Alternatives @@ If[ (tni) || (atni) , mN[[tnt]] , Append[mN[[tnt]],tnt] ] ] ];

			(* add target nodes for E moves of the black Rook, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mE[[n]]] , i++ , tnt=mE[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mE[[n]] , Alternatives @@ If[ (tni) || (atni) , mE[[tnt]] , Append[mE[[tnt]],tnt] ] ] ];

			(* add target nodes for S moves of the black Rook, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mS[[n]]] , i++ , tnt=mS[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mS[[n]] , Alternatives @@ If[ (tni) || (atni) , mS[[tnt]] , Append[mS[[tnt]],tnt] ] ] ];

			(* add target nodes for W moves of the black Rook, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mW[[n]]] , i++ , tnt=mW[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mW[[n]] , Alternatives @@ If[ (tni) || (atni) , mW[[tnt]] , Append[mW[[tnt]],tnt] ] ] ];
 
        , bB ,

			(* add target nodes for NE moves of the black Bishop, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mNE[[n]]] , i++ , tnt=mNE[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mNE[[n]] , Alternatives @@ If[ (tni) || (atni) , mNE[[tnt]] , Append[mNE[[tnt]],tnt] ] ] ];

			(* add target nodes for SE moves of the black Bishop, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mSE[[n]]] , i++ , tnt=mSE[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mSE[[n]] , Alternatives @@ If[ (tni) || (atni) , mSE[[tnt]] , Append[mSE[[tnt]],tnt] ] ] ];

			(* add target nodes for SW moves of the black Bishop, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mSW[[n]]] , i++ , tnt=mSW[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mSW[[n]] , Alternatives @@ If[ (tni) || (atni) , mSW[[tnt]] , Append[mSW[[tnt]],tnt] ] ] ];

			(* add target nodes for NW moves of the black Bishop, terminated by presence of white/black pieces *)
			For[ tnt=n ; tni=True ; i=1 , i<=Length[mNW[[n]]] , i++ , tnt=mNW[[n,i]] ; If[ MemberQ[bn,tnt] , tni=False ; Break[] , If[ MemberQ[wn,tnt] , tni=True ; Break[] ] ] ];
			tn = Join[ tn , DeleteCases[ mNW[[n]] , Alternatives @@ If[ (tni) || (atni) , mNW[[tnt]] , Append[mNW[[tnt]],tnt] ] ] ];
              
        , bN ,
               
            (* add target nodes for standard moves of the black Knight, modulo target nodes occupied with black pieces *)
            tn = Join[ tn , If[ !atni , DeleteCases[ mKnight[[n]] , Alternatives @@ bn ] , mKnight[[n]] ] ];

        , bP ,
               
            (* add target nodes for standard moves of the black Pawn, modulo target nodes occupied with white/black pieces *)
            tn = Join[ tn , DeleteCases[ mbPawn[[n]] , Alternatives @@ an ] ];

            (* add target nodes for rank 7 moves of the black Pawn, modulo target nodes occupied with white/black pieces *)
            If[ (r==7) && (Length[tn]!=0) , tn = Join[ tn , DeleteCases[ mbPawnR[[n]] , Alternatives @@ an ] ] ];
                                             
            (* add capture moves of the black Pawn *)
            tn = Join[ tn , Intersection[ mbPawnX[[n]] , If[ !atni , wn , an ] ] ];
               
            (* add en passant moves of the black Pawn *)
            If[ (r==4) && (ep!=0) , tn = Join[ tn , Intersection[ mbPawnEP[[n]] , {ep} ] ] ];
              
		];
 
		(* generate edges and add to master list *)
		edges = Append[ edges , { n , # }& /@ tn ];    
	
    ];
        
    Return[ DeleteDuplicates[ Flatten[ edges , 1 ] ] ];
];
 
(* ------------------------------------------------------------------------------ *)
(* getEdgesFromPosition                                                           *)
(*                                                                                *)
(* Function returning a 2d list of all weighted edges in a given chess position.  *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  pos - chess position                                                          *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State            - edge state/weight:                                         *)
(*                 (D) State->"Color" : state indicating color of chess pieces    *)
(*                     State->"Simple" : state indicating whether node is         *)
(*                                       occupied or not                          *)
(*  SameColorTargets - True/False indicating whether edges are included for       *)
(*                     which both source and target nodes have same color;        *)
(*                 (D) sameColorTargets->False                                    *)
(*                     NOTE: this argument must be set to False (default) for     *)
(*                           generating valid chess graphs                        *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  list {{<SOURCE>,<TARGET>,<WEIGHT>},{<SOURCE>,<TARGET>,<WEIGHT>},...}          *)
(*  holding the set of all edges indicated by source node, target node and edge   *)
(*  weight (state) information                                                    *)
(* ------------------------------------------------------------------------------ *)

getEdgesFromPosition[ pos_ , OptionsPattern[{State->"Color",SameColorTargets->False}] ] := Module[ 
    { 
        supp=pos[[1]],       (* supplementary information for position *)
        position=pos[[2]],   (* chess position *)
        edges={},            (* master list of edges to be returned *)
        wedges={},           (* list of all edges originating from white pieces *)
        bedges={},           (* list of all edges originating from black pieces *)        
        wn={},               (* list of white-occupied nodes *)
        bn={},               (* list of black-occupied nodes *)
        an={},               (* list of all occupied nodes *)
        wKn,bKn,             (* node of white/black King *)
        wtn={},              (* list of all nodes targeted by white pieces *)
        btn={},              (* list of all nodes targeted by black pieces *)
        tpos,                (* temporary position being evaluated *)
        tKn,                 (* node for King in temporary position being evaluated *)
        wCastling,
        bCastling,
        i,te,
        atni=OptionValue[SameColorTargets]
    },
    
    wCastling = supp["castling"][[1]];
    bCastling = supp["castling"][[2]];

    (* get white/black/all occupied nodes *)
	
    If[ MemberQ[wPieces,#[[2]]] , wn=Append[wn,#[[1]]] , bn=Append[bn,#[[1]]] ]& /@ position;
    an = Join[ wn , bn ];
    
    (* get node with white/black King *)
    
    If[ #[[2]]==wK , wKn=#[[1]] ]& /@ position;
    If[ #[[2]]==bK , bKn=#[[1]] ]& /@ position;
        
    (* get all potential edges for white/black pieces *)

    wedges = getWhiteEdgesFromPosition[ pos , SameColorTargets->atni ];
    bedges = getBlackEdgesFromPosition[ pos , SameColorTargets->atni ];
    
    (* get all nodes targeted by white/black pieces *)

    wtn = If[ wedges!={} , Transpose[ wedges ][[2]] , {} ];
    btn = If[ bedges!={} , Transpose[ bedges ][[2]] , {} ];
    
    (* assess if white/black King is in check; *)
     
    If[ MemberQ[ btn , wKn ] , wCheck=True , wCheck=False ];
    If[ MemberQ[ wtn , bKn ] , bCheck=True , bCheck=False ];

    (* perform each possible move of white pieces and    *)
    (* remove those which yield check for the white King *)
    
    For[ i=1 , i<=Length[wedges] , i++ ,        
        (* generate new temporary position from move *)
        tpos = Select[ Map[ If[ (#[[1]]==wedges[[i,2]]) && (#[[2]]!=bK) , {0,0} , { #[[1]] , #[[2]] } ] & , position ] , UnsameQ[ # , {0,0} ] & ];
        tpos = Map[ { If[ #[[1]]==wedges[[i,1]] , wedges[[i,2]] , #[[1]] ] , #[[2]] } & , tpos ];
        If[ #[[2]]==wK , tKn=#[[1]] ]& /@ tpos;         
        (* test/discard edge if white King is in check *)
        If[ MemberQ[ If[ (te=getBlackEdgesFromPosition[ { supp , tpos } , SameColorTargets->atni ])!={} , Transpose[ te ][[2]] , {} ] , tKn ] , wedges[[i]]={} ];
    ];
    
    wedges = Select[ wedges , UnsameQ[ # , {} ] & ];
    
    (* perform each possible move of black pieces and    *)
    (* remove those which yield check for the black King *)
   
    For[ i=1 , i<=Length[bedges] , i++ ,        
        (* generate new temporary position from move *)
        tpos = Select[ Map[ If[ (#[[1]]==bedges[[i,2]]) && (#[[2]]!=wK) , {0,0} , { #[[1]] , #[[2]] } ] & , position ] , UnsameQ[ # , {0,0} ] & ];
        tpos = Map[ { If[ #[[1]]==bedges[[i,1]] , bedges[[i,2]] , #[[1]] ] , #[[2]] } & , tpos ];
        If[ #[[2]]==bK , tKn=#[[1]] ]& /@ tpos;         
        (* test/discard edge if black King is in check *)
        If[ MemberQ[ If[ (te=getWhiteEdgesFromPosition[ { supp , tpos } , SameColorTargets->atni ])!={} , Transpose[ te ][[2]] , {} ] , tKn ] , bedges[[i]]={} ];
    ];

    bedges = Select[ bedges , UnsameQ[ # , {} ] & ];

    (* add edges for queen/kingside castling moves of white King/Rook *)
    
    If[ (!wCheck) && (MemberQ[position,{5,wK}]) && (MemberQ[position,{1,wR}]) && (wCastling[[1]]) && (Length[Intersection[{2,3,4},an]]==0) && (Length[Intersection[{3,4},btn]]==0) , wedges = Join[ wedges , {{5,3},{1,4}} ] ];
    If[ (!wCheck) && (MemberQ[position,{5,wK}]) && (MemberQ[position,{8,wR}]) && (wCastling[[2]]) && (Length[Intersection[{6,7},an]]==0) && (Length[Intersection[{6,7},btn]]==0) , wedges = Join[ wedges , {{5,7},{8,6}} ] ];
    
    (* add edges for queen/kingside castling moves of black King/Rook *)
    
    If[ (!bCheck) && (MemberQ[position,{61,bK}]) && (MemberQ[position,{57,bR}]) && (bCastling[[1]]) && (Length[Intersection[{58,59,60},an]]==0) && (Length[Intersection[{59,60},wtn]]==0) , bedges = Join[ bedges , {{61,59},{57,60}} ] ];
    If[ (!bCheck) && (MemberQ[position,{61,bK}]) && (MemberQ[position,{64,bR}]) && (bCastling[[2]]) && (Length[Intersection[{62,63},an]]==0) && (Length[Intersection[{62,63},wtn]]==0) , bedges = Join[ bedges , {{61,63},{64,62}} ] ];

    (* set global checkmate variable *)
    
    If[ Length[wedges]==0 , wCheckmate=True ];
    If[ Length[bedges]==0 , bCheckmate=True ]; 
    
    (* format and return master list of weighted edges *)
    
    edges = DeleteDuplicates[ Join[ MapThread[ Append , {wedges,Table[pw,Length[wedges]]}] , MapThread[ Append , {bedges,Table[pb,Length[bedges]]}] ] ];

    If[ OptionValue[State]=="Simple" , Return[ Transpose[Abs[Transpose[edges]]]] ];
    
    Return[ edges ];
];
 
(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* VISUALIZATION                                                                  *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

(* ------------------------------------------------------------------------------ *)
(* showChessPosition                                                              *)
(*                                                                                *)
(* Function returning an array plot displaying a chess position in                *)
(* the a classical (pieces on chessboard) style.                                  *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  pos - chess position                                                          *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  graphics                                                                      *)
(* ------------------------------------------------------------------------------ *)

showChessPosition[ pos_ ] := Module[ 
    { 
        supp=pos[[1]], 
        position=pos[[2]] 
    },
    
    Return[
        ArrayPlot[
            MapAt[ Boole[OddQ[#]] &, MapAt[ Boole[EvenQ[#]] &, Partition[ Range[64], 8 ] , 1 ;; 8 ;; 2 ] , 2 ;; 8 ;; 2 ],
            ColorRules -> { 0->RGBColor[1, 0.8078, 0.6196] , 1->RGBColor[0.8196, 0.5451, 0.2784] },
            FrameTicks -> { {#,#} &@ Table[{i,8-i+1,0},{i, 8}] , {#,#} &@ Table[{i,FromCharacterCode[ToCharacterCode["a"]+i-1],0},{i,8}] },
            FrameStyle -> Directive[ Thickness[-1] ],
            FrameTicksStyle -> Directive[ Black , FontFamily->"Arial" , FontSize->16 ],
            PlotRangePadding -> 0.1,
            Frame -> { True , True , False , False },
            Epilog -> ( Inset[ gPieces[#[[2]]] , {file[#[[1]]],rank[#[[1]]]}-0.5 , {Center,Center} , Scaled[{0.1,0.1}] ]& /@ position )
        ]
    ];
];
 
(* ------------------------------------------------------------------------------ *)
(* showPositionalChessGraph                                                       *)
(*                                                                                *)
(* Function returning chessgraph with given nodes and edges. For display reasons, *)
(* nodes occupied by white/black pieces are colored in gray/black, unoccupied     *)
(* nodes in open circles; edges originating from nodes occupied by white/black.   *) 
(* If NodeLayout->"Chessboard" is used, nodes are arranged in a classical 8x8     *)
(* chessboard layout, with arrows indicating directed edges between source and    *)
(* target nodes. If NodeLayout->"LinearV" or "LinearH" are used, nodes are        *)
(* arranged in a linear fashion (vertical or horizontal, respectively). In this   *)
(* case, edges originating from nodes occupied by white/black pieces are colored  *)
(* in gray/black; edges targeting nodes are drawn without arrows, as source is    *)
(* indicated by the edges' color. If GraphType->"Simple" is chosen, all nodes and *)
(* edges are displayed in one color.                                              *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  ShowNodeID - show node ID (True/False)                                        *)
(*               ShowNodeID->True : display numbered nodes (NOTE: this option     *)
(*                                  only applies if NodeLayout->"Chessboard"))    *)
(*           (D) ShowNodeID->False : display only nodes                           *)
(*  GraphType  - type of returned graph                                           *)
(*           (D) GraphType->"Color" : graph with "Color" edge/node states         *)
(*               GraphType->"Simple" : simple graph ("Simple" edge/node states)   *)
(*  NodeLayout - node layout of returned graph                                    *)
(*           (D) NodeLayout->"Chessboard" : 8x8 node layout                       *)
(*               NodeLayout->"LinearV" : linear vertical node arrangement         *)
(*               NodeLayout->"LinearH" : linear horizontal node arrangement       *)
(*  EdgeLayout - edge layout of returned graph                                    *)
(*           (D) EdgeLayout->"Automatic"                                          *)
(*               EdgeLayout->"Chessboard" : height of arched edges indicates      *)
(*                                          chessboard distance between source    *)
(*                                          and target node (NOTE: this option    *)
(*                                          only applies if NodeLayout->"Linear") *)
(*  ShowFrame  - True/False for showing frame with tickmarks indicating the       *)
(*               chessboard distance                                              *)
(*           (D) ShowFrame->False                                                 *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  graphics                                                                      *)
(* ------------------------------------------------------------------------------ *)

showPositionalChessGraph[ nodes_ , edges_ , OptionsPattern[{ShowNodeID->False,GraphType->"Color",NodeLayout->"Chessboard",EdgeLayout->"Automatic",ShowFrame->False}] ] := Module[ 
    { 
        snodes=Sign[nodes],
        sedges=edges
    },    
    
    If[ OptionValue[GraphType]=="Simple" , snodes=Abs[Sign[nodes]] ; sedges=edges /. {n1_,n2_,s_}->{n1,n2,Abs[s]} ];
    
    Switch[ OptionValue[NodeLayout]
        , "Chessboard" ,
                Return[
                    Graph[
                        MapIndexed[ Property[ First@#2 , { VertexStyle->If[#==pw,{GrayLevel[0.7],EdgeForm[GrayLevel[0.7]]},If[#==pb,{Black,EdgeForm[Black]},White]] } ] &, snodes ],
                        Flatten[ { Style[ #[[1]]->#[[2]] , If[#[[3]]==pw,GrayLevel[0.7],Black] ] }& /@ sedges ],
                        VertexCoordinates -> Table[ {file[n],rank[n]} , {n,1,64} ],
                        VertexSize -> 0.5,
                        AspectRatio -> 1,
                        PlotRangePadding -> 0.4,
                        EdgeShapeFunction -> ({ Thickness[1/200] , Arrowheads[1/24] , Arrow[#,0.2] } &),
                        EdgeStyle -> Directive[ Black , Thickness[0.008] ],
                        VertexLabels -> If[ OptionValue[ShowNodeID] , Placed[ "Name" , Center ] , {} ], 
                        VertexLabelStyle -> If[ OptionValue[ShowNodeID] , Directive[ Gray , 12 , FontFamily->"Arial" ] , {} ]
                    ] 
                ]
        , "LinearV" ,
                Return[
                    Graph[
                        MapIndexed[ Property[ First@#2 , { VertexStyle->If[#==pw,{GrayLevel[0.7],EdgeForm[GrayLevel[0.7]]},If[#==pb,{Black,EdgeForm[Black]},White]] } ] &, snodes ],
                        Flatten[ { Style[ #[[1]]->#[[2]] , If[#[[3]]==pw,GrayLevel[0.7],Black] , Arrowheads[0] ] }& /@ sedges ],
                        VertexCoordinates -> Table[ {0,n} , {n,1,64} ],
                        VertexSize -> 1,
                        GraphLayout -> "LinearEmbedding",
                        EdgeShapeFunction -> If[ OptionValue[EdgeLayout]=="Chessboard" 
                                                ,( 
                                                    BezierCurve[
                                                        LinearSolve[
                                                            Transpose[ Outer[ BernsteinBasis[2,#1,#2] &, Range[0,2] , FoldList[ Plus , 0 , Normalize[ (Norm /@ Differences[ { First[#1] , { snodes[[#2[[1]]]]*4*ChessboardDistance[{file[#2[[1]]],rank[#2[[1]]]},{file[#2[[2]]],rank[#2[[2]]]}] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[#2[[1]]]]/2,0},{0,0},{-snodes[[#2[[1]]]]/2,0} } ] ) , Total ] ] ] ], 
                                                            { First[#1] , { snodes[[#2[[1]]]]*4*ChessboardDistance[{file[#2[[1]]],rank[#2[[1]]]},{file[#2[[2]]],rank[#2[[2]]]}] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[#2[[1]]]]/2,0},{0,0},{-snodes[[#2[[1]]]]/2,0} }
                                                        ] 
                                                    ] 
                                                &)
                                                ,( BezierCurve[ { First[#1] , { snodes[[#2[[1]]]]*(Abs[(Last[#1]-First[#1])]/2+1/2)[[2]] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[#2[[1]]]]/2,0},{0,0},{-snodes[[#2[[1]]]]/2,0} } ] &) 
                                             ],
                        PlotRange -> {{-32,32},{0,65}},
                        If[ !(OptionValue[ShowFrame]) || !(OptionValue[EdgeLayout]=="Chessboard")
                           ,Frame->None
                           ,{ 
                               Frame->{ True , False , False , False },
                               FrameTicks -> {{{-28,"7"},{-24,"6"},{-20,"5"},{-16,"4"},{-12,"3"},{-8,"2"},{-4,"1"},{4,"1"},{8,"2"},{12,"3"},{16,"4"},{20,"5"},{24,"6"},{28,"7"}},None},
                               FrameTicksStyle -> Directive[ Black , FontFamily->"Arial" , FontSize->12 ]
                            }
                        ]
                    ]
                ]
        , "LinearH" ,
                Return[
                    Rotate[Graph[
                        MapIndexed[ Property[ First@#2 , { VertexStyle->If[#==pw,{GrayLevel[0.7],EdgeForm[GrayLevel[0.7]]},If[#==pb,{Black,EdgeForm[Black]},White]] } ] &, snodes ],
                        Flatten[ { Style[ #[[1]]->#[[2]] , If[#[[3]]==pw,GrayLevel[0.7],Black] , Arrowheads[0] ] }& /@ sedges ],
                        VertexCoordinates -> Table[ {0,n} , {n,64,1,-1} ],
                        VertexSize -> 1,
                        GraphLayout -> "LinearEmbedding",
                        EdgeShapeFunction -> If[ OptionValue[EdgeLayout]=="Chessboard" 
                                                ,( 
                                                    BezierCurve[
                                                        LinearSolve[
                                                            Transpose[ Outer[ BernsteinBasis[2,#1,#2] &, Range[0,2] , FoldList[ Plus , 0 , Normalize[ (Norm /@ Differences[ { First[#1] , { snodes[[#2[[1]]]]*4*ChessboardDistance[{file[#2[[1]]],rank[#2[[1]]]},{file[#2[[2]]],rank[#2[[2]]]}] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[#2[[1]]]]/2,0},{0,0},{-snodes[[#2[[1]]]]/2,0} } ] ) , Total ] ] ] ], 
                                                            { First[#1] , { snodes[[#2[[1]]]]*4*ChessboardDistance[{file[#2[[1]]],rank[#2[[1]]]},{file[#2[[2]]],rank[#2[[2]]]}] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[#2[[1]]]]/2,0},{0,0},{-snodes[[#2[[1]]]]/2,0} }
                                                        ] 
                                                    ] 
                                                &)
                                                ,( BezierCurve[ { First[#1] , { snodes[[#2[[1]]]]*(Abs[(Last[#1]-First[#1])]/2+1/2)[[2]] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[#2[[1]]]]/2,0},{0,0},{-snodes[[#2[[1]]]]/2,0} } ] &) 
                                             ],
                        PlotRange -> {{-32,32},{0,65}},
                        If[ !(OptionValue[ShowFrame]) || !(OptionValue[EdgeLayout]=="Chessboard")
                           ,Frame->None
                           ,{ 
                               Frame->{ False , False , True , False },
                               FrameTicks -> {{{-28,"7"},{-24,"6"},{-20,"5"},{-16,"4"},{-12,"3"},{-8,"2"},{-4,"1"},{4,"1"},{8,"2"},{12,"3"},{16,"4"},{20,"5"},{24,"6"},{28,"7"}},None},
                               FrameTicksStyle -> Directive[ Black , FontFamily->"Arial" , FontSize->12 ]
                            }
                        ]
                    ] , 90 Degree ]
                ]
    ];

    Return[];
];

(* ------------------------------------------------------------------------------ *)
(* animateChessPositions                                                          *)
(*                                                                                *)
(* Function returning a graphics displaying in an interactive fashion multiple    *)
(* chess positions of a chess game.                                               *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  moves - list of PGN-formatted moves {<MOVE>,<MOVE>,...}                       *)
(*  pos   - list of chess positions                                               *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  graphics                                                                      *)
(* ------------------------------------------------------------------------------ *)
     
animateChessPositions[ moves_ , pos_ ] := Module[ 
    { 
        i
    },
    
    Return[
        Manipulate[ 
            ArrayPlot[
                MapAt[ Boole[OddQ[#]] &, MapAt[ Boole[EvenQ[#]] &, Partition[ Range[64], 8 ] , 1 ;; 8 ;; 2 ] , 2 ;; 8 ;; 2 ],
                PlotLabel -> Style[ If[ i>1 , ToString[Floor[i/2]]<>". "<>StringSplit[ moves[[Floor[i/2]]] ][[ Mod[i,2]+1 ]] , "" ] , 20 , If[ Mod[i,2]==0 , GrayLevel[0.7] , Black ] ],
                ColorRules -> { 0->RGBColor[1, 0.8078, 0.6196] , 1->RGBColor[0.8196, 0.5451, 0.2784] },
                FrameTicks -> { {#,#} &@ Table[{i,8-i+1,0},{i, 8}] , {#,#} &@ Table[{i,FromCharacterCode[ToCharacterCode["a"]+i-1],0},{i,8}] },
                FrameStyle -> Directive[ White, Thickness[-1] ],
                FrameTicksStyle -> Directive[ Black , FontFamily->"Arial" , FontSize->16 ],
                PlotRangePadding -> 0.1,
                Frame -> { True , True , False , False },
                Epilog -> ( Inset[ gPieces[#[[2]]] , {file[#[[1]]],rank[#[[1]]]}-0.5 , {Center,Center} , Scaled[{0.1,0.1}] ]& /@ pos[[i,2]] )
            ], 
            { 
                {i , 1 , Style["position",14,Italic] } , 1 , Length[pos] , 1 , 
                Grid[ { { Slider[ ## , Appearance -> Large ], 
                          InputField[ # , FieldSize -> Tiny ], 
                          Animator[ ## , AnimationRunning -> False , AppearanceElements -> { "StepLeftButton" , "StepRightButton" , "PlayPauseButton" , "DirectionButton" } ]
                      } } , Alignment -> { Center , Center } ] & 
            }
        ]
    ];

    Return[];
];
 
(* ------------------------------------------------------------------------------ *)
(* animatePositionalChessGraphs                                                   *)
(*                                                                                *)
(* Function returning a graphics displaying in an interactive fashion multiple    *)
(* positional chess graphs, with given nodes and edges associated with the        *)
(* positions of a chess game. For display reasons, nodes occupied by white/black  *)
(* pieces are colored in gray/black, unoccupied nodes in open circles; edges      *)
(* originating from nodes occupied by white/black. If NodeLayout->"Chessboard"    *)
(* is used, nodes are arranged in a classical 8x8 chessboard layout, with arrows  *)
(* indicating directed edges between source and target nodes. If NodeLayout->     *)
(* "LinearV" or "LinearH" are used, nodes are arranged in a linear fashion        *)
(* (vertical or horizontal, respectively). In this case, edges originating from   *)
(* nodes occupied by white/black pieces are colored in gray/black; edges          *)
(* targeting nodes are drawn without arrows, as source is indicated by the edges' *)
(* color. If GraphType->"Simple" is chosen, all nodes and edges are displayed     *)
(* in one color.                                                                  *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  ShowNodeID - show node ID (True/False)                                        *)
(*               ShowNodeID->True : display numbered nodes (NOTE: this option     *)
(*                                  only applies if NodeLayout->"Chessboard"))    *)
(*           (D) ShowNodeID->False : display only nodes                           *)
(*  GraphType  - type of returned graph                                           *)
(*           (D) GraphType->"Color" : graph with "Color" edge/node states         *)
(*               GraphType->"Simple" : simple graph ("Simple" edge/node states)   *)
(*  NodeLayout - node layout of returned graph                                    *)
(*           (D) NodeLayout->"Chessboard" : 8x8 node layout                       *)
(*               NodeLayout->"LinearV" : linear vertical node arrangement         *)
(*               NodeLayout->"LinearH" : linear horizontal node arrangement       *)
(*  EdgeLayout - edge layout of returned graph                                    *)
(*           (D) EdgeLayout->"Automatic"                                          *)
(*               EdgeLayout->"Chessboard" : height of arched edges indicates      *)
(*                                          chessboard distance between source    *)
(*                                          and target node (NOTE: this option    *)
(*                                          only applies if NodeLayout->"Linear") *)
(*  ShowFrame  - True/False for showing frame with tickmarks indicating the       *)
(*               chessboard distance                                              *)
(*           (D) ShowFrame->False                                                 *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  graphics                                                                      *)
(* ------------------------------------------------------------------------------ *)

animatePositionalChessGraphs[ nodes_ , edges_ , OptionsPattern[{ShowNodeID->False,GraphType->"Color",NodeLayout->"Chessboard",EdgeLayout->"Automatic",ShowFrame->False}] ] := Module[ 
    { 
        snodes=Sign[nodes],
        sedges=edges,
        i 
    },

    If[ OptionValue[GraphType]=="Simple" , snodes=Abs[Sign[nodes]] ; sedges=edges /. {n1_,n2_,s_}->{n1,n2,Abs[s]} ];
 
    Switch[ OptionValue[NodeLayout]
        , "Chessboard" ,
                Return[
                    Manipulate[             
                        Graph[
                            MapIndexed[ Property[ First@#2 , { VertexStyle->If[#==pw,{GrayLevel[0.7],EdgeForm[GrayLevel[0.7]]},If[#==pb,{Black,EdgeForm[Black]},White]] } ] &, snodes[[i]] ],
                            Flatten[ { Style[ #[[1]]->#[[2]] , If[#[[3]]==pw,GrayLevel[0.7],Black] ] }& /@ sedges[[i]] ],
                            VertexCoordinates -> Table[ {file[n],rank[n]} , {n,1,64} ],
                            VertexSize -> 0.5,
                            AspectRatio -> 1,
                            PlotRangePadding -> 0.4,
                            EdgeShapeFunction -> ({ Thickness[1/200] , Arrowheads[1/24] , Arrow[#,0.2] } &),
                            EdgeStyle -> Directive[ Black , Thickness[0.008] ],
                            VertexLabels -> If[ OptionValue[ShowNodeID] , Placed[ "Name" , Center ] , {} ], 
                            VertexLabelStyle -> If[ OptionValue[ShowNodeID] , Directive[ Gray ,  12 , FontFamily->"Arial" ] , {} ]
                        ],
                        { 
                            {i , 1 , Style["position",14,Italic] } , 1 , Length[snodes] , 1 , 
                            Grid[ { { Slider[ ## , Appearance -> Large ], 
                                      InputField[ # , FieldSize -> Tiny ], 
                                      Animator[ ## , AnimationRunning -> False , AppearanceElements -> { "StepLeftButton" , "StepRightButton" , "PlayPauseButton" , "DirectionButton" } ]
                                  } } , Alignment -> { Center , Center } ] & 
                        }
                    ]
                ]
        , "LinearV" ,
                Return[
                    Manipulate[             
                        Graph[
                            MapIndexed[ Property[ First@#2 , { VertexStyle->If[#==pw,{GrayLevel[0.7],EdgeForm[GrayLevel[0.7]]},If[#==pb,{Black,EdgeForm[Black]},White]] } ] &, snodes[[i]] ],
                            Flatten[ { Style[ #[[1]]->#[[2]] , If[#[[3]]==pw,GrayLevel[0.7],Black] , Arrowheads[0] ] }& /@ sedges[[i]] ],
                            VertexCoordinates -> Table[ {0,n} , {n,1,64} ],
                            VertexSize -> 1,
                            GraphLayout -> "LinearEmbedding",
                            EdgeShapeFunction -> If[ OptionValue[EdgeLayout]=="Chessboard" 
                                                    ,( 
                                                        BezierCurve[
                                                            LinearSolve[
                                                                Transpose[ Outer[ BernsteinBasis[2,#1,#2] &, Range[0,2] , FoldList[ Plus , 0 , Normalize[ (Norm /@ Differences[ { First[#1] , { snodes[[i,#2[[1]]]]*4*ChessboardDistance[{file[#2[[1]]],rank[#2[[1]]]},{file[#2[[2]]],rank[#2[[2]]]}] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[i,#2[[1]]]]/2,0},{0,0},{-snodes[[i,#2[[1]]]]/2,0} } ] ) , Total ] ] ] ], 
                                                                { First[#1] , { snodes[[i,#2[[1]]]]*4*ChessboardDistance[{file[#2[[1]]],rank[#2[[1]]]},{file[#2[[2]]],rank[#2[[2]]]}] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[i,#2[[1]]]]/2,0},{0,0},{-snodes[[i,#2[[1]]]]/2,0} }
                                                            ] 
                                                        ] 
                                                    &)
                                                    ,( BezierCurve[ { First[#1] , { snodes[[i,#2[[1]]]]*(Abs[(Last[#1]-First[#1])]/2+1/2)[[2]] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[i,#2[[1]]]]/2,0},{0,0},{-snodes[[i,#2[[1]]]]/2,0} } ] &) 
                                                 ],
                            PlotRange -> {{-32,32},{0,65}},
                            If[ !(OptionValue[ShowFrame]) || !(OptionValue[EdgeLayout]=="Chessboard")
                               ,Frame->None
                               ,{ 
                                   Frame->{ True , False , False , False },
                                   FrameTicks -> {{{-28,"7"},{-24,"6"},{-20,"5"},{-16,"4"},{-12,"3"},{-8,"2"},{-4,"1"},{4,"1"},{8,"2"},{12,"3"},{16,"4"},{20,"5"},{24,"6"},{28,"7"}},None},
                                   FrameTicksStyle -> Directive[ Black , FontFamily->"Arial" , FontSize->12 ]
                                }
                            ]
                        ],
                        { 
                            {i , 1 , Style["position",14,Italic] } , 1 , Length[snodes] , 1 , 
                            Grid[ { { Slider[ ## , Appearance -> Large ], 
                                      InputField[ # , FieldSize -> Tiny ], 
                                      Animator[ ## , AnimationRunning -> False , AppearanceElements -> { "StepLeftButton" , "StepRightButton" , "PlayPauseButton" , "DirectionButton" } ]
                                  } } , Alignment -> { Center , Center } ] & 
                        }
                    ]
                ]
        , "LinearH" ,
                Return[
                    Manipulate[             
                        Rotate[Graph[
                            MapIndexed[ Property[ First@#2 , { VertexStyle->If[#==pw,{GrayLevel[0.7],EdgeForm[GrayLevel[0.7]]},If[#==pb,{Black,EdgeForm[Black]},White]] } ] &, snodes[[i]] ],
                            Flatten[ { Style[ #[[1]]->#[[2]] , If[#[[3]]==pw,GrayLevel[0.7],Black] , Arrowheads[0] ] }& /@ sedges[[i]] ],
                            VertexCoordinates -> Table[ {0,n} , {n,64,1,-1} ],
                            VertexSize -> 1,
                            GraphLayout -> "LinearEmbedding",
                            EdgeShapeFunction -> If[ OptionValue[EdgeLayout]=="Chessboard" 
                                                    ,( 
                                                        BezierCurve[
                                                            LinearSolve[
                                                                Transpose[ Outer[ BernsteinBasis[2,#1,#2] &, Range[0,2] , FoldList[ Plus , 0 , Normalize[ (Norm /@ Differences[ { First[#1] , { snodes[[i,#2[[1]]]]*4*ChessboardDistance[{file[#2[[1]]],rank[#2[[1]]]},{file[#2[[2]]],rank[#2[[2]]]}] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[i,#2[[1]]]]/2,0},{0,0},{-snodes[[i,#2[[1]]]]/2,0} } ] ) , Total ] ] ] ], 
                                                                { First[#1] , { snodes[[i,#2[[1]]]]*4*ChessboardDistance[{file[#2[[1]]],rank[#2[[1]]]},{file[#2[[2]]],rank[#2[[2]]]}] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[i,#2[[1]]]]/2,0},{0,0},{-snodes[[i,#2[[1]]]]/2,0} }
                                                            ] 
                                                        ] 
                                                    &)
                                                    ,( BezierCurve[ { First[#1] , { snodes[[i,#2[[1]]]]*(Abs[(Last[#1]-First[#1])]/2+1/2)[[2]] , (Abs[(Last[#1]+First[#1])]/2)[[2]] } , Last[#1] } + { {-snodes[[i,#2[[1]]]]/2,0},{0,0},{-snodes[[i,#2[[1]]]]/2,0} } ] &) 
                                                 ],
                            PlotRange -> {{-32,32},{0,65}},
                            If[ !(OptionValue[ShowFrame]) || !(OptionValue[EdgeLayout]=="Chessboard")
                               ,Frame->None
                               ,{ 
                                   Frame->{ False , False , True , False },
                                   FrameTicks -> {{{-28,"7"},{-24,"6"},{-20,"5"},{-16,"4"},{-12,"3"},{-8,"2"},{-4,"1"},{4,"1"},{8,"2"},{12,"3"},{16,"4"},{20,"5"},{24,"6"},{28,"7"}},None},
                                   FrameTicksStyle -> Directive[ Black , FontFamily->"Arial" , FontSize->12 ]
                                }
                            ]
                        ] , 90 Degree ],
                        { 
                            {i , 1 , Style["position",14,Italic] } , 1 , Length[snodes] , 1 , 
                            Grid[ { { Slider[ ## , Appearance -> Large ], 
                                      InputField[ # , FieldSize -> Tiny ], 
                                      Animator[ ## , AnimationRunning -> False , AppearanceElements -> { "StepLeftButton" , "StepRightButton" , "PlayPauseButton" , "DirectionButton" } ]
                                  } } , Alignment -> { Center , Center } ] & 
                        }
                    ]
                ]
    ];

    Return[];
];


(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* CHESS GRAPH ANALYSIS                                                           *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

(* ------------------------------------------------------------------------------ *)
(* getAdjacencyMatrix                                                             *)
(*                                                                                *)
(* Function returning the adjacency matrices a_ij of a chess graph.               *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - edge state/weight:                                                    *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Color" : weighted adjacency matrix                                    *)
(*  State->"Simple" : relational adjacency matrix                                 *)
(* ------------------------------------------------------------------------------ *)
    
getAdjacencyMatrix[ edges_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        aij=ConstantArray[ 0 , {64,64} ]
    },
    
    ( aij[[ #[[1]] , #[[2]] ]] = #[[3]] )& /@ edges;
    If[ OptionValue[State]=="Simple" , aij = Abs[aij] ];
    
    Return[ aij ];
];     
     
(* ------------------------------------------------------------------------------ *)
(* getNumberOfNodes                                                               *)
(*                                                                                *)
(* Function returning a list containing the number of nodes N of a chess graph.   *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - node states to be counted                                             *)
(*          State->"Piece" : state indicating individual type of chess pieces     *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Piece" : {#wP,#wN,#wB,#wR,#wQ,#wK,#bK,#bQ,#bR,#bB,#bN,#bP}            *)
(*  State->"Color" : {#white nodes,#black nodes}                                  *)
(*  State->"Simple" : #occupied nodes                                             *)
(* ------------------------------------------------------------------------------ *)
 
getNumberOfNodes[ nodes_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        nn
    },
    
    Switch[ OptionValue[State]
        , "Piece"  , nn = Drop[ BinCounts[ nodes , {-7,8,1} ] , {7,9} ];
        , "Color"  , nn = Drop[ BinCounts[ Sign[nodes] , {-1,2,1} ] , {2} ];
        , "Simple" , nn = Total[ Abs[Sign[nodes]] ];
    ];
     
    Return[ nn ];
];     
     
(* ------------------------------------------------------------------------------ *)
(* getNumberOfEdges                                                               *)
(*                                                                                *)
(* Function returning the number of edges N_E of a chess graph.                   *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - edge state/weight:                                                    *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Color" : {#edges (white source nodes),#edges (black source nodes)}    *)
(*  State->"Simple" : #edges                                                      *)
(* ------------------------------------------------------------------------------ *)

getNumberOfEdges[ edges_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        ne
    },
    
    Switch[ OptionValue[State]
        , "Color"  , ne = Drop[ BinCounts[ Transpose[ edges ][[3]] , {-1,2,1} ] , {2} ];
        , "Simple" , ne = Total[ Abs[ Transpose[ edges ][[3]] ] ];
    ];
 
    Return[ ne ];
];     

(* ------------------------------------------------------------------------------ *)
(* getConnectedness                                                               *)
(*                                                                                *)
(* Function returning the connectedness Co of a chess graph. If the argument is   *)
(* a list containing the number of edges originating from white and black nodes,  *)
(* the returned list contains the connectedness of the corresponding subgraphs,   *)
(* otherwise the total connectedness is returned.                                 *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  ne - number of edges                                                          *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  {Co of white source node subgraph, Co of black source node subgraph} or Co    *)
(* ------------------------------------------------------------------------------ *)

getConnectedness[ ne_ ] := Module[ 
    { },
    
    Return[ ne/4032.0 ];
];     
     
(* ------------------------------------------------------------------------------ *)
(* getMobility                                                                    *)
(*                                                                                *)
(* Function returning the mobility M of a chess graph.                            *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - edge state/weight:                                                    *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Color" : {M of white nodes, M of black nodes}                         *)
(*  State->"Simple" : total M                                                     *)
(* ------------------------------------------------------------------------------ *)

getMobility[ nodes_ , edges_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        wn,
        bn,
        an,
        m 
    },
    
    Switch[ OptionValue[State]
        , "Color"  , 
           wn = DeleteCases[ MapIndexed[ If[ #==pw , First@#2 ] &, Sign[nodes] ] , Null ];
           bn = DeleteCases[ MapIndexed[ If[ #==pb , First@#2 ] &, Sign[nodes] ] , Null ];
           m = { Length[ Complement[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[wn,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] , bn ] ] , Length[ Complement[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[bn,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] , wn ] ] }/64.0
        , "Simple" , 
           an = DeleteCases[ MapIndexed[ If[ #==1 , First@#2 ] &, Abs[Sign[nodes]] ] , Null ];
           m = Length[ Complement[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[an,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] , an ] ]/64.0
    ];
 
    Return[ m ];
];
                                                                         
(* ------------------------------------------------------------------------------ *)
(* getControl                                                                     *)
(*                                                                                *)
(* Function returning the control C of a chess graph.                             *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - edge state/weight:                                                    *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Color" : {C of white nodes, C of black nodes}                         *)
(*  State->"Simple" : total C                                                     *)
(* ------------------------------------------------------------------------------ *)
    
getControl[ nodes_ , edges_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        wn,
        bn,
        an,
        c 
    },

    Switch[ OptionValue[State]
        , "Color"  , 
           wn = DeleteCases[ MapIndexed[ If[ #==pw , First@#2 ] &, Sign[nodes] ] , Null ];
           bn = DeleteCases[ MapIndexed[ If[ #==pb , First@#2 ] &, Sign[nodes] ] , Null ];
           c = { Length[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[wn,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ] , Length[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[bn,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ] }/64.0
        , "Simple" , 
           an = DeleteCases[ MapIndexed[ If[ #==1 , First@#2 ] &, Abs[Sign[nodes]] ] , Null ];
           c = Length[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[an,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ]/64.0
    ];
 
    Return[ c ];
];

(* ------------------------------------------------------------------------------ *)
(* getDominance                                                                   *)
(*                                                                                *)
(* Function returning the dominance D of a chess graph.                           *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - edge state/weight:                                                    *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Color" : {D of white nodes, D of black nodes}                         *)
(*  State->"Simple" : total D                                                     *)
(* ------------------------------------------------------------------------------ *)
                                                                         
getDominance[ nodes_ , edges_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        wn,
        bn,
        an,
        d 
    },

    Switch[ OptionValue[State]
        , "Color"  , 
           wn = DeleteCases[ MapIndexed[ If[ #==pw , First@#2 ] &, Sign[nodes] ] , Null ];
           bn = DeleteCases[ MapIndexed[ If[ #==pb , First@#2 ] &, Sign[nodes] ] , Null ];
           d = { Length[ DeleteDuplicates[ Join[ wn , DeleteCases[ If[ MemberQ[wn,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ] ] , Length[ DeleteDuplicates[ Join[ bn , DeleteCases[ If[ MemberQ[bn,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ] ] }/64.0
        , "Simple" , 
           an = DeleteCases[ MapIndexed[ If[ #==1 , First@#2 ] &, Abs[Sign[nodes]] ] , Null ];
           d = Length[ DeleteDuplicates[ Join[ an , DeleteCases[ If[ MemberQ[an,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ] ]/64.0
    ];
 
    Return[ d ];
];
                                                                         
(* ------------------------------------------------------------------------------ *)
(* getAverageNodeReach                                                            *)
(*                                                                                *)
(* Function returning the average node reach <R> of a chess graph.                *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - edge state/weight:                                                    *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Color" : {<R> of white nodes, <R> of black nodes}                     *)
(*  State->"Simple" : total <R>                                                   *)
(* ------------------------------------------------------------------------------ *)

getAverageNodeReach[ nodes_ , edges_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        wn,
        bn,
        an,
        r 
    },

    Switch[ OptionValue[State]
        , "Color"  , 
           wn = DeleteCases[ MapIndexed[ If[ #==pw , First@#2 ] &, Sign[nodes] ] , Null ];
           bn = DeleteCases[ MapIndexed[ If[ #==pb , First@#2 ] &, Sign[nodes] ] , Null ];
           r = { Length[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[wn,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ]/Length[wn] , Length[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[bn,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ]/Length[bn] }
        , "Simple" , 
           an = DeleteCases[ MapIndexed[ If[ #==1 , First@#2 ] &, Abs[Sign[nodes]] ] , Null ];
           r = Length[ DeleteDuplicates[ DeleteCases[ If[ MemberQ[an,#[[1]]] , #[[2]] ]& /@ edges , Null ] ] ]/Length[an]
    ];
 
    Return[ N[ r ] ];
];
    
(* ------------------------------------------------------------------------------ *)
(* getOffensiveness                                                               *)
(*                                                                                *)
(* Function returning the offensiveness O of a chess graph.                       *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*  edges - 2d list of weighted edges                                             *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - edge state/weight:                                                    *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Color" : {O of white nodes, O of black nodes}                         *)
(*  State->"Simple" : total O                                                     *)
(* ------------------------------------------------------------------------------ *)

getOffensiveness[ nodes_ , edges_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        wn,
        bn,
        an,
        o 
    },

    Switch[ OptionValue[State]
        , "Color"  , 
           wn = DeleteCases[ MapIndexed[ If[ #==pw , First@#2 ] &, Sign[nodes] ] , Null ];
           bn = DeleteCases[ MapIndexed[ If[ #==pb , First@#2 ] &, Sign[nodes] ] , Null ];
           o = { Length[ DeleteDuplicates[ DeleteCases[ If[ (MemberQ[wn,#[[1]]]) && (MemberQ[bn,#[[2]]]) , #[[2]] ]& /@ edges , Null ] ] ] , Length[ DeleteDuplicates[ DeleteCases[ If[ (MemberQ[bn,#[[1]]]) && (MemberQ[wn,#[[2]]]) , #[[2]] ]& /@ edges , Null ] ] ] }
        , "Simple" , 
           an = DeleteCases[ MapIndexed[ If[ #==1 , First@#2 ] &, Abs[Sign[nodes]] ] , Null ];
           o = Length[ DeleteDuplicates[ DeleteCases[ If[ (MemberQ[an,#[[1]]]) && (MemberQ[an,#[[2]]]) , #[[2]] ]& /@ edges , Null ] ] ]
    ];
 
    Return[ o ];
];

(* ------------------------------------------------------------------------------ *)
(* getDefensiveness                                                               *)
(*                                                                                *)
(* Function returning the defensiveness D associated with a chess position.       *)
(*                                                                                *)
(* ARGUMENTS:                                                                     *)
(*  nodes - 1d list of node states                                                *)
(*  pos   - chess position                                                        *)
(*                                                                                *)
(* OPTIONS:                                                                       *)
(*  State - edge state/weight:                                                    *)
(*      (D) State->"Color" : state indicating color of chess pieces               *)
(*          State->"Simple" : state indicating whether node is occupied or not    *)
(*                                                                                *)
(* RETURN:                                                                        *)
(*  State->"Color" : {D of white nodes, D of black nodes}                         *)
(*  State->"Simple" : total D                                                     *)
(* ------------------------------------------------------------------------------ *)

getDefensiveness[ nodes_ , pos_ , OptionsPattern[{State->"Color"}] ] := Module[ 
    { 
        edges,
        wn,
        bn,
        d 
    },

    edges = getEdgesFromPosition[ pos , SameColorTargets->True ];
    wn = DeleteCases[ MapIndexed[ If[ #==pw , First@#2 ] &, Sign[nodes] ] , Null ];
    bn = DeleteCases[ MapIndexed[ If[ #==pb , First@#2 ] &, Sign[nodes] ] , Null ];
    
    Switch[ OptionValue[State]
        , "Color"  , 
           d = { Length[ DeleteDuplicates[ DeleteCases[ If[ (MemberQ[wn,#[[1]]]) && (MemberQ[wn,#[[2]]]) , #[[2]] ]& /@ edges , Null ] ] ] , Length[ DeleteDuplicates[ DeleteCases[ If[ (MemberQ[bn,#[[1]]]) && (MemberQ[bn,#[[2]]]) , #[[2]] ]& /@ edges , Null ] ] ] }
        , "Simple" , 
           d = Length[ DeleteDuplicates[ DeleteCases[ If[ (MemberQ[wn,#[[1]]]) && (MemberQ[wn,#[[2]]]) , #[[2]] ]& /@ edges , Null ] ] ] + Length[ DeleteDuplicates[ DeleteCases[ If[ (MemberQ[bn,#[[1]]]) && (MemberQ[bn,#[[2]]]) , #[[2]] ]& /@ edges , Null ] ] ]
    ];
 
    Return[ d ];
];

(* ============================================================================== *)
(* ============================================================================== *)
(* ============================================================================== *)
