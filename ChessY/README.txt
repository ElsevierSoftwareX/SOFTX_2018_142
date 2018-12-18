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



=================
TABLE OF CONTENTS
=================

[1] Data Objects, Constants and Auxiliary Functions
[1.1] Position Data Object
[1.2] Nodes Data Object
[1.3] Edges Data Object
[1.4] File/Rank-NodeID Mapping
[1.5] Moves
[1.6] Visualization

[2] Chess Position Generation
[2.1] Manual
[2.2] PGN Parser

[3] Chess Graph Generation

[4] Visualization

[5] Chess Graph Analysis 

[6] Examples
[6.1] Analysis of a Chess Game


(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* [1] Data Objects, Constants and Auxiliary Functions                            *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

ChessY is build around three principal types of data objects: position, nodes and 
edges. While the position object contains a list of pieces and their location on
the chessboard, thus uniquely encodes a given chess position, nodes and edges
are lists which characterize the chess graph associated with a given position.

==========================
[1.1] Position Data Object
==========================

position = { <SUPPLEMENTARY INFO> , <LIST OF OCCUPIED SQUARES> }

Here, <SUPPLEMENTARY INFO> is a key-value association list containing supplementary
information characterizing a given chess position. This association list has the 
form

<SUPPLEMENTARY INFO> = <| "enpassant"-><VALUE> , "castling"-><VALUE> , "check"-><VALUE> , "checkmate"-><VALUE> |>

where 

"enpassant" indicates the node ID (see below) of the target node of a possible 
    en passant move (default is 0)
    
"castling" contains information about possible castling moves in form of a list
    { { QSwhite , KSwhite } , { QSblack , KSblack } }, where 
    QSwhite = True/False for white queenside castling
    KSwhite = True/False for white kingside castling
    QSblack = True/False for black queenside castling
    KSblack = True/False for black kingside castling
    (default is {{True,True},{True,True}})

"check" indicates whether a check was issued in the given position using 
    True/False (default is False)
    
"checkmate" indicates whether a checkmate was issued in the given position 
    using True/False (default is False)
    
The <LIST OF OCCUPIED SQUARES> is a list of elements of type { node ID , piece ID }
containing the set of all occupied squares or nodes. The node ID is a unique 
identifier of each chessboard square, ranging from 1 to 64, starting in the lower
left-hand corner with the a1 square and ending in the upper right-hand corner 
(h8 square). That is,

file/rank | node ID
----------+--------
    a1    |    1
    b1    |    2
    ...   |   ...
    h1    |    8
    a2    |    9
    ...   |   ...
    h8    |   64

For mapping the file/rank chessboard square identifier to the node ID, see [1/4].
The piece ID is a unique identifier for the chess piece type:

    piece    | piece ID
-------------+---------
white king   |    -2
white queen  |    -3
white rook   |    -4
white bishop |    -5
white knight |    -6
white pawn   |    -7
black king   |     2
black queen  |     3
black rook   |     4
black bishop |     5
black knight |     6
black pawn   |     7
none         |     0    

EXAMPLES:

(* empty chess board *)
positionEmpty = {<|"enpassant"->0,"castling"->{{True,True},{True,True}},"check"->False,"checkmate"->False|>,{}};

(* initial chess position *)
positionStart = {<|"enpassant"->0,"castling"->{{True,True},{True,True}},"check"->False,"checkmate"->False|>,
                 {{1,wR},{2,wN},{3,wB},{4,wQ},{5,wK},{6,wB},{7,wN},{8,wR},{9,wP},{10,wP},{11,wP},{12,wP},{13,wP},{14,wP},{15,wP},{16,wP},
                 {49,bP},{50,bP},{51,bP},{52,bP},{53,bP},{54,bP},{55,bP},{56,bP},{57,bR},{58,bN},{59,bB},{60,bQ},{61,bK},{62,bB},{63,bN},{64,bR}}};

(* famous move 11 in match Spassky-Fischer (8th World Championship, Jul 16, 1972)"]
position23SpasskyFischer1972 = {<|"enpassant"->0,"castling"->{{False,False},{False,False}},"check"->False,"checkmate"->False|>,
                 {{1,wR},{3,wB},{6,wR},{7,wK},{9,wP},{10,wP},{11,wQ},{12,wN},{13,wB},{14,wP},{15,wP},{16,wP},{19,wN},{29,wP},{35,bP},
                 {36,wP},{40,bN},{44,bP},{47,bP},{49,bP},{50,bP},{52,bN},{54,bP},{55,bB},{56,bP},{57,bR},{59,bB},{60,bQ},{61,bR},{63,bK}}};

=======================
[1.2] Nodes Data Object
=======================

nodes = { node states }

The nodes data object is a 1-dimensional list (vector) of length 64 with element 
i containing the node state of node i. The node state characterizes each node, and 
can take various values depending on an optional argument "State" which can be chosen 
in many functions (see below). Specifically,

State -> "Piece" - the node state indicates the type of piece occupying the node:

    piece    | node state
-------------+-----------
white king   |    -2
white queen  |    -3
white rook   |    -4
white bishop |    -5
white knight |    -6
white pawn   |    -7
black king   |     2
black queen  |     3
black rook   |     4
black bishop |     5
black knight |     6
black pawn   |     7
none         |     0    

State -> "Color" - the node state indicates the color of the piece occupying the node:

color | node state
------+-----------
white |    -1
black |     1
none  |     0

State -> "Simple" - the node state indicates whether a node is occupied or not:

occupied | node state
---------+-----------
   yes   |   1
   no    |   0

=======================
[1.3] Edges Data Object
=======================

edges = { { source node ID , target node ID , edge state } , ... }

The edges data object is a 2-dimensional list (matrix), specifically a variable-length 
list with elements of the form { source node ID , target node ID , edge state }, 
where the edge state is equal to the state of the source node. 

==============================
[1.4] File/Rank-NodeID Mapping
==============================

Each square on the chessboard is uniquely labelled in terms of file (a,b,...,h) and 
rank (1,2,...,8). In order to simplify the mapping between file/rank and node ID, 
ChessY associates each file with numbers: a->1, b->2, ..., h->8. For converting 
between file, rank and node IDs, ChessY provides three functions:

file[ node ID ]      ::  node ID -> file
rank[ node ID ]      ::  node ID -> rank
node[ file , rank ]  ::  file,rank -> node ID

===========
[1.5] Moves
===========

In order to generate positional chess graphs from given chess positions, Chessy 
employs 2-dimensional data arrays which effectively encode all possible moves for 
each type of chess piece:

mN, mNE, mE, mSE, mS, mSW, mW, mNW - lists providing the nodes in each of the 
    cardinal and intercardinal directions (N,NE,E,SE,S,SW,W,NW), indexed by the 
    node of origin; these lists are used to generate edges for queen, rook and bishop

mKing - standard moves of the white/black king

mKnight - standard moves of the white/black knight

mwPawn, mbPawn - standard moves of the white/black pawn 

mwPawnR, mbPawnR - additional rank 2/7 move of the white/black pawn

mwPawnX, mbPawnX - capture moves of the white/black pawn

mwPawnEP, mbPawnEP - en passant moves of the white/black pawn

enPassantXw, enPassantXb - en passant target nodes, indexed by the target node of 
the opposing pawn subject to potential en passant capture; the lists contain the 
allowed values for the en passant variable used during the evaluation of game 
positions

===================
[1.6] Visualization
===================

Additional graphical objects are required to visualize a given chess position in the
classical style. These objects are imported as EPS files residing in the subdirectory 
"pieces/".  

(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* [2] Chess Position Generation                                                  *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

ChessY provides two ways to generate chess positions, a manual one and by parsing
PGN chess records.

============
[2.1] Manual
============

ChessY has two functions for populating a chess board with chess pieces, thus 
creating valid chess positions.

(1) getPositionFromPieceFileRank[ {p1,p2,...} ]

Using this function, pieces p1, p2, ... can be placed on the board by specifying 
the piece color and type, as well as the file and rank of the target square. 
Entries in the argument list must be of the form

<COLOR><TYPE><FILE><RANK>

The function takes optional arguments:

EnPassant 
    FileRank of target node of possible en passant move 
    Default: enPassant->0

Castling
    possible castling moves in the given position in form of 
    {{QSwhite,KSwhite},{QSblack,KSblack}}, where QS=queenside
    and KS=kingside castling (see [1.1])
    Default: Castling->{{True,True},{True,True}}
    
Check
    check issued in generated position (True/False)
    Default: Check->False
    
Checkmate
    checkmate issued in generated position (True/False)
    Default: Checkmate->False

EXAMPLES:

(* white queen on square a1 *)
wQa1 

(* generates a chess position with a white queen on c5 and a black king on e8 *)
position = getPositionFromPieceFileRank[{wQc5,bKe8}];

(* generates ply 29 in Pietzcker Christmas Tournament (1928) between Gundersen and Faul *)
position = getPositionFromPieceFileRank[{wRa1,wBc1,wKe1,wRh1,wPa2,wPb2,wPf2,wPg2,wNc3,wQg4,wPe5,wPh5,wNe6,bRa8,bBc8,bQd8,bRf8,bPa7,bPb7,bNe7,bKh6,bPd5,bPf5,bPg5,bBb4,bNd4},EnPassant->g6,Castling->{{True,True},{False,False}},Check->False,Checkmate->False];

(2) getPositionFromPieceNode[ {p1,p2,...} ]

This function generates, akin to (1), a position by populating a chess graph's 
nodes with pieces. Entries in the argument are lists of the form

<COLOR><TYPE><NODE ID>

where <NODE ID> ranges from 1 to 64 (see [1.1]). Optional arguments are the same 
as for (1), except that the value of the EnPassant option is a node ID.

EXAMPLES:

(* white queen on square a1 *)
wQ1

(* generates a chess position with a white queen on node 35 (c5) and a black king on node 61 (e8) *)
position = getPositionFromPieceNode[{wQ35,bK61}];

(* generates ply 29 in Pietzcker Christmas Tournament (1928) between Gundersen and Faul *)
position = getPositionFromPieceNode[{wR1,wB3,wK5,wR8,wP9,wP10,wP14,wP15,wN19,wQ31,wP37,wP40,wN45,bR57,bB59,bQ60,bR62,bP49,bP50,bN53,bK48,bP36,bP38,bP39,bB26,bN28},EnPassant->47,Castling->{{True,True},{False,False}},Check->False,Checkmate->False];

================
[2.2] PGN Parser
================

The second way to generate chess positions is to utilize ChessY's PGN parser, which
processes PGN-formatted chess game records. Two functions are available, with the
first being (internally) used to get specific information of the move from a single
PGN-formatted string, and the second for parsing a whole PGN game record. Only the 
latter returns a list of chess positions.

(1) parsePGNMove[ move ]

This function parses a single PGN chessmove string and returns a list with details 
of the move. Please note that this is a very baseline parser for PNG-formatted 
strings which might not cover deviations from the FISA-recommended SAN format as 
well as special cases. Use with care!

The returned list contains the following information:

{ piece , file , rank , capture , disambiguation , castling , enpassant , promotion , check , checkmate , result }

Again, this function is merely used for internal purposes (the actual PGN chess 
game record parser, see (2)).

(2) getPositionsFromGamePGN[ moves ]

This is the actual PGN game record parser, taking a list of PGN-formatted moves as
argument, and returning a list of all successive positions of the chess game. 

EXAMPLE:

(* load the ChessY toolbox *)
Get["ChessY.m"];

(* open PGN record file *)
pgnfile = OpenRead["example1.pgn"];
eof = False;

(* setup list for handling data *)
game = Array[0,11];
{ cEvent , cSite , cDate , cRound , cWhite , cBlack , cResult , cECO , cWhiteElo , cBlackElo , cMoves } = Table[ i , {i,1,11} ];

(* parse PGN record *)
While[ !eof ,
    If[ (record = Read[ pgnfile , Record , RecordSeparators -> {"\n"}])==EndOfFile , eof = True ];
    If[ (!eof) && (StringTake[record,7]=="[Event ") , game[[cEvent]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,6]=="[Site ") , game[[cSite]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,6]=="[Date ") , game[[cDate]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[Round ") , game[[cRound]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[White ") , game[[cWhite]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[Black ") , game[[cBlack]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,8]=="[Result ") , game[[cResult]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,5]=="[ECO ") , game[[cECO]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,10]=="[WhiteElo ") , game[[cWhiteElo]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,10]=="[BlackElo ") , game[[cBlackElo]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,2]=="1.") , game[[cMoves]] = StringTrim[ StringSplit[ record , RegularExpression["[0-9]+[\\.]"] ] ] ];
];

(* close PGN record file *)
Close[pgnfile];

(* get list of game positions from moves *)
positions = getPositionsFromGamePGN[game[[cMoves]]];

(* print result *)
Print[ Panel[ Text[ Grid[
    {
        { Style[ "Event \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cEvent]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Site \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cSite]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Date \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cDate]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Round \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cRound]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "White \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cWhite]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Black \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cBlack]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Result \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cResult]] , Black , FontFamily->"Arial" , FontSize->12 ] }
    }
    ]] , FrameMargins -> {{30, 30}, {10, 10}}
]];

Print[game[[cMoves]]];
Print[positions];

(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* [3] Chess Graph Generation                                                     *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

A positional chess graph is fully defined by its nodes and edges. In order to 
obtain both from chess positions obtained with functions presented in [2], ChessY
provides several functions.

(1) getNodesFromPosition[ p ]

This function returns a 1-dimensional list of length 64 with all node states in 
a given chess position p. An optional argument can be used to specify the type of
state:

State
    State->"Piece" - state indicating individual type of chess pieces
    State->"Color" - state indicating color of chess pieces
    State->"Simple" - state indicating whether node is occupied or not
    Default: State->"Color"
    
The returned list is of the form

{<NODE 1 STATE>,<NODE 2 STATE>,...}

EXAMPLE:

position = positionStart;
nodes = getNodesFromPosition[position, State->"Colors"];
Print[nodes];

(2) getEdgesFromPosition[ p ]

This function returns a 2-dimensional list of all weighted edges in a given 
chess position p. Two optional arguments can be specified:

State
    specifies the edge state/weight
    State->"Color" - state indicating color of chess pieces
    State->"Simple" - state indicating whether node is occupied or not
    Default: State->"Color"

SameColorTargets
    True/False indicating whether edges are included for which both source and 
    target nodes have same color. Please note that this argument must be set to 
    False for generating valid chess graphs!
    Default: sameColorTargets->False
    
The returned list is of the form

{{<SOURCE>,<TARGET>,<WEIGHT>},{<SOURCE>,<TARGET>,<WEIGHT>},...}

holding the set of all edges indicated by source node, target node and edge
weight (state) information.

EXAMPLE:

position = positionStart;
edges = getNodesFromPosition[position, State->"Simple"];
Print[edges];

(3) getWhiteEdgesFromPosition[ p ]
    getBlackEdgesFromPosition[ p ]

The function getEdgesFromPosition[] uses these two internal functions, returning
a 2-dimensional list of all potential edges originating from white/black pieces 
(except edges from castling moves of the white/black king/rook) in a given 
chess position. 

(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* [4] Visualization                                                              *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

ChessY provides various functions for visualizing chess positions, positional chess
graphs, and whole chess games.

(1) showChessPosition[ p ]

This function returns an array plot displaying a chess position p in the a 
classical (pieces on chessboard) style.

EXAMPLE:

Print[showChessPosition[positionStart]];

(2) showPositionalChessGraph[ nodes , edges ]

This function returns chessgraph with given nodes and edges, generated with 
functions listed in [3]. For display reasons, nodes occupied by white/black 
pieces are colored in gray/black, unoccupied nodes in open circles; edges 
originating from nodes occupied by white/black.

Various optional arguments are available:

ShowNodeID
    (True/False) displays the node ID in form of numbered nodes; note that
    this option only applies if NodeLayout->"Chessboard"
    Default: ShowNodeID->False
    
GraphType
    defines which type of graph is generated
    GraphType->"Color" : graph with "Color" edge/node states
    GraphType->"Simple" : simple graph ("Simple" edge/node states)
    Default: GraphType->"Color"
    
NodeLayout
    defines the node layout of the generated graph
    NodeLayout->"Chessboard" : 8x8 node layout
    NodeLayout->"LinearV" : linear vertical node arrangement
    NodeLayout->"LinearH" : linear horizontal node arrangement
    Default: NodeLayout->"Chessboard"
    
EdgeLayout
    specifies the edge layout of the generated graph
    EdgeLayout->"Automatic" 
    EdgeLayout->"Chessboard" : height of arched edges indicates chessboard 
        distance between source and target node (NOTE: this option only applies 
        if NodeLayout->"Linear")
    Default: EdgeLayout->"Automatic"
        
ShowFrame
    True/False for showing frame with tickmarks indicating the chessboard distance 
    Default: ShowFrame->False

If NodeLayout->"Chessboard" is used, nodes are arranged in a classical 8x8
chessboard layout, with arrows indicating directed edges between source and 
target nodes. If NodeLayout->"LinearV" or "LinearH" are used, nodes are
arranged in a linear fashion (vertical or horizontal, respectively). In this
case, edges originating from nodes occupied by white/black pieces are colored
in gray/black; edges targeting nodes are drawn without arrows, as source is
indicated by the edges' color. If GraphType->"Simple" is chosen, all nodes and
edges are displayed in one color.

EXAMPLE:

position = positionStart;
nodes = getNodesFromPosition[position,State->"Color"];
edges = getEdgesFromPosition[position,State->"Color"];
Print[showPositionalChessGraph[nodes,edges,NodeLayout->"LinearV",EdgeLayout->"Chessboard",ShowFrame->True]];

(3) animateChessPositions[ moves , pos ]

This function returns a graphics displaying in an interactive fashion multiple
chess positions, e.g. that of a chess game, given a list of moves and positions.

EXAMPLE:

(* load the ChessY toolbox *)
Get["ChessY.m"];

(* open PGN record file *)
pgnfile = OpenRead["example2.pgn"];
eof = False;

(* setup list for handling data *)
game = Array[0,11];
{ cEvent , cSite , cDate , cRound , cWhite , cBlack , cResult , cECO , cWhiteElo , cBlackElo , cMoves } = Table[ i , {i,1,11} ];

(* parse PGN record *)
While[ !eof ,
    If[ (record = Read[ pgnfile , Record , RecordSeparators -> {"\n"}])==EndOfFile , eof = True ];
    If[ (!eof) && (StringTake[record,7]=="[Event ") , game[[cEvent]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,6]=="[Site ") , game[[cSite]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,6]=="[Date ") , game[[cDate]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[Round ") , game[[cRound]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[White ") , game[[cWhite]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[Black ") , game[[cBlack]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,8]=="[Result ") , game[[cResult]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,5]=="[ECO ") , game[[cECO]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,10]=="[WhiteElo ") , game[[cWhiteElo]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,10]=="[BlackElo ") , game[[cBlackElo]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,2]=="1.") , game[[cMoves]] = StringTrim[ StringSplit[ record , RegularExpression["[0-9]+[\\.]"] ] ] ];
];

(* close PGN record file *)
Close[pgnfile];

(* get list of moves and positions *)
moves = game[[cMoves]];
positions = getPositionsFromGamePGN[moves];

(* animate chess game *)
Print[animateChessPositions[moves,positions]];

(4) animatePositionalChessGraphs[ nodes , edges ]

This function returns a graphics displaying in an interactive fashion multiple
positional chess graphs, given a list of nodes and edges associated with the 
positions of a chess game. The chess graphs displayed are the same to those 
generated by showPositionalChessGraph[] (see (2)), thus the same optional
arguments apply.

EXAMPLE:

(* load the ChessY toolbox *)
Get["ChessY.m"];

(* open PGN record file *)
pgnfile = OpenRead["example2.pgn"];
eof = False;

(* setup list for handling data *)
game = Array[0,11];
{ cEvent , cSite , cDate , cRound , cWhite , cBlack , cResult , cECO , cWhiteElo , cBlackElo , cMoves } = Table[ i , {i,1,11} ];

(* parse PGN record *)
While[ !eof ,
    If[ (record = Read[ pgnfile , Record , RecordSeparators -> {"\n"}])==EndOfFile , eof = True ];
    If[ (!eof) && (StringTake[record,7]=="[Event ") , game[[cEvent]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,6]=="[Site ") , game[[cSite]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,6]=="[Date ") , game[[cDate]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[Round ") , game[[cRound]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[White ") , game[[cWhite]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[Black ") , game[[cBlack]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,8]=="[Result ") , game[[cResult]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,5]=="[ECO ") , game[[cECO]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,10]=="[WhiteElo ") , game[[cWhiteElo]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,10]=="[BlackElo ") , game[[cBlackElo]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,2]=="1.") , game[[cMoves]] = StringTrim[ StringSplit[ record , RegularExpression["[0-9]+[\\.]"] ] ] ];
];

(* close PGN record file *)
Close[pgnfile];

(* get list of positions, nodes and edges *)
positions = getPositionsFromGamePGN[game[[cMoves]]];

nodes = {};
edges = {};

For[ i=1 , i<=Length[positions] , i++ ,
    nodes = Append[ nodes , getNodesFromPosition[ positions[[i]] ] ];
    edges = Append[ edges , getEdgesFromPosition[ positions[[i]] ] ];
];

(* animate chess graph *)
Print[animatePositionalChessGraphs[nodes,edges,ShowNodeID->False,GraphType->"Color",NodeLayout->"Chessboard"]];


(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* [5] Chess Graph Analysis                                                       *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

ChessY provides a baseline set of tools for analyzing positional chess graphs.
However, with the knowledge of the data objects returned by ChessY, speficially
position, node and edge lists, this set of tools can be easily extended utilizing
the unlimited potential of Mathematica.

The functions defined in ChessY are:

(1) getNumberOfNodes[ nodes ]
    
Function returning a list containing the number of nodes of a chess graph from
a 1-dimensional list of nodes. Options are:

State
    specifies the node states to be counted
    State->"Piece" : state indicating individual type of chess pieces
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"
    
Depending on the chosen option, this function returns:

State->"Piece": {#wP,#wN,#wB,#wR,#wQ,#wK,#bK,#bQ,#bR,#bB,#bN,#bP}
State->"Color": {#white nodes,#black nodes}
State->"Simple": #occupied nodes

(2) getAdjacencyMatrix[ edges ]

Function returning the adjacency matrices a_ij of a chess graph. Options are:

State
    specifies the edge state/weight
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"

Depending on the chosen option, this function returns:

State->"Color": weighted adjacency matrix
State->"Simple": relational adjacency matrix

(3) getNumberOfEdges[ edges ]

Function returning the number of edges of a chess graph. Options are:

State
    specifies the edge state/weight
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"

Depending on the chosen option, this function returns:

State->"Color": {#edges (white source nodes),#edges (black source nodes)}
State->"Simple": #edges

(4) getConnectedness[ ne ]

Function returning the connectedness Co of a chess graph. If the argument is
a list containing the number of edges originating from white and black nodes,
the returned list contains the connectedness of the corresponding subgraphs,
otherwise the total connectedness is returned.

(5) getMobility[ nodes , edges ]

Function returning the mobility M of a chess graph. Options are:

State
    specifies the edge state/weight
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"

Depending on the chosen option, this function returns:

State->"Color" : {M of white nodes, M of black nodes}
State->"Simple" : total M

(6) getControl[ nodes , edges ]

Function returning the control C of a chess graph. Options are:

State
    specifies the edge state/weight
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"

Depending on the chosen option, this function returns:

State->"Color" : {C of white nodes, C of black nodes}
State->"Simple" : total C

(7) getDominance[ nodes , edges ]

Function returning the dominance D of a chess graph. Options are:

State
    specifies the edge state/weight
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"

Depending on the chosen option, this function returns:

State->"Color" : {D of white nodes, D of black nodes}
State->"Simple" : total D

(8) getAverageNodeReach[ nodes , edges ]

Function returning the average node reach <R> of a chess graph. Options are:

State
    specifies the edge state/weight
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"

Depending on the chosen option, this function returns:

State->"Color" : {<R> of white nodes, <R> of black nodes}
State->"Simple" : total <R>

(9) getOffensiveness[ nodes , edges ]

Function returning the offensiveness O of a chess graph. Options are:

State
    specifies the edge state/weight
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"

Depending on the chosen option, this function returns:

State->"Color" : {O of white nodes, O of black nodes}
State->"Simple" : total O

(10) getDefensiveness[ nodes , position ]

Function returning the defensiveness D associated with a chess position. 
Options are:

State
    specifies the edge state/weight
    State->"Color" : state indicating color of chess pieces
    State->"Simple" : state indicating whether node is occupied or not
    Default: State->"Color"

Depending on the chosen option, this function returns:

State->"Color" : {D of white nodes, D of black nodes}
State->"Simple" : total D

EXAMPLE:

(* generate position *)
position = positionStart;

(* get list of nodes and edges *)
nodes = getNodesFromPosition[position,State->"Color"];
edges = getEdgesFromPosition[position,State->"Color"];

(* display positional chess graph *)
Print[showPositionalChessGraph[nodes,edges,ShowNodeID->False,GraphType->"Color",NodeLayout->"Chessboard"]];

(* analyze chess graph *)
aij = getAdjacencyMatrix[edges,State->"Color"];
nn = getNumberOfNodes[nodes,State->"Color"];
ne = getNumberOfEdges[edges,State->"Color"];
co = getConnectedness[ne];
m = getMobility[nodes,edges,State->"Color"];
c = getControl[nodes,edges,State->"Color"];
do = getDominance[nodes,edges,State->"Simple"];
r = getAverageNodeReach[nodes,edges,State->"Color"];
o = getOffensiveness[nodes,edges,State->"Color"];
d = getDefensiveness[nodes,position,State->"Simple"];

(* print results *)
Print["a_ij = ",MatrixForm[aij]];
Print["N = ",nn];
Print["N_E = ",ne];
Print["Co = ",co];
Print["M = ",m];
Print["C = ",c];
Print["Do = ",do];
Print["<R> = ",r];
Print["O = ",o];
Print["D = ",d];

(* ============================================================================== *)
(* ============================================================================== *)
(*                                                                                *)
(* [6] Examples                                                                   *)
(*                                                                                *)
(* ============================================================================== *)
(* ============================================================================== *)

==============================
[6.1] Analysis of a Chess Game
==============================

(* load the ChessY toolbox *)
Get["ChessY.m"];

(* open PGN record file *)
pgnfile = OpenRead["example3.pgn"];
eof = False;

(* setup list for handling data *)
game = Array[0,11];
{ cEvent , cSite , cDate , cRound , cWhite , cBlack , cResult , cECO , cWhiteElo , cBlackElo , cMoves } = Table[ i , {i,1,11} ];

(* parse PGN record *)
While[ !eof ,
    If[ (record = Read[ pgnfile , Record , RecordSeparators -> {"\n"}])==EndOfFile , eof = True ];
    If[ (!eof) && (StringTake[record,7]=="[Event ") , game[[cEvent]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,6]=="[Site ") , game[[cSite]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,6]=="[Date ") , game[[cDate]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[Round ") , game[[cRound]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[White ") , game[[cWhite]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,7]=="[Black ") , game[[cBlack]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,8]=="[Result ") , game[[cResult]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,5]=="[ECO ") , game[[cECO]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,10]=="[WhiteElo ") , game[[cWhiteElo]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,10]=="[BlackElo ") , game[[cBlackElo]] = StringSplit[ record , "\"" ][[2]] ];
    If[ (!eof) && (StringTake[record,2]=="1.") , game[[cMoves]] = StringTrim[ StringSplit[ record , RegularExpression["[0-9]+[\\.]"] ] ] ];
];

(* close PGN record file *)
Close[pgnfile];

(* print information about game *)
Print[ Panel[ Text[ Grid[
    {
        { Style[ "Event \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cEvent]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Site \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cSite]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Date \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cDate]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Round \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cRound]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "White \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cWhite]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Black \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cBlack]] , Black , FontFamily->"Arial" , FontSize->12 ] },
        { Style[ "Result \t" , Black , Bold , FontFamily->"Arial" , FontSize->12 ] , Style[ game[[cResult]] , Black , FontFamily->"Arial" , FontSize->12 ] }
    }
    ]] , FrameMargins -> {{30, 30}, {10, 10}}
]];

(* get list of moves, positions, nodes and edges *)
moves = game[[cMoves]];
positions = getPositionsFromGamePGN[ moves ];

nodes = {};
edges = {};

For[ i=1 , i<=Length[positions] , i++ ,
    nodes = Append[ nodes , getNodesFromPosition[ positions[[i]] ] ];
    edges = Append[ edges , getEdgesFromPosition[ positions[[i]] ] ];
];

(* analyse all positions of game *)
nn = {};
ne = {};
co = {};
m = {};
c = {};
do = {};
r = {};
o = {};
d = {};

For[ i=1 , i<=Length[positions] , i++ ,
    nn = Append[ nn , getNumberOfNodes[nodes[[i]],State->"Color"] ];
    ne = Append[ ne , getNumberOfEdges[edges[[i]],State->"Color"] ];
    co = Append[ co , getConnectedness[ne[[i]]] ];
    m = Append[ m , getMobility[nodes[[i]],edges[[i]],State->"Color"] ];
    c = Append[ c , getControl[nodes[[i]],edges[[i]],State->"Color"] ];
    do = Append[ do , getDominance[nodes[[i]],edges[[i]],State->"Color"] ];
    r = Append[ r , getAverageNodeReach[nodes[[i]],edges[[i]],State->"Color"] ];
    o = Append[ o , getOffensiveness[nodes[[i]],edges[[i]],State->"Color"] ];
    d = Append[ d , getDefensiveness[nodes[[i]],positions[[i]],State->"Color"] ];
];

(* display results *)

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, nn ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "number of nodes " <> ToString[ Subscript["N", "N"] , StandardForm ],
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, ne ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "number of edges " <> ToString[ Subscript["N", "E"] , StandardForm ],
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, co ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "connectedness Co",
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, m ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "mobility M",
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, c ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "control C",
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, do ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "dominance D",
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, r ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "average node reach <R>",
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, o ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "offensiveness O",
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

Print[
    ListPlot[
        Transpose[ MapIndexed[ { {First@#2,#1[[1]]} , {First@#2,#1[[2]]} } &, d ] ],
        PlotRange -> All,
        Joined -> True,
        PlotLabel -> "defensiveness D",
        PlotLegends -> {"White","Black"},
        PlotStyle -> { GrayLevel[0.7] , Black }
    ]
];

(* ============================================================================== *)
(* ============================================================================== *)
(* ============================================================================== *)
