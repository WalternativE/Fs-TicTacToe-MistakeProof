(*

1. Analyse domain

What are the "things" we have in a Tick-Tack-Toe 
* what is the data we deal with?
* What are the functions we need?

Data
* Player, 1, 2
* Token, O, X
* Position, Row, Column, "A3", "top/left"
* Winner, O, X
* Draw
* Field (is series of positions from a player, or empty)
* empty field
* Turn or Move
* Game itself
* ThreeInARow/Column/Diagonal (winning condition)

Idee for future expansion/advanced
* game loop with win 2 out of 3 introduces Rounds

Ideas for dojo moderation
* go through the domain with the whole group
* keep list for us
* propeare simplified list as starting points "there are some potential types... bla"

Behaviour
* start a game
* take a turn = make a move
* (take back last move)
* check if we can continue ("hasEnded" from assignment)
* check if there is a winner
* check who is winner

*)

// define simple types

type Player = O | X

type Row = Top | Center | Bottom
type Column = Left | Middle | Right
type Position = Row * Column

type Winner = Player // the winner is a player

type EndedGame = Draw | Won of Winner // an ended game is either won or draw
type Game = Running | Ended // a game is either running or ended

type Turn = Player
type Move = Player * Position // a player makes his move, starting point

// -> It is not possible to play out of turn.
// We need behaviour. Let's start with simple behaviour.

type CreateNewGame = unit -> Game
let newGame () = Running

// now we need to encode who's turn it is. need to encode
