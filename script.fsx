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

type Winner = PlayerOWins | PlayerXWins // the winner is a player (we use different names for different types)
type Turn = PlayerOTurn | PlayerXTurn

type PastMove = Player * Position
type PastMoves = PastMove list

type EndedGame = Draw | Won of Winner // an ended game is either won or draw

type GameState = WhosTurn of Turn * PastMoves

// -> It is not possible to play out of turn.
// We need behaviour. Let's start with simple behaviour.

// first try, but Game is also Ended :-(
type Game1 = Ended | Running of Turn // a game is either running or ended
type Move1 = Player * Position // a player makes his move, starting point

// now we need to encode who's turn it is.
let newGame () = Running(PlayerXTurn) // starts a new game, new game is running and it is player x's turn

let makeTurn1 (m : Move1) (g : Game1) =
    match g with
    | Running t -> ()
    | Ended -> ()

let makeTurn2 (p : Position) (g : GameState) =
    ()