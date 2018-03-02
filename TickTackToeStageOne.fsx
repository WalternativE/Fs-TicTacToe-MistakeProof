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
type NextTurn = PlayerOTurn | PlayerXTurn

type PastMove = Player * Position
type PastMoves = PastMove list

type EndedGame = Draw | Won of Winner // an ended game is either won or draw

// now we need to encode who's turn it is.
type RunningGame =
    { Turn : NextTurn
      PastMoves : PastMoves } // ugly collection of all state in the game, but nobody is using it

type MakeTurn = Position -> Game // circular type is a smell
and Game = Ended of EndedGame | Running of MakeTurn * PastMoves // a game is either running or ended

let newGame () = 
    let tryGetWinner (pm : PastMoves) : (Winner option) =
        None // fake behaviour, ignore winner

    let mapTurnToPlayer = function
        | PlayerXTurn -> X
        | PlayerOTurn -> O

    let createMove (nt : NextTurn) (p : Position) =
        mapTurnToPlayer nt, p

    let nextTurn = function
        | PlayerXTurn -> PlayerOTurn
        | PlayerOTurn -> PlayerXTurn
    
    let nextRunningGame (nt : NextTurn) (m : PastMoves) =
        { Turn = nt ; PastMoves = m }
        
    let rec takeTurn (rg : RunningGame) (p : Position) =
        let currentMoves = (createMove rg.Turn p)::rg.PastMoves

        match (tryGetWinner currentMoves) with
        | Some w -> Ended(Won(w))
        | None ->
            let nrg = nextRunningGame (nextTurn rg.Turn) currentMoves
            Running(takeTurn nrg, nrg.PastMoves)
        // -> Ended(Draw)

    // starts a new game, new game is running and it is player x's turn
    let initialGame = { Turn = PlayerXTurn; PastMoves = [] } 

    Running(takeTurn initialGame, initialGame.PastMoves) 

// we cannot makeTurn on ended game. cool.
// we cannot play out of turn (solved with API)
// we cannot use positions other than 9
// we cannot pass null. cool.
// we cannot send in another RunningGame because we do not see it

// the list of moves is maybe a problem. The evil client can reconstruct it in different way.
// can we hide the types inside RunningGame. Can we restrict creation of RunningGame?

// now the client
match (newGame ()) with
| Ended _ -> None
| Running (mt, pm) ->  // we get the past moves, so we can draw it on the screen.
    (Center, Middle) |> mt |> Some
