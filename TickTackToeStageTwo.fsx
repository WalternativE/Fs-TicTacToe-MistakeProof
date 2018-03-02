// util
let product (xs : 'a list) (ys : 'b list) = [ for x in xs do for y in ys do yield x,y ]

// define simple types

type Player = O | X

type Row = Top | Center | Bottom
let rows = [ Top; Center; Bottom ] // constant

type Column = Left | Middle | Right
let columns = [ Left; Middle; Right ] // constant

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

type MakeMove = unit -> Game // circular type is a smell
and MakeTurn = MakeMove list
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
        
    let getPossiblePositions (m : PastMoves) =
        let pastPositions = List.map (fun move -> snd move ) m
        let allPositions = product rows columns

        (Set.ofList allPositions) - (Set.ofList pastPositions)
        |> List.ofSeq

    let rec takeTurn (rg : RunningGame) (p : Position) () =
        let currentMoves = (createMove rg.Turn p)::rg.PastMoves

        match (tryGetWinner currentMoves) with
        | Some w -> Ended(Won(w))
        | None ->
            let nrg = nextRunningGame (nextTurn rg.Turn) currentMoves
            let possibleMoves =
                getPossiblePositions currentMoves
                |> List.map (fun pos -> takeTurn nrg pos)
            Running( possibleMoves, nrg.PastMoves)
        // -> Ended(Draw)

    // starts a new game, new game is running and it is player x's turn
    let initialGame = { Turn = PlayerXTurn; PastMoves = [] } 
    let initialPossibleMoves =
        product rows columns
        |> List.map (fun pos -> takeTurn initialGame pos)

    Running(initialPossibleMoves, initialGame.PastMoves)

// we cannot makeTurn on ended game
// we cannot play out of turn (solved with API)
// we cannot play occupied positions
// we cannot pass null

// now the client
match (newGame ()) with
| Ended _ -> None
| Running (mt, pm) ->  // we get the past moves, so we can draw it on the screen.
    (List.head mt) () |> Some
