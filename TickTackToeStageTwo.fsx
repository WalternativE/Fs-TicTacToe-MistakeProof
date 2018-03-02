// util
let product (xs : 'a list) (ys : 'b list) = [ for x in xs do for y in ys do yield x,y ]

// define simple types

type Player = O | X

type Row = Top | Center | Bottom
let rows = [ Top; Center; Bottom ] // constant

type Column = Left | Middle | Right
let columns = [ Left; Middle; Right ] // constant

type Position = Row * Column

type Winner = PlayerOWins | PlayerXWins 
type NextTurn = PlayerOTurn | PlayerXTurn

type PastMove = Player * Position
type PastMoves = PastMove list

type EndedGame = Draw | Won of Winner 

type RunningGame =
    { Turn : NextTurn
      PastMoves : PastMoves }

type MakeMove = unit -> Game // circular type is a smell, now it is 3
and MakeTurn = MakeMove list
and Game = Ended of EndedGame | Running of MakeTurn * PastMoves

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
    
    let nextRunningGame (nt : NextTurn) (pm : PastMoves) =
        { Turn = nt ; PastMoves = pm }

    let allPositions = 
        product rows columns

    let possiblePositionsAfter (pm : PastMoves) =
        let pastPositions = List.map (fun move -> snd move ) pm
        (Set.ofList allPositions) - (Set.ofList pastPositions) |> List.ofSeq

    let runningGameUsing takeTurn (game : RunningGame) (possiblePositions : Position list) = 
        let possibleMoves =
            possiblePositions
            |> List.map (fun pos -> takeTurn game pos)

        Running(possibleMoves, game.PastMoves)

    let advance takeTurn (currentMoves : PastMoves) (rg : RunningGame) =
        let nrg = nextRunningGame (nextTurn rg.Turn) currentMoves
        runningGameUsing takeTurn nrg (possiblePositionsAfter currentMoves)

    let rec takeTurn (rg : RunningGame) (p : Position) () =
        let currentMoves = (createMove rg.Turn p)::rg.PastMoves

        match (tryGetWinner currentMoves) with
        | Some w -> Ended(Won(w))
        | None ->
            advance takeTurn currentMoves rg
            // -> Ended(Draw)

    let initialGame = { Turn = PlayerXTurn; PastMoves = [] } 
    runningGameUsing takeTurn initialGame allPositions

// we cannot makeTurn on ended game
// we cannot play out of turn (solved with API)
// we cannot play occupied positions
// we cannot pass null

// now the client
match (newGame ()) with
| Ended _ -> None
| Running (mt, pm) ->  // we get the past moves, so we can draw it on the screen.
    (List.head mt) () |> Some
