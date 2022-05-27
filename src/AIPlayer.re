open CS17SetupGame;
open Game;
open Player;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;

  /*Minimax! */

  let rec minimax: (PlayerGame.state, int) => float =
    (state, depth) =>
      switch (state, depth) {
      | (s, 0) => PlayerGame.estimateValue(s)
      | (s, _) =>
        switch (PlayerGame.gameStatus(s)) {
        | Draw => PlayerGame.estimateValue(s)
        | Win(_) => PlayerGame.estimateValue(s)
        | Ongoing(P1) =>
          let ns = PlayerGame.nextState(s, argMax(s, depth, minimax));
          PlayerGame.estimateValue(ns) +. minimax(ns, depth - 1);
        | Ongoing(P2) =>
          let ns = PlayerGame.nextState(s, argMin(s, depth, minimax));
          PlayerGame.estimateValue(ns) +. minimax(ns, depth - 1);
        }
      }

  /*Argmax */

  and argMaxHelper:
    (
      PlayerGame.state,
      list(PlayerGame.move),
      (float, PlayerGame.move),
      int,
      (PlayerGame.state, int) => float
    ) =>
    (float, PlayerGame.move) =
    (state, moveList, best, depth, funct) =>
      switch (moveList, best) {
      | ([], _) => best
      | ([hd], (v2, m2)) =>
        let (v1, m1) = (funct(PlayerGame.nextState(state, hd), depth), hd);
        if (v1 > v2) {
          (v1, m1);
        } else {
          (v2, m2);
        };
      | ([hd, ...tl], (v2, m2)) =>
        let (v1, m1) = (funct(PlayerGame.nextState(state, hd), depth), hd);
        let newBest =
          if (v1 > v2) {
            (v1, m1);
          } else {
            (v2, m2);
          };
        argMaxHelper(state, tl, newBest, depth, funct);
      }

  and argMax:
    (PlayerGame.state, int, (PlayerGame.state, int) => float) =>
    PlayerGame.move =
    (state, depth, funct) =>
      switch (PlayerGame.legalMoves(state)) {
      | [] => failwith("no legal moves")
      | [hd] => hd
      | [hd, ...tl] =>
        switch (
          argMaxHelper(
            state,
            tl,
            (funct(PlayerGame.nextState(state, hd), depth - 1), hd),
            depth - 1,
            funct,
          )
        ) {
        | (_, m) => m
        }
      }

  and argMin:
    (PlayerGame.state, int, (PlayerGame.state, int) => float) =>
    PlayerGame.move =
    (state, depth, funct) => {
      let fneg = (x, y) => -. funct(x, y);
      argMax(state, depth, fneg);
    };

  let depth: int = 3;

  let nextMove: PlayerGame.state => PlayerGame.move =
    state =>
      switch (PlayerGame.gameStatus(state)) {
      | Ongoing(P1) => argMax(state, depth, minimax)
      | Ongoing(P2) => argMin(state, depth, minimax)
      | _ => failwith("game over")
      };

  let playerName = "Tran";
};

module TestGame = Connect4.Connect4;
module TestAIPlayer = AIPlayer(TestGame);
open TestAIPlayer;
module MyAIPlayer: Player = TestAIPlayer;

/* insert test cases for any procedures that don't take in
 * or return a state here */
