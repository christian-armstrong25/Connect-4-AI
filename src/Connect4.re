open CS17SetupGame;
open Game;

module Connect4 = {
  /* player 1 is P1, player 2 is P2 */
  type whichPlayer =
    | P1
    | P2;

  /*helper for nextState, returns player thats not inputted
    Input: whichPlayer, either P1 or P2
    Output: The other player than who is not inputted
     */
  let otherPlayer: whichPlayer => whichPlayer =
    player =>
      switch (player) {
      | P1 => P2
      | P2 => P1
      };

  /* given a player, returns a string representing who it is
     Input: whichPlayer, either P1 or P2
     Output: a string corresponding to P1 or P2, printed exactly
     */
  // Unused in Connect4.re, but used in Referee.re
  let stringOfPlayer: whichPlayer => string =
    p =>
      switch (p) {
      | P1 => "P1"
      | P2 => "P2"
      };

  /* Status is either a player has won, it's a draw, or it's ongoing */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  /* Type token represents the “pieces” used by players on the board,
     Thus each space is either Empty (not filled), T1 (for P1) or T2 (for P2) */

  type token =
    | Empty
    | T1
    | T2;

  /*string_of_token
      Input: a token, either Empty, T1, or T2
      Output: a string representing the token exactly

    */

  let string_of_token: token => string =
    t =>
      switch (t) {
      | Empty => "  "
      | T1 => "T1"
      | T2 => "T2"
      };

  /* Type board represents the game board, and is a list of lists of tokens.
     The inner lists of tokens are the columns, with as many tokens as rows.
     The number of list(token) within the larger list is the number of rows  */
  type board = list(list(token));

  /* Type move is Move(int), where int is the column chosen by the player
     to place their token */
  type move =
    | Move(int);

  /* Type state consists of the status of the game (either ongoing, won, or
     draw) and the board itself (thus the columns/rows and the spaces filled) */
  type state =
    | State(status, board);

  /*createEmptyRow
      Input: an int
      Output:a list of the int number of Empty tokens

    Recursion Diagrams
    1. Original Input: 5
    		Recursive Input: 4
    		Recursive Output: [Empty, Empty, Empty, Empty]
    	Overall Output is recursive output with one more Empty token.
    	Natural number recursion?
    Overall Output: [Empty, Empty, Empty, Empty, Empty]

    2. Original Input: 0
    		Recursive Input:N/A
    		Recursive Output:N/A
    	0 would have no rows, thus no Empty tokens, thus just an empty list
    Overall Output: []

    */

  let rec createEmptyRow: int => list(token) =
    columns =>
      switch (columns) {
      | 0 => []
      | x when x > 0 => [Empty, ...createEmptyRow(x - 1)]
      | _ => failwith("a row needs a positive number of columns")
      };

  /*createBoard
      Input: a tuple, an int and a list of tokens
      Output: a board (type defined above)
    Recursion Diagrams
    1. Original Input: (2, [Empty, Empty, Empty])
    		Recursive Input: 1
    		Recursive Output:[Empty, Empty, Empty]
    Overall Output: [[Empty, Empty, Empty],
                    [Empty, Empty, Empty]]

    2. Original Input: (0, [Empty, Empty])
    		Recursive Input: N/A
    		Recursive Output:N/A
    	If you have 0 (which means 0 columns), it doesn't matter how many rows
      (“length of columns”) are, they don't exist. Maybe don't even need a case?
      NOTE: don't need a case to catch the empty list of tokens, since that’s caught
      in createEmptyRow
    Overall Output: []

    3. Original Input:  (1, [Empty, Empty, Empty, Empty])
    		Recursive Input: 0
    		Recursive Output: []
    	If the int is one, the OO is just a list of the imputed list(token),
    	The RO is an empty list (which makes sense). Thus when recursion
    	Hits 0, em
    Overall Output:  [[Empty, Empty, Empty, Empty]]

    */

  let rec createBoard: (int, list(token)) => board =
    (rows, listOfColumns) =>
      switch (rows) {
      | 0 => []
      | x when x > 0 => [listOfColumns, ...createBoard(x - 1, listOfColumns)]
      | _ => failwith("a board needs a positive number of rows")
      };

  /*initialState
      Input: a string, representing the rows and columns of desired board dimensions
      Output: the initial state of the board, with the desired amount of rows and
      columns filled with Empty tokens

    */

  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let boardHeight = getBoardHeight(boardDims);
      let boardWidth = getBoardWidth(boardDims);
      let initialBoard =
        createBoard(boardHeight, createEmptyRow(boardWidth));
      if (boardHeight >= 4 && boardWidth >= 4) {
        State(Ongoing(P1), initialBoard);
      } else {
        failwith("Domain error, board must be at least 4x4");
      };
    };

  /* printing functions */

  /*stringOfPlayer
      Input: whichPlayer, either P1 or P2
      Output: a string representing the inputted player

    */
  let stringOfPlayer: whichPlayer => string =
    p =>
      switch (p) {
      | P1 => "P1"
      | P2 => "P2"
      };

  //helpers for stringOfState

  /*printIntListContents
      Input: a list of tokens (either Empty, T1, or T2)
      Output: a string representing the contents of the list
    Recursion Diagrams
    1. Original Input: [Empty, T2, T1]
    		Recursive Input: [T2, T1]
    		Recursive Output: “T2, T1”
    	OO is RO with the first item in the list concatenated as a string.
    	Use switch cases to operate just on head of list
    Overall Output: “Empty | T2 | T1”

    2. Original Input: []
    		Recursive Input: N/A
    		Recursive Output:N/A
    	If the list is empty, empty string. Will this add extra space?
    Overall Output:””

    3. Original Input: [Empty]
    		Recursive Input: N/A
    		Recursive Output: N/A
    	Lists with only one token dont need recursion, covered in switch case
    Overall Output: “Empty”

    */

  let rec printIntListContents: list(token) => string =
    aloi =>
      switch (aloi) {
      | [] => ""
      | [hd] => string_of_token(hd)
      | [hd, ...tl] =>
        string_of_token(hd) ++ " | " ++ printIntListContents(tl)
      };

  /*printIntList
      Input: a list of tokens
      Output: a string printing the contents of that list in a “list”
      (aka with brackets around the contents)

    */
  let printIntList: list(token) => string =
    aloi => "[" ++ printIntListContents(aloi) ++ "]";

  /*printBoard
    Recursion Diagrams

      1. Original Input: []
        Recursive Input: N/A
        Recursive Output: N/A
      If there is an empty board, you print nothing
      Overall Output: “”

    2. Original Input: [[Empty T1 T2 T2] [Empty Empty T1 T2] [Empty Empty T1 Empty]
                        [Empty Empty Empty Empty]]

        Recursive Input: [Empty Empty T1 T2] [Empty Empty T1 Empty]
                        [Empty Empty Empty Empty]
        Recursive Output: [[Empty Empty T1 T2]
                            [Empty Empty T1 Empty]
                            [Empty Empty Empty Empty]]
      Your OO is your RO is plus the first column. The end brackets should be
      separate, not added in each recursive call. Also, the columns should be on
       separate lines.
    Overall Output:  [Empty | T1 | T2 | T2]
                      [Empty | Empty | T1 | T2]
                      [Empty | Empty | T1 | Empty]
                      [Empty | Empty | Empty | Empty]

    3. Original Input: [[Empty T1 T2]]
        Recursive Input: N/A
        Recursive Output: N/A
      If only one column (and one row), then overall output is just the input,
      or what is created in printIntList
    Overall Output: [Empty | T1 | T2]


    */

  let rec printBoard: board => string =
    board =>
      switch (board) {
      | [] => ""
      | [hd] => printIntList(hd)
      | [hd, ...tl] => printIntList(hd) ++ "\n" ++ printBoard(tl)
      };

  /*stringOfState: Produces a string that represents the state of of the game and
    a corresponding message on the user interface
    */

  let stringOfState: state => string =
    sos =>
      switch (sos) {
      | State(Win(_), board) => printBoard(board)
      | State(Draw, board) => printBoard(board)
      | State(Ongoing(_), board) => printBoard(board)
      };

  /*stringOfMove: Produces a string of the move a player made (which column they
     placed their token in).
    */
  let stringOfMove: move => string =
    m =>
      switch (m) {
      | Move(int) => "Player placed token in column " ++ string_of_int(int)
      };

  /* Game Logic */

  /* legalMovesHelper: produces the list of legal moves at a state
     Recursion Diagrams
     1 . Original Input: [[Empty, T1, T2, Empty],
                                             [Empty, T2, T1, T2],
                                             [Empty, T1, T1, T2],
                                             [Empty, T2, T1, T2]], 1
              Recursive Input:T1, T2, Empty
              Recursive Output: [Move(4)]
            Only checking hd, aka first row. If first is empty, still recur
            on rest of list, any other Empty is added
         Overall Output: [Move(1), Move(4)]

     2 . Original Input: [[T2], [T1], [T1]], 1
              Recursive Input: N/A
              Recursive Output:N/A
            When there is only on column, no matter how many rows only possible
            move is Move(1) or none
          Overall Output: []

     3. Original Input: [[T2, T1, Empty, Empty],
                                             [Empty, T2, Empty, Empty],
                                             [Empty, T1, T1, T2],
                                             [Empty, T2, T1, T2]], 1
              Recursive Input:  T1, Empty, Empty
              Recursive Output: [Move(3), Move(4)]
            Same as first, check through the head of the board aka the first row
         Overall Output: [Move(3), Move(4)]
     */
  //written for columns while our board is organized in rows...doesnt work
  let rec legalMovesHelper: (list(token), int) => list(move) =
    (topRow, int) =>
      switch (topRow) {
      | [] => failwith("empty row does not exist")
      | [hd] =>
        if (hd == Empty) {
          [Move(int)];
        } else {
          [];
        }
      | [hd, ...tl] =>
        if (hd == Empty) {
          [Move(int), ...legalMovesHelper(tl, int + 1)];
        } else {
          legalMovesHelper(tl, int + 1);
        }
      };

  /* legalMoves intakes a state and outputs a list of all the legal moves
     availible at that state (aka which columns tokens can be placed into) */
  let legalMoves: state => list(move) =
    state =>
      switch (state) {
      | State(_, [hd, ..._]) => legalMovesHelper(hd, 1)
      | State(_, []) => failwith("empty row does not exist")
      };

  /* returns the status of the game at the given state */
  let gameStatus: state => status =
    state =>
      switch (state) {
      | State(x, _) => x
      };

  //transpose:

  //Recursion Diagrams
  //1 . Original Input [[2 5 7]
  //                    [8 1 9]
  //                    [5 6 6]]

  //         Recursive Input:[[8 1 9]
  //                          [5 6 6]]
  //         Recursive Output: [[8 5]
  //                            [1 6]
  //                            [9 6]]
  // Every item in first list becomes first for every resulting list.
  // add head of second to first and then head of third to first? stops when no
  //more lists, then recurses
  // Overall Output:[[2 8 5]
  //                 [5 1 6]
  //                 [7 9 6]]
  //
  //Original Input [[a t g]
  //                [k s t]]

  //         Recursive Input:[[k s t]]
  //         Recursive Output: [[k s t]]
  // In two list matrix, RO (tail) form the tail for each
  // new list. Rectangle matrix, # of rows and columns swtiched
  // Overall Output:[[a k]
  //                 [t s]
  //                 [g t]]

  //3 . Original Input [[a]
  //                    [b]
  //                    [d]]
  //         Recursive Input: [[b] [d]]
  //         Recursive Output:[[b][d]]
  //  if a single list, create new lists with every element of it
  // Overall Output:[[a b d]]

  let rec transpose: board => board =
    xiexie =>
      switch (xiexie) {
      | []
      | [[], ..._] => failwith("A board cannot be 0 - dimensional.")
      | [[_], ..._] => [List.flatten(xiexie)]
      | [[_, ..._], ..._] => [
          List.map(List.hd, xiexie),
          ...transpose(List.map(List.tl, xiexie)),
        ]
      };

  /* placeHolders
     Recursion Diagrams
     1 . Original Input: 5
              Recursive Input: 4
              Recursive Output: [Empty, Empty, Empty, Empty]
     OO is RO plus one more Empty token
         Overall Output:[Empty, Empty, Empty, Empty, Empty]

     2 . Original Input: 0
              Recursive Input:N/A
              Recursive Output: N/A
     When 0, add no Empty placeholders to board
          Overall Output: []

     3 . Original Input: 1
              Recursive Input: 0
              Recursive Output: []
         When only one Empty, it still works, because it'll recur to 0 and add []
          Overall Output: [Empty]
     */

  let rec placeHolders: int => list('a) =
    int =>
      switch (int) {
      | 0 => []
      | _ => [Empty, ...placeHolders(int - 1)]
      };

  /* diagonalTransposeHelper
     Recursion Diagrams
     1 . Original Input: [[Empty T1 T2 T2]
                         [Empty Empty T1 T2]
                         [Empty T1 T1 T2]], 2, 0
              Recursive Input: [Empty Empty T1 T2]
                               [Empty T1 T1 T2]], 1, 1
              Recursive Output: [Empty Empty Empty T1 T2 Empty]
                                [Empty Empty Empty T1 T1 T2]
         for RO, n goes down 1 k goes up... cons versus append as you move through
         Overall Output: [[Empty T1 T2 T2 Empty Empty]
                         [Empty Empty Empty T1 T2 Empty]
                         [Empty Empty Empty T1 T1 T2]]

     2 . Original Input: [[Empty T1 T1]], 1, 0
              Recursive Input:N/A
              Recursive Output: N/A
             If one row, doesnt really work...create new base case? Need to have
             each item in own list, not adding any empties
          Overall Output: [[Empty] [T1] [T1]]

     3. Original Input : [], 0, 0
              Recursive Input: N/A
              Recursive Output: N/A
              If board empty, no new board created
         Overall Output: []
     */

  let rec diagonalTransposeHelper: (board, list('a), list('a)) => board =
    (board, n, k) =>
      switch (board, n, k) {
      | ([], _, _) => []
      | ([hd], n, k) => [k @ hd @ n]
      | ([hd, ...tl], n, k) => [
          k @ hd @ n,
          ...diagonalTransposeHelper(tl, List.tl(n), [Empty, ...k]),
        ]
      };

  /*diagonalTranspose intakes a board and outputs the diagonal transposed version
   */
  let diagonalTranspose: board => board =
    board =>
      transpose(
        diagonalTransposeHelper(
          board,
          placeHolders(List.length(board) - 1),
          [],
        ),
      );

  /*produces the vertical flip of a board */
  let horzFlip: board => board =
    board =>
      switch (board) {
      | [] => failwith("matrix cant be empty")
      | [_, ..._] => List.rev(board)
      };

  /*tokenOfPlayer intakes the player and outputs their respective token type*/
  let tokenOfPlayer: whichPlayer => token =
    p =>
      switch (p) {
      | P1 => T1
      | P2 => T2
      };

  /* updateColumn
     Recursion Diagrams
     1 . Original Input: [T2 T1 T2], T2
              Recursive Input: N/A
              Recursive Output: N/A
              No need to recur on a list of just player tokens, just add on top
              (this is assuming it is a legal move)
         Overall Output: [T2 T2 T1 T2]

     2 . Original Input [], T1
              Recursive Input: N/A
              Recursive Output: N/A
             If the list is empty, add the token, no recursion
          Overall Output: [T1]

     3. Original Input: [Empty T2 T1], T1
              Recursive Input: [T2 T1]
              Recursive Output: [T1 T2 T1]
           OO is RO plus the token...only case w recursion, but then a "new" row will
           be created thanks to the nonrecursive base cases
         Overall Output:[T1 T2 T1]
     */

  let rec updateColumn: (list(token), token) => list(token) =
    (board, token) =>
      switch (board) {
      | [] => [token]
      | [Empty] => [token]
      | [Empty, T1] => [token, T1]
      | [Empty, T2] => [token, T2]
      | [Empty, T1, ...tl] => [token, T1, ...tl]
      | [Empty, T2, ...tl] => [token, T2, ...tl]
      | [Empty, ...tl] => [Empty, ...updateColumn(tl, token)]
      | _ => failwith("illegal move")
      };

  /* updateBoardHelper
     Recursion Diagrams
     1 . Original Input :[[Empty T1 T2 T2]
                         [Empty Empty T1 T2]
                         [Empty T1 T1 T2]], P1, 2
              Recursive Input:[Empty Empty T1 T2]
                              [Empty T1 T1 T2]], P1, 1
              Recursive Output: [Empty T1 T1 T2]
                                [Empty T1 T1 T2]
             If the given move/location of token is not 1, have to recur to find
             correct row. Thus OO is just the edited RO appended to unedited first
         Overall Output:[[Empty T1 T2 T2]
                         [Empty T1 T1 T2]
                         [Empty T1 T1 T2]]

     2 . Original Input:[[Empty T1 T2 T2]
                         [Empty Empty T1 T2]
                         [Empty T1 T1 T2]], P2, 1
              Recursive Input: N/a
              Recursive Output: N/a
              If the player is placing their token in the first list, no need to
              recur, helper handles it and updates row, rest of board same

          Overall Output:[[T2 T1 T2 T2]
                         [Empty Empty T1 T2]
                         [Empty T1 T1 T2]]

     3. Original Input: [], P1, 1
              Recursive Input: N/A
              Recursive Output: N/A
              Empty list has no spaces, thus cant have any moves
         Overall Output: failwith
     */

  let rec updateBoardHelper: (board, whichPlayer, int) => board =
    (board, p, n) =>
      switch (board, n) {
      | ([hd, ...tl], 1) => [updateColumn(hd, tokenOfPlayer(p)), ...tl]
      | ([hd, ...tl], n) => [hd, ...updateBoardHelper(tl, p, n - 1)]
      | ([], _) => failwith("illegal move")
      };

  /* updateBoard: updates the board with the legal move
   */
  let updateBoard: (board, whichPlayer, move) => board =
    (board, p, m) =>
      switch (board, p, m) {
      | (board, whichPlayer, Move(n)) =>
        transpose(updateBoardHelper(transpose(board), whichPlayer, n))
      };

  /* checkForDraw: checks board for draw
   */
  let checkForDraw: board => bool =
    b =>
      switch (b) {
      | [hd, ..._] => !List.mem(Empty, hd)
      | [] => false
      };

  /* checkFourWinHelper
     Recursion Diagrams
     1 . Original Input : [T1 T2]
              Recursive Input:N/a
              Recursive Output: N/A
              Any list with three or less tokens is automatically false, cant have
              four of a kind. No recursion here, base cases
         Overall Output:false

     2 . Original Input : [T2 T2 T2 T2 T1]
              Recursive Input: N/A
              Recursive Output: N/A
              Check the first four tokens in a list, if they're from the same player
              automatically true
          Overall Output:true

     3. Original Input : [T1 T2 T2 T2 T2]
              Recursive Input:[T2 T2 T2 T2]
              Recursive Output: true
              If first four dont match, thats when we get into recursion, check the
              rest of list, if RO is ever true then overall true
         Overall Output:true
     */
  let rec checkFourWinHelper: list(token) => bool =
    b =>
      switch (b) {
      | [] => false
      | [_] => false
      | [_, _] => false
      | [_, _, _] => false
      | [T1, T1, T1, T1, ..._] => true
      | [T2, T2, T2, T2, ..._] => true
      | [_, ...tl] => checkFourWinHelper(tl)
      };

  /* checkFourWin: Checks for four in a row in rows, columns and diagonals
     Recursion Diagrams
     1 . Original Input: [[T2 T2 T2 T2]
                         [Empty Empty T1 T2]
                         [Empty T1 T1 T2]]
              Recursive Input: N/a
              Recursive Output: N/a
              check via helper, if pattern match in first row no recursion
         Overall Output: true

     2 . Original Input:[[Empty T1 T2 T2]
                         [Empty Empty T1 T2]
                         [Empty T1 T1 T2]
                         [T1 T1 T1 T1]]
              Recursive Input: [Empty Empty T1 T2]
                               [Empty T1 T1 T2]
                               [T1 T1 T1 T1]]
              Recursive Output: true
               If helper doesnt catch in first row, call recursively and
              apply to rest of rows, continuing until you either get to empty or
              four. if reach end of every row and no matches, needs to be false
          Overall Output:true

     3. Original Input: []
              Recursive Input: n/a
              Recursive Output: n/a
              cant have an empty board
         Overall Output: failwith
     */

  let rec checkFourWin: board => bool =
    b =>
      switch (b) {
      | [hd, ...tl] =>
        switch (hd) {
        | [] => false
        | _ => checkFourWinHelper(hd) || checkFourWin(tl)
        }
      | [] => false
      };

  /* checkFour: checks for draw and win*/
  let checkFour: (board, whichPlayer) => state =
    (b, p) =>
      if (checkForDraw(b)) {
        State(Draw, b);
      } else if (checkFourWin(b)) {
        State(Win(p), b);
      } else if (checkFourWin(transpose(b))) {
        State(Win(p), b);
      } else if (checkFourWin(diagonalTranspose(b))) {
        State(Win(p), b);
      } else if (checkFourWin(diagonalTranspose(horzFlip(b)))) {
        State(Win(p), b);
      } else {
        State(Ongoing(otherPlayer(p)), b);
      };

  /* given a state and a legal move, yields the next state */
  //update player status as well along with board
  let nextState: (state, move) => state =
    (state, m) =>
      switch (state) {
      | State(Ongoing(p), x) =>
        let updatedBoard = updateBoard(x, p, m);
        checkFour(updatedBoard, p);
      | State(Draw | Win(_), _) => failwith("Game already over!")
      };

  /* for transforming human player input into internal representation of move */
  let moveOfString: (string, state) => move =
    (string, state) =>
      if (List.mem(Move(int_of_string(string)), legalMoves(state))) {
        Move(int_of_string(string));
      } else {
        failwith(
          "This is not a legal move, try
                                  something else.",
        );
      };

  /*estimateRow estimates the value of a row
      /*estimateRow estimates the value of a row
     Recursion Diagrams
     1. Original Input: [T1 T2 T1 T1]
           Recursive Input: [T2 T1 T1]
           Recursive Output:9.0
           Pattern matches one in a row for first, gives 1 point. Then called
           recursively, spots one T2 and subtracts 1, then spots the two T1s next
           to each other and gives 10 points
       Overall Output:10.0
      2. Original Input: [Empty Empty Empty Empty]
           Recursive Input: N/a
           Recursive Output:N/a
           If four empty, no recursion...the value is 0 because this board does not
           benefit or harm either player
       Overall Output:0.0
      3. Original Input: [T2 T2 T2 T1]
           Recursive Input: N/A
           Recursive Output:N/A
           If three in a row, no need to be recursive. Since it would be caught
           by the four in a row case
       Overall Output:-100.0
       */
    */
  let rec estimateRow: list(token) => float =
    token =>
      switch (token) {
      | [Empty, Empty, Empty, Empty] => 0.0
      | [T1, T1, T1, T1] => 1000000.0
      | [T2, T2, T2, T2] => (-1000000.0)
      | [Empty, T1, T1, T1, Empty] => 900000.0
      | [Empty, T2, T2, T2, Empty] => (-900000.0)
      | [Empty, T1, T1, Empty] => 90000.0
      | [Empty, T2, T2, Empty] => (-90000.0)
      | [T2, T2, T2, T1]
      | [T2, T2, T1, T2]
      | [T2, T1, T2, T2]
      | [T1, T2, T2, T2] => 10000.0
      | [T1, T1, T1, T2]
      | [T1, T1, T2, T1]
      | [T1, T2, T1, T1]
      | [T2, T1, T1, T1] => (-10000.0)
      | [Empty, Empty, Empty] => 0.0
      | [T1, T1, T1] => 100.0
      | [T2, T2, T2] => (-100.0)
      | [Empty, Empty] => 0.0
      | [T1, T1] => 10.0
      | [T2, T2] => (-10.0)
      | [Empty] => 0.0
      | [T1] => 1.0
      | [T2] => (-1.0)
      | [] => 0.0
      | [Empty, T1, T1, T1, Empty, ...tl] => 900000.0 +. estimateRow(tl)
      | [Empty, T2, T2, T2, Empty, ...tl] => (-900000.0) +. estimateRow(tl)
      | [Empty, T1, T1, Empty, ...tl] => 90000.0 +. estimateRow(tl)
      | [Empty, T2, T2, Empty, ...tl] => (-90000.0) +. estimateRow(tl)
      | [Empty, Empty, Empty, Empty, ...tl] => 0.0 +. estimateRow(tl)
      | [T2, T2, T2, T1, ...tl]
      | [T2, T2, T1, T2, ...tl]
      | [T2, T1, T2, T2, ...tl]
      | [T1, T2, T2, T2, ...tl] => 5000.0 +. estimateRow(tl)
      | [T1, T1, T1, T2, ...tl]
      | [T1, T1, T2, T1, ...tl]
      | [T1, T2, T1, T1, ...tl]
      | [T2, T1, T1, T1, ...tl] => (-5000.0) +. estimateRow(tl)
      | [T1, T1, T1, T1, ...tl] => 1000000.0 +. estimateRow(tl)
      | [T2, T2, T2, T2, ...tl] => (-1000000.0) +. estimateRow(tl)
      | [Empty, Empty, Empty, ...tl] => 0.0 +. estimateRow(tl)
      | [T1, T1, T1, ...tl] => 100.0 +. estimateRow(tl)
      | [T2, T2, T2, ...tl] => (-100.0) +. estimateRow(tl)
      | [Empty, Empty, ...tl] => 0.0 +. estimateRow(tl)
      | [T1, T1, ...tl] => 10.0 +. estimateRow(tl)
      | [T2, T2, ...tl] => (-10.0) +. estimateRow(tl)
      | [Empty, ...tl] => 0.0 +. estimateRow(tl)
      | [T1, ...tl] => 1.0 +. estimateRow(tl)
      | [T2, ...tl] => (-1.0) +. estimateRow(tl)
      };

  /*estimateBoards produces the value of a regular, transposed, and diagonally
        transposed board using estimateRow
        /*estimateBoards produces the value of a regular, transposed, and diagonally
       transposed board using estimateRow
       Recursion Diagrams
       1 . Original Input: [[Empty, T1, T2, Empty],
                            [Empty, T2, T1, T2],
                            [Empty, T1, T1, T2],
                            [Empty, T2, T1, T2]])
                 Recursive Input:[Empty, T2, T1, T2],
                                 [Empty, T1, T1, T2],
                                 [Empty, T2, T1, T2]]
                 Recursive Output: -1.0 + 1.0 - 1.0 = -1.0
                 Interesting case, here the Ro and the OO are the same, simply bc
                 the first row (hd) had a value of 0...
            Overall Output: -1.0
       2. Original Input: [[Empty, T1, T2, T2],
                            [Empty, T2, T1, T2],
                            [Empty, T1, T1, T1],
                            [Empty, T2, T1, T1]])
                 Recursive Input:[Empty, T2, T1, T2],
                                 [Empty, T1, T1, T1],
                                 [Empty, T2, T1, T1]]
                 Recursive Output: 9.0 + 100.0 - 1.0 = 108.0
             OO is RO plus float value of first row
            Overall Output: 107.0*/
    */
  let rec estimateBoard: board => float =
    board =>
      switch (board) {
      | [] => 0.0
      | [hd] => estimateRow(hd)
      | [hd, ...tl] => estimateRow(hd) +. estimateBoard(tl)
      };

  /* estimates the value of a given state (static evaluation) */
  let estimateValue: state => float =
    instate =>
      switch (instate) {
      | State(Ongoing(_), b) =>
        estimateBoard(b)
        +. estimateBoard(transpose(b))
        +. estimateBoard(diagonalTranspose(b))
        +. estimateBoard(diagonalTranspose(horzFlip(b)))
      | State(Win(P1), _) => 1000000.0
      | State(Win(P2), _) => (-1000000.0)
      | State(Draw, _) => 0.0
      };
};
open Connect4;
module MyGame: Game = Connect4;

//};

/* test cases */

// otherPlayer
checkExpect(otherPlayer(P1), P2, "otherPlater P1");
checkExpect(otherPlayer(P2), P1, "otherPlater P2");

// stringOfPlayer
checkExpect(stringOfPlayer(P1), "P1", "stringOfPlayer P1");
checkExpect(stringOfPlayer(P2), "P2", "stringOfPlayer P2");

// string_of_token
checkExpect(string_of_token(T1), "T1", "string_of_token T1");
checkExpect(string_of_token(T2), "T2", "string_of_token T2");

// createEmptyRow
checkExpect(createEmptyRow(3), [Empty, Empty, Empty], "createEmptyRow 3");
checkExpect(createEmptyRow(0), [], "createEmptyRow 0");
checkExpect(createEmptyRow(1), [Empty], "createEmptyRow 1");
checkError(
  () => createEmptyRow(-4),
  "a row needs a positive number of columns",
);

// createBoard
checkExpect(
  createBoard(3, [Empty, Empty, Empty]),
  [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]],
  "createBoard 3 x 3",
);
checkExpect(
  createBoard(2, [Empty, Empty, Empty]),
  [[Empty, Empty, Empty], [Empty, Empty, Empty]],
  "createBoard 2 x 3",
);
checkExpect(createBoard(0, [Empty, Empty]), [], "createBoard empty");
checkError(
  () => createBoard(-4, [Empty, Empty]),
  "a board needs a positive number of rows",
);

//initialState
checkExpect(
  initialState("4 4"),
  State(
    Ongoing(P1),
    [
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
    ],
  ),
  "initialState 4x4",
);
checkExpect(
  initialState("5 4"),
  State(
    Ongoing(P1),
    [
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
    ],
  ),
  "initialState 5x4",
);

checkError(
  () => initialState("3 4"),
  "Domain error, board must be at least 4x4",
);

//printIntListContents
checkExpect(
  printIntListContents([Empty, T2, T1]),
  "   | T2 | T1",
  "printIntListContents",
);

checkExpect(printIntListContents([]), "", "printIntListContents empty");

//printIntList
checkExpect(printIntList([Empty, T2, T1]), "[   | T2 | T1]", "printIntList");

//printBoard
checkExpect(
  printBoard([
    [Empty, T1, T2, T2],
    [Empty, Empty, T1, Empty],
    [Empty, Empty, T1, Empty],
    [Empty, Empty, T2, Empty],
  ]),
  "[   | T1 | T2 | T2]\n[   |    | T1 |   ]\n[   |    | T1 |   ]\n[   |    | T2 |   ]",
  "printBoard",
);

checkExpect(printBoard([]), "", "emptyBoard");
checkExpect(printBoard([[Empty, T1, T2]]), "[   | T1 | T2]", "printBoard");

//stringOfState
checkExpect(
  stringOfState(
    State(
      Ongoing(P1),
      [
        [Empty, T1, T2, T2],
        [Empty, Empty, T1, T2],
        [Empty, Empty, T1, T2],
        [Empty, Empty, T2, T2],
      ],
    ),
  ),
  "[   | T1 | T2 | T2]\n[   |    | T1 | T2]\n[   |    | T1 | T2]\n[   |    | T2 | T2]",
  "stringOfState ongoing",
);
checkExpect(
  stringOfState(
    State(
      Win(P1),
      [
        [Empty, T1, T1, T2],
        [Empty, Empty, T1, T2],
        [Empty, Empty, T1, Empty],
        [Empty, Empty, T1, Empty],
      ],
    ),
  ),
  "[   | T1 | T1 | T2]\n[   |    | T1 | T2]\n[   |    | T1 |   ]\n[   |    | T1 |   ]",
  "stringOfState win",
);

//stringOfMove
checkExpect(
  stringOfMove(Move(4)),
  "Player placed token in column 4",
  "stringOfMove",
);
checkExpect(
  stringOfMove(Move(1)),
  "Player placed token in column 1",
  "stringOfMove",
);

//legalMoves
checkExpect(
  legalMoves(
    State(
      Ongoing(P1),
      [
        [Empty, T1, T2, Empty],
        [Empty, T2, T1, T2],
        [Empty, T1, T1, T2],
        [Empty, T2, T1, T2],
      ],
    ),
  ),
  [Move(1), Move(4)],
  "legalMoves",
);
checkExpect(
  legalMoves(State(Ongoing(P1), [[T2], [T1], [T2]])),
  [],
  "legalMovesNone",
);
checkExpect(
  legalMoves(
    State(
      Ongoing(P1),
      [
        [T2, T1, T2, Empty],
        [Empty, T2, T1, T2],
        [Empty, T1, T1, T2],
        [Empty, T2, T1, T2],
      ],
    ),
  ),
  [Move(4)],
  "legalMoves",
);

checkError(
  () => legalMoves(State(Ongoing(P1), [])),
  "empty row does not exist",
);

//legalMovesHelper
checkExpect(
  legalMovesHelper([Empty, T1, T2, Empty], 1),
  [Move(1), Move(4)],
  "legalMovesHelper",
);

checkExpect(legalMovesHelper([T2], 1), [], "legalMovesNone");

//gameStatus
checkExpect(
  gameStatus(
    State(
      Ongoing(P1),
      [
        [Empty, T1, T2, Empty],
        [Empty, T2, T1, T2],
        [Empty, T1, T1, T2],
        [Empty, T2, T1, T2],
      ],
    ),
  ),
  Ongoing(P1),
  "gameStatus Ongoing",
);

checkExpect(
  gameStatus(
    State(
      Win(P1),
      [
        [Empty, T1, T1, Empty],
        [Empty, T2, T1, T2],
        [Empty, T1, T1, T2],
        [Empty, T2, T1, T2],
      ],
    ),
  ),
  Win(P1),
  "gameStatus win",
);

checkExpect(
  gameStatus(
    State(
      Draw,
      [
        [T2, T1, T2, T1],
        [T1, T2, T1, T2],
        [T2, T1, T1, T2],
        [T2, T2, T1, T2],
      ],
    ),
  ),
  Draw,
  "gameStatus draw",
);

//estimateRow
checkExpect(
  estimateRow([Empty, Empty, Empty, Empty]),
  0.0,
  "estimateRow 4 E",
);
checkExpect(estimateRow([T1, T1, T1, T1]), 1000000.0, "estimateRow 4 T1");
checkExpect(estimateRow([T2, T2, T2, T2]), -1000000.0, "estimateRow 4 T2");
checkExpect(
  estimateRow([T2, T2, T2, T1]),
  10000.0,
  "estimateRow 3 T2 block",
);
checkExpect(
  estimateRow([T2, T1, T2, T2]),
  10000.0,
  "estimateRow 3 T2 block",
);
checkExpect(
  estimateRow([T1, T2, T1, T1]),
  -10000.0,
  "estimateRow 3 T1 block",
);
checkExpect(
  estimateRow([T1, T1, T1, T2]),
  -10000.0,
  "estimateRow 3 T1 block",
);
checkExpect(estimateRow([Empty, Empty, Empty]), 0.0, "3 empty");
checkExpect(estimateRow([T1, T1, T1]), 100.0, "3 T1");
checkExpect(estimateRow([T2, T2, T2]), -100.0, "3 T2");
checkExpect(estimateRow([Empty, Empty]), 0.0, "2 empty");
checkExpect(estimateRow([T1, T1]), 10.0, "2 T1");
checkExpect(estimateRow([T2, T2]), -10.0, "2 empty");
checkExpect(estimateRow([Empty]), 0.0, "1 empty");
checkExpect(estimateRow([T1]), 1.0, "1 T1");
checkExpect(estimateRow([T2]), -1.0, "1 T2");
checkExpect(estimateRow([]), 0.0, "empty value");
checkExpect(
  estimateRow([Empty, Empty, Empty, Empty, T1]),
  1.0,
  "4 empty1 T1",
);
checkExpect(
  estimateRow([T2, T2, T2, T1, T1]),
  5001.0,
  "estimateRow 3 T2 block 1 T1",
);
checkExpect(
  estimateRow([T1, T2, T1, T1, T1]),
  -4999.0,
  "estimateRow 3 T1 block 1 T1",
);
checkExpect(
  estimateRow([T1, T1, T1, T1, T1]),
  1000001.0,
  "estimateRow 4 T1 and 1 T1",
);
checkExpect(
  estimateRow([T2, T2, T2, T2, T1]),
  -999999.0,
  "estimateRow 4 T2 and 1 T1",
);
checkExpect(
  estimateRow([Empty, Empty, Empty, T2]),
  -1.0,
  "estimateRow 3 E 1 T2",
);
checkExpect(
  estimateRow([T1, T1, T1, T2]),
  -10000.0,
  "estimateRow 3 T1 space 1 T1",
);
checkExpect(
  estimateRow([T2, T2, T2, T1]),
  10000.0,
  "estimateRow 3 T2 space 1 T1",
);
checkExpect(estimateRow([Empty, Empty, T1]), 1.0, "2 Empty space 1 T1");
checkExpect(estimateRow([T1, T1, T1]), 100.0, "2 T1 space 1 T1");
checkExpect(estimateRow([T2, T2, T1]), -9.0, "2 T2, space 1 T1");
checkExpect(estimateRow([Empty, T2]), -1.0, "Empty space 1 T2");
checkExpect(estimateRow([T1, T1]), 10.0, "estimateRow T1 space T1");
checkExpect(estimateRow([T2, T1]), 0.0, "estimateRow T2 space T1");

// transpose
checkExpectListListAlpha(
  transpose([
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
  ]),
  [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]],
  "transpose empty 3 x 3",
);
checkExpectListListAlpha(
  transpose([[T1, T1, T1], [T1, T1, T1], [T1, T1, T1]]),
  [[T1, T1, T1], [T1, T1, T1], [T1, T1, T1]],
  "transpose T1 3 x 3",
);
checkExpectListListAlpha(
  transpose([[T2, T2, T2], [T2, T2, T2], [T2, T2, T2]]),
  [[T2, T2, T2], [T2, T2, T2], [T2, T2, T2]],
  "transpose T2 3 x 3",
);
checkExpectListListAlpha(
  transpose([[Empty, T2, T1, Empty], [T1, T2, Empty, Empty]]),
  [[Empty, T1], [T2, T2], [T1, Empty], [Empty, Empty]],
  "transpose 2 x 4",
);
checkExpectListListAlpha(
  transpose([[T1, T1], [T2, T2]]),
  [[T1, T2], [T1, T2]],
  "transpose t1 t2 2 x 2",
);
checkExpectListListAlpha(
  transpose([[Empty, Empty]]),
  [[Empty], [Empty]],
  "transpose single row into single column",
);
checkExpectListListAlpha(
  transpose([[T1], [T2]]),
  [[T1, T2]],
  "transpose single column into single row",
);

// placeHolders
checkExpect(placeHolders(0), [], "placeHolders Empty");
checkExpect(placeHolders(1), [Empty], "placeHolders 1");
checkExpect(placeHolders(2), [Empty, Empty], "placeHolders 2");

// diagonalTransposeHelper
checkExpect(
  diagonalTransposeHelper([[T1]], [], []),
  [[T1]],
  "diagonalTransposeHelper on 1 x 1",
);
checkExpect(
  diagonalTransposeHelper([[T1, Empty, T2]], [], []),
  [[T1, Empty, T2]],
  "diagonalTransposeHelper on 1 x 3",
);
checkExpect(
  diagonalTransposeHelper([[T1, T2], [Empty, Empty]], [Empty], []),
  [[T1, T2, Empty], [Empty, Empty, Empty]],
  "diagonalTransposeHelper on 2 x 2",
);
checkExpect(
  diagonalTransposeHelper(
    [[T1, T2], [Empty, Empty], [T2, Empty]],
    [Empty, Empty],
    [],
  ),
  [
    [T1, T2, Empty, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Empty, T2, Empty],
  ],
  "diagonalTransposeHelper on 3 x 2",
);

// diagonalTranspose
checkExpect(
  diagonalTranspose([[T1]]),
  [[T1]],
  "diagonalTranspose on 1 x 1",
);
checkExpect(
  diagonalTranspose([[T1, Empty, T2]]),
  [[T1], [Empty], [T2]],
  "diagonalTranspose on 1 x 3",
);
checkExpect(
  diagonalTranspose([[T1, T2], [Empty, Empty]]),
  [[T1, Empty], [T2, Empty], [Empty, Empty]],
  "diagonalTranspose on 2 x 2",
);
checkExpect(
  diagonalTranspose([[T1, T2], [Empty, Empty], [T2, Empty]]),
  [
    [T1, Empty, Empty],
    [T2, Empty, Empty],
    [Empty, Empty, T2],
    [Empty, Empty, Empty],
  ],
  "diagonalTranspose on 3 x 2",
);

checkExpect(
  diagonalTranspose([
    [Empty, T1, T2, Empty],
    [Empty, T2, T1, T2],
    [Empty, T1, T1, T2],
    [Empty, T2, T1, T2],
  ]),
  [
    [Empty, Empty, Empty, Empty],
    [T1, Empty, Empty, Empty],
    [T2, T2, Empty, Empty],
    [Empty, T1, T1, Empty],
    [Empty, T2, T1, T2],
    [Empty, Empty, T2, T1],
    [Empty, Empty, Empty, T2],
  ],
  "check diagonalTranspose",
);

// tokenOFPlayer
checkExpect(tokenOfPlayer(P1), T1, "p1 to t1");
checkExpect(tokenOfPlayer(P2), T2, "p2 to t2");

// updateRow
checkExpect(updateColumn([Empty], T2), [T2], "update Column, empty");
checkExpect(
  updateColumn([Empty, T1], T2),
  [T2, T1],
  "update Column, empty t1",
);
checkExpect(updateColumn([Empty], T1), [T1], "update Column, empty");
checkExpect(
  updateColumn([Empty, Empty], T1),
  [Empty, T1],
  "update Column, empty empty",
);
checkExpect(
  updateColumn([Empty, T1, T2], T1),
  [T1, T1, T2],
  "update Column, empty t1 t2",
);
