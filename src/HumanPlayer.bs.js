// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Connect4$Game = require("./Connect4.bs.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function HumanPlayer(MyGame) {
  var getInputJSLine = (function() {
        const readlineSync = require('readline-sync');
        const rl = readlineSync;
        var ans = rl.question('What move do you want to make? ');
        return ans;
    });
  var nextMove = function (s) {
    while(true) {
      var input = Curry._1(getInputJSLine, undefined);
      if (input === "exit") {
        return Pervasives.failwith("Exiting Game REPL");
      }
      try {
        return Curry._2(MyGame.moveOfString, input, s);
      }
      catch (raw_msg){
        var msg = Caml_js_exceptions.internalToOCamlException(raw_msg);
        if (msg.RE_EXN_ID !== "Failure") {
          return Pervasives.failwith("Unexpected Error, Closing Game REPL");
        }
        console.log(msg._1);
        continue ;
      }
    };
  };
  return {
          PlayerGame: MyGame,
          getInputJSLine: getInputJSLine,
          nextMove: nextMove,
          playerName: "Alex"
        };
}

var MyGame_stringOfPlayer = Connect4$Game.Connect4.stringOfPlayer;

var MyGame_stringOfState = Connect4$Game.Connect4.stringOfState;

var MyGame_stringOfMove = Connect4$Game.Connect4.stringOfMove;

var MyGame_initialState = Connect4$Game.Connect4.initialState;

var MyGame_legalMoves = Connect4$Game.Connect4.legalMoves;

var MyGame_gameStatus = Connect4$Game.Connect4.gameStatus;

var MyGame_nextState = Connect4$Game.Connect4.nextState;

var MyGame_moveOfString = Connect4$Game.Connect4.moveOfString;

var MyGame_estimateValue = Connect4$Game.Connect4.estimateValue;

var MyGame = {
  stringOfPlayer: MyGame_stringOfPlayer,
  stringOfState: MyGame_stringOfState,
  stringOfMove: MyGame_stringOfMove,
  initialState: MyGame_initialState,
  legalMoves: MyGame_legalMoves,
  gameStatus: MyGame_gameStatus,
  nextState: MyGame_nextState,
  moveOfString: MyGame_moveOfString,
  estimateValue: MyGame_estimateValue
};

var getInputJSLine = (function() {
        const readlineSync = require('readline-sync');
        const rl = readlineSync;
        var ans = rl.question('What move do you want to make? ');
        return ans;
    });

function nextMove(s) {
  while(true) {
    var input = Curry._1(getInputJSLine, undefined);
    if (input === "exit") {
      return Pervasives.failwith("Exiting Game REPL");
    }
    try {
      return Curry._2(Connect4$Game.Connect4.moveOfString, input, s);
    }
    catch (raw_msg){
      var msg = Caml_js_exceptions.internalToOCamlException(raw_msg);
      if (msg.RE_EXN_ID !== "Failure") {
        return Pervasives.failwith("Unexpected Error, Closing Game REPL");
      }
      console.log(msg._1);
      continue ;
    }
  };
}

var playerName = "Alex";

var TestHumanPlayer = {
  PlayerGame: MyGame,
  getInputJSLine: getInputJSLine,
  nextMove: nextMove,
  playerName: playerName
};

var TestGame;

var MyHumanPlayer = {
  PlayerGame: MyGame,
  nextMove: nextMove,
  playerName: playerName
};

exports.HumanPlayer = HumanPlayer;
exports.TestGame = TestGame;
exports.TestHumanPlayer = TestHumanPlayer;
exports.MyHumanPlayer = MyHumanPlayer;
/* Connect4-Game Not a pure module */
