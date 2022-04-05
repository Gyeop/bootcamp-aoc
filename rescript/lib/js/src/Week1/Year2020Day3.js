// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

var input = Fs.readFileSync("input/Week1/Year2020Day3.sample.txt", "utf8");

var forest_ = Belt_Array.map(input.split("\n"), (function (x) {
        return x.split("");
      }));

var forest = Belt_Array.map(forest_, (function (xs) {
        return Belt_Array.map(xs, (function (x) {
                      if (x === "#") {
                        return 1;
                      } else {
                        return 0;
                      }
                    }));
      }));

function findTree(input, param) {
  var columnIncreaseCount = param[1];
  var rowIncreaseCount = param[0];
  var _row = 0;
  var _col = 0;
  var _count = 0;
  while(true) {
    var count = _count;
    var col = _col;
    var row = _row;
    if (row >= input.length) {
      return count;
    }
    var nextCount = count + Caml_array.get(Caml_array.get(input, row), col) | 0;
    var nextRow = row + rowIncreaseCount | 0;
    var nextCol = Caml_int32.mod_(col + columnIncreaseCount | 0, Caml_array.get(input, row).length);
    _count = nextCount;
    _col = nextCol;
    _row = nextRow;
    continue ;
  };
}

var pos = [
  [
    1,
    1
  ],
  [
    1,
    3
  ],
  [
    1,
    5
  ],
  [
    1,
    7
  ],
  [
    2,
    1
  ]
];

console.log(Belt_Array.reduce(Belt_Array.map(pos, (function (param) {
                return findTree(forest, param);
              })), 1, (function (acc, cur) {
            return Math.imul(acc, cur);
          })));

exports.input = input;
exports.forest_ = forest_;
exports.forest = forest;
exports.findTree = findTree;
exports.pos = pos;
/* input Not a pure module */
