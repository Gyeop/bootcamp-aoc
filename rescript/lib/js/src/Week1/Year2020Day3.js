const fs = require("fs");
const sample = fs.readFileSync("input/Week1/Year2020Day3.sample.txt", "utf8");

// 문자열에 (0,0)에서 우측으로 N, 아래로 M씩 이동하면서, 해당 위치에 존재하는 #(tree)의 개수를 구하시오.

const input = sample.split("\n");
const forest_ = Array.from({ length: input.length }, (_, i) => input[i].split(""));
const forest = forest_.map((xs) => xs.map((x) => (x === "#" ? 1 : 0)));

const findTree =
  (input) =>
  ([rowIncreaseCount, columnIncreaseCount]) => {
    const recur = (row, col, count) => {
      if (input.length > row) {
        return count;
      } else {
        const nextCount = count + input[row][col];
        const nextRow = row + rowIncreaseCount;
        const nextCol = (col + columnIncreaseCount) % input[row].length;
        recur(nextRow, nextCol, nextCount);
      }
    };

    return recur(0, 0, 0);
  };

function multiplicationAll(params) {
  return params.reduce((acc, cur) => acc * cur, 1);
}

const pos = [
  [1, 1],
  [1, 3],
  [1, 5],
  [1, 7],
  [2, 1],
];

console.log(multiplicationAll(pos.map(findTree(forest))));
