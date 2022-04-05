const fs = require("fs");
const sample = fs.readFileSync("input/Week1/Year2020Day3.sample.txt", "utf8");

// 주어지는 string의 (0,0)에서 우측으로 N 아래로 M씩 이동하면서, 조건에 해당하는 문자열("#")을 카운트하시오.
const input = sample.split("\n");
const forest_ = Array.from({ length: input.length }, (_, i) => input[i].split(""));
const forest = forest_.map((xs) => xs.map((x) => (x === "#" ? 1 : 0)));

const findTree =
  (input) =>
  ([rowIncreaseCount, columnIncreaseCount]) => {
    const recur = (row, col, count) => {
      if (row >= input.length) {
        return count;
      } else {
        const nextCount = count + input[row][col];
        const nextRow = row + rowIncreaseCount;
        const nextCol = (col + columnIncreaseCount) % input[row].length;
        return recur(nextRow, nextCol, nextCount);
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
