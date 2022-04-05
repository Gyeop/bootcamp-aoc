let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.sample.txt")
let forest_ = Js.String2.split(input, "\n")->Belt.Array.map(x => Js.String2.split(x, ""))
let forest = Belt.Array.map(forest_, xs => Belt.Array.map(xs, x => x === "#" ? 1 : 0))

let findTree = (input, (rowIncreaseCount, columnIncreaseCount)) => {
  let rec recur = (row, col, count) => {
    if row >= input->Belt.Array.length {
      count
    } else {
      let nextCount = count + input[row][col]
      let nextRow = row + rowIncreaseCount
      let nextCol = mod(col + columnIncreaseCount, Belt.Array.length(input[row]))
      recur(nextRow, nextCol, nextCount)
    }
  }

  recur(0, 0, 0)
}

let pos = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
Belt.Array.map(pos, forest->findTree)->Belt.Array.reduce(1, (acc, cur) => acc * cur)->Js.log
