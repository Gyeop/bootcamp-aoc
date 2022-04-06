let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.sample.txt")
let passes = Js.String2.split(input, "\n")

let splitPass = (pass: string) => {
  let rowChar = pass->Js.String2.slice(~from=0, ~to_=7)
  let columnChar = pass->Js.String2.sliceToEnd(~from=7)
  (rowChar, columnChar)
}

let checkLocation = (direction: string) =>
  switch direction {
  | "F" | "L" => true
  | "B" | "R" => false
  | _ => false
  }

let getSeatNumber = (location: string) => {
  let maxSeatCountFloat = Js.Math.pow_float(
    ~base=2.0,
    ~exp=Belt.Int.toFloat(Js.String2.length(location)),
  )

  let maxSeatCount = Belt.Float.toInt(maxSeatCountFloat) - 1
  let splittedLocations = location->Js.String2.split("")

  let rec recur = (minSeatCount: int, maxSeatCount: int, index: int) => {
    if minSeatCount === maxSeatCount {
      minSeatCount
    } else {
      let result = checkLocation(splittedLocations[index])
      let sum = minSeatCount + maxSeatCount
      let nextMin = result ? minSeatCount : sum / 2 + 1
      let nextMax = result ? sum / 2 : maxSeatCount

      recur(nextMin, nextMax, index + 1)
    }
  }

  recur(0, maxSeatCount, 0)
}

let findPosition = ((rowChar, colChar)) => {
  let row = getSeatNumber(rowChar)
  let col = getSeatNumber(colChar)
  (row, col)
}

let getSeatId = ((row, col)): int => {
  row * 8 + col
}

let refinePass = x => x->splitPass->findPosition->getSeatId
let maxPassId = passes->Belt.Array.map(refinePass)->Js.Math.maxMany_int

maxPassId->Js.log

// Part2
let partitionWithNextItem = (arr: array<int>, size) =>
  arr->Belt.Array.mapWithIndex((index, _) => arr->Belt.Array.slice(~offset=index, ~len=size))

let emptyId =
  passes
  ->Belt.Array.map(refinePass)
  ->Belt.SortArray.stableSortBy((a, b) => a - b)
  ->partitionWithNextItem(2)
  ->Js.Array2.find(part => part[1] - part[0] > 1)
  ->Js.log
