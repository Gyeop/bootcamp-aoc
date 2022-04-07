let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.sample.txt")
let passes = Js.String2.split(input, "\n")

let splitPass = (pass: string) => {
  pass->Js.String2.split("")
}

let parseToBinary = string =>
  switch string {
  | "B" | "R" => 1
  | _ => 0
  }

let changeStringsToBinary = (arr: array<string>) => {
  arr->Belt.Array.map(parseToBinary)
}

/*
  바이너리로 처리해보면 더 간단하게 처리할 수 있다.
  B,R은 1, 나머지는 0으로 처리한 바이너리로 계산하게 되면,
  좌석의 id는 2의 (arr.length - index+1) 제곱에 위치한 값을 순차적으로 모두 더한 값과 같게되므로,
  해당 값을 다 더하면 id를 찾는 공식을 생략할 수 있다.

  FBFBBFFRLR
  0101100101 
  -> 512*0 + 256*1 + 128*0 + 64*1 + 32*1 + 16*0 + 8*0 + 4*1 + 2*0 + 1*1 = 357
  -> 256+ 64 + 32 + 4 + 1 = 357 
  8을 곱하는 이유는 뒤에 3자리(Right, Left)를 의미하는 것. 2^3 = 8
*/

let calculateDecimal = (~base, ~exp) => {
  let decimalFloat = Js.Math.pow_float(~base, ~exp)
  let decimal = Belt.Float.toInt(decimalFloat)
  decimal
}

let getSeatId = (binarys: array<int>): int => {
  binarys->Belt.Array.reduceWithIndex(0, (acc, item, index) => {
    // Labeled Arguments를 사용할 때.
    // 1. 라벨은 순서에 영향을 받지 않는다.
    // 2. JS에서와 마찬가지로 key, value가 같다면 property shorthand처럼 적용할 수 있다.
    // 3. 다만 라벨과 일반 파라미터가 공존할 경우엔 라벨을 제외한 나머지는 순서가 보장되어야한다.
    let exp = Belt.Int.toFloat(Belt.Array.length(binarys) - (index + 1))
    let decimal = calculateDecimal(~base=2.0, ~exp)
    decimal * item + acc
  })
}

let refinePass = x => x->splitPass->changeStringsToBinary->getSeatId
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
