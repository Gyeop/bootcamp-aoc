let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.sample.txt")
let passes = Js.String2.split(input, "\n")

let splitPass = (pass: string) => {
  let rowChar = pass->Js.String2.slice(~from=0, ~to_=7)
  let columnChar = pass->Js.String2.sliceToEnd(~from=7)
  (rowChar, columnChar)
}

// 일반적으로 TS에서는 이런 값들을 enum으로 처리할텐데,
// 패턴매칭을 이용하려면 배리언트를 만들어서 사용해야하는건지?
// enum과 같이 지정하는 방법은 없는지?
let checkLocation = (direction: string) =>
  switch direction {
  | "F" | "L" => true
  | "B" | "R" => false
  | _ => false // 이런 경우는 어떻게 처리해야하는지?
  // 리스크립트에서는 default가 항상 존재해야하는데,
  // 리턴타입이 자동으로 유추되기 때문에 예외처리를 하려면 리턴타입의 어노테이션 따로 설정해줘야 하는건지..?
  }

// 숫자형(int, float)을 다룰 때 형변환이 굉장히 자주 일어나는데, 간단하게 처리하는 방법은 없을까?
let getSeatNumber = (location: string) => {
  // Math때문에 float을 사용하는 경우
  let maxSeatCountFloat = Js.Math.pow_float(
    ~base=2.0,
    ~exp=Belt.Int.toFloat(Js.String2.length(location)),
  )

  // 그렇지만 다시 int로 타입이 변환시켜야 한다.
  let maxSeatCount = Belt.Float.toInt(maxSeatCountFloat) - 1
  let splittedLocations = location->Js.String2.split("")

  let rec recur = (minSeatCount: int, maxSeatCount: int, index: int) => {
    if minSeatCount === maxSeatCount {
      minSeatCount
    } else {
      let result = checkLocation(splittedLocations[index])
      let sum = minSeatCount + maxSeatCount
      let nextMin = result ? minSeatCount : sum / 2 + 1 // 여기서도 ceil처리가 어려움
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

// 🔥 Belt_Array, Belt.Array 차이 찾아보기.
let maxPassId =
  passes
  ->Belt.Array.map(splitPass)
  ->Belt.Array.map(findPosition)
  ->Belt.Array.map(getSeatId)
  ->Js.Math.maxMany_int

Js.log(maxPassId)
