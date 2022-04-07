// open Belt
let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.sample.txt")
let formsString = Js.String2.split(input, "\n")

// Part.1
// new line으로 구별된 그룹에서 서로 중복되는 문자열을 무시한 모든 문자열의 종류를 더해 반환하시오.

let forms =
  formsString
  ->Js.Array2.joinWith(" ")
  ->Js.String2.split("  ")
  ->Belt.Array.map(form => form->Js.String2.split("")->Belt.Array.keep(string => string !== " "))

let filterUniquePerson = (persons: array<string>) => {
  let s0 = Belt.Set.String.fromArray(persons)
  s0->Belt.Set.String.size
}

forms->Belt.Array.map(filterUniquePerson)->Belt.Array.reduce(0, (acc, cur) => acc + cur)->Js.log

// Part2
// 그룹별 모든 질문에 응답한 사람의 수를 더해서 반환하시오.
// Map으로 만들어서 카운트하고, 질문의 수와 같은 값을 가진 사람을 조회해서 반환하기.

let getAnyoneAnswered = (persons: array<string>) => {
  let rec recur = (map, index) => {
    if Belt.Array.length(persons) === index {
      let questionCount = map->Belt.Map.String.getWithDefault(" ", 0)
      map
      ->Belt.Map.String.valuesToArray
      ->Belt.Array.keep(v => v === questionCount + 1)
      ->Belt.Array.length
    } else if Belt.Map.String.has(map, persons[index]) {
      recur(
        Belt.Map.String.update(map, persons[index], v => Some(Js.Option.getExn(v) + 1)),
        index + 1,
      )
    } else {
      recur(Belt.Map.String.set(map, persons[index], 1), index + 1)
    }
  }

  recur(Belt.Map.String.empty, 0)
}

let formsString =
  Js.String2.split(input, "\n")
  ->Js.Array2.joinWith(" ")
  ->Js.String2.split("  ")
  ->Belt.Array.map(form => form->Js.String2.split("")->Belt.Array.keep(string => string !== ""))
  ->Belt.Array.map(getAnyoneAnswered)
  ->Belt.Array.reduce(0, (acc, cur) => acc + cur)
  ->Js.log

// Todo: Refactor
// reduce를 이용해 리팩토링해보기.
