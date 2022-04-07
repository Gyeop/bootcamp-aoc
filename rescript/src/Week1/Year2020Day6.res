let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.sample.txt")
let formsString = Js.String2.split(input, "\n")

// new line으로 구별된 그룹에서 서로 중복되는 문자열을 무시한 모든 문자열의 종류를 더해 반환하시오.
// Set자료구조를 이용해서 루프를 순회하면서 저장한 후 size를 리턴해서 더하면 끝.

let forms =
  formsString
  ->Js.Array2.joinWith(" ")
  ->Js.String2.split("  ")
  ->Belt.Array.map(form => form->Js.String2.split("")->Belt.Array.keep(string => string !== " "))
// 🔥 직접 모듈을 생성하는 방법?
// 문자열을 여러번 쪼개고 나눠서 배열형태로 나누는 작업을 하고 있는데,
// groupBy같은 메서드를 직접 만들면 편리할 것 같음

// Set 자료구조에 대해 더 알아보기
module IntCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

let filterUniquePerson = (persons: array<string>) => {
  let s0 = Belt.Set.fromArray(persons, ~id=module(IntCmp))
  s0->Belt.Set.size
}

forms->Belt.Array.map(filterUniquePerson)->Belt.Array.reduce(0, (acc, cur) => acc + cur)->Js.log

// 문제를 풀 때,
// 자꾸 JS의 서브셋이다보니 JS로 풀 수 있는 방법을 생각해보고, 그와 비슷한 문법을 지원하는지 찾고, 풀게된다.
// 함수형으로 뇌바꾸기를 쉽게 하는 과정이 필요하다. 아니면 많이풀어보는게 방법일까?
