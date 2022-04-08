open Belt
let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.sample.txt")
let passwords = input->Js.String2.split("\n")

type password = {
  min: int,
  max: int,
  target: string,
  password: string,
}

/*
    getExn은 컴파일되는 코드를 보니 그냥 None(undefined)이면 에러를 발생시키는 코드인데,
    모든 option마다 패턴매칭을 진행하기도 어렵고, option이 나올때마다 getExn, getWithDefault를 사용할 순 없을 것 같은데,
    유연하게 처리하는 방법?
*/
let toPasswordRecord = (arr: array<string>): password => {
  min: arr
  ->Belt.Array.get(0)
  ->Belt.Option.getExn
  ->Js.String2.split("-")
  ->Belt.Array.get(0)
  ->Belt.Option.getExn
  ->Belt.Int.fromString
  ->Belt.Option.getExn,
  max: arr
  ->Belt.Array.get(0)
  ->Belt.Option.getExn
  ->Js.String2.split("-")
  ->Belt.Array.get(1)
  ->Belt.Option.getExn
  ->Belt.Int.fromString
  ->Belt.Option.getExn,
  target: arr
  ->Belt.Array.get(1)
  ->Belt.Option.getExn
  ->Js.String2.split(":")
  ->Belt.Array.get(0)
  ->Belt.Option.getExn,
  password: arr->Belt.Array.get(2)->Belt.Option.getExn,
}

let checkValidCountPassword = (passwordRecord: password) => {
  let {min, max, target, password} = passwordRecord
  let targetCount =
    password
    ->Js.String2.split("")
    ->Belt.Array.reduce(0, (acc, cur) => cur === target ? acc + 1 : acc)

  targetCount >= min && max >= targetCount
}

let checkValidIndexPassword = (passwordRecord: password) => {
  let {min, max, target, password} = passwordRecord

  (password->Js.String2.get(min - 1) === target && password->Js.String2.get(max - 1) !== target) ||
    (password->Js.String2.get(max - 1) === target && password->Js.String2.get(min - 1) !== target)
}

// Part 1
passwords
->Belt.Array.map(password => password->Js.String2.split(" ")->toPasswordRecord)
->Belt.Array.map(checkValidCountPassword)
->Belt.Array.keep(x => x)
->Belt.Array.length
->Js.log

// Part 2
passwords
->Belt.Array.map(password => password->Js.String2.split(" ")->toPasswordRecord)
->Belt.Array.map(checkValidIndexPassword)
->Belt.Array.keep(x => x)
->Belt.Array.length
->Js.log
