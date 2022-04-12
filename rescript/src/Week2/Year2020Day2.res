open Belt
let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.sample.txt")
let passwords = input->Js.String2.split("\n")

type password = {
  min: int,
  max: int,
  target: string,
  password: string,
}

let sequence = (arr: array<option<'a>>): option<array<'a>> => {
  let arr2 = arr->Belt.Array.keepMap(x => x)
  arr->Belt.Array.length === arr2->Belt.Array.length ? Some(arr2) : None
}

let toPasswordRecord = (arr: array<string>): option<password> => {
  switch arr {
  | [min, max, target, password] =>
    switch (min->Belt.Int.fromString, max->Belt.Int.fromString) {
    | (Some(min'), Some(max')) => Some({min: min', max: max', target: target, password: password})
    | _ => None
    }
  | _ => None
  }
}

let checkValidCountPassword = (passwordRecord: password) => {
  let {min, max, target, password} = passwordRecord
  let targetCount =
    password->Js.String2.split("")->Belt.Array.keep(s => s == target)->Belt.Array.length

  targetCount >= min && max >= targetCount ? Some(passwordRecord) : None
}

let checkValidIndexPassword = (passwordRecord: password) => {
  let {min, max, target, password} = passwordRecord
  let m1 = password->Js.String2.get(min - 1)
  let m2 = password->Js.String2.get(max - 1)
  m1 != m2 && (m1 == target || m2 == target) ? Some(passwordRecord) : None
}

let checkPassword = func =>
  passwords
  ->Belt.Array.map(password => password->Js.String2.splitByRe(%re("/\s|-|:\s/")))
  ->Belt.Array.map(sequence)
  ->Belt.Array.map(x => x->Belt.Option.flatMap(toPasswordRecord))
  ->Belt.Array.map(x => x->Belt.Option.flatMap(func))
  ->Belt.Array.keepMap(x => x)
  ->Belt.Array.length

// Part 1
checkPassword(checkValidCountPassword)->Js.log

// Part 2
checkPassword(checkValidIndexPassword)->Js.log
