open Belt
let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.sample.txt")
let formsString = Js.String2.split(input, "\n")

let forms =
  formsString
  ->Js.Array2.joinWith(" ")
  ->Js.String2.split("  ")
  ->Belt.Array.map(form =>
    form->Js.String2.split(" ")->Belt.Array.map(x => x->Js.String2.split(""))
  )

let unionDefaultSet = Belt.Set.String.empty
let intersectDefaultSet =
  "abcdefghijklmnopqrstuvwxyz"->Js.String2.split("")->Belt.Set.String.fromArray

let union = (acc, cur) => {
  let currentSet = Belt.Set.String.fromArray(cur)
  let nextSet = Belt.Set.String.union(acc, currentSet)
  nextSet
}

let intersect = (acc, cur) => {
  let currentSet = Belt.Set.String.fromArray(cur)
  let nextSet = Belt.Set.String.intersect(acc, currentSet)
  nextSet
}

let checkSetSize = (set: Belt.Set.String.t) => {
  set->Belt.Set.String.toArray->Belt.Array.length
}

let sum = (a, b) => a + b

// Part1
forms
->Belt.Array.map(xs => xs->Belt.Array.reduce(unionDefaultSet, union)->checkSetSize)
->Belt.Array.reduce(0, sum)
->Js.log

// Part2
forms
->Belt.Array.map(xs => xs->Belt.Array.reduce(intersectDefaultSet, intersect)->checkSetSize)
->Belt.Array.reduce(0, sum)
->Js.log
