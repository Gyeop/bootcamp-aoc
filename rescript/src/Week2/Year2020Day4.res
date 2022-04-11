open Belt
let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.sample.txt")
let passportStrings = input->Js.String2.split("\n")->Js.Array2.joinWith(" ")->Js.String2.split("  ")

let passports =
  passportStrings->Belt.Array.map(passportString => passportString->Js.String2.split(" "))

// String 형태의 passports를 잘 파싱해서, passport 타입으로 바꾸어주는 방법
// 초기시도 -> Json.parse를 이용해서 한번에 만들려했는데, 명확한 타입매치의 어려움. 강제로 어노테이션 해줄 수는 있으나 이후의 타입 검사가 불가능함 (신뢰불가).
// 리팩토링 -> Object(Dictionary)형태로 만들어서 해당 모든 키가 존재하면 passport로 타입으로 변경시켜줌.

/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/

type passport = {
  byr: string, // byr: int,            1920 ~ 2002                   =>
  iyr: string, // iyr: int,            2010 ~ 2020                   =>
  eyr: string, // eyr: int,            2020 ~ 2030                   =>
  hgt: string, // hgt: string,         150~193cm or 59~76in          => 배리언트를 이용해 나눠주어야 함.
  hcl: string, // hcl: string,         # 0-9 a-f                     => regex?
  ecl: string, // ecl: string,         amb blu brn gry grn hzl oth   => 새로운 타입 지정, 패턴매치
  pid: string, // pid: int,            0을 포함한 9자리                 => string length ->
  cid: option<string>, // option<int>  optional
}

/*
2. string 타입의 입력을 passport 타입으로 파싱하는 parsePassport 함수를 작성해보세요.
   (우선 parsePassport 타입의 타입 시그니처를 생각해보세요)
*/

let checkField = str => {
  let field = str->Js.String2.split(":")

  switch field {
  | [key, value] =>
    switch key {
    | "byr" => (key, value)
    | "iyr" => (key, value)
    | "eyr" => (key, value)
    | "hgt" => (key, value)
    | "hcl" => (key, value)
    | "ecl" => (key, value)
    | "pid" => (key, value)
    | "cid" => (key, value)
    | _ => ("", "") // ...
    }
  | _ => ("", "")
  }
}

let parseToDict = passports =>
  passports
  ->Belt.Array.map(passport => passport->Belt.Array.map(checkField))
  ->Belt.Array.map(passport => passport->Js.Dict.fromArray)

let passportDicts = parseToDict(passports)

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.
*/

let checkValidPassport = (passport: Js.Dict.t<string>) => {
  let byr = passport->Js.Dict.get("byr")
  let iyr = passport->Js.Dict.get("iyr")
  let eyr = passport->Js.Dict.get("eyr")
  let hgt = passport->Js.Dict.get("hgt")
  let hcl = passport->Js.Dict.get("hcl")
  let ecl = passport->Js.Dict.get("ecl")
  let pid = passport->Js.Dict.get("pid")
  let cid = passport->Js.Dict.get("cid")

  switch (byr, iyr, eyr, hgt, hcl, ecl, pid, cid) {
  | (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid), _) =>
    Some({
      byr: byr,
      iyr: iyr,
      eyr: eyr,
      hgt: hgt,
      hcl: hcl,
      ecl: ecl,
      pid: pid,
      cid: cid,
    })
  | _ => None
  }
}

// Part1
let validPassports = passportDicts->Belt.Array.map(passport => passport->checkValidPassport)
validPassports->Belt.Array.keep(passport => passport !== None)->Belt.Array.size->Js.log

// part2
/*
4. part1과 동일하게, *문제를 그대로* 코드로 옮겨보세요.
*/

/*
참고 링크
- https://rescript-lang.org/docs/manual/latest/record
- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
*/
