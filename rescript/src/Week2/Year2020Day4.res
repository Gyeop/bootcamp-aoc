open Belt
let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.sample.txt")
let passportStrings = input->Js.String2.split("\n")->Js.Array2.joinWith(" ")->Js.String2.split("  ")

// --- Common Codes ----
type keys = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid
let keysToStringFromPassportField = key =>
  switch key {
  | Byr => "byr"
  | Iyr => "iyr"
  | Eyr => "eyr"
  | Hgt => "hgt"
  | Hcl => "hcl"
  | Ecl => "ecl"
  | Pid => "pid"
  | Cid => "cid"
  }

let createKeyValueTuple = str => {
  let field = str->Js.String2.split(":")
  switch (Belt.Array.get(field, 0), Belt.Array.get(field, 1)) {
  | (Some(key), Some(value)) => (key, value)
  | _ => ("", "")
  }
}

let parseToDict = passports =>
  passports
  ->Belt.Array.map(x => x->Js.String2.split(" "))
  ->Belt.Array.map(passport => passport->Belt.Array.map(createKeyValueTuple))
  ->Belt.Array.map(passport => passport->Js.Dict.fromArray)

let passportDicts = parseToDict(passportStrings)

// ----- Part 1 Codes -----
type passport = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

let checkValidPassport = (dict: Js.Dict.t<string>) => {
  let byr = dict->Js.Dict.get(keysToStringFromPassportField(Byr))
  let iyr = dict->Js.Dict.get(keysToStringFromPassportField(Iyr))
  let eyr = dict->Js.Dict.get(keysToStringFromPassportField(Eyr))
  let hgt = dict->Js.Dict.get(keysToStringFromPassportField(Hgt))
  let hcl = dict->Js.Dict.get(keysToStringFromPassportField(Hcl))
  let ecl = dict->Js.Dict.get(keysToStringFromPassportField(Ecl))
  let pid = dict->Js.Dict.get(keysToStringFromPassportField(Pid))
  let cid = dict->Js.Dict.get(keysToStringFromPassportField(Cid))

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

// Part1 Result
let validPassports = passportDicts->Belt.Array.map(checkValidPassport)->Belt.Array.keepMap(x => x)
let validPassportsCount = validPassports->Belt.Array.size
validPassportsCount->Js.log // 190

// ----- Part 2 -----
type eyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
type height = Cm(int) | Inch(int)
let stringToKeyFromEyeColor = (key: string): option<eyeColor> =>
  switch key {
  | "amb" => Some(Amb)
  | "blu" => Some(Blu)
  | "brn" => Some(Brn)
  | "gry" => Some(Gry)
  | "grn" => Some(Grn)
  | "hzl" => Some(Hzl)
  | "oth" => Some(Oth)
  | _ => None
  }

type passport2 = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: height,
  hcl: string,
  ecl: eyeColor,
  pid: string,
  cid: option<string>,
}

let parseInt = (str: string) => Belt.Int.fromString(str)

// 파싱은 파싱만, 검증은 검증만 각각 함수를 분리한다.
// 파싱 -> 데이터를 형태(타입)에 맞게 변환
// 검증 -> 조건에 따라 필터링(특정 범위, 특정 문자열, 특정길이 등)

// 처음 각각 파싱과 검증에 따른 함수를 분리하려했는데,
// 파싱단계에서 타입을 맞춰주면서 패턴매칭으로 인해 데이터가 없는 여권들은 전부 None으로 변해버린다.
// 그렇게되면 해당 파싱단계에서 일부 여권에 대한 검증이 한 번에 처리된 것이 아닌가?

// 그럼 그냥 합칠 수 없나? => 합친 결과, 하나를 "파싱->...->검증" 단계를 거치며 각 프로퍼티에 대한 파이프가 길어짐
// 그렇지만 중간에 검증되지 않은 데이터는 전혀 사용할 필요가 없으므로, 한 번에 이 과정을 전처리해주는 것이 더욱 이득이 큰가?
// 더 유연하게 함수를 나누어서 처리하는 방법? (패턴매칭이 길어지지 않으면서도.)

let parsePassport = (dict: Js.Dict.t<string>) => {
  let byr = dict->Js.Dict.get(keysToStringFromPassportField(Byr))
  let iyr = dict->Js.Dict.get(keysToStringFromPassportField(Iyr))
  let eyr = dict->Js.Dict.get(keysToStringFromPassportField(Eyr))
  let hgt = dict->Js.Dict.get(keysToStringFromPassportField(Hgt))
  let hcl = dict->Js.Dict.get(keysToStringFromPassportField(Hcl))
  let ecl = dict->Js.Dict.get(keysToStringFromPassportField(Ecl))
  let pid = dict->Js.Dict.get(keysToStringFromPassportField(Pid))
  let cid = dict->Js.Dict.get(keysToStringFromPassportField(Cid))

  let checkRange = (~min: int, ~max: int, number: int): option<int> => {
    number >= min && max >= number ? Some(number) : None
  }

  let checkHeight = (height: height) => {
    switch height {
    | Cm(v) =>
      switch v->checkRange(~min=150, ~max=193) {
      | Some(v) => Some(Cm(v))
      | _ => None
      }
    | Inch(v) =>
      switch v->checkRange(~min=59, ~max=76) {
      | Some(v) => Some(Inch(v))
      | _ => None
      }
    }
  }

  let checkHairColor = hex => {
    let re = %re("/(#){1}([a-f]|[A-F]|[0-9]){6}/")
    Js.Re.test_(re, hex) ? Some(hex) : None
  }

  let checkPassportId = (id: string) => id->Js.String2.length === 9 ? Some(id) : None

  let parseHeight = (heightString: string): option<height> => {
    // regex로 처리하려니, nested option이 되어 처리하기가 어려운데, 좋은 처리방법?
    // let numberString = heightString->Js.String2.splitByRe(%re("/cm|in/"))->Belt.Array.get(0) // option<option<int>> ..

    // 아래처럼 특정 index로 자르면 만약 단위가 "m" 처럼 한글자가 들어오게되면 대응할 수 없어진다.
    let number = heightString->Js.String2.slice(~from=0, ~to_=-2)->Belt.Int.fromString
    let unit = heightString->Js.String2.sliceToEnd(~from=-2)

    switch (number, unit) {
    | (Some(number), "cm") => Some(Cm(number))
    | (Some(number), "in") => Some(Inch(number))
    | _ => None
    }
  }

  let validByr =
    byr
    ->Belt.Option.flatMap(x => x->Belt.Int.fromString)
    ->Belt.Option.flatMap(x => x->checkRange(~min=1920, ~max=2002))

  let validIyr =
    iyr
    ->Belt.Option.flatMap(x => x->Belt.Int.fromString)
    ->Belt.Option.flatMap(x => x->checkRange(~min=2010, ~max=2020))

  let validEyr =
    eyr
    ->Belt.Option.flatMap(x => x->Belt.Int.fromString)
    ->Belt.Option.flatMap(x => x->checkRange(~min=2020, ~max=2030))

  let validHgt =
    hgt->Belt.Option.flatMap(x => x->parseHeight)->Belt.Option.flatMap(x => x->checkHeight)

  let validHcl = hcl->Belt.Option.flatMap(x => x->checkHairColor)

  let validPid = pid->Belt.Option.flatMap(x => x->checkPassportId)

  let validEcl = ecl->Belt.Option.flatMap(x => x->stringToKeyFromEyeColor)

  switch (validByr, validIyr, validEyr, validHgt, validHcl, validEcl, validPid, cid) {
  | (
      Some(validByr),
      Some(validIyr),
      Some(validEyr),
      Some(validHgt),
      Some(validHcl),
      Some(validEcl),
      Some(validPid),
      cid,
    ) =>
    Some({
      byr: validByr,
      iyr: validIyr,
      eyr: validEyr,
      hgt: validHgt,
      hcl: validHcl,
      ecl: validEcl,
      pid: validPid,
      cid: cid,
    })
  | _ => None
  }
}

// Part2 Result
let validPassports2 = passportDicts->Belt.Array.map(parsePassport)
validPassports2->Belt.Array.keepMap(x => x)->Js.Array.length->Js.log // 121
