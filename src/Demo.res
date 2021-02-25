open Belt.Result

type passport<'a, 'b> = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: float,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

let wrapInOk: string => Belt.Result.t<string, string> = a => Ok(a)

let cidEncoder = str => {
  if Js.Re.test_(%re("/^\s*$/"), str) {
    Error("Empty CID")
  } else {
    Ok(str)
  }
}

let trimLastTwoChar = str => {
  Js.String.substring(str, ~from=0, ~to_=Js.String2.length(str) - 2)
}

let heightEncoder: string => Belt.Result.t<string, string> = str => {
  if Js.String2.endsWith(str, "cm") {
    Ok(trimLastTwoChar(str))
  } else if Js.String2.endsWith(str, "in") {
    let heightInInch = trimLastTwoChar(str)->float_of_string
    Ok(Js.Float.toString(heightInInch *. 2.54))
  } else {
    Error("invalid height")
  }
}

let hclEncoder: string => Belt.Result.t<string, string> = str => {
  if Js.String2.startsWith(str, "#") && 7 == Js.String2.length(str) {
    Ok(str)
  } else {
    Error("invalid hexa color code")
  }
}

let stringEncoder = str => "\"" ++ str ++ "\""

let passportTypeEncoder = Belt.HashMap.String.fromArray([
  ("byr", wrapInOk),
  ("iyr", wrapInOk),
  ("eyr", wrapInOk),
  ("hgt", heightEncoder),
  ("hcl", hclEncoder),
  ("ecl", wrapInOk),
  ("pid", wrapInOk),
  ("cid", cidEncoder),
])

let myPassport = {
  byr: 1999,
  iyr: 2015,
  eyr: 2025,
  hgt: 175.5,
  hcl: "#fffffd",
  ecl: "#fffffd",
  pid: "860033327",
  cid: None,
}

let validPassports = list{myPassport}

let passportLines = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
"

let passports = Js.String2.splitByRe(passportLines, %re("/\\n{2}/"))

let encodePassportLine = passportLine => {
  let passportFields = Js.String2.splitByRe(passportLine, %re("/\s/"))->Belt_Array.map(field => {
    switch field {
    | None => []
    | Some(field) => Js.String2.split(field, ":")
    }
  })

  let validatedFields = Belt_Array.map(passportFields, field => {
    switch Belt_HashMapString.get(passportTypeEncoder, field[0]) {
    | None => Error("Invalid prop " ++ field[0])
    | Some(fn) =>
      switch fn(field[1]) {
      | Ok(value) => Ok("\"" ++ field[0] ++ "\"" ++ ":" ++ value)
      | Error(msg) => Error(msg)
      }
    }
  })

  let passportJSONBuillder = Belt_Array.reduce(validatedFields, Ok("{"), (acc, field) => {
    switch acc {
    | Ok(acc) =>
      switch field {
      | Ok(field) => Ok(acc ++ field ++ ",")
      | Error(msg) => Error(msg)
      }
    | Error(msg) => Error(msg)
    }
  })

  let passportJSON = switch passportJSONBuillder {
  | Ok(passportJSONBuillder) => Ok(Js.String2.replaceByRe(passportJSONBuillder, %re("/,$/"), "}"))
  | Error(msg) => Error(msg)
  }

  passportJSON
}

encodePassportLine(Belt.Option.getWithDefault(passports[0], ""))->ignore

Js.log("Number of valid passports are: " ++ string_of_int(Belt_List.length(validPassports)))
