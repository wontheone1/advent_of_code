open Belt.Result


let cidEncoder = str => {
  if Js.Re.test_(%re("/^\s*$/"), str) {
    Error("Empty CID")
  } else {
    Ok(str)
  }
}


let hclEncoder: string => Belt.Result.t<string, string> = str => {
  if Js.String2.startsWith(str, "#") && 7 == Js.String2.length(str) {
    Ok(str)
  } else {
    Error("invalid hexa color code")
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


let stringEncoder = str => "\"" ++ str ++ "\""

let wrapInOk: string => Belt.Result.t<string, string> = a => Ok(a)

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
