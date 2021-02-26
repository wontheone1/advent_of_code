open Belt.Result

type encodingErrors =
  | EmptyCID
  | InvalidHexaColorCode
  | InvalidHeight
  | UnknownProp(string)
  | NoPassportFromInputLines

let cidEncoder = str => {
  if Js.Re.test_(%re("/^\s*$/"), str) {
    Error(EmptyCID)
  } else {
    Ok("\"" ++ str ++ "\"")
  }
}

let hclEncoder: string => Belt.Result.t<string, encodingErrors> = str => {
  if Js.String2.startsWith(str, "#") && 7 == Js.String2.length(str) {
    Ok("\"" ++ str ++ "\"")
  } else {
    Error(InvalidHexaColorCode)
  }
}

let trimLastTwoChar = str => {
  Js.String.substring(str, ~from=0, ~to_=Js.String2.length(str) - 2)
}

let heightEncoder: string => Belt.Result.t<string, encodingErrors> = str => {
  if Js.String2.endsWith(str, "cm") {
    Ok(trimLastTwoChar(str))
  } else if Js.String2.endsWith(str, "in") {
    let heightInInch = trimLastTwoChar(str)->float_of_string
    Ok(Js.Float.toString(heightInInch *. 2.54))
  } else {
    Error(InvalidHeight)
  }
}

let stringEncoder = str => Ok("\"" ++ str ++ "\"")

let numericEncoder: string => Belt.Result.t<string, encodingErrors> = a => Ok(a)

let passportTypeEncoder = Belt.HashMap.String.fromArray([
  ("byr", numericEncoder),
  ("iyr", numericEncoder),
  ("eyr", numericEncoder),
  ("hgt", heightEncoder),
  ("hcl", hclEncoder),
  ("ecl", stringEncoder),
  ("pid", stringEncoder),
  ("cid", cidEncoder),
])

let singleJSONProperty = ((key, value)) => {
  "\"" ++ key ++ "\"" ++ ":" ++ value
}

let encodePassportLine = passportLine => {
  let passportFields = Js.String2.splitByRe(passportLine, %re("/\s/"))->Belt_Array.map(field => {
    switch field {
    | None => []
    | Some(field) => Js.String2.split(field, ":")
    }
  })

  let validatedFields = Belt_Array.map(passportFields, field => {
    switch Belt_HashMapString.get(passportTypeEncoder, field[0]) {
    | None => Error(UnknownProp(field[0]))
    | Some(encoderFn) =>
      switch encoderFn(field[1]) {
      | Ok(value) => Ok((field[0], value))
      | Error(msg) => Error(msg)
      }
    }
  })

  let passportJSONBuillder = Belt_Array.reduce(validatedFields, Ok("{"), (acc, field) => {
    switch acc {
    | Ok(acc) =>
      switch field {
      | Ok(field) => Ok(acc ++ singleJSONProperty(field) ++ ",")
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

let encodePassports = passportLines => {
  let passports = Js.String2.splitByRe(passportLines, %re("/\\n{2}/"))
  let passportsJSONBuilder = Belt_Array.reduce(passports, Ok("["), (passports, passport) => {
    switch passports {
    | Error(_) => passports
    | Ok(acc) =>
      switch passport {
      | None => Error(NoPassportFromInputLines)
      | Some(passport) => {
          let passportEncoded = encodePassportLine(Js.String2.trim(passport))
          switch passportEncoded {
          | Error(e) => Error(e)
          | Ok(passportEncoded) => Ok(acc ++ passportEncoded ++ ",")
          }
        }
      }
    }
  })

  let passportJSON = switch passportsJSONBuilder {
  | Ok(passportsJSONBuilder) => Ok(Js.String2.replaceByRe(passportsJSONBuilder, %re("/,$/"), "]"))
  | Error(_) => passportsJSONBuilder
  }

  passportJSON
}
