open Belt

let log = Js.log

type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: float,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

type parseError =
  | ObjectDecodingFailure
  | RootArrayParsingFailure
  | NumberTypeError(string)
  | StringTypeError(string)
  | PropertyNotFound(string)

let mapTogether = (
  first: Result.t<'a, 'error>,
  second: Result.t<'b, 'error>,
  func: ('a, 'b) => 'c,
): Result.t<'c, 'error> =>
  Result.flatMap(first, first => Result.map(second, second => func(first, second)))

let required = (
  t: Result.t<'t, parseError>,
  prop: string,
  decoder: (Js.Dict.t<Js.Json.t>, string) => Result.t<'prop, parseError>,
  dict: Js.Dict.t<Js.Json.t>,
  update: ('t, 'prop) => 't,
): Result.t<'t, parseError> => mapTogether(t, decoder(dict, prop), update)

let optional = (
  t: Result.t<'t, parseError>,
  prop: string,
  decoder: (Js.Dict.t<Js.Json.t>, string) => Result.t<'prop, parseError>,
  dict: Js.Dict.t<Js.Json.t>,
  update: ('t, option<'prop>) => 't,
): Result.t<'t, parseError> =>
  switch t {
  | Error(_) => t
  | Ok(obj) => Ok(update(obj, OptionResult.toOption(decoder(dict, prop))))
  }

let getProp = (dict: Js.Dict.t<Js.Json.t>, prop: string): Result.t<Js.Json.t, parseError> =>
  Js.Dict.get(dict, prop)->OptionResult.toResult(PropertyNotFound(prop))

let numberDecoder = (dict: Js.Dict.t<Js.Json.t>, prop: string): Result.t<float, parseError> =>
  getProp(dict, prop)
  ->Result.map(json => Js.Json.decodeNumber(json))
  ->Result.flatMap(op => OptionResult.toResult(op, NumberTypeError(prop)))

let stringDecoder = (dict: Js.Dict.t<Js.Json.t>, prop: string): Result.t<string, parseError> =>
  getProp(dict, prop)
  ->Result.map(json => Js.Json.decodeString(json))
  ->Result.flatMap(opt => OptionResult.toResult(opt, StringTypeError(prop)))

let initialPassport: passport = {
  byr: 0,
  iyr: 0,
  eyr: 0,
  hgt: 0.,
  hcl: "#abcdef",
  ecl: "",
  pid: "",
  cid: None,
}

let parsePassport = (passport: Js.Json.t): Belt.Result.t<passport, parseError> => {
  switch Js.Json.classify(passport) {
  | Js.Json.JSONObject(dict) =>
    Belt.Result.Ok(initialPassport)
    ->required("byr", numberDecoder, dict, (obj, byr) => {
      ...obj,
      byr: int_of_float(byr),
    })
    ->required("iyr", numberDecoder, dict, (obj, iyr) => {
      ...obj,
      iyr: int_of_float(iyr),
    })
    ->required("eyr", numberDecoder, dict, (obj, eyr) => {
      ...obj,
      eyr: int_of_float(eyr),
    })
    ->required("hgt", numberDecoder, dict, (obj, hgt) => {
      ...obj,
      hgt: hgt,
    })
    ->required("hcl", stringDecoder, dict, (obj, hcl) => {
      ...obj,
      hcl: hcl,
    })
    ->required("ecl", stringDecoder, dict, (obj, ecl) => {
      ...obj,
      ecl: ecl,
    })
    ->required("ecl", stringDecoder, dict, (obj, ecl) => {
      ...obj,
      ecl: ecl,
    })
    ->required("pid", stringDecoder, dict, (obj, pid) => {
      ...obj,
      pid: pid,
    })
    ->optional("cid", stringDecoder, dict, (obj, cid) => {
      ...obj,
      cid: cid,
    })
  | _ => Result.Error(ObjectDecodingFailure)
  }
}

let parsePassports = (passports: string): Belt.Result.t<list<passport>, parseError> => {
  let passports = Js.Json.parseExn(passports)
  switch Js.Json.classify(passports) {
  | Js.Json.JSONArray(array) =>
    Ok(
      array
      ->Belt_Array.keepMap(passport => passport->parsePassport->OptionResult.toOption)
      ->Belt_List.fromArray,
    )
  | _ => Error(RootArrayParsingFailure)
  }
}
