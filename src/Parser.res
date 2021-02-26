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
  | StringTypeError(string)
  | PropertyNotFound(string)

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

let toResult = (opt: option<'a>, err: 'b): Result.t<'a, 'b> =>
  switch opt {
  | None => Result.Error(err)
  | Some(x) => Result.Ok(x)
  }

let toOption = (result: Result.t<'t, 'e>): option<'t> =>
  switch result {
  | Error(_) => None
  | Ok(x) => Some(x)
  }

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
  | Ok(obj) => Ok(update(obj, toOption(decoder(dict, prop))))
  }

let getProp = (dict: Js.Dict.t<Js.Json.t>, prop: string): Result.t<Js.Json.t, parseError> =>
  Js.Dict.get(dict, prop)->toResult(PropertyNotFound(prop))

let stringDecoder = (dict: Js.Dict.t<Js.Json.t>, prop: string): Result.t<string, parseError> =>
  getProp(dict, prop)
  ->Result.map(json => Js.Json.decodeString(json))
  ->Result.flatMap(opt => toResult(opt, StringTypeError(prop)))

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

let parsePassport = (passport: string): Belt.Result.t<passport, parseError> => {
  let passport = Js.Json.parseExn(passport)
  switch Js.Json.classify(passport) {
  | Js.Json.JSONObject(dict) =>
    Belt.Result.Ok(initialPassport)
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
