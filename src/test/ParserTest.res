open Test

let passportResultEqual = (
  ~message=?,
  a: Belt.Result.t<Parser.passport, Parser.parseError>,
  b: Belt.Result.t<Parser.passport, Parser.parseError>,
) => assertion(~message?, ~operator="passportResultEqual", (a, b) => a == b, a, b)

test("Parse passport successfully", () => {
  passportResultEqual(
    Ok({
      byr: 1937,
      iyr: 2017,
      eyr: 2020,
      hgt: 183.,
      hcl: "#fffffd",
      ecl: "gry",
      pid: "860033327",
      cid: Some("147"),
    }),
    Parser.parsePassport(
      Js.Json.parseExn(
        "{\"ecl\":\"gry\",\"pid\":\"860033327\",\"eyr\":2020,\"hcl\":\"#fffffd\",\"byr\":1937,\"iyr\":2017,\"cid\":\"147\",\"hgt\":183}",
      ),
    ),
  )
})

test("Parse passport successfully when cid does NOT exist", () => {
  passportResultEqual(
    Ok({
      byr: 1931,
      iyr: 2013,
      eyr: 2024,
      hgt: 179.,
      hcl: "#ae17e1",
      ecl: "brn",
      pid: "760753108",
      cid: None,
    }),
    Parser.parsePassport(
      Js.Json.parseExn(
        "{\"hcl\":\"#ae17e1\",\"iyr\":2013,\"eyr\":2024,\"ecl\":\"brn\",\"pid\":\"760753108\",\"byr\":1931,\"hgt\":179}",
      ),
    ),
  )
})

test("Parse passport unsuccessfully due to a missing prop", () => {
  passportResultEqual(
    Error(Parser.PropertyNotFound("hgt")),
    Parser.parsePassport(
      Js.Json.parseExn(
        "{\"iyr\":2013,\"ecl\":\"amb\",\"cid\":\"350\",\"eyr\":2023,\"pid\":\"028048884\",\"hcl\":\"#cfa07d\",\"byr\":1929}",
      ),
    ),
  )
})
