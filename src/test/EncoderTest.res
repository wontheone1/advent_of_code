open Test
open Belt.Result

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="intEqual", (a, b) => a === b, a, b)

let stringEqual = (~message=?, a: string, b: string) =>
  assertion(~message?, ~operator="stringEqual", (a, b) => a == b, a, b)

let stringResultEqual = (
  ~message=?,
  a: Belt.Result.t<string, Encoder.encodingErrors>,
  b: Belt.Result.t<string, Encoder.encodingErrors>,
) => assertion(~message?, ~operator="stringResultEqual", (a, b) => a == b, a, b)

let stringResultArrayEqual = (
  ~message=?,
  a: array<Belt.Result.t<string, Encoder.encodingErrors>>,
  b: array<Belt.Result.t<string, Encoder.encodingErrors>>,
) => assertion(~message?, ~operator="stringResultArrayEqual", (a, b) => a == b, a, b)

test("heightEncoder", () => {
  stringResultEqual(Ok("178"), Encoder.heightEncoder("178cm"))
  stringResultEqual(Ok("134.62"), Encoder.heightEncoder("53in"))
  stringResultEqual(Error(Encoder.InvalidHeight), Encoder.heightEncoder("hello"))
})

test("hclEncoder", () => {
  stringResultEqual(Ok("\"#123456\""), Encoder.hclEncoder("#123456"))
  stringResultEqual(Error(Encoder.InvalidHexaColorCode), Encoder.hclEncoder("533333"))
  stringResultEqual(Error(Encoder.InvalidHexaColorCode), Encoder.hclEncoder("#123"))
})

test("cidEncoder", () => {
  stringResultEqual(Ok("\"12345\""), Encoder.cidEncoder("12345"))
  stringResultEqual(Error(Encoder.EmptyCID), Encoder.cidEncoder("    "))
})

test("encodePassportLine", () => {
  stringResultEqual(
    Ok(
      "{\"ecl\":\"gry\",\"pid\":\"860033327\",\"eyr\":2020,\"hcl\":\"#fffffd\",\"byr\":1937,\"iyr\":2017,\"cid\":\"147\",\"hgt\":183}",
    ),
    Encoder.encodePassportLine("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm"),
  )
  stringResultEqual(
    Ok(
      "{\"iyr\":2013,\"ecl\":\"amb\",\"cid\":\"350\",\"eyr\":2023,\"pid\":\"028048884\",\"hcl\":\"#cfa07d\",\"byr\":1929}",
    ),
    Encoder.encodePassportLine("iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929"),
  )
})

test("encodePassports", () => {
  stringResultArrayEqual(
    [
      Ok(
        "{\"ecl\":\"gry\",\"pid\":\"860033327\",\"eyr\":2020,\"hcl\":\"#fffffd\",\"byr\":1937,\"iyr\":2017,\"cid\":\"147\",\"hgt\":183}",
      ),
    ],
    Encoder.encodePassports("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm"),
  )
  stringResultArrayEqual(
    [
      Ok(
        "{\"ecl\":\"gry\",\"pid\":\"860033327\",\"eyr\":2020,\"hcl\":\"#fffffd\",\"byr\":1937,\"iyr\":2017,\"cid\":\"147\",\"hgt\":183}",
      ),
      Ok(
        "{\"iyr\":2013,\"ecl\":\"amb\",\"cid\":\"350\",\"eyr\":2023,\"pid\":\"028048884\",\"hcl\":\"#cfa07d\",\"byr\":1929}",
      ),
      Ok(
        "{\"hcl\":\"#ae17e1\",\"iyr\":2013,\"eyr\":2024,\"ecl\":\"brn\",\"pid\":\"760753108\",\"byr\":1931,\"hgt\":179}",
      ),
      Ok(
        "{\"hcl\":\"#cfa07d\",\"eyr\":2025,\"pid\":\"166559648\",\"iyr\":2011,\"ecl\":\"brn\",\"hgt\":149.86}",
      ),
    ],
    Encoder.encodePassports("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
"),
  )
})
