type hairColor<'a> = [> #Black | #Blond | #Ginger] as 'a
type eyeColor<'a> = [> #Black | #Blue | #Emerald] as 'a

type passport<'a, 'b> = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: float,
  hcl: hairColor<'a>,
  ecl: eyeColor<'b>,
  pid: string,
  cid: option<string>,
}

let myPassport = {
  byr: 1999,
  iyr: 2015,
  eyr: 2025,
  hgt: 175.5,
  hcl: #Black,
  ecl: #Blue,
  pid: "860033327",
  cid: None,
}

let validPassports = list{myPassport}

let passportLines = "ecl:gry pid:860033327 eyr:2020 hcl:#fffff
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

let parsePassportLine = passportLine => {
  let passportFields = Js.String2.splitByRe(passportLine, %re("/\s/"))->Belt_Array.map(field => {
    switch field {
    | None => []
    | Some(field) => Js.String2.split(field, ":")
    }
  })
  Js.log(passportFields)
}

parsePassportLine(Belt.Option.getWithDefault(passports[0], ""))

Js.log("Number of valid passports are: " ++ string_of_int(Belt_List.length(validPassports)))
