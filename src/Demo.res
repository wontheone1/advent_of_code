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

Js.log("Number of valid passports are: " ++ string_of_int(Belt_List.length(validPassports)))
