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

Js.log("Number of valid passports are: " ++ string_of_int(Belt_List.length(validPassports)))
