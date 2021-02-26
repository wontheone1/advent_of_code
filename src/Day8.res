let log = Js.log

let instructionLines = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"

let parseInstructions = instructionLines => {
  Js.String2.splitByRe(instructionLines, %re("/\\n/"))
}

log(parseInstructions(instructionLines))
