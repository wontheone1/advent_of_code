open Belt

exception InvalidInstruction

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

let instructionTuplesFromInstructionLines = instructionLines => {
  Js.String2.splitByRe(instructionLines, %re("/\\n/"))
  ->ArrayUtil.removeNones
  ->Array.map(instructionLine => {
    let instruction = Js.String2.splitByRe(instructionLine, %re("/\s/"))->ArrayUtil.removeNones
    if Array.length(instruction) != 2 {
      raise(InvalidInstruction)
    }
    let maybeOpArg = Option.flatMap(instruction[0], op =>
      Option.map(instruction[1], arg => (op, arg))
    )
    switch maybeOpArg {
    | None => raise(InvalidInstruction)
    | Some((op, arg)) => (op, arg)
    }
  })
}

let parseInstructions = instructionLines => {
  let instructionTuples = instructionTuplesFromInstructionLines(instructionLines)
  
  instructionTuples
}

log(parseInstructions(instructionLines))
