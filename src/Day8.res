open Belt

let instructionLines = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"

exception InvalidInstruction

let log = Js.log

type instructionKind =
  | NoOp
  | Accumulate(int)
  | Jump(int)

let instructionFromInstructionTuple = ((instruction, arg)) => {
  let arg = int_of_string(arg)
  switch instruction {
  | "nop" => NoOp
  | "acc" => Accumulate(arg)
  | "jmp" => Jump(arg)
  | _ => raise(InvalidInstruction)
  }
}

let parseInstructions = instructionLines => {
  Js.String2.splitByRe(instructionLines, %re("/\\n/"))
  ->ArrayUtil.removeNones
  ->Array.map(instructionLine => {
    let instruction = Js.String2.splitByRe(instructionLine, %re("/\s/"))->ArrayUtil.removeNones
    if Array.length(instruction) != 2 {
      raise(InvalidInstruction)
    }
    let maybeOpArg = Option.flatMap(instruction[0], op =>
      Option.map(instruction[1], arg => instructionFromInstructionTuple((op, arg)))
    )
    switch maybeOpArg {
    | None => raise(InvalidInstruction)
    | Some(instruction) => instruction
    }
  })
}

log(parseInstructions(instructionLines))
