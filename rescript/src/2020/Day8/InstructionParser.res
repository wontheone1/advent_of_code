open InstructionModel
open Belt

exception InvalidInstruction

let instructionFromInstructionTuple = ((instruction, arg)) => {
  let arg = int_of_string(arg)
  switch instruction {
  | "nop" => NoOp
  | "acc" => Accumulate(arg)
  | "jmp" => Jump(arg)
  | _ => raise(InvalidInstruction)
  }
}

let parseInstructions = (instructionLines: string): array<InstructionModel.instructionKind> => {
  Js.String2.splitByRe(instructionLines, %re("/\\n/"))
  ->ArrayUtil.removeNones
  ->Array.map((instructionLine: string) => {
    let instruction = Js.String2.splitByRe(instructionLine, %re("/\s/"))->ArrayUtil.removeNones
    if Array.length(instruction) != 2 {
      raise(InvalidInstruction)
    }
    switch (instruction[0], instruction[1]) {
    | (Some(op), Some(arg)) => instructionFromInstructionTuple((op, arg))
    | _ => raise(InvalidInstruction)
    }
  })
}
