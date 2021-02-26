open Test
open InstructionModel

let instructionKindArrayEqual = (
  ~message=?,
  a: array<InstructionModel.instructionKind>,
  b: array<InstructionModel.instructionKind>,
) => assertion(~message?, ~operator="instructionKindArrayEqual", (a, b) => a == b, a, b)

let appStateEqual = (~message=?, a: InstructionModel.appState, b: InstructionModel.appState) =>
  assertion(~message?, ~operator="appStateEqual", (a, b) => a == b, a, b)

test("Parse instructions successfully", () => {
  instructionKindArrayEqual(
    [
      NoOp,
      Accumulate(1),
      Jump(4),
      Accumulate(3),
      Jump(-3),
      Accumulate(-99),
      Accumulate(1),
      Jump(-4),
      Accumulate(6),
    ],
    InstructionParser.parseInstructions("nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"),
  )
})

test("Running instructions find out solution correctly", () => {
  let instructions = InstructionParser.parseInstructions("nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
  appStateEqual(
    InstructionRunner.runInstruction(
      InstructionModel.makeInitialState(Belt_Array.length(instructions)),
      instructions,
    ),
    {
      isFinished: true,
      accumulator: 5,
      currentInstructionIndex: 1,
      numberOfInstructionsVisited: [1, 1, 1, 1, 1, 0, 1, 1, 0],
    },
  )
})
