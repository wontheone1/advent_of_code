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

let log = Js.log

let instructions = InstructionParser.parseInstructions(instructionLines)

let initialState: InstructionModel.appState = {
  isFinished: false,
  accumulator: 0,
  currentInstructionIndex: 0,
  numberOfInstructionsVisited: Array.make(Array.length(instructions), 0),
}

let printAccumulatorBeforeAnyInstructionIsExecutedForSecondTime = (
  appState: InstructionModel.appState,
) =>
  log(
    "The value immediately before any instruction is executed for the second time is: " ++
    string_of_int(appState.accumulator),
  )

let finalState = InstructionRunner.runInstruction(initialState, instructions)

log(finalState)
printAccumulatorBeforeAnyInstructionIsExecutedForSecondTime(finalState)