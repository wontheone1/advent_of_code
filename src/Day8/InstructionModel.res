type instructionKind =
  | NoOp
  | Accumulate(int)
  | Jump(int)


type appState = {
  isFinished: bool,
  accumulator: int,
  currentInstructionIndex: int,
  numberOfInstructionsVisited: array<int>,
}

let makeInitialState = (instructionLength) => {
  isFinished: false,
  accumulator: 0,
  currentInstructionIndex: 0,
  numberOfInstructionsVisited: Array.make(instructionLength, 0),
}
