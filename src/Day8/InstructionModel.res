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
