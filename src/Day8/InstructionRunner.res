open Belt
open InstructionModel

let reducer = (action, appState) => {
  switch action {
  | None => {
      ...appState,
      isFinished: true,
    }
  | Some(instruction) => {
      let numberOfCurrentInstructionVisited = Option.getWithDefault(
        appState.numberOfInstructionsVisited[appState.currentInstructionIndex],
        0,
      )
      if numberOfCurrentInstructionVisited > 0 {
        {
          ...appState,
          isFinished: true,
        }
      } else {
        switch instruction {
        | NoOp => {
            ignore(
              appState.numberOfInstructionsVisited[
                appState.currentInstructionIndex
              ] =
                numberOfCurrentInstructionVisited + 1,
            )
            {
              ...appState,
              currentInstructionIndex: appState.currentInstructionIndex + 1,
            }
          }

        | Accumulate(arg) => {
            ignore(
              appState.numberOfInstructionsVisited[
                appState.currentInstructionIndex
              ] =
                numberOfCurrentInstructionVisited + 1,
            )
            {
              ...appState,
              currentInstructionIndex: appState.currentInstructionIndex + 1,
              accumulator: appState.accumulator + arg,
            }
          }

        | Jump(arg) => {
            ignore(
              appState.numberOfInstructionsVisited[
                appState.currentInstructionIndex
              ] =
                numberOfCurrentInstructionVisited + 1,
            )
            {
              ...appState,
              currentInstructionIndex: appState.currentInstructionIndex + arg,
            }
          }
        }
      }
    }
  }
}

let rec runInstruction = (appState: InstructionModel.appState, instructions) => {
  if appState.isFinished {
    appState
  } else {
    runInstruction(reducer(instructions[appState.currentInstructionIndex], appState), instructions)
  }
}
