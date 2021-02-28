open Belt
open InstructionModel

let rec runInstruction = (appState: InstructionModel.appState, instructions) => {
  if appState.isFinished {
    appState
  } else {
    switch instructions[appState.currentInstructionIndex] {
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
              runInstruction(
                {
                  ...appState,
                  currentInstructionIndex: appState.currentInstructionIndex + 1,
                },
                instructions,
              )
            }

          | Accumulate(arg) => {
              ignore(
                appState.numberOfInstructionsVisited[
                  appState.currentInstructionIndex
                ] =
                  numberOfCurrentInstructionVisited + 1,
              )
              runInstruction(
                {
                  ...appState,
                  currentInstructionIndex: appState.currentInstructionIndex + 1,
                  accumulator: appState.accumulator + arg,
                },
                instructions,
              )
            }

          | Jump(arg) => {
              ignore(
                appState.numberOfInstructionsVisited[
                  appState.currentInstructionIndex
                ] =
                  numberOfCurrentInstructionVisited + 1,
              )
              runInstruction(
                {
                  ...appState,
                  currentInstructionIndex: appState.currentInstructionIndex + arg,
                },
                instructions,
              )
            }
          }
        }
      }
    }
  }
}
