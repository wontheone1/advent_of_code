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

type appState = {
  isFinished: bool,
  accumulator: int,
  currentInstructionIndex: int,
  numberOfInstructionsVisited: array<int>,
}

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

let instructions = parseInstructions(instructionLines)

let initialState: appState = {
  isFinished: false,
  accumulator: 0,
  currentInstructionIndex: 0,
  numberOfInstructionsVisited: Array.make(Array.length(instructions), 0),
}

let rec runInstruction = (appState, instructions) => {
  if appState.isFinished {
    appState
  } else {
    let instructionToRun = instructions[appState.currentInstructionIndex]
    switch instructionToRun {
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

let printAccumulatorBeforeAnyInstructionIsExecutedForSecondTime = appState =>
  log("The value immediately before any instruction is executed for the second time is: " ++ string_of_int(appState.accumulator))

log(parseInstructions(instructionLines))
log(initialState)

let finalState = runInstruction(initialState, instructions)

log(finalState)
printAccumulatorBeforeAnyInstructionIsExecutedForSecondTime(finalState)
