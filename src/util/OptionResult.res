open Belt

let toResult = (opt: option<'a>, err: 'b): Result.t<'a, 'b> =>
  switch opt {
  | None => Result.Error(err)
  | Some(x) => Result.Ok(x)
  }

let toOption = (result: Result.t<'t, 'e>): option<'t> =>
  switch result {
  | Error(_) => None
  | Ok(x) => Some(x)
  }
