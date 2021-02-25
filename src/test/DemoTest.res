open Test
open Belt.Result

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="intEqual", (a, b) => a === b, a, b)

let stringEqual = (~message=?, a: string, b: string) =>
  assertion(~message?, ~operator="stringEqual", (a, b) => a == b, a, b)

let stringResultEqual = (
  ~message=?,
  a: Belt.Result.t<string, string>,
  b: Belt.Result.t<string, string>,
) => assertion(~message?, ~operator="stringEqual", (a, b) => a == b, a, b)

test("heightEncoder", () => {
  stringResultEqual(Ok("178"), Demo.heightEncoder("178cm"))
  stringResultEqual(Ok("134.62"), Demo.heightEncoder("53in"))
  stringResultEqual(Error("invalid height"), Demo.heightEncoder("hello"))
})
