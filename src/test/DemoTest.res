open Test

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="intEqual", (a, b) => a === b, a, b)

let stringEqual = (~message=?, a: string, b: string) =>
  assertion(~message?, ~operator="stringEqual", (a, b) => a == b, a, b)

test("heightEncoder", () => {
  stringEqual("178", Demo.heightEncoder("178cm"))
  stringEqual("134.62", Demo.heightEncoder("53in"))
  stringEqual("invalid", Demo.heightEncoder("hello"))
})
