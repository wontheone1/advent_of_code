// @module("./readInput") external input: string = "input"

open NodeJs
let input2 = Fs.readFileSync("./src/2018/Day1/input.txt", ())
Fs.FileHandle.readFileWith(input2, ~encoding="UTF-8")

Js.log2("input2", input2)
