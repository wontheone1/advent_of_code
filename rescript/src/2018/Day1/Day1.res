@module("./readInput") external input: string = "input"

let input = Node.Fs.readFileSync("../inputs/2018/Day1/input.txt", #utf8)

Js.log2("input:", input)
