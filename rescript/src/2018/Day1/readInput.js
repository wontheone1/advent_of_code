const fs = require("fs");

let input;

try {
  input = fs.readFileSync("./src/2018/Day1/input.txt", "utf8");
} catch (err) {
  console.error(err);
}

module.exports = { input };
