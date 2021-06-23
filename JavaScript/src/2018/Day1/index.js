const { input } = require("./readInput");

const frequencyChanges = input
  .trim()
  .split(/\r?\n/)
  .map((item) => Number(item));

const sum = frequencyChanges.reduce((acc, num) => acc + num, 0);

console.log("Part1:", sum);

// reduceWhile could be used   https://ramdajs.com/docs/#reduceWhile
let seenFrequencies = {};
let currentFrequency = 0;
let i = 0;

while (!seenFrequencies[String(currentFrequency)]) {
  let key = String(currentFrequency);
  seenFrequencies[key] = true;
  currentFrequency = currentFrequency + frequencyChanges[i];
  //   console.log(currentFrequency, i, frequencyChanges.length);
  if (i + 1 < frequencyChanges.length) {
    i++;
  } else {
    i = 0;
  }
}

let firstFrequencySeenTwice = currentFrequency;

console.log("Part2:", firstFrequencySeenTwice);
