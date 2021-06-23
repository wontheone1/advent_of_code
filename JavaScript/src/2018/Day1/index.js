const { input } = require("./readInput");

const frequencies = input.split(/\r?\n/).map((item) => Number(item));

const sum = frequencies.reduce((acc, num) => acc + num, 0);

console.log(sum);
