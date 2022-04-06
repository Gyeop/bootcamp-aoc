const fs = require("fs");
const sample = fs.readFileSync("input/Week1/Year2020Day5.sample.txt", "utf8");
const passes = sample.split("\n");

/**
 * @param {string} string "F", "B", "R", "L"
 * @returns {boolean} 상하좌우 중 "상", "좌"를 "앞", "하", "우"를 "뒤"라고 표현하며, 앞인 경우 true, 뒤인경우 false를 반환합니다.
 */
function checkLocation(string) {
  switch (string) {
    case "F":
    case "L":
      return true;

    case "B":
    case "R":
      return false;
    default:
      throw new Error("잘못된 위치정보입니다.");
  }
}

/**
 * @param {string} locations "FBFBBFB", "RFRRF"등과 같은 위치를 추적할 문자열입니다.
 * @returns {number} 좌석의 위치정보입니다.
 */
function getSeatNumber(locations) {
  const maxSeatCount = 2 ** locations.length - 1;
  const splittedLocations = locations.split("");

  /**
   * 주어진 문자열에서 좌석의 범위를 좁혀나가기 위한 재귀함수입니다.
   * @param {number} minSeatCount 좌석의 최소 위치값입니다.
   * @param {number} maxSeatCount 좌석의 최대 위치값입니다.
   * @param {string} index splittedLocations에서의 index입니다.
   * @returns {number} 좌석의 위치정보입니다.
   */
  function recur(minSeatCount, maxSeatCount, index) {
    if (minSeatCount === maxSeatCount) {
      return minSeatCount;
    } else {
      const result = checkLocation(splittedLocations[index]);
      const sum = minSeatCount + maxSeatCount;

      const nextMin = result ? minSeatCount : Math.ceil(sum / 2);
      const nextMax = result ? Math.floor(sum / 2) : maxSeatCount;
      return recur(nextMin, nextMax, index + 1);
    }
  }

  return recur(0, maxSeatCount, 0);
}

/**
 * @param {string} locations "FBFBBFB", "RFRRF"등과 같은 위치를 추적할 문자열입니다.
 * @returns {string[]} [rowLocations, colLocation] 형태의 tuple입니다. ex) ["FBFFBF", "RLLRLRLR"]
 */
function splitPass(locations) {
  const rowCharacters = locations.slice(0, 7);
  const columnCharacters = locations.slice(7);
  return [rowCharacters, columnCharacters];
}

/**
 * @param {number[]} 튜플 형태로 이루어진 좌석의 위치입니다.
 * @returns {number} 좌석의 id입니다.
 */
function getSeatId([row, col]) {
  return row * 8 + col;
}

const maxPassId = Math.max(
  ...passes
    .map(splitPass)
    .map((locations) => locations.map(getSeatNumber))
    .map(getSeatId)
);

console.log(maxPassId);
