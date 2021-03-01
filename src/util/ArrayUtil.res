open Belt

let removeNones = arr => {
  Array.keepMap(arr, Standard.identity)
}
