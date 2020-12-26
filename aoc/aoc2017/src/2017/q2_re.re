open Belt;

let input = {
  let lines =
    Node_fs.readFileAsUtf8Sync("input/q2in.txt")
    ->Js.String2.trim
    ->Js.String2.split("\n");
  let parseInts = line =>
    line->Js.String2.split("\t")->Array.map(int_of_string);
  
  Js.log2("input=>", lines);
  lines->Array.map(parseInts);
  
};


  let liness =
    Node_fs.readFileAsUtf8Sync("input/testj.json");    
  Js.log2("lines=>", liness);


// //Js.log(input);

let part1 = () => {
  input
  ->Array.map(row =>
      Array.reduce(row, min_int, max) - Array.reduce(row, max_int, min)
    )
  ->Array.reduce(0, (+));
} /* */;

// part1()->Js.log;
// let getResultOfDivision = (input) => {
//     let len = Array.length(input);
//     for(i in 0 to len-1){
//         for(j in i+1 to len-1){
//             if(input[i] mod input[j] === 0)
//         }
//     }
// }
// let part2 = () => {
//     Array.(
//         input
//         ->map(row => getResultOfDivision(row))
//         ->reduce(0, (+))
//     )
