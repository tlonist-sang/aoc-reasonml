/**
 * O(n) solution
 * 1. read from the beginning, update max and min
 * 2. calculate the difference in the end, add to ans
 */


/**
 * 질문
 * 1) 언제 Js.String을 쓰고 언제 String 만 쓸 수 있을까? (Js.Array, Js.List)
 * 2) Switch - Some - None 시 Some(arg)와 swtich(arg)는 왜 다를까?
 * 
 * 
 * 'a => generic type
 *  * array는 spread가 안됨 (list만 됨)
 
 */


open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/q2in.txt")
  ->Js.String2.split("\n")
  ->Array.map(row => Js.String.split("\t", row));



let len = Array.length(input);
let answer = 0;

let getIntValue = (input) => {
    switch(input){
        | Some(e) => (e)
        | None => 0
    }
}

let calculate_min_max_diff = (arr) =>{
    let len = Array.length(arr);
    let min_value = Js.Int.max;
    let max_value = Js.Int.min;
    for(i in 0 to len-1){

        let arri = getIntValue(arr[i]);
        Js.log("=============");
        Js.log(i);
        Js.log(arri);
        Js.log(max_value);
        Js.log(min_value);
        Js.log("=============");

        
        if(arri > max_value){
            Js.log("max-printing");
            let max_value = max_value-max_value + arri;
            Js.log(max_value);
        }else{
            Js.log("max-printing");
            Js.log(max_value);
        }
        
        if(arri < min_value){
            Js.log("min-printing");
            let min_value = min_value-min_value + arri;
            Js.log(min_value);
        }else{
            Js.log("min-printing");
            Js.log(min_value);
        }
    }
    max_value - min_value;
}
// 함수 검증은 따로 가능
let test = calculate_min_max_diff([|1,2,3,4|])
Js.log(test);
let solve = (arr) => {
    switch(arr) {
        | Some(row) => calculate_min_max_diff(row);
        | None => 0
    }
}

let get_rid_of_option = (input) => {
    switch(input){
        | Some(r) => Array.map(r, r=>int_of_string(r));
        | None => [||];
    }
}

// for(i in 0 to len){
//     let no_option = get_rid_of_option(input[i]);
//     let answer = answer + calculate_min_max_diff(no_option);
// }

// Js.log(answer);



