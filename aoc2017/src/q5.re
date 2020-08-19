open Belt;

let arr = 
    Node_fs.readFileAsUtf8Sync("input/q5in.txt")
    ->Js.String2.trim
    ->Js.String2.split("\n")
    ->Array.map(int_of_string);

let len = Array.length(arr);
let pos = ref(0);
let something = -112;

let getJump = (arr, pos):int => {
   switch(arr[pos^]){
       | Some(e) => e;
       | None => 0
   }
}

let rec escape = (arr, pos, step) => {
    if(pos^ >= len || pos^ < 0){
        step;
    }else{
        let cur = pos^;
        let jump = getJump(arr, pos);
        pos := jump + pos^;
        
        Js.Array.unsafe_set(arr, cur, jump+1);
        escape(arr, pos, step+1);
    }
}

Js.log(escape(arr, pos, 0));
