open Belt;

let arr = Node_fs.readFileAsUtf8Sync("input/q16in.txt")
->Js.String2.split(",");
let original: array<string>= ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p"];
let rec solve = (arr, original, index) => {
    switch(arr[index]){
        | Some(v)=>{
            if(Js.String2.startsWith(v, "x")){
                let key = Js.String2.substr(v, ~from=1);
                let keyArr = Js.String2.split(key, "/")->Array.map(int_of_string);
                
                switch(keyArr[0]){
                    |Some(v)=>{
                        switch(keyArr[1]){
                            |Some(w)=>{
                                let temp = Js.Array.unsafe_get(original, v);
                                Js.Array.unsafe_set(original, v,Js.Array.unsafe_get(original, w));
                                Js.Array.unsafe_set(original, w, temp);
                            }
                            |None=>{
                                ()
                            }
                        }
                    }
                    |None=>{
                        ()
                    }
                }
                //Js.log2("keyArr=>",keyArr);
                solve(arr, original, index+1);
            }else if(Js.String2.startsWith(v, "s")){
                let key = int_of_string(Js.String2.substr(v, ~from=1));
                let original1 = Js.Array2.sliceFrom(original, key)
                let original2 = Js.Array2.slice(original, ~start=0, ~end_=key);
                let newOriginal = Js.Array2.concat(original1, original2);            
                solve(arr, newOriginal, index+1);
            }else{
                let key = Js.String2.substr(v, ~from=1);
                let keyArr = Js.String2.split(key, "/");

                let temp1 = Js.Array2.unsafe_get(keyArr,0);
                let temp2 = Js.Array2.unsafe_get(keyArr,1);

                let rec go = (input, index) => {
                    switch(input[index]){
                        |Some(v)=>{
                            if(v==Js.Array2.unsafe_get(keyArr,0)){
                                Js.Array2.unsafe_set(input, index, temp2);
                            }else if(v==Js.Array2.unsafe_get(keyArr, 1)){
                                Js.Array2.unsafe_set(input, index, temp1);
                            }else{
                                ()
                            }
                        }
                        |None=>{
                            ()
                        }
                    }
                }

                go(original, 0);
                solve(arr, original, index+1);
            }
        }
        | None => {
            ()
        }
    }
}
solve(arr, original, 0);

Js.log(original);
