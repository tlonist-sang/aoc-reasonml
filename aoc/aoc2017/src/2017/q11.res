open Belt

let arr = Node_fs.readFileAsUtf8Sync("input/q11in.txt")
->Js.String2.split(",");


type pos = {
    x: int,
    y: int,
}
let compass = (dir, cur: pos) => {
    switch(dir) {
        | "nw" => {x: cur.x-1, y: cur.y+1}
        | "n" => {...cur, y: cur.y+1}
        | "ne" => {x: cur.x+1, y: cur.y+1}
        | "se" => {x: cur.x+1, y: cur.y-1}
        | "s" => {...cur, y: cur.y-1}
        | "sw"=> {x: cur.x-1, y: cur.y-1}
        | _=> cur
    }
}

let position = {x: 0, y: 0}
let max = ref(Js.Int.min);
let find_shortest = (input: pos) => {
    let x = Js.Math.abs_int(input.x);
    let y = Js.Math.abs_int(input.y);
    x>y?x:y;
}

let get_position = (arr, input: pos) => {
    let rec go = (index, input) => {
        switch(arr[index]){
            | Some(v) => {
                let update = compass(v, input);
                //part2
                let v = find_shortest(update)
                if(max.contents < v){
                    max:= v;
                }
                go(index+1, update)
            }
            | None => {
                input;
            }
        }
    }
    go(0, input)
}


//get_position(arr, position)->find_shortest->Js.log;
get_position(arr, position);
Js.log(max.contents)
