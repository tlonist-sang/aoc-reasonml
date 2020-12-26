open Belt;

/*
    - **trie 포기 버전**
    - 가장 큰 값을 기준으로 분배 (distribute)
    - hub(list) 에서 분배된 배열이 존재하는지 확인
    - 존재하지 않는다면 insert, 존재한다면 step return
 */

let arr =
  Node_fs.readFileAsUtf8Sync("input/q6in.txt")
  ->Js.String2.trim
  ->Js.String2.split("\t")
  ->Array.map(int_of_string);

let hub = [||];
let hub_index = ref(0);

let distribute = arr => {
  let length = Array.length(arr);
  let max_index = Garter.Array.maxIndex(arr);
  let max = Array.getUnsafe(arr, max_index);
  
  let rec f = (max, i) => 
    if(max > 0){ //keep on distributing
      Garter.Array.updateUnsafe(arr, i, (+)(1));  //increase arr[i] by 1
      f(max-1, (i+1) mod length);
    }else{
      arr;
    }
  f(max, (max_index+1) mod length);
};

let save = (arr, hub, hub_index) => {
  let hub_index_ = hub_index^;
  Js.Array.unsafe_set(hub, hub_index_, Js.Array.copy(arr));
  hub_index := hub_index^ + 1;
}

let rec check = (arr, index, hub, hub_index)=> {
  let hub_index_ = hub_index^;
  //Js.log2("check_hub=>", hub);
  if(index >= hub_index_){
    false;
  }else{
    switch(hub[index]){
      | Some(hub_arr)=>{
        if(hub_arr == arr){
          true;
        }else{
          check(arr, index+1, hub, hub_index);  
        }
      }
      | None => {
        false;
      }
    }
  }
}

let rec checkidx = (arr, index, hub, hub_index)=> {
  let hub_index_ = hub_index^;
  //Js.log2("check_hub=>", hub);
  if(index >= hub_index_){
    -1;
  }else{
    switch(hub[index]){
      | Some(hub_arr)=>{
        if(hub_arr == arr){
          index;
        }else{
          checkidx(arr, index+1, hub, hub_index);  
        }
      }
      | None => {
        -1
      }
    }
  }
}

let rec part1 = (arr, step, hub) => {
  let distributed_array = distribute(arr);
  if (check(distributed_array, 0, hub, hub_index) == true) {
    Js.log("array is found!");
    Js.log2("index=>", checkidx(distributed_array, 0, hub, hub_index));
    step+1;
  } else {
    let saved = save(distributed_array, hub, hub_index);
    part1(distributed_array, step + 1, hub);  };
};

save(arr, hub, hub_index);
Js.log(part1(arr, 0, hub));

