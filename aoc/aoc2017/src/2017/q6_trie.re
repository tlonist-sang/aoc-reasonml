open Belt;

/*
    - 가장 큰 값을 기준으로 분배 (distribute)
    - trie에 해당 분포가 존재하는지 확인,
        존재한다면 step을 return
        존재하지 않으면 trie에 저장, 계속 확인 (recursion)

    -
 */

let arr =
  Node_fs.readFileAsUtf8Sync("input/q6in.txt")
  ->Js.String2.trim
  ->Js.String2.split("\t")
  ->Array.map(int_of_string);

type node = {
  next: array(node),
  mutable exist: bool,
};

let create_node = (
  ~exist=false,
  ~next=Belt.Array.make(20, {exist:false, next:[||]}),
  (),
)=> {
  exist, next
};


let distribute = arr => {
  let length = Belt.Array.length(arr);
  let max = ref(min_int);
  let max_index = ref(0);

  for (i in 0 to length - 1) {
    switch (arr[i]) {
    | Some(v) =>
      if (v > max^) {
        max := v;
        max_index := i;
      }
    | None => ()
    };
  };

  let toAdd = max^ / (length - 1);
  Js.Array.unsafe_set(arr, max_index^, 0);

  if (toAdd === 0) {
    let max_index_ = max_index^;
    for (i in max_index_ + 1 to length - 1) {
      switch (arr[i]) {
      | Some(e) => Js.Array.unsafe_set(arr, i, e + 1)
      | None => ()
      };
    };
    max := max^ - (length - 1 - max_index_ - 1);
    for (i in 0 to max_index_) {
      switch (arr[i]) {
      | Some(e) => Js.Array.unsafe_set(arr, i, e + 1)
      | None => ()
      };
    };
    arr;
  } else {
    let added_arr = Js.Array.map(i => i + toAdd, arr);
    Js.Array.unsafe_set(added_arr, max_index^, max^ - toAdd * (length - 1));
    added_arr;
  };
};

let rec save = (index, arr, node: node): bool =>{
  if (index >= Array.length(arr)) {
    node.exist = true;
    node.exist;
  } else {
    switch (arr[index]) {
    | Some(v) =>
      switch (node.next[v]) {
      | Some(w) =>{
        Js.log2("save some=>",w);
        save(
          index + 1,
          arr,
          w //(2)
        )
      }
      | None =>{
        Js.log2("save none, making new. v =>",v);
        let inserted = create_node();
        Js.Array.unsafe_set(node.next, v, inserted);
        switch(node.next[v]){
          | Some(w) => save(index + 1, arr, w);
          | None => false
        }
      }
      }
    | None => false
    };
  }
  };

let rec check_trie = (index, arr, node: node): bool => {
  if (index >= Array.length(arr)) {
    if (node.exist == true) {
      true;
    } else {
      false;
    };
  } else {
    switch (arr[index]) {
    | Some(v) =>{
      switch (node.next[v]) {
      | Some(w) => {
        check_trie(index + 1, arr, w)
      }
      | None => {
        false
        }
      }
    }
    | None => {
      false
    }
    };
  };
};

let rec part1 = (arr, step, node) => {
  Js.log2("step=>",step);
  let distributed_array = distribute(arr);
  if (check_trie(0, distributed_array, node) == true) {
    Js.log(distributed_array);
    step+1;
  } else {
    let saved = save(0, distributed_array, node);
    part1(distributed_array, step + 1, node);
  };
};

let init_node: node = create_node();
save(0, [|0,2,7,0|], init_node);
Js.log("hello world!");
Js.log(check_trie(0, [|2,2,7,0|], init_node));


// let answer = part1(arr, 0, init_node);
// Js.log(answer);

//Js.log(arr);
//distribute(arr)

/*
 * - (0) max 구현 : external max : 'a -> 'a -> 'a = "%bs_max"
 * - (1) Some | Null 체크를 위해 계속 nested 되는 문제는 어떻게 해결할까
 * - (2) Some(v)에 넘겨지는 값은 얕은 복사?
 *
 */
