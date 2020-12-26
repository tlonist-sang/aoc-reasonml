open Belt;


let init = ref(1);
let limitFilter = s => {
  let rec go = () => {
    let v = Stream.next(s);
    Js.log2("v=>",v);
    switch (v^ >= 100) {
      | false => go()
      | true => {
        Js.log("Reached 100!") 
      }
    };
  };
  go();
};

let numberStream = input => {
  let state = input;
  let next = _ => {
    state :=  state^ + 1;
    Some(state);
  };
  Stream.from(next);
};

let answer = numberStream(init)->limitFilter->Js.log;
Js.log(answer);