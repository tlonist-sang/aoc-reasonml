open Belt;

let pair = ("name", "age");
let triple = ("hello", 1, true);

Js.log2("pair=>", pair);
Js.log2("triple=>",triple);

let (_, second) = pair;
let (first, _, third) = triple;

Js.log2("second=>", second);
Js.log2("first=>", first);
Js.log2("third=>", third);

let pattern_match = (triple) => {
    switch (triple) {
    | ("hello", 1, false) => print_endline("first");
    | ("none", 1, false) => print_endline("second");
    | (_, _, true) => print_endline("third");
    | (_, _, _) => print_endline("default");
    };
}

pattern_match(triple);