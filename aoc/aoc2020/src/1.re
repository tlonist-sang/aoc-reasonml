let input =
  Node_fs.readFileAsUtf8Sync("1in.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(v => int_of_string(v))
  ->Belt.Array.reduce(
      Js.Dict.empty(),
      (acc, v) => {
        acc->Js.Dict.set(string_of_int(v), v);
        acc;
      },
    );

let keys = input->Js.Dict.keys;
let part2_keys =
  input->Js.Dict.keys->Belt.Array.map(v => 2020 - int_of_string(v));

let part1 =
  Js.Array.filter(
    v => {
      switch (input->Js.Dict.get(string_of_int(2020 - int_of_string(v)))) {
      | Some(_) => true
      | None => false
      }
    },
    keys,
  );

let part2 =
  part2_keys->Belt.Array.keepWithIndex((v, i) => {
    let temp =
      Js.Array.filter(
        w => {
          switch (input->Js.Dict.get(string_of_int(v - int_of_string(w)))) {
          | Some(x) =>
            Js.log4(
              "(v1, v2, i)=>",
              int_of_string(w),
              x,
              2020 - part2_keys->Belt.Array.getUnsafe(i),
            );
            true;
          | None => false
          }
        },
        keys,
      );
    temp->Js.Array.length == 2 ? true : false;
  });

part1->Belt.Array.reduce(1, (acc, v) => acc * int_of_string(v))->Js.log;
part2->Js.log;
