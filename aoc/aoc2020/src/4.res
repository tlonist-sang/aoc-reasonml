let input = Node_fs.readFileAsUtf8Sync("4in.txt")

type passport = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
}

let byr = input =>
  input->Belt.Option.flatMap(v =>
    switch v->Js.Dict.get("byr") {
    | Some(v) => v->int_of_string >= 1920 && v->int_of_string <= 2002 ? input : None
    | None => None
    }
  )

let iyr = input =>
  input->Belt.Option.flatMap(v =>
    switch v->Js.Dict.get("iyr") {
    | Some(v) => v->int_of_string >= 2010 && v->int_of_string <= 2020 ? input : None
    | None => None
    }
  )

let eyr = input =>
  input->Belt.Option.flatMap(v =>
    switch v->Js.Dict.get("eyr") {
    | Some(v) => v->int_of_string >= 2020 && v->int_of_string <= 2030 ? input : None
    | None => None
    }
  )

let hgt = input => {
  let regex = Js.Re.fromString("^((1[5-8][0-9]|19[0-3])(cm))|^((59|6[0-9]|7[0-6])(in))$")
  input->Belt.Option.flatMap(v =>
    switch v->Js.Dict.get("hgt") {
    | Some(v) => Js.String2.match_(v, regex)->Belt.Option.flatMap(r => input)
    | None => None
    }
  )
}
let hcl = input => {
  let regex = Js.Re.fromString("^#[0-9a-z]{6}$")
  input->Belt.Option.flatMap(v =>
    switch v->Js.Dict.get("hcl") {
    | Some(v) => Js.String2.match_(v, regex)->Belt.Option.flatMap(r => input)
    | None => None
    }
  )
}

let ecl = input => {
  let regex = Js.Re.fromString("^(amb|blu|brn|gry|grn|hzl|oth)$")
  input->Belt.Option.flatMap(v =>
    switch v->Js.Dict.get("ecl") {
    | Some(v) => Js.String2.match_(v, regex)->Belt.Option.flatMap(r => input)
    | None => None
    }
  )
}

let pid = input => {
  let regex = Js.Re.fromString("^\d{9}$")
  input->Belt.Option.flatMap(v =>
    switch v->Js.Dict.get("pid") {
    | Some(v) => Js.String2.match_(v, regex)->Belt.Option.flatMap(r => input)
    | None => None
    }
  )
}

let passportify = input => {
  let obj = Js.Dict.empty()
  let validators = [byr, iyr, eyr, hgt, hcl, ecl, pid]
  input
  ->Belt.Array.map(v =>
    switch v->Js.String2.substring(~from=0, ~to_=3) {
    | "byr" => obj->Js.Dict.set("byr", v->Js.String2.substr(~from=4))
    | "iyr" => obj->Js.Dict.set("iyr", v->Js.String2.substr(~from=4))
    | "eyr" => obj->Js.Dict.set("eyr", v->Js.String2.substr(~from=4))
    | "hgt" => obj->Js.Dict.set("hgt", v->Js.String2.substr(~from=4))
    | "hcl" => obj->Js.Dict.set("hcl", v->Js.String2.substr(~from=4))
    | "ecl" => obj->Js.Dict.set("ecl", v->Js.String2.substr(~from=4))
    | "pid" => obj->Js.Dict.set("pid", v->Js.String2.substr(~from=4))
    | "cid" => obj->Js.Dict.set("cid", v->Js.String2.substr(~from=4))
    | _ => ()
    }
  )
  ->ignore

  switch validators->Belt.Array.reduce(Some(obj), (acc, v) => acc->v) {
  | Some(v) =>
    Some({
      byr: v->Js.Dict.unsafeGet("byr"),
      iyr: v->Js.Dict.unsafeGet("iyr"),
      eyr: v->Js.Dict.unsafeGet("eyr"),
      hgt: v->Js.Dict.unsafeGet("hgt"),
      hcl: v->Js.Dict.unsafeGet("hcl"),
      ecl: v->Js.Dict.unsafeGet("ecl"),
      pid: v->Js.Dict.unsafeGet("pid"),
    })
  | None => None
  }
}

let parse = input => {
  input
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(v => v->Js.String2.split("\n")->Belt.Array.map(w => w->Js.String2.split(" ")))
  ->Belt.Array.map(v => v->Belt.Array.reduce([], (acc, v) => acc->Belt.Array.concat(v)))
  ->Belt.Array.map(v => v->passportify)
}

let process = parsed => {
  parsed->Belt.Array.keepMap(v => v)->Array.length
}

input->parse->process->Js.log
