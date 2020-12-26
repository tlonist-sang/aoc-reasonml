module rec A: {
  type t = {
    name: string,
    children: ASet.t,
  };
  let compare: (t, t) => int;
} = {
  type t = {
    name: string,
    children: ASet.t,
  };
  let compare = (t1, t2) => compare(t1.name, t2.name);
}
and ASet: Set.S with type elt = A.t = Set.Make(A);


