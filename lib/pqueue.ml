type 'a t = {
    d : int;
    arr : 'a option array;
    priority_fn : 'a -> int;
    mutable length : int;
}

let children d i = List.init d (fun o -> (i * 2) + 1 + o)
let parent d i = (i - 1) / d

let priority_of_index { arr; length; priority_fn; _ } i =
  if i < length then Option.map priority_fn arr.(i) else None

let push_down ({ arr; length; priority_fn; d } as pq) i =
  let v = arr.(i) in
  let priority = priority_fn (Option.get v) in
  let i = ref i in
  let continue = ref true in
  while !i < length - 1 && !continue do
    let children_is = children d !i in
    let cipris =
      List.filter_map
        (fun i -> Option.map (fun p -> (p, i)) (priority_of_index pq i))
        children_is
    in
    let cipris = List.sort (fun (a, _) (b, _) -> Int.compare b a) cipris in
    match cipris with
    | (child_priority, child_index) :: _ when priority < child_priority ->
        arr.(!i) <- arr.(child_index);
        i := child_index
    | _ -> continue := false
  done;
  arr.(!i) <- v

let bubble_up { arr; priority_fn; d; _ } i =
  if i > 0
  then begin
      let v = arr.(i) in
      let priority = priority_fn (Option.get v) in
      let i = ref i in
      let continue = ref true in
      while !i > 0 && !continue do
        let parent_index = parent d !i in
        let parent = arr.(parent_index) in
        if priority > priority_fn (Option.get parent)
        then (arr.(!i) <- parent;
              i := parent_index)
        else continue := false
      done;
      arr.(!i) <- v
    end

let make d cap priority_fn = { d; arr = Array.make cap None; length = 0; priority_fn }

let peak { arr; length; _ } = if length = 0 then None else arr.(0)

let top ({ arr; length; _ } as pq) =
  match length with
  | 0 -> None
  | 1 ->
      pq.length <- length - 1;
      arr.(0)
  | _ ->
      pq.length <- length - 1;
      let t = arr.(0) in
      arr.(0) <- arr.(length - 1);
      push_down pq 0;
      t

let insert ({ arr; length; _ } as pq) x =
  if length <> Array.length arr
  then begin
      arr.(length) <- (Some x);
      pq.length <- length + 1;
      bubble_up pq length
    end
