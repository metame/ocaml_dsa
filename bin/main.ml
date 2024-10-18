open Dsa

let test_pqueue () =
  print_endline "Testing Priority Queue d-ary heap!";
  let open PQ in
  let pq = make 3 10 Fun.id in
  insert pq 5;
  assert (peak pq = Some 5);
  insert pq 10;
  assert (peak pq = Some 10);
  insert pq 7;
  assert (peak pq = Some 10);
  insert pq 21;
  assert (peak pq = Some 21);
  insert pq 16;
  assert (peak pq = Some 21);
  assert (top pq = Some 21);
  assert (peak pq = Some 16);
  assert (top pq = Some 16);
  assert (top pq = Some 10);
  assert (top pq = Some 7);
  assert (top pq = Some 5);
  assert (top pq = None);
  assert (peak pq = None);
  print_endline "Priority Queue tests passed!";
     ()


let () = print_endline "Testing DSA!"

let () = test_pqueue ()

let () = print_endline "DSA tests passed!"
