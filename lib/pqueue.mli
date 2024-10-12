type 'a t
val make : int -> int -> ('a -> int) -> 'a t
val peak : 'a t -> 'a option
val top : 'a t -> 'a option
val insert : 'a t -> 'a -> unit
