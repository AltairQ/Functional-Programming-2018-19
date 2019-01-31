  type 'a t
  exception Empty of string
  val create: unit -> 'a t
  val push: 'a * 'a t -> unit
  val top: 'a t -> 'a
  val pop: 'a t -> unit
  val isEmpty: 'a t -> bool

