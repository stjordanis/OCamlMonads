module type S = sig
  type _ t

  type state

  module Monad_instance : Monad_class.S with type 'a t = 'a t

  module Monad : module type of Monad_class.Extend(Monad_instance)

  module Functor_instance : module type of Functor_class.Of_monad(Monad_instance)

  val run : state -> 'a t -> state * 'a

  val get : state t

  val set : state -> unit t

  val modify : (state -> state) -> unit t
end

module Make(K : sig type t end) : (S with type state := K.t) = struct
  module Run = struct
    type 'a t = K.t -> K.t * 'a
  end

  type 'a t = 'a Run.t

  let run k ma =
    ma k

  module Monad_instance = struct
    type 'a t = 'a Run.t

    let pure a =
      fun k -> (k, a)

    let bind f ma =
      fun k ->
        let (k2, a) = ma k in
        (f a) k2
  end

  module Monad = Monad_class.Extend(Monad_instance)

  module Functor_instance = Functor_class.Of_monad(Monad_instance)

  let get =
    fun k -> (k, k)

  let set k =
    fun _ -> (k, ())

  let modify f =
    Monad.(get >>= fun k -> set (f k))
end
