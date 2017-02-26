type 'a t = 'a option

module Monad_instance : (Monad_class.S with type 'a t = 'a option) = struct
  type 'a t = 'a option

  let pure a =
    Some a

  let bind f = function
    | None -> None
    | Some a -> f a
end

module Monad = Monad_class.Extend(Monad_instance)

module Functor_instance = Functor_class.Of_monad(Monad_instance)
