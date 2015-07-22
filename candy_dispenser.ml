type input =
  | Insert_coin
  | Turn

module Machine_state = struct
  type t = {
    locked: bool;
    candies: int;
    coins : int
  }
end

type t = Machine_state.t

let make ~candies ~coins =
  Machine_state.{ locked = true; candies; coins }

module Machine = State.Make(Machine_state)

let insert_coin =
  Machine.modify (fun s ->
      let open Machine_state in
      if s.locked && (s.coins > 0) then
        { s with locked = false; coins = s.coins + 1 }
      else s)

let turn =
  Machine.modify (fun s ->
      let open Machine_state in
      if s.locked then s
      else
        { s with locked = true; candies = s.candies - 1 })

let next input =
  let open Machine_state in
  let open Machine.Monad in
  Machine.get >>= fun s ->
  if s.candies = 0 then pure (lazy ())
  else
    match input with
  | Turn -> turn
  | Insert_coin -> insert_coin

let run inputs t =
  let open Machine_state in
  Machine.Monad.sequence (List.map next inputs)
  |> Machine.run t
  |> fst
  |> fun s -> (s.candies, s.coins)
