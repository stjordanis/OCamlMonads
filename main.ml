open Printf

let () =
  let open Candy_dispenser in
  let dispenser = make ~candies:10 ~coins:3 in
  let (candies, coins) = dispenser |> run [Insert_coin; Turn; Insert_coin; Turn; Turn] in
  printf "Got %d candies remaining with %d coins.\n" candies coins
