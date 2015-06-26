module type MONOID =
	sig
		type element
		val zero1 : element
		val zero2 : element
		val mul : element -> element -> element
		val add : element -> element -> element
		val div : element -> element -> element
		val sub : element -> element -> element
	end

module MonoidInt : MONOID =
struct
	type element = int

	let zero1 = 0
	let zero2 = 1

	let mul = ( * )
	let add = ( + )
	let div = ( / )
	let sub = ( - )

end
module MonoidFloat : MONOID =
struct
	type element = float

	let zero1 = 0.
	let zero2 = 1.

	let mul = ( *. )
	let add = ( +. )
	let div = ( /. )
	let sub = ( -. )

end

module Calc :
	functor (M : MONOID) ->
	sig
		val add : M.element -> M.element -> M.element
		val sub : M.element -> M.element -> M.element
		val mul : M.element -> M.element -> M.element
		val div : M.element -> M.element -> M.element
		val power : M.element -> int -> M.element
		val fact : M.element -> M.element
	end =
	functor (M : MONOID) ->
	struct
		let add x y = M.add x y
		let sub x y = M.sub x y
		let mul x y = M.mul x y
		let div x y = M.div x y
		let rec power a i = if i = 1 then M.zero2 else M.mul a (power a (i - 1))

		let rec fact x =
			if x = M.zero2 then M.zero2
			else if x > M.zero2 then M.add x (fact (M.sub x M.zero2))
			else invalid_arg "x invalid"

	end

