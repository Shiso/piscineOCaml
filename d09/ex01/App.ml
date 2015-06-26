module type APP =
	sig
		type project = string * string * int
		val zero : project
		val combine : project -> project -> project
		val fail : project -> project
		val success : project -> project
	end

module App : APP =
	struct
		type project = string * string * int

		let zero = "", "", 0

		let combine x y = match x, y with
			| (nameX, statusX, gradeX), (nameY, statusY, gradeY) ->
				let newGrade = (gradeX + gradeY) / 2 in
				if newGrade > 80
				then (nameX^nameY, "success", newGrade)
				else (nameX^nameY, "failed", newGrade)

		let fail x = match x with | (name, _, _) -> (name, "failed", 0)

		let success x = match x with | (name, _, _) -> (name, "success", 80)

	end