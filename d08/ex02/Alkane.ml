let atomlist n = 
	let res = [new Atom.hydrogen;new Atom.hydrogen;] in
	let rec loop res = function
		| 0 -> res
		| n -> loop (res @ [new Atom.carbon;new Atom.hydrogen;new Atom.hydrogen;]) (n - 1)
	in
	loop res n

let alkaneList n : string =
	if n < 1 then failwith "invalid argument"
	else if n > 120 then ("Alkane n"^(string_of_int n))
	else begin
		let lst = [
			(1, "Methane");
			(2, "Ethane");
			(3, "Propane");
			(4, "n-Butane");
			(5, "n-Pentane");
			(6, "n-Hexane");
			(7, "n-Heptane");
			(8, "n-Octane");
			(9, "n-Nonane");
			(10, "n-Decane");
			(11, "n-Undecane");
			(12, "n-Dodecane");
			(13, "n-Tridecane");
			(14, "n-Tetradecane");
			(15, "n-Pentadecane");
			(16, "n-Hexadecane");
			(17, "n-Heptadecane");
			(18, "n-Octadecane");
			(19, "n-Nonadecane");
			(20, "n-eicosane");
			(21, "n-Heneicosane(R)");
			(22, "n-Docosane(R)");
			(23, "n-Tricosane(R)");
			(24, "n-Tetracosane");
			(25, "n-Pentacosane(R)");
			(26, "n-Hexacosane(R)");
			(27, "n-Heptacosane(R)");
			(28, "n-Octacosane(R)");
			(29, "n-Nonacosane");
			(30, "n-Triacontane(R)");
			(31, "n-Hentriacontane");
			(32, "n-Dotriacontane(R)");
			(33, "n-Tritriacontane(R)");
			(34, "n-Tetratriacontane(R)");
			(35, "n-Pentatriacontane(R)");
			(36, "n-Hexatriacontane(R)");
			(37, "n-Heptatriacontane");
			(38, "n-Octatriacontane");
			(39, "n-Nonatriacontane");
			(40, "n-Tetracontane(R)");
			(41, "n-Hentetracontane");
			(42, "n-Dotetracontane");
			(43, "n-Tritetracontane");
			(44, "n-Tetratetracontane");
			(45, "n-Pentatetracontane");
			(46, "n-Hexatetracontane");
			(47, "n-Heptatetracontane");
			(48, "n-Octatetracontane");
			(49, "n-Nonatetracontane");
			(50, "n-Pentacontane(R)");
			(51, "n-Henpentacontane");
			(52, "n-Dopentacontane");
			(53, "n-Tripentacontane");
			(54, "n-Tetrapentacontane");
			(55, "n-Pentapentacontane");
			(56, "n-Hexapentacontane");
			(57, "n-Heptapentacontane");
			(58, "n-Octapentacontane");
			(59, "n-Nonapentacontane");
			(60, "n-Hexacontane(R)");
			(61, "n-Henhexacontane");
			(62, "n-Dohexacontane");
			(63, "n-Trihexacontane");
			(64, "n-Tetrahexacontane");
			(65, "n-Pentahexacontane");
			(66, "n-Hexahexacontane");
			(67, "n-Heptahexacontane");
			(68, "n-Octahexacontane");
			(69, "n-Nonahexacontane");
			(70, "n-Heptacontane");
			(71, "n-Henheptacontane");
			(72, "n-Doheptacontane");
			(73, "n-Triheptacontane");
			(74, "n-Tetraheptacontane");
			(75, "n-Pentaheptacontane");
			(76, "n-Hexaheptacontane");
			(77, "n-Heptaheptacontane");
			(78, "n-Octaheptacontane");
			(79, "n-Nonaheptacontane");
			(80, "n-Octacontane");
			(81, "n-Henoctacontane");
			(82, "n-Dooctacontane");
			(83, "n-Trioctacontane");
			(84, "n-Tetraoctacontane");
			(85, "n-Pentaoctacontane");
			(86, "n-Hexaoctacontane");
			(87, "n-Heptaoctacontane");
			(88, "n-Octaoctacontane");
			(89, "n-Nonaoctacontane");
			(90, "n-Nonacontane");
			(91, "n-Hennonacontane");
			(92, "n-Dononacontane");
			(93, "n-Trinonacontane");
			(94, "n-Tetranonacontane");
			(95, "n-Pentanonacontane");
			(96, "n-Hexanonacontane");
			(97, "n-Heptanonacontane");
			(98, "n-Octanonacontane");
			(99, "n-Nonanonacontane");
			(100, "n-Hectane");
			(101, "n-Henihectane");
			(102, "n-Dohectane");
			(103, "n-Trihectane");
			(104, "n-Tetrahectane");
			(105, "n-Pentahectane");
			(106, "n-Hexahectane");
			(107, "n-Heptahectane");
			(108, "n-Octahectane");
			(109, "n-Nonahectane");
			(110, "n-Decahectane");
			(111, "n-Undecahectane");
			(112, "n-Dodecahectane");
			(113, "n-Tridecahectane");
			(114, "n-Tetradecahectane");
			(115, "n-Pentadecahectane");
			(116, "n-Hexadecahectane");
			(117, "n-Heptadecahectane");
			(118, "n-Octadecahectane");
			(119, "n-Nonadecahectane");
			(120, "n-Icosahectane");
		] in
		snd (
			List.hd (
				List.filter (
					fun x -> fst x = n
				) lst
			)
		)
	end

class alkane n =
	object (self)
		inherit Molecule.molecule (alkaneList n) (atomlist n)
	end


class methane =
	object (self)
		inherit alkane 1
	end

class ethane =
	object (self)
		inherit alkane 2
	end

class octane =
	object (self)
		inherit alkane 8
	end

