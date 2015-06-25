class virtual atom name symbol atomic_number =
	object (self)

		val _name = name
		val _symbol = symbol
		val _atomic_number = atomic_number

		method get_name : string = _name
		method get_symbol : string = _symbol
		method get_atomic_number : int = _atomic_number

		method to_string : string = ((self#get_symbol)^(String.make (5 - (String.length (self#get_symbol))) ' ')^":  "^(self#get_name)^(String.make (20 - (String.length (self#get_name))) ' ')^"("^(string_of_int self#get_atomic_number))^")"
	
	end


class hydrogen name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class helium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class lithium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class beryllium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class boron name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class carbon name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class nitrogen name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class oxygen name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class fluorine name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class neon name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class sodium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class magnesium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class aluminium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class silicon name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class phosphorus name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class sulfur name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class chlorine name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class argon name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class potassium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class calcium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class scandium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class titanium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class vanadium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class chromium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class manganese name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class iron name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class cobalt name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class nickel name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class copper name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class zinc name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class gallium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class germanium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class arsenic name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class selenium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class bromine name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class krypton name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class rubidium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class strontium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class yttrium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class zirconium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class niobium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class molybdenum name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class technetium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class ruthenium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class rhodium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class palladium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class silver name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class cadmium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class indium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class tin name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class antimony name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class tellurium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class iodine name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class xenon name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class cesium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class barium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class lanthanum name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class cerium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class praseodymium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class neodymium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class promethium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class samarium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class europium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class gadolinium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class terbium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class dysprosium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class holmium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class erbium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class thulium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class ytterbium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class lutetium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class hafnium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class tantalum name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class tungsten name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class rhenium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class osmium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class iridium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class platinum name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class gold name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class mercury name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class thallium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class lead name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class bismuth name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class polonium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class astatine name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class radon name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class francium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class radium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class actinium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class thorium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class protactinium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class uranium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class neptunium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class plutonium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class americium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class curium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class berkelium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class californium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class einsteinium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class fermium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class mendelevium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class nobelium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class lawrencium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class rutherfordium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class dubnium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class seaborgium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class bohrium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class hassium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class meitnerium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class darmstadtium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class roentgenium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class copernicium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class ununtrium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class flerovium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class ununpentium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class livermorium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class ununseptium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end

class ununoctium name symbol atomic_number = 
	object
		inherit atom name symbol atomic_number
	end
