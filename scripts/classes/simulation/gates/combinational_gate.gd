# The name of the Class
class_name CombinationalGate
# The class this class extends
extends GateDescription
# Docstring
## Base Class for basic logic gates (AND, NAND, OR, NOR, XOR, ...). 1-Bit ONLY
## 
## Uses "truth tables" for input mapping

# Signals

# Enums

# Constants

# @export variables

# public variables
## uses strings created from value.make_multi_string(). "*" = if contains any of the following, seperated by "_". If immediate next (without "_") is a number, it sets the priority (0-9, high-low)
var truth_table: Dictionary[String, Value]

# private variables

# @onready variables

# optional built-in _init() function
func _init() -> void:
	group = "COMBINATIONAL"
	size = Vector2i(4, 2)
	color = Color8(160, 82, 255) # a052ff | Purple
	
	inputs = [
		PinDescription.create("A", 0, 1),
		PinDescription.create("B", 1, 1)
	]
	outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	buttons = []

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions

# subclasses
class CombinationalAndGate extends CombinationalGate:
	func _init():
		super()
		name = "AND"
		truth_table = {
			"*1_L": Value.from_string("1_L"),
			"H_H": Value.from_string("1_H"),
			"*9_!" : Value.from_string("1_!"),
			"*8_?" : Value.from_string("1_?")
		}

class CombinationalNandGate extends CombinationalGate:
	func _init():
		super()
		name = "NAND"
		truth_table = {
			"*1_L": Value.from_string("1_H"),
			"H_H": Value.from_string("1_L"),
			"*9_!" : Value.from_string("1_!"),
			"*8_?" : Value.from_string("1_?")
		}

class CombinationalOrGate extends CombinationalGate:
	func _init():
		super()
		name = "OR"
		truth_table = {
			"*1_H": Value.from_string("1_H"),
			"L_L": Value.from_string("1_L"),
			"*9_!" : Value.from_string("1_!"),
			"*8_?" : Value.from_string("1_?")
		}

class CombinationalNorGate extends CombinationalGate:
	func _init():
		super()
		name = "NOR"
		truth_table = {
			"*1_H": Value.from_string("1_L"),
			"L_L": Value.from_string("1_H"),
			"*9_!" : Value.from_string("1_!"),
			"*8_?" : Value.from_string("1_?")
		}

class CombinationalNotGate extends CombinationalGate:
	func _init():
		super()
		name = "NOT"
		inputs.remove_at(-1) # only 1 input
		truth_table = {
			"H": Value.from_string("1_L"),
			"L": Value.from_string("1_H"),
			"*9_!" : Value.from_string("1_!"),
			"*8_?" : Value.from_string("1_?")
		}

class CombinationalXorGate extends CombinationalGate:
	func _init():
		super()
		name = "XOR"
		truth_table = {
			"L_L": Value.from_string("1_L"),
			"H_H": Value.from_string("1_L"),
			"L_H": Value.from_string("1_H"),
			"H_L": Value.from_string("1_H"),
			"*9_!" : Value.from_string("1_!"),
			"*8_?" : Value.from_string("1_?")
		}

class CombinationalXnorGate extends CombinationalGate:
	func _init():
		super()
		name = "XOR"
		truth_table = {
			"L_L": Value.from_string("1_H"),
			"H_H": Value.from_string("1_H"),
			"L_H": Value.from_string("1_L"),
			"H_L": Value.from_string("1_L"),
			"*9_!" : Value.from_string("1_!"),
			"*8_?" : Value.from_string("1_?")
		}

class CombinationalTriStateGate extends CombinationalGate:
	func _init():
		super()
		name = "TRI-STATE"
		truth_table = {
			"L_L": Value.from_string("1_L"),
			"H_L": Value.from_string("1_L"),
			"L_H": Value.from_string("1_L"),
			"H_H": Value.from_string("1_H"),
			"*9_!" : Value.from_string("1_!"),
			"*8_?" : Value.from_string("1_?")
		}
