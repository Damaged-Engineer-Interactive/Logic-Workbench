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
		type = "AND"

class CombinationalNandGate extends CombinationalGate:
	func _init():
		super()
		name = "NAND"
		type = "NAND"

class CombinationalOrGate extends CombinationalGate:
	func _init():
		super()
		name = "OR"
		type = "OR"

class CombinationalNorGate extends CombinationalGate:
	func _init():
		super()
		name = "NOR"
		type = "NOR"

class CombinationalNotGate extends CombinationalGate:
	func _init():
		super()
		name = "NOT"
		type = "NOT"
		inputs.remove_at(-1) # only 1 input

class CombinationalXorGate extends CombinationalGate:
	func _init():
		super()
		name = "XOR"
		type = "XOR"

class CombinationalXnorGate extends CombinationalGate:
	func _init():
		super()
		name = "XNOR"
		type = "XNOR"

class CombinationalTriStateGate extends CombinationalGate:
	func _init():
		super()
		name = "TRI-STATE"
		type = "TRI-STATE"
