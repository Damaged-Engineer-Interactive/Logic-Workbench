# The name of the Class
class_name Circuit
# The class this class extends
extends GateDescription
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
## The Gates of the Circuit
var gates: Dictionary[int, Gate]

## The Connections of the Circuit
var connections: Dictionary[int, Connection]

# private variables

# @onready variables

# optional built-in _init() function
func _init(_name: String, _group: String, _size: Vector2,
	_inputs: Array[PinDescription], _outputs: Array[PinDescription], 
	_buses: Array[PinDescription], _buttons: Array[ButtonDescription]):
		super._init(_name, _group, _size, _inputs, _outputs, _buses, []) # Buttons are not supported
		
		gates = {}
		connections = {}

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Simulates the Gates[br]
## Should only be called on a seperate thread
func simulate_gates() -> void:
	for gate in gates.values():
		if gate is CombinationalGate:
			pass

# private functions

# subclasses
