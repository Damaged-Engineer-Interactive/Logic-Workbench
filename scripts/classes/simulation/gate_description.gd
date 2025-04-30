# The name of the Class
class_name GateDescription
# The class this class extends
extends Object
# Docstring
## Basically the "blueprint" for a gate

# Signals

# Enums

# Constants

# @export variables

# public variables
## The Name of the Gate
var name: String

## The group, that this gate is part of
var group: String

## The Size of the Gate
var size: Vector2

#region AT RUNTIME
## The ID of the Gate
var id: int

## The Position of the Gate[br]
## x, y, layer
var position: Vector3i

## The Internal State of the Gate
var internal_state: Array[Value]

#endregion

var inputs: Array[PinDescription]
var outputs: Array[PinDescription]
var buses: Array[PinDescription]
var buttons: Array[ButtonDescription]

# private variables

# @onready variables

# optional built-in _init() function
func _init(_name: String, _group: String, _size: Vector2,
	_inputs: Array[PinDescription], _outputs: Array[PinDescription], 
	_buses: Array[PinDescription], _buttons: Array[ButtonDescription]):
		name = _name
		group = _group
		size = _size
		
		id = -1
		position = Vector3i.ZERO
		internal_state = []
		
		inputs = _inputs
		outputs = _outputs
		buses = _buses
		buttons = _buttons

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions

# subclasses
