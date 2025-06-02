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

## The Type of the Gate
var type: String

## The Size of the Gate
var size: Vector2i

## The Color of the Gate
var color: Color

## If this Gate is maintained by the GateRegistry
var registry: bool = false

## How many ticks this Gate need, to fully simulate
var ticks: int = 1

#region AT RUNTIME
## The ID of the Gate
var id: String = GateRegistry.make_uuid()

## The Position of the Gate[br]
## x, y, layer
var position: Vector3i

#endregion

var inputs: Array[PinDescription]
var outputs: Array[PinDescription]
var buttons: Array[ButtonDescription]

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Returns a copy of this GateDescription, with the new id
func copy() -> GateDescription:
	var res: GateDescription = GateDescription.new()
	res.name = name
	res.type = type
	res.size = size
	res.color = color
	
	res.inputs = inputs
	res.outputs = outputs
	res.buttons = buttons
	return res

# private functions

# subclasses
