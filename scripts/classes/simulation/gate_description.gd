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

## The priority when loading the gate | 0 = builtin, 1+ = custom
var priority: int = 0

#region AT RUNTIME
## The ID of the Gate
var id: String = GateRegistry.make_uuid()

## The Position of the Gate[br]
## x, y, (layer)
var position: Vector2 = Vector2.ZERO

#endregion

var inputs: Array[PinDescription]
var outputs: Array[PinDescription]

var data: Dictionary[String, Variant]

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
	res.ticks = ticks
	res.priority = priority
	res.position = Vector2(position)
	
	res.inputs = []
	res.outputs = []
	
	for pin: PinDescription in inputs:
		res.inputs.append(pin.copy())
	for pin: PinDescription in outputs:
		res.outputs.append(pin.copy())
	
	res.data = data.duplicate()
	return res

## Saving of Builtin gates!
func save() -> Dictionary:
	return {
		"id": id,
		"type": type,
		"position": position
	}

## Loading of Builtin gates!
static func load(from: Dictionary) -> GateDescription:
	var res: GateDescription = GateRegistry.get_gate(from["type"])
	res.id = from["id"]
	res.position = from["position"]
	return res

# private functions

# subclasses
