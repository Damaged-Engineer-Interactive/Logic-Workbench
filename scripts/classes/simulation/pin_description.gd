# The name of the Class
class_name PinDescription
# The class this class extends
extends Object
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables
## The Name of the Pin
@export var name: String

## The State of the Pin
@export var state: ValueDescription

# public variables

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Constructor
static func create(_name: String, bit_size: int) -> PinDescription:
	var pin := PinDescription.new()
	pin.name = _name
	pin.state = ValueDescription.new(bit_size)
	return pin

func copy() -> PinDescription:
	var pin := PinDescription.new()
	pin.name = name
	pin.state = state.copy()
	return pin

func save() -> Dictionary:
	return { "name": name, "state": state.save() }

static func load(from: Dictionary) -> PinDescription:
	var pin := PinDescription.new()
	pin.name = from["name"]
	pin.state = ValueDescription.load(from["state"])
	return pin

# private functions

# subclasses
