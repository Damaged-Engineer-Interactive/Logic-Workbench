# The name of the Class
class_name ButtonDescription
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
## The Text of the Button
@export var text: String

## The "slot" of the Pin (top to bottom)
@export var slot: int

## The Callable that will be called when the button is pressed
@export var callable: Callable

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
static func create(_name: String, _slot: int, size: int) -> PinDescription:
	var pin := PinDescription.new()
	pin.name = _name
	pin.slot = _slot
	pin.state = Value.from_default(size)
	return pin

# private functions

# subclasses
