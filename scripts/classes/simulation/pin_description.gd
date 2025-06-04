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
@export var state: Value

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
	pin.state = Value.from_default(bit_size)
	return pin

# private functions

# subclasses
