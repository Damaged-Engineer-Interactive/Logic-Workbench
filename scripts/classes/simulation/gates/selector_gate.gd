# The name of the Class
class_name SelectorGate
# The class this class extends
extends Gate
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
var state: Value.States = Value.States.UNKNOWN

# private variables

# @onready variables

# optional built-in _init() function
func _init() -> void:
	gate_name = "SELECTOR"
	gate_type = 13
	
	add_io(IOTypes.OUTPUT, Value.Sizes.BIT_1, "OUT")
	
	add_button("BTN_state_low", "LOW", change_state.bind(Value.States.LOW))
	add_button("BTN_state_high", "HIGH", change_state.bind(Value.States.HIGH))
	add_button("BTN_state_unknown", "UNKOWN", change_state.bind(Value.States.UNKNOWN))
	add_button("BTN_state_error", "ERROR", change_state.bind(Value.States.ERROR))

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
func change_state(_state: Value.States) -> void:
	state = _state

# private functions

# subclasses
