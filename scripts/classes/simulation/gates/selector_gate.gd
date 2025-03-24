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
var state: Simulation.States = Simulation.States.UNKNOWN

# private variables

# @onready variables

# optional built-in _init() function
func _init() -> void:
	gate_name = "SELECTOR"
	gate_type = 13
	
	add_io(Simulation.IO_TYPES.OUTPUT, Simulation.Sizes.BIT_1, "OUT")
	
	add_button("BTN_state_low", "LOW", change_state.bind(Simulation.States.LOW))
	add_button("BTN_state_high", "HIGH", change_state.bind(Simulation.States.HIGH))
	add_button("BTN_state_unknown", "UNKOWN", change_state.bind(Simulation.States.UNKNOWN))
	add_button("BTN_state_error", "ERROR", change_state.bind(Simulation.States.ERROR))

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
func change_state(_state: Simulation.States) -> void:
	state = _state

# private functions

# subclasses
