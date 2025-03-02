# The name of the Class
class_name TriStateGate
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

# private variables

# @onready variables

# optional built-in _init() function
func _init() -> void:
    gate_name = "TRI-STATE"
    gate_type = 8

    add_io(Simulation.IO_TYPES.INPUT, Simulation.Sizes.BIT_1, "A")
    add_io(Simulation.IO_TYPES.INPUT, Simulation.Sizes.BIT_1, "B")

    add_io(Simulation.IO_TYPES.OUTPUT, Simulation.Sizes.BIT_1, "OUT")

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions

# subclasses
