# The name of the Class
class_name Connection
# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
## The ID of the Connection
var id: int = 0

## The ID of the first Gate
var gate_in: int = -1

## The Port of the first gate
var port_in: int = -1

## The Type of the first gate
var type_in: Simulation.IO_TYPES = Simulation.IO_TYPES.UNKNOWN

## The Size of the first gate
var size_in: Simulation.Sizes = Simulation.Sizes.BIT_1

## The ID of the second Gate
var gate_out: int = -1

## The Port of the second gate
var port_out: int = -1

## The Type of the second gate
var type_out: Simulation.IO_TYPES = Simulation.IO_TYPES.UNKNOWN

## The Size of the first gate
var size_out: Simulation.Sizes = Simulation.Sizes.BIT_1

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions
func uses_gate(gate: Gate) -> bool:
	if gate.id in [gate_in, gate_out]:
		return true
	return false

func get_con_id() -> String:
	return "%s:%s_%s:%s" % [str(gate_in), str(port_in), str(gate_out), str(port_out)]

# virtual functions to override

# public functions

# private functions

# subclasses
