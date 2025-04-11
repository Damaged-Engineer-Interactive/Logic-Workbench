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

#region Output
## The ID of the first Gate
var gate_in: int = -1

## The Port of the first gate
var port_in: int = -1

## The Type of the first gate
var type_in: Gate.IOTypes = Gate.IOTypes.UNKNOWN

## The Size of the first gate
var size_in: Value.Sizes = Value.Sizes.BIT_1
#endregion

#region Input
## The ID of the second Gate
var gate_out: int = -1

## The Port of the second gate
var port_out: int = -1

## The Type of the second gate
var type_out: Gate.IOTypes = Gate.IOTypes.UNKNOWN

## The Size of the first gate
var size_out: Value.Sizes = Value.Sizes.BIT_1
#endregion

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

func get_con_id() -> String:
	return "%s:%s|%s:%s" % [str(gate_in), str(port_in), str(gate_out), str(port_out)]

static func make_con_id(from_gate: String, from_port: int, to_gate: String, to_port: int) -> String:
	return "%s:%s|%s:%s" % [str(from_gate.split("_")[-1]), str(from_port), str(to_gate.split("_")[-1]), str(to_port)]

# virtual functions to override

# public functions

# private functions

# subclasses
