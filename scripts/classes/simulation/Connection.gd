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
var id: String:
	get:
		return "%s:%s|%s:%s" % [str(gate_in), str(port_in), str(gate_out), str(port_out)]

#region Output
## The ID of the first Gate
var gate_in: int = -1

## The Port of the first gate
var port_in: int = -1
#endregion

#region Input
## The ID of the second Gate
var gate_out: int = -1

## The Port of the second gate
var port_out: int = -1
#endregion

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions

# subclasses
