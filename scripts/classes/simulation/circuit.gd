# The name of the Class
class_name Circuit
# The class this class extends
extends GateDescription
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
## The Gates of the Circuit
var gates: Dictionary[int, GateDescription]

## The Connections of the Circuit
var connections: Dictionary[int, Connection]

# private variables

# @onready variables

# optional built-in _init() function
func _init():
		gates = {}
		connections = {}

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
func flatten_recursive() -> Array[Dictionary]: # [Gates, Connections]
	var _gates: Dictionary[String, GateDescription]
	var _connections: Dictionary[String, Connection]
	
	for gate in gates.values():
		if gate.type == "CIRCUIT":
			var res: Array[Dictionary] = gate._flatten_recursive()
			_gates.assign(res[0])
			_connections.assign(res[1])
		else:
			_gates[gate.id] = gate
	
	for connection in connections.values():
		connection[connection.id] = connection
	
	return [_gates, _connections]

# private functions

# subclasses
