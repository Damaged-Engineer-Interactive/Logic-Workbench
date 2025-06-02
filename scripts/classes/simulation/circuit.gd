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
var gates: Dictionary[String, GateDescription]

## The Connections of the Circuit
var connections: Dictionary[String, Connection]

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
func copy() -> Circuit:
	var res: Circuit = super() as Circuit
	var old_new_map: Dictionary[String, String] = {}
	for gate: GateDescription in gates.values():
		var new_gate: GateDescription = gate.copy()
		res.gates[new_gate.id] = new_gate
		old_new_map[gate.id] = new_gate.id
	for connection: Connection in connections.values():
		var new_connection: Connection = connection.copy()
		new_connection.gate_in = old_new_map[connection.gate_in]
		new_connection.gate_out = old_new_map[connection.gate_out]
		res.connections[new_connection.id] = new_connection
	return res

func flatten_recursive() -> Array[Dictionary]: # [Gates, Connections]
	var _gates: Dictionary[String, GateDescription]
	var _connections: Dictionary[String, Connection]
	
	for gate in gates.values():
		if gate.type == "CIRCUIT":
			var res: Array[Dictionary] = gate.flatten_recursive()
			_gates.assign(res[0])
			_connections.assign(res[1])
		else:
			_gates[gate.id] = gate
	
	for connection in connections.values():
		_connections[connection.id] = connection
	
	return [_gates, _connections]

# private functions

# subclasses
