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

## The Configuration of the Circuit's inputs[br]
## Gate ID -> port
var input_config: Dictionary[String, PinDescription]

## The Configuration of the Circuit's outputs[br]
## Gate ID -> port
var output_config: Dictionary[String, PinDescription]

# private variables

# @onready variables

# optional built-in _init() function
func _init():
		gates = {}
		connections = {}
		
		input_config = {}
		output_config = {}

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
func add_gate(gate: GateDescription) -> String:
	gates[gate.id] = gate
	return gate.id

func remove_gate(gate_id: String) -> GateDescription:
	var gate: GateDescription = gates.get(gate_id)
	gates.erase(id)
	return gate

func add_connection(connection: Connection) -> String:
	connections[connection.id] = connection
	return connection.id

func remove_connection(connection_id: String) -> Connection:
	var connection: Connection = connections.get(connection_id)
	connections.erase(connection_id)
	return connection

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
			
			var conn_in_list: Array[Connection] = [] #			IN connections
			var conn_through_list: Array[Connection] = [] #		THROUGH connections
			var conn_out_list: Array[Connection] = [] #			OUT connections
			for connection: Connection in _connections.values(): #	Gather the IOT connections
				if connection.uses_io():
					if connection.uses_to_io(): #				Input
						conn_in_list.append(connection)
					elif connection.uses_from_io(): #			Output
						conn_out_list.append(connection)
					else:
						conn_through_list.append(connections)
			
			var connection_pairs: Array[Array] = [] # 			IN connection | THROUGH connection | OUT connection
			for connection: Connection in conn_in_list: #		make connection pairs | IN connections
				connection_pairs.append([connection])
			
			for connection: Connection in conn_through_list: #	make connection pairs | THROUGH connections
				for pair: Array[Connection] in connection_pairs:
					if pair.size() == 2: # already visited
						continue
					if pair[0].id != connection.from_gate: # wrong gate
						continue
					pair.append(connection)
			
			for connection: Connection in conn_out_list: #	make connection pairs | OUT connections
				for pair: Array[Connection] in connection_pairs:
					if pair.size() == 3: # already visited
						continue
					if pair[1].to_gate != connection.from_gate: # wrong gate
						continue
					pair.append(connection)
			
			# Pairs should be done now, replacing them now :D
			for pair: Array[Connection] in connection_pairs:
				var new: Connection = Connection.create(pair[0].from_gate, pair[0].from_port, pair[2].to_gate, pair[2].to_port)
				_connections[new.id] = new
				_connections.erase(pair[0].id)
				_connections.erase(pair[1].id)
				_connections.erase(pair[2].id)
				_gates.erase(pair[1].from_gate) # out component
				_gates.erase(pair[1].to_gate) # in component
			
		else:
			_gates[gate.id] = gate
	
	for connection in connections.values():
		_connections[connection.id] = connection
	
	return [_gates, _connections]

# private functions

# subclasses
