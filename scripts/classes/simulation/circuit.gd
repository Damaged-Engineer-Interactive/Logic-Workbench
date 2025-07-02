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

var _gate_conn_map: Dictionary[String, Array] = {}

var _conn_id_map: Dictionary[String, String] = {}

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
	_gate_conn_map[gate.id] = []
	match gate.type:
		"IO.INPUT.*":
			input_config[gate.id] = PinDescription.create(gate.data["name"], gate.outputs[0].state.size)
		"IO.OUTPUT.*":
			output_config[gate.id] = PinDescription.create(gate.data["name"], gate.inputs[0].state.size)
		# TODO : Add Tunnels
		"ROUTING.TUNNEL_IN.*":
			push_error("TUNNELS NOT IMPLEMENTED")
			printerr("TUNNELS NOT IMPLEMENTED")
		"ROUTING.TUNNEL_OUT.*":
			push_error("TUNNELS NOT IMPLEMENTED")
			printerr("TUNNELS NOT IMPLEMENTED")
	return gate.id

func remove_gate(gate_id: String) -> Array:
	var gate: GateDescription = gates.get(gate_id)
	match gate.type:
		"IO.INPUT.*":
			input_config.erase(gate_id)
		"IO.OUTPUT.*":
			output_config.erase(gate_id)
		# TODO : Add Tunnels
		"ROUTING.TUNNEL_IN.*":
			push_error("TUNNELS NOT IMPLEMENTED")
			printerr("TUNNELS NOT IMPLEMENTED")
		"ROUTING.TUNNEL_OUT.*":
			push_error("TUNNELS NOT IMPLEMENTED")
			printerr("TUNNELS NOT IMPLEMENTED")
	gates.erase(gate_id)
	var removed: Array = _gate_conn_map.get(gate_id).duplicate()
	for connection: Connection in removed:
		remove_connection(connection)
	_gate_conn_map.erase(gate_id)
	return [gate, removed]

func add_connection(connection: Connection) -> String:
	if _conn_id_map.has(str(connection)):
		return _conn_id_map[str(connection)]
	_conn_id_map[str(connection)] = connection.id
	connections[connection.id] = connection
	_gate_conn_map[connection.from_gate].append(connection)
	_gate_conn_map[connection.to_gate].append(connection)
	return connection.id

func remove_connection(connection_template: Connection) -> Connection:
	var connection_id: String = _conn_id_map.get(str(connection_template))
	var connection: Connection = connections.get(connection_id)
	
	connections.erase(connection_id)
	_gate_conn_map[connection.from_gate].erase(connection)
	_gate_conn_map[connection.to_gate].erase(connection)
	_conn_id_map.erase(str(connection))
	return connection

func is_empty() -> bool:
	return gates.is_empty() and connections.is_empty()

func copy() -> Circuit:
	var res: Circuit = Circuit.new()
	res.name = name
	res.type = type
	res.size = size
	res.color = color
	res.ticks = ticks
	res.priority = priority
	
	res.inputs = []
	res.outputs = []
	
	for pin: PinDescription in inputs:
		res.inputs.append(pin.copy())
	for pin: PinDescription in outputs:
		res.outputs.append(pin.copy())
	
	res.data = data.duplicate()
	
	var old_new_map: Dictionary[String, String] = {}
	for gate: GateDescription in gates.values():
		var new_gate: GateDescription = gate.copy()
		res.gates[new_gate.id] = new_gate
		old_new_map[gate.id] = new_gate.id
	for connection: Connection in connections.values():
		var new_connection: Connection = connection.copy()
		new_connection.from_gate = old_new_map[connection.from_gate]
		new_connection.to_gate = old_new_map[connection.to_gate]
		res.connections[new_connection.id] = new_connection
	for _id: String in input_config.keys():
		res.input_config[old_new_map[_id]] = input_config[_id].copy()
	for _id: String in output_config.keys():
		res.output_config[old_new_map[_id]] = output_config[_id].copy()
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

func to_description() -> Circuit:
	var res: Circuit = copy() # make a copy...
	res.inputs = res.input_config.values()
	res.outputs = res.output_config.values()
	return res # done? ...

func save() -> Dictionary:
	var res: Dictionary =  {
		"name": name,
		"type": type,
		"color": var_to_str(color),
		"priority": priority,
		"gates": "", # generated
		"connections": "", # generated
		"input_config": {}, # generated
		"output_config": {}, # generated
	}
	
	for _id: String in gates.keys():
		res["gates"][_id] = gates[_id].save()
	
	for _id: String in connections.keys():
		res["connections"][_id] = connections[_id].save()
	
	for _id: String in input_config.keys():
		res["input_conig"][_id] = input_config[_id].save()
	
	for _id: String in output_config.keys():
		res["output_config"][_id] = output_config[_id].save()
	
	return res

static func load(from: Dictionary) -> Circuit:
	var chip := Circuit.new()
	chip.name = from["name"]
	chip.type = from["type"]
	chip.color = str_to_var(from["color"])
	chip.priority = from["priority"]
	
	for _id: String in from["gates"].keys():
		chip.gates[_id] = GateDescription.load(from["gates"][_id])
	
	for _id: String in from["connections"].keys():
		chip.connections[_id] = Connection.load(from["connections"][_id])
	
	for _id: String in from["input_conig"].keys():
		chip.input_config[_id] = PinDescription.load(from["input_conig"][_id])
	
	for _id: String in from["output_config"].keys():
		chip.output_config[_id] = PinDescription.load(from["output_config"][_id])
	
	return chip
# private functions

# subclasses
