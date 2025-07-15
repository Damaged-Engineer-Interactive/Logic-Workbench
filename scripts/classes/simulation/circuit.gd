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
		"IO.INPUT.#":
			input_config[gate.id] = PinDescription.create(gate.data["name"], gate.outputs[0].state.size)
		"IO.OUTPUT.#":
			output_config[gate.id] = PinDescription.create(gate.data["name"], gate.inputs[0].state.size)
		# TODO : Add Tunnels
		"ROUTING.TUNNEL_IN.#":
			push_error("TUNNELS NOT IMPLEMENTED")
			printerr("TUNNELS NOT IMPLEMENTED")
		"ROUTING.TUNNEL_OUT.#":
			push_error("TUNNELS NOT IMPLEMENTED")
			printerr("TUNNELS NOT IMPLEMENTED")
	return gate.id

func remove_gate(gate_id: String) -> Array:
	var gate: GateDescription = gates.get(gate_id)
	match gate.type:
		"IO.INPUT.#":
			input_config.erase(gate_id)
		"IO.OUTPUT.#":
			output_config.erase(gate_id)
		# TODO : Add Tunnels
		"ROUTING.TUNNEL_IN.#":
			push_error("TUNNELS NOT IMPLEMENTED")
			printerr("TUNNELS NOT IMPLEMENTED")
		"ROUTING.TUNNEL_OUT.#":
			push_error("TUNNELS NOT IMPLEMENTED")
			printerr("TUNNELS NOT IMPLEMENTED")
	gates.erase(gate_id)
	var removed: Array = _gate_conn_map.get(gate_id).duplicate()
	for connection: Connection in removed:
		remove_connection(connection)
	_gate_conn_map.erase(gate_id)
	return [gate, removed]

func add_connection(connection: Connection) -> String:
	if _conn_id_map.has(connection.template()):
		return _conn_id_map[connection.template()]
	_conn_id_map[connection.template()] = connection.id
	connections[connection.id] = connection
	_gate_conn_map[connection.from_gate].append(connection)
	_gate_conn_map[connection.to_gate].append(connection)
	return connection.id

func remove_connection(connection_template: Connection) -> Connection:
	var connection_id: String = _conn_id_map.get(connection_template.template())
	var connection: Connection = connections.get(connection_id)
	
	connections.erase(connection_id)
	_gate_conn_map[connection.from_gate].erase(connection)
	_gate_conn_map[connection.to_gate].erase(connection)
	_conn_id_map.erase(connection.template())
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
	res.position = position
	
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
		res.add_gate(new_gate)
		old_new_map[gate.id] = new_gate.id
	for connection: Connection in connections.values():
		var new_connection: Connection = connection.copy()
		new_connection.from_gate = old_new_map[connection.from_gate]
		new_connection.to_gate = old_new_map[connection.to_gate]
		res.add_connection(new_connection)
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
	push_warning("Check TODO")
	# TODO : calculate ticks
	res.inputs = res.input_config.values()
	res.outputs = res.output_config.values()
	return res # done? ...

func save(path: String) -> void:
	var file: FileAccess
	if not DirAccess.dir_exists_absolute(path):
		DirAccess.make_dir_recursive_absolute(path)
	
	file = FileAccess.open(path + "/%s.lwc" % type, FileAccess.WRITE)
	file.store_string("version_1\n")
	file.store_string("[DATA]\n")
	file.store_string('type: {type}\nticks: {ticks}\npriority: {priority}\ncolor: {color}\n'.format({"type": type, "ticks": ticks, "priority": priority, "color": var_to_str(color)}))
	file.store_string("[CFG_IN]\n")
	for i: String in input_config.keys():
		var desc: PinDescription = input_config[i]
		file.store_string('{i},{size},{name}\n'.format({"i": i, "size": desc.state.size, "name": desc.name}))
	file.store_string("[CFG_OUT]\n")
	for i: String in output_config.keys():
		var desc: PinDescription = output_config[i]
		file.store_string('{i},{size},{name}\n'.format({"i": i, "size": desc.state.size, "name": desc.name}))
	file.store_string("[GATES]\n")
	for gate: GateDescription in gates.values():
		var str_data: String = JSON.stringify(gate.data)
		var values: Dictionary = {
			"id": gate.id,
			"type": gate.type,
			"position": var_to_str(gate.position),
			"data": str_data
		}
		file.store_string("{id}|{type}|{position}|{data}\n".format(values))
	file.store_string("[CONNECTIONS]\n")
	for conn: Connection in connections.values():
		var values: Dictionary = {
			"id": conn.id,
			"from_gate": conn.from_gate,
			"from_port": conn.from_port,
			"to_gate": conn.to_gate,
			"to_port": conn.to_port
		}
		file.store_string("{id} {from_gate} {from_port} {to_gate} {to_port}\n".format(values))
	file.store_string("[END]")
	file.close()

static func load(path: String) -> Circuit: # load using full path
	if not path.get_extension() == "lwc":
		printerr("File [%s] has an invalid extension! aborting load" % path)
		return
	if not FileAccess.file_exists(path):
		printerr("File [%s] does not exist! aborting load" % path)
		return
	
	var file := FileAccess.open(path, FileAccess.ModeFlags.READ)
	var res := Circuit.new()
	
	if not file.get_line() != "version_1\n":
		printerr("File [%s] has an invalid version! aborting load" % path)
		return
	if not file.get_line() != "[DATA]\n":
		printerr("File [%s] has an invalid format! aborting load" % path)
		return
	# data
	res.type = file.get_line().get_slice(": ",1)
	res.name = res.type.get_slice(".", 1)
	res.ticks = int(file.get_line().get_slice(": ",1))
	res.priority = int(file.get_line().get_slice(": ",1))
	res.color = str_to_var(file.get_line().get_slice(": ",1))
	if not file.get_line() != "[CFG_IN]\n":
		printerr("File [%s] has an invalid format! aborting load" % path)
		return
	# input config
	var text: String = file.get_line()
	while true:
		var segments: PackedStringArray = text.split(",", false, 3)
		var pin := PinDescription.create(segments[2], int(segments[1]))
		res.input_config[segments[0]] = pin
		text = file.get_line()
		if text == "[CFG_OUT]":
			break
	# output config
	text = file.get_line()
	if text != "[GATES]":
		while true:
			var segments: PackedStringArray = text.split(",", false, 3)
			var pin := PinDescription.create(segments[2], int(segments[1]))
			res.output_config[segments[0]] = pin
			text = file.get_line()
			if text == "[GATES]":
				break
	# gates
	text = file.get_line()
	if text != "[CONNECTIONS]":
		while true:
			var segments: PackedStringArray = text.split("|", false, 4)
			var d = JSON.parse_string(segments[3])
			var gate: GateDescription = GateRegistry.get_gate(segments[1]).from_data(d)
			gate.id = segments[0]
			gate.position = str_to_var(segments[2])
			res.add_gate(gate)
			text = file.get_line()
			if text == "[CONNECTIONS]":
				break
	# connections
	text = file.get_line()
	while true:
		var segments: PackedStringArray = text.split(" ", false, 5)
		var conn := Connection.new()
		conn.id = segments[0]
		conn.from_gate = segments[1]
		conn.from_port = int(segments[2])
		conn.to_gate = segments[3]
		conn.to_port = int(segments[4])
		res.add_connection(conn)
		text = file.get_line()
		if text == "[END]":
			break
	file.close()
	return res

# private functions

# subclasses
