extends Node

signal loaded
signal updated

var _map: Dictionary[String, GateDescription] = {}

var builtin_gates: Array[String] = []
var custom_gates: Array[String] = []

var priority: int = 0

# Group.Gate.Version -> Group[Gate] = [Versions]
var _grouped: Dictionary[String, Dictionary] = {}

func add_gate(gate: GateDescription) -> void:
	_map[gate.type] = gate
	custom_gates.append(gate.type)
	priority += 1
	gate.priority = priority
	Global.active_project.gates[gate.type] = gate
	Global.active_project.save()
	_make_grouped()
	updated.emit()

func remove_gate(type: String) -> void:
	_map.erase(type)
	custom_gates.erase(type)
	Global.active_project.gates.erase(type)
	Global.active_project.save()
	_make_grouped()
	updated.emit()

func _add_builtin_gate(gate: GateDescription) -> void:
	_map[gate.type] = gate
	builtin_gates.append(gate.type)

func has_gate(type: String) -> bool:
	return _map.has(type)

func get_gate(type: String) -> GateDescription:
	return _map.get(type, null)

func make_uuid() -> String:
	var time: int = int(Time.get_ticks_msec() % 0x10000)  # 4 hex chars
	var time_hex: String = PackedByteArray([time]).hex_encode()

	var part1 = PackedByteArray([randi_range(0, 65535)]).hex_encode()
	var part2 = PackedByteArray([randi_range(0, 65535)]).hex_encode()
	var part3 = PackedByteArray([randi_range(0, 65535)]).hex_encode()

	return "%s#%s#%s#%s" % [time_hex, part1, part2, part3]

func _make_grouped() -> void:
	_grouped = {}
	for gate: GateDescription in _map.values():
		var sections: PackedStringArray = gate.type.split(".", false, 3)
		if not _grouped.has(sections[0]):
			_grouped[sections[0]] = {sections[1]: [sections[2]]}
		elif not _grouped[sections[0]].has(sections[1]):
			_grouped[sections[0]][sections[1]] = [sections[2]]
		else:
			_grouped[sections[0]][sections[1]].append(sections[2])

func _ready() -> void:
	reset()

func reset() -> void:
	print("\nGateRegistry reset!")
	priority = 0
	_map = {}
	builtin_gates = []
	custom_gates = []
	_grouped = {}
	
	make_collections()
	
	_make_grouped()
	
	loaded.emit()
	
	print("GateRegistry done!")
	for group: String in _grouped.keys():
		print(group)
		for gate: String in _grouped[group].keys():
			print("- " + gate)
			for version: String in _grouped[group][gate]:
				print("| - " + version)
	print("")

#region Chip Making
func make_collections() -> void:
	make_io()
	make_routing()
	make_combinational()
	builtin_gates = _map.keys().duplicate()

func make_io() -> void:
	var gate: GateDescription
	
	gate = GateDescription.new()
	gate.name = "INPUT"
	gate.type = "IO.INPUT.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.ROYAL_BLUE
	gate.inputs = []
	gate.outputs = [ PinDescription.create("OUT", 0) ]
	_add_builtin_gate(gate)
	
	gate = GateDescription.new()
	gate.name = "OUTPUT"
	gate.type = "IO.OUTPUT.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.ROYAL_BLUE
	gate.inputs = [ PinDescription.create("IN", 0) ]
	gate.outputs = []
	_add_builtin_gate(gate)

func make_routing() -> void:
	var gate: GateDescription
	
	gate = GateDescription.new()
	gate.name = "TUNNEL IN"
	gate.type = "ROUTING.TUNNEL_IN.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.ORANGE
	gate.inputs = []
	gate.outputs = [ PinDescription.create("OUT", 0) ]
	#_add_builtin_gate(gate)
	
	gate = GateDescription.new()
	gate.name = "TUNNEL OUT"
	gate.type = "ROUTING.TUNNEL_OUT.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.ORANGE
	gate.inputs = [ PinDescription.create("IN", 0) ]
	gate.outputs = []
	#_add_builtin_gate(gate)
	
	gate = GateDescription.new()
	gate.name = "MUX"
	gate.type = "ROUTING.MUX.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.ORANGE
	gate.inputs = [
		PinDescription.create("Select", 1),
		PinDescription.create("A", 0),
		PinDescription.create("B", 0)
	]
	gate.outputs = [
		PinDescription.create("OUT", 0)
	]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "DEMUX"
	gate.type = "ROUTING.DEMUX.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.ORANGE
	gate.inputs = [
		PinDescription.create("Select", 1),
		PinDescription.create("IN", 0)
	]
	gate.outputs = [
		PinDescription.create("A", 0),
		PinDescription.create("B", 0)
	]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "DECODER"
	gate.type = "ROUTING.DECODER.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.ORANGE
	gate.inputs = [ PinDescription.create("Select", 0) ]
	gate.outputs = [ ]
	#_add_builtin_gate(gate)
	
	# ###
	gate = GateDescription.new()
	gate.name = "CONVERTER"
	gate.type = "ROUTING.CONVERTER.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.ORANGE
	gate.inputs = []
	gate.outputs = []
	_add_builtin_gate(gate)

func make_combinational() -> void:
	var gate: GateDescription
	
	gate = GateDescription.new()
	gate.name = "AND"
	gate.type = "COMBINATIONAL.AND.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.CRIMSON
	gate.inputs = [ ]
	gate.outputs = [ PinDescription.create("OUT", 0) ]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "NAND"
	gate.type = "COMBINATIONAL.NAND.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.CRIMSON
	gate.inputs = [ ]
	gate.outputs = [ PinDescription.create("OUT", 0) ]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "OR"
	gate.type = "COMBINATIONAL.OR.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.CRIMSON
	gate.inputs = [ ]
	gate.outputs = [ PinDescription.create("OUT", 0) ]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "NOR"
	gate.type = "COMBINATIONAL.NOR.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.CRIMSON
	gate.inputs = [ ]
	gate.outputs = [ PinDescription.create("OUT", 0) ]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "NOT"
	gate.type = "COMBINATIONAL.NOT.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.CRIMSON
	gate.inputs = [
		PinDescription.create("IN", 0)
	]
	gate.outputs = [
		PinDescription.create("OUT", 0)
	]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "XOR"
	gate.type = "COMBINATIONAL.XOR.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.CRIMSON
	gate.inputs = [ ]
	gate.outputs = [ PinDescription.create("OUT", 0) ]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "XNOR"
	gate.type = "COMBINATIONAL.XNOR.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.CRIMSON
	gate.inputs = [ ]
	gate.outputs = [ PinDescription.create("OUT", 0) ]
	_add_builtin_gate(gate)

func make_time() -> void:
	var gate: GateDescription
	
	gate = GateDescription.new()
	gate.name = "CLOCK"
	gate.type = "TIME.CLOCK.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.REBECCA_PURPLE
	gate.inputs = []
	gate.outputs = [ PinDescription.create("OUT", 1) ]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "CAPACITOR"
	gate.type = "TIME.CAPACITOR.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.REBECCA_PURPLE
	gate.inputs = [
		PinDescription.create("TIME", 0),
		PinDescription.create("SIGNAL", 1)
	]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "PULSE"
	gate.type = "TIME.PULSE.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.REBECCA_PURPLE
	gate.inputs = [ PinDescription.create("SIGNAL", 1), ]
	gate.outputs = [ PinDescription.create("OUT", 1) ]
	_add_builtin_gate(gate)
	
	# ### ### ### ###
	gate = GateDescription.new()
	gate.name = "DELAY"
	gate.type = "TIME.DELAY.#"
	gate.size = Vector2i(8, 4)
	gate.color = Color.REBECCA_PURPLE
	gate.inputs = [
		PinDescription.create("AMOUNT", 8),
		PinDescription.create("SIGNAL", 1)
	]
	gate.outputs = [ PinDescription.create("OUT", 1) ]
	_add_builtin_gate(gate)

#endregion
