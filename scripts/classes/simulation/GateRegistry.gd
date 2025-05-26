extends Node

class GateEntry:
	var name: String
	# Group -> Gate -> Version
	var versions: Dictionary[String, GateDescription] = {}

class GroupEntry:
	var name: String
	# Group -> Gate
	var gates: Dictionary[String, GateEntry] = {}

# Group
var _map: Dictionary[String, GroupEntry] = {}

func get_gate(type: String) -> GateDescription:
	var components: PackedStringArray = type.split(".", false, 3) # splitting at ".", no empty strings, 3 components max
	var group: GroupEntry = _map.get(components[0])
	if group == null:
		return null
	var gate: GateEntry = group.gates.get(components[1])
	if gate == null:
		return null
	return gate.versions.get(components[2])

func make_uuid() -> String:
	var time: int = int(Time.get_ticks_msec() % 0x10000)  # 4 hex chars
	var time_hex: String = PackedByteArray([time]).hex_encode()

	var part1 = PackedByteArray([randi_range(0, 65535)]).hex_encode()
	var part2 = PackedByteArray([randi_range(0, 65535)]).hex_encode()
	var part3 = PackedByteArray([randi_range(0, 65535)]).hex_encode()

	return "%s:%s:%s:%s" % [time_hex, part1, part2, part3]

func _ready() -> void:
	make_collections()
	
	print("GateRegistry done!")
	for group: GroupEntry in _map.values():
		print(group.name + " :")
		for component: GateEntry in group.gates.values():
			print("- " + component.name)
			for version: String in component.versions.keys():
				print("| - " + version.split(".", false, 3)[-1])
	print("")

#region Chip Making
func make_collections() -> void:
	make_io()
	#make_bus()
	make_combinational()

func make_io() -> void:
	var group: GroupEntry = GroupEntry.new()
	group.name = "IO"
	
	var gate: GateEntry
	var version: GateDescription
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "INPUT"
	
	version = GateDescription.new()
	version.name = "INPUT"
	version.type = "IO.INPUT.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ROYAL_BLUE
	version.inputs = []
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["IO.INPUT.1"] = version
	
	version = GateDescription.new()
	version.name = "INPUT"
	version.type = "IO.INPUT.8"
	version.size = Vector2i(8, 4)
	version.color = Color.ROYAL_BLUE
	version.inputs = []
	version.outputs = [ PinDescription.create("OUT", 0, 8) ]
	gate.versions["IO.INPUT.8"] = version
	
	group.gates["IO.INPUT"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "OUTPUT"
	
	version = GateDescription.new()
	version.name = "OUTPUT"
	version.type = "IO.OUTPUT.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ROYAL_BLUE
	version.inputs = [ PinDescription.create("IN", 0, 1) ]
	version.outputs = []
	gate.versions["IO.OUTPUT.1"] = version
	
	version = GateDescription.new()
	version.name = "OUTPUT"
	version.type = "IO.OUTPUT.8"
	version.size = Vector2i(8, 4)
	version.color = Color.ROYAL_BLUE
	version.inputs = [ PinDescription.create("IN", 0, 8) ]
	version.outputs = []
	gate.versions["IO.OUTPUT.8"] = version
	
	group.gates["IO.OUTPUT"] = gate
	
	_map["IO"] = group

func make_bus() -> void:
	var group: GroupEntry = GroupEntry.new()
	group.name = "BUS"
	
	var gate: GateEntry
	var version: GateDescription
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "MUX"
	
	version = GateDescription.new()
	version.name = "MUX"
	version.type = "IO.MUX.8"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("Select", 0, 1),
		PinDescription.create("A", 1, 8),
		PinDescription.create("B", 2, 8)
	]
	version.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	gate.versions["BUS.MUX.8"] = version
	
	group.gates["BUS.MUX"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "DEMUX"
	
	version = GateDescription.new()
	version.name = "DEMUX"
	version.type = "BUS.DEMUX.8"
	gate.group = "BUS"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("Select", 0, 1),
		PinDescription.create("IN", 1, 8)
	]
	version.outputs = [
		PinDescription.create("A", 0, 8),
		PinDescription.create("B", 1, 8)
	]
	gate.versions["BUS.DEMUX.8"] = version
	
	group.gates["BUS.DEMUX"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "DECODER"
	
	version = GateDescription.new()
	version.name = "DECODER"
	version.type = "BUS.DECODER.4"
	gate.group = "BUS"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("Select", 0, 4)
	]
	version.outputs = []
	for i in range(0, 16): # 16 Outputs, "0" to "15"
		version.outputs.append(PinDescription.create(str(i), i, 1))
	gate.versions["BUS.DECODER.4"] = version
	
	group.gates["BUS.DECODER"] = gate
	
	_map["BUS"] = group

func make_combinational() -> void:
	var group: GroupEntry = GroupEntry.new()
	group.name = "COMBINATIONAL"
	
	var gate: GateEntry
	var version: GateDescription
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "AND"
	
	version = GateDescription.new()
	version.name = "AND"
	version.type = "COMBINATIONAL.AND.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("A", 0, 1),
		PinDescription.create("B", 1, 1)
	]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["COMBINATIONAL.AND.1"] = version
	
	group.gates["COMBINATIONAL.AND"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "NAND"
	
	version = GateDescription.new()
	version.name = "NAND"
	version.type = "COMBINATIONAL.NAND.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("A", 0, 1),
		PinDescription.create("B", 1, 1)
	]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["COMBINATIONAL.NAND.1"] = version
	
	group.gates["COMBINATIONAL.NAND"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "OR"
	
	version = GateDescription.new()
	version.name = "OR"
	version.type = "COMBINATIONAL.OR.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("A", 0, 1),
		PinDescription.create("B", 1, 1)
	]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["COMBINATIONAL.OR.1"] = version
	
	group.gates["COMBINATIONAL.OR"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "NOR"
	
	version = GateDescription.new()
	version.name = "NOR"
	version.type = "COMBINATIONAL.NOR.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("A", 0, 1),
		PinDescription.create("B", 1, 1)
	]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["COMBINATIONAL.NOR.1"] = version
	
	group.gates["COMBINATIONAL.NOR"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "NOT"
	
	version = GateDescription.new()
	version.name = "NOT"
	version.type = "COMBINATIONAL.NOT.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("IN", 0, 1)
	]
	version.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	gate.versions["COMBINATIONAL.NOT.1"] = version
	
	group.gates["COMBINATIONAL.NOT"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "XOR"
	
	version = GateDescription.new()
	version.name = "XOR"
	version.type = "COMBINATIONAL.XOR.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("A", 0, 1),
		PinDescription.create("B", 1, 1)
	]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["COMBINATIONAL.XOR.1"] = version
	
	group.gates["COMBINATIONAL.XOR"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "XNOR"
	
	version = GateDescription.new()
	version.name = "XNOR"
	version.type = "COMBINATIONAL.XNOR.1"
	version.size = Vector2i(8, 4)
	version.color = Color.ORANGE
	version.inputs = [
		PinDescription.create("A", 0, 1),
		PinDescription.create("B", 1, 1)
	]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["COMBINATIONAL.XNOR.1"] = version
	
	group.gates["COMBINATIONAL.XNOR"] = gate
	
	_map["COMBINATIONAL"] = group

func make_time() -> void:
	var group: GroupEntry = GroupEntry.new()
	group.name = "TIME"
	
	var gate: GateEntry
	var version: GateDescription
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "CLOCK"
	
	version = GateDescription.new()
	version.name = "CLOCK"
	version.type = "TIME.CLOCK.*"
	version.size = Vector2i(8, 4)
	version.color = Color.REBECCA_PURPLE
	version.inputs = []
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["TIME.CLOCK.*"] = version
	
	group.gates["TIME.CLOCK"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "CAPACITOR"
	
	version = GateDescription.new()
	version.name = "CAPACITOR"
	version.type = "TIME.CAPACITOR.1"
	version.size = Vector2i(8, 4)
	version.color = Color.REBECCA_PURPLE
	version.inputs = [
		PinDescription.create("TIME", 0, 8),
		PinDescription.create("SIGNAL", 1, 1)
	]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["TIME.CAPACITOR.1"] = version
	
	group.gates["TIME.CAPACITOR"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "PULSE"
	
	version = GateDescription.new()
	version.name = "PULSE"
	version.type = "TIME.PULSE.1"
	version.size = Vector2i(8, 4)
	version.color = Color.REBECCA_PURPLE
	version.inputs = [ PinDescription.create("SIGNAL", 0, 1), ]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["TIME.PULSE.1"] = version
	
	group.gates["TIME.PULSE"] = gate
	
	# ### ### ### ###
	gate = GateEntry.new()
	gate.name = "DELAY"
	
	version = GateDescription.new()
	version.name = "DELAY"
	version.type = "TIME.DELAY.1"
	version.size = Vector2i(8, 4)
	version.color = Color.REBECCA_PURPLE
	version.inputs = [
		PinDescription.create("AMOUNT", 0, 8),
		PinDescription.create("SIGNAL", 1, 1)
	]
	version.outputs = [ PinDescription.create("OUT", 0, 1) ]
	gate.versions["TIME.DELAY.1"] = version
	
	group.gates["TIME.DELAY"] = gate
	
	_map["TIME"] = group

#endregion
