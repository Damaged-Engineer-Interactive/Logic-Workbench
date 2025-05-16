extends Node

var in_gate: GateDescription = GateDescription.new()
var out_gate: GateDescription = GateDescription.new()

var b8_in_gate: GateDescription = GateDescription.new()
var b8_out_gate: GateDescription = GateDescription.new()

var bus_1_8

var and_gate: GateDescription = GateDescription.new()
var nand_gate: GateDescription = GateDescription.new()
var or_gate: GateDescription = GateDescription.new()
var nor_gate: GateDescription = GateDescription.new()
var not_gate: GateDescription = GateDescription.new()
var xor_gate: GateDescription = GateDescription.new()
var xnor_gate: GateDescription = GateDescription.new()

var b8_and_gate: GateDescription = GateDescription.new()
var b8_nand_gate: GateDescription = GateDescription.new()
var b8_or_gate: GateDescription = GateDescription.new()
var b8_nor_gate: GateDescription = GateDescription.new()
var b8_not_gate: GateDescription = GateDescription.new()
var b8_xor_gate: GateDescription = GateDescription.new()
var b8_xnor_gate: GateDescription = GateDescription.new()

var b8_mux: GateDescription = GateDescription.new()
var b8_adder: GateDescription = GateDescription.new()
var b8_register: GateDescription = GateDescription.new()
var b8_rom: GateDescription = GateDescription.new()
var b8_ram: GateDescription = GateDescription.new()
var dec_3_8: GateDescription = GateDescription.new()

var timeit: TimeIt = TimeIt.new()

func _ready() -> void:
	print("Preparing Testing")
	timeit.register_start("preparing")
	
	#region IO
	in_gate.name = "INPUT"
	in_gate.type = "IN"
	in_gate.group = "IO"
	in_gate.size = Vector2i(8, 4)
	in_gate.color = Color.ROYAL_BLUE
	in_gate.inputs = []
	in_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	
	out_gate.name = "OUTPUT"
	out_gate.type = "OUT"
	out_gate.group = "IO"
	out_gate.size = Vector2i(8, 4)
	out_gate.color = Color.ROYAL_BLUE
	out_gate.inputs = [
		PinDescription.create("IN", 0, 1)
	]
	out_gate.outputs =[]
	
	b8_in_gate.name = "8-INPUT"
	b8_in_gate.type = "IN8"
	b8_in_gate.group = "IO"
	b8_in_gate.size = Vector2i(8, 4)
	b8_in_gate.color = Color.ROYAL_BLUE
	b8_in_gate.inputs = []
	b8_in_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	
	b8_out_gate.name = "8-OUTPUT"
	b8_out_gate.type = "OUT8"
	b8_out_gate.group = "IO"
	b8_out_gate.size = Vector2i(8, 4)
	b8_out_gate.color = Color.ROYAL_BLUE
	b8_out_gate.inputs = [
		PinDescription.create("IN", 0, 1)
	]
	b8_out_gate.outputs =[]
	#endregion
	
	#region COMBINATIONAL
	and_gate.name = "AND"
	and_gate.type = "AND"
	and_gate.group = "COMBINATIONAL"
	and_gate.size = Vector2i(8, 4)
	and_gate.color = Color.CRIMSON
	and_gate.inputs = [
		PinDescription.create("IN A", 0, 1),
		PinDescription.create("IN B", 1, 1)
	]
	and_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	
	nand_gate.name = "NAND"
	nand_gate.type = "NAND"
	nand_gate.group = "COMBINATIONAL"
	nand_gate.size = Vector2i(8, 4)
	nand_gate.color = Color.CRIMSON
	nand_gate.inputs = [
		PinDescription.create("IN A", 0, 1),
		PinDescription.create("IN B", 1, 1)
	]
	nand_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	
	or_gate.name = "OR"
	or_gate.type = "OR"
	or_gate.group = "COMBINATIONAL"
	or_gate.size = Vector2i(8, 4)
	or_gate.color = Color.CRIMSON
	or_gate.inputs = [
		PinDescription.create("IN A", 0, 1),
		PinDescription.create("IN B", 1, 1)
	]
	or_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	
	nor_gate.name = "NOR"
	nor_gate.type = "NOR"
	nor_gate.group = "COMBINATIONAL"
	nor_gate.size = Vector2i(8, 4)
	nor_gate.color = Color.CRIMSON
	nor_gate.inputs = [
		PinDescription.create("IN A", 0, 1),
		PinDescription.create("IN B", 1, 1)
	]
	nor_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	
	not_gate.name = "NOT"
	not_gate.type = "NOT"
	not_gate.group = "COMBINATIONAL"
	not_gate.size = Vector2i(8, 4)
	not_gate.color = Color.CRIMSON
	not_gate.inputs = [
		PinDescription.create("IN A", 0, 1),
		PinDescription.create("IN B", 1, 1)
	]
	not_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	
	xor_gate.name = "XOR"
	xor_gate.type = "XOR"
	xor_gate.group = "COMBINATIONAL"
	xor_gate.size = Vector2i(8, 4)
	xor_gate.color = Color.CRIMSON
	xor_gate.inputs = [
		PinDescription.create("IN A", 0, 1),
		PinDescription.create("IN B", 1, 1)
	]
	xor_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	
	xnor_gate.name = "XNOR"
	xnor_gate.type = "XNOR"
	xnor_gate.group = "COMBINATIONAL"
	xnor_gate.size = Vector2i(8, 4)
	xnor_gate.color = Color.CRIMSON
	xnor_gate.inputs = [
		PinDescription.create("IN A", 0, 1),
		PinDescription.create("IN B", 1, 1)
	]
	xnor_gate.outputs = [
		PinDescription.create("OUT", 0, 1)
	]
	#endregion
	
	#region BITWISE
	b8_and_gate.name = "8-AND"
	b8_and_gate.type = "AND8"
	b8_and_gate.group = "BITWISE"
	b8_and_gate.size = Vector2i(8, 4)
	b8_and_gate.color = Color.CRIMSON
	b8_and_gate.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8)
	]
	b8_and_gate.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	
	b8_nand_gate.name = "8-NAND"
	b8_nand_gate.type = "NAND8"
	b8_nand_gate.group = "BITWISE"
	b8_nand_gate.size = Vector2i(8, 4)
	b8_nand_gate.color = Color.CRIMSON
	b8_nand_gate.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8)
	]
	b8_nand_gate.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	
	b8_or_gate.name = "8-OR"
	b8_or_gate.type = "OR8"
	b8_or_gate.group = "BITWISE"
	b8_or_gate.size = Vector2i(8, 4)
	b8_or_gate.color = Color.CRIMSON
	b8_or_gate.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8)
	]
	b8_or_gate.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	
	b8_nor_gate.name = "8-NOR"
	b8_nor_gate.type = "NOR8"
	b8_nor_gate.group = "BITWISE"
	b8_nor_gate.size = Vector2i(8, 4)
	b8_nor_gate.color = Color.CRIMSON
	b8_nor_gate.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8)
	]
	b8_nor_gate.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	
	b8_not_gate.name = "8-NOT"
	b8_not_gate.type = "NOT8"
	b8_not_gate.group = "BITWISE"
	b8_not_gate.size = Vector2i(8, 4)
	b8_not_gate.color = Color.CRIMSON
	b8_not_gate.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8)
	]
	b8_not_gate.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	
	b8_xor_gate.name = "8-XOR"
	b8_xor_gate.type = "XOR8"
	b8_xor_gate.group = "BITWISE"
	b8_xor_gate.size = Vector2i(8, 4)
	b8_xor_gate.color = Color.CRIMSON
	b8_xor_gate.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8)
	]
	b8_xor_gate.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	
	b8_xnor_gate.name = "8-XNOR"
	b8_xnor_gate.type = "XNOR8"
	b8_xnor_gate.group = "BITWISE"
	b8_xnor_gate.size = Vector2i(8, 4)
	b8_xnor_gate.color = Color.CRIMSON
	b8_xnor_gate.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8)
	]
	b8_xnor_gate.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	#endregion
	
	#region OTHER
	b8_mux.name = "8-MUX"
	b8_mux.type = "MUX8"
	b8_mux.group = "OTHER"
	b8_mux.size = Vector2i(8, 4)
	b8_mux.color = Color.DIM_GRAY
	b8_mux.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8),
		PinDescription.create("SEL", 2, 1)
	]
	b8_mux.outputs = [
		PinDescription.create("OUT", 0, 8)
	]
	
	b8_adder.name = "8-ADD"
	b8_adder.type = "ADD8"
	b8_adder.group = "OTHER"
	b8_adder.size = Vector2i(8, 4)
	b8_adder.color = Color.DIM_GRAY
	b8_adder.inputs = [
		PinDescription.create("IN A", 0, 8),
		PinDescription.create("IN B", 1, 8),
		PinDescription.create("CARRY", 2, 1)
	]
	b8_adder.outputs = [
		PinDescription.create("OUT", 0, 8),
		PinDescription.create("CARRY", 1, 1)
	]
	
	b8_register.name = "8-REG"
	b8_register.type = "REG8"
	b8_register.group = "OTHER"
	b8_register.size = Vector2i(8, 4)
	b8_register.color = Color.DIM_GRAY
	b8_register.inputs = [
		PinDescription.create("DATA", 0, 8),
		PinDescription.create("CLOCK", 1, 1),
		PinDescription.create("STORE", 2, 1)
	]
	b8_register.outputs = [
		PinDescription.create("OUT", 0, 8),
	]
	
	b8_rom.name = "8-ROM"
	b8_rom.type = "ROM8"
	b8_rom.group = "OTHER"
	b8_rom.size = Vector2i(8, 4)
	b8_rom.color = Color.DIM_GRAY
	b8_rom.inputs = [
		PinDescription.create("ADDR", 0, 8),
	]
	b8_rom.outputs = [
		PinDescription.create("OUT", 0, 8),
	]
	
	b8_ram.name = "8-RAM"
	b8_ram.type = "RAM8"
	b8_ram.group = "OTHER"
	b8_ram.size = Vector2i(8, 4)
	b8_ram.color = Color.DIM_GRAY
	b8_ram.inputs = [
		PinDescription.create("ADDR", 0, 8),
		PinDescription.create("DATA", 1, 8),
		PinDescription.create("WRITE", 2, 1)
	]
	b8_ram.outputs = [
		PinDescription.create("OUT", 0, 8),
	]
	
	dec_3_8.name = "3-DEC"
	dec_3_8.type = "DEC3"
	dec_3_8.group = "OTHER"
	dec_3_8.size = Vector2i(8, 4)
	dec_3_8.color = Color.DIM_GRAY
	dec_3_8.inputs = [
		PinDescription.create("IN", 0, 3)
	]
	dec_3_8.outputs = [
		PinDescription.create("A", 0, 1),
		PinDescription.create("B", 1, 1),
		PinDescription.create("C", 2, 1),
		PinDescription.create("D", 3, 1),
		PinDescription.create("E", 4, 1),
		PinDescription.create("F", 5, 1),
		PinDescription.create("G", 6, 1),
		PinDescription.create("H", 7, 1)
	]
	
	#endregion
	
	timeit.register_stop("preparing")
	timeit.result()
	
	print("Starting Testing\n\n")
	
	test_adder()
	
	print("Ending Testing\n\n")

func test_adder() -> void:
	timeit.register_start("test_adder")
	timeit.register_start("test_adder_building")
	var gates: Dictionary[String, GateDescription] = {
		"0:0"	: in_gate.copy("0:0"),			# IN A
		"0:1"	: in_gate.copy("0:1"),			# IN B
		"0:2"	: in_gate.copy("0:2"),			# IN C
		
		"0:3"	: xor_gate.copy("0:3"), 		# XOR 1
		"0:4"	: and_gate.copy("0:4"), 		# AND 1
		
		"0:5"	: xor_gate.copy("0:5"), 		# XOR 2
		"0:6"	: and_gate.copy("0:6"), 		# AND 2
		
		"0:7"	: or_gate.copy("0:7"),			# OR
		"0:8"	: out_gate.copy("0:8"),			# OUT
		"0:9"	: out_gate.copy("0:9"),			# OUT CARRY
		
		"1:0"	: and_gate.copy("1:0"),			# UNUSED (single)
		"1:1"	: not_gate.copy("1:1"),			# UNUSED (multi)
		"1:2"	: not_gate.copy("1:2"),			# UNUSED (multi)
		"1:3"	: and_gate.copy("1:3"),			# UNUSED (multi)
	}
	
	var connections: Dictionary[String, Connection] = {#
		# 1st Half Adder
		"0:0"	: Connection.create("0:0", "0:0", 0, "0:3", 0),		# IN A      @ 0 -> XOR 1     @ 0
		"0:1"	: Connection.create("0:1", "0:1", 0, "0:3", 1),		# IN B      @ 0 -> XOR 1     @ 1
		"0:2"	: Connection.create("0:2", "0:0", 0, "0:4", 0),		# IN A      @ 0 -> AND 1     @ 0
		"0:3"	: Connection.create("0:3", "0:1", 0, "0:4", 1),		# IN B      @ 0 -> AND 1     @ 1
		
		# 2nd Half Adder
		"0:4"	: Connection.create("0:4", "0:3", 0, "0:5", 0),		# XOR 1     @ 0 -> XOR 2     @ 0
		"0:5"	: Connection.create("0:5", "0:2", 0, "0:5", 1),		# IN C      @ 0 -> XOR 2     @ 1
		"0:6"	: Connection.create("0:6", "0:3", 0, "0:6", 0),		# XOR 2     @ 0 -> AND 2     @ 0
		"0:7"	: Connection.create("0:7", "0:2", 0, "0:6", 1),		# IN C      @ 0 -> AND 2     @ 1
		
		# Carry
		"0:8"	: Connection.create("0:8", "0:4", 0, "0:7", 0),		# AND 1     @ 0 -> OR        @ 0
		"0:9"	: Connection.create("0:9", "0:6", 0, "0:7", 1),		# AND 2     @ 0 -> OR        @ 1
		"0:10"	: Connection.create("0:10", "0:7", 0, "0:9", 0),	# OR        @ 0 -> OUT Carry @ 0
		
		# Output
		"0:11"	: Connection.create("0:11", "0:5", 0, "0:8", 0),	# XOR 2     @ 0 -> OUT       @ 0
		
		# Unused (should be purged)
		"1:0"	: Connection.create("1:0", "1:1", 0, "1:3", 0),
		"1:1"	: Connection.create("1:1", "1:2", 0, "1:3", 1)
	}
	var circuit: Circuit = Circuit.new()
	circuit.gates = gates
	circuit.connections = connections
	timeit.register_stop("test_adder_building")
	
	print("\nGates (id): \n%s" % str(gates.keys()))
	print("\nConns (id): \n%s\n" % str(connections.keys()))
	var flattened: Array[Dictionary] = circuit.flatten_recursive()
	print("\nF Gates (id): \n%s" % str(flattened[0].keys()))
	print("\nF Conns (id): \n%s\n" % str(flattened[1].keys()))
	
	timeit.register_start("test_adder_compiling")
	var final: CachedCircuit = CachedCircuit.new(circuit)
	timeit.register_stop("test_adder_compiling")
	timeit.register_stop("test_adder")
	timeit.result()
	print("\n")
	print("Gate Map :\n%s\n" % str(final.data["gate_map"].keys()))
	print("Conn Map :\n%s\n" % str(final.data["conn_map"].keys()))
	
	print("Rank Map :\n%s\n" % str(final.data["rank_map"]))
	print("Ranked Map :")
	var ranked_map: Dictionary = final["data"]["ranked_map"]
	for rank: int in ranked_map.keys():
		var id_string: String = ""
		for gate: CachedGate in ranked_map[rank]:
			id_string += str(gate.id) + ", "
		print("%3s : [%s]" % [rank, id_string.rstrip(", ")])
	print("")
	#print("Rank Map : {%s}" % ", ".join(["%s : %s" % [k, v.id] for k in my_dict.keys()]))
	
	
	print("Gate ID Map :\n%s\n" % str(final.data["gate_id_map"]))
	print("Conn ID Map :\n%s\n" % str(final.data["conn_id_map"]))
	print("Old Gate ID Map :\n%s\n" % str(final.data["old_gate_map"]))
	print("Old Conn Map :\n%s\n" % str(final.data["old_conn_map"]))
	print("Dependency (what the gate depends on):\n%s\n" % str(final.data["dependency"]))
	print("R Rependency (gates that depend on this one):\n%s\n" % str(final.data["reverse_dependency"]))
	print("Removed List :\n%s\n" % str(final.data["removed_list"]))
	print("Complexity :\n%s\n" % str(final.data["complexity"]))

func test_cpu_8bit() -> void:
	timeit.register_start("test_cpu_8bit")
	timeit.register_start("test_cpu_8bit_building")
	var gates: Dictionary[String, GateDescription] = {}
	var connections: Dictionary[String, Connection] = {}
	
	var circuit: Circuit = Circuit.new()
	circuit.gates = gates
	circuit.connections = connections
	timeit.register_stop("test_cpu_8bit_building")
	
	print("\nGates (id): \n%s" % str(gates.keys()))
	print("\nConns (id): \n%s\n" % str(connections.keys()))
	var flattened: Array[Dictionary] = circuit.flatten_recursive()
	print("\nF Gates (id): \n%s" % str(flattened[0].keys()))
	print("\nF Conns (id): \n%s\n" % str(flattened[1].keys()))
	
	timeit.register_start("test_cpu_8bit_compiling")
	var final: CachedCircuit = CachedCircuit.new(circuit)
	timeit.register_stop("test_cpu_8bit_compiling")
	timeit.register_stop("test_cpu_8bit")
	timeit.result()
	print("\n")
	print("Gate Map :\n%s\n" % str(final.data["gate_map"].keys()))
	print("Conn Map :\n%s\n" % str(final.data["conn_map"].keys()))
	
	print("Rank Map :\n%s\n" % str(final.data["rank_map"]))
	print("Ranked Map :")
	var ranked_map: Dictionary = final["data"]["ranked_map"]
	for rank: int in ranked_map.keys():
		var id_string: String = ""
		for gate: CachedGate in ranked_map[rank]:
			id_string += str(gate.id) + ", "
		print("%3s : [%s]" % [rank, id_string.rstrip(", ")])
	print("")
	
	print("Gate ID Map :\n%s\n" % str(final.data["gate_id_map"]))
	print("Conn ID Map :\n%s\n" % str(final.data["conn_id_map"]))
	print("Old ID Map :\n%s\n" % str(final.data["old_id_map"]))
	print("Dependency (what the gate depends on):\n%s\n" % str(final.data["dependency"]))
	print("R Rependency (gates that depend on this one):\n%s\n" % str(final.data["reverse_dependency"]))
	print("Removed List :\n%s\n" % str(final.data["removed_list"]))
