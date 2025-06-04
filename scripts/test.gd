extends Node

var simulation: Simulation
var circuit: Circuit

# 8-Bit Adder using sub-chips

func _ready() -> void:
	var time: TimeIt = TimeIt.new()
	# ### Testing 1
	time.register_start("test")
	time.register_start("creation")
	circuit = Circuit.new()
	
	circuit.inputs = [
		
	]
	
	# ### Testing 2
	time.register_stop("creation")
	
	print("\nGates (id): \n%s" % str(circuit.gates.keys()))
	print("\nConns (id): \n%s" % str(circuit.connections.keys()))
	print("\nIN    (id): \n%s" % str(circuit.inputs))
	print("\nOUT   (id): \n%s" % str(circuit.outputs))
	print("\nC_IN  (id): \n%s" % str(circuit.input_config))
	print("\nC_OUT (id): \n%s" % str(circuit.output_config))
	print()
	var flattened: Array[Dictionary] = circuit.flatten_recursive()
	print("\nF Gates (id): \n%s" % str(flattened[0].keys()))
	print("\nF Conns (id): \n%s\n" % str(flattened[1].keys()))
	
	time.register_start("test_cpu_8bit_compiling")
	var final: CachedCircuit = CachedCircuit.new(circuit)
	time.register_stop("test_cpu_8bit_compiling")
	time.register_stop("test_cpu_8bit")
	time.result()
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
	print("IO IN Map :\n%s\n" % str(final.data["io_in_map"]))
	print("IO OUT Map :\n%s\n" % str(final.data["io_out_map"]))
	print("IO Conn List :\n%s\n" % str(final.data["io_conn_list"]))
	print("Dependency (what the gate depends on):\n%s\n" % str(final.data["dependency"]))
	print("R Rependency (gates that depend on this one):\n%s\n" % str(final.data["reverse_dependency"]))
	print("Removed List :\n%s\n" % str(final.data["removed_list"]))
