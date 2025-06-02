# The name of the Class
class_name CachedCircuit
# The class this class extends
extends Object
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
var gates: Dictionary[int, CachedGate]

## The Connections of the Circuit
var connections: Dictionary[int, CachedConnection]

## The Parallel schedule of the CachedCircuit
var parallel_schedule: Dictionary[int, Array]

## The Dirty Gate IDs of the Circuit
var dirty_gates: Array[int]

## The Static Gate IDs of the Circuit
var static_gates: Array[int]

## The Complexity of the Circuit
var complexity: float

## How many ticks are needed to fully simulate it
var ticks: int

## If the circuit is valid[br]
## Invalid examples : 2 gates writing to the same port without using a gate inbetween
var valid: bool

## All of the Circuits possible Data
var data: Dictionary[String, Variant]

# private variables

# @onready variables

# optional built-in _init() function
func _init(from: Circuit) -> void:
	# Step  1 : Create tempoary variables
	var gate_map: Dictionary[int, CachedGate] = {} # 		GateID to Gate Map 			| Gate ID (int) 		-> Gate (CachedGate)
	var conn_map: Dictionary[int, CachedConnection] = {} # 	ConnID to Conn Map			| Conn ID (int) 		-> Connection (CachedConnection)
	var rank_map: Dictionary[int, int] = {} #				Rank Map					| Gate ID (int) 		-> Rank (int)
	var ranked_map: Dictionary[int, Array] = {} #			Ranked Gates				| Rank (int) 			-> Gates (Array(CachedGate))
	var gate_id_map: Dictionary[String, int] = {} #			Old ID to New ID Map		| Gate ID (String) 		-> Gate ID (int)
	var old_gate_map: Dictionary[int, String] = {} #		New ID to Old ID Map		| Gate ID (int) 		-> Gate ID (String)
	var old_conn_map: Dictionary[int, String] = {} #		New ID to Old ID Map		| Conn ID (int) 		-> Conn ID (String)
	var conn_id_map: Dictionary[String, int] = {} #			Old ID to New ID Map		| Conn ID (String) 		-> Conn ID (int)
	var port_write_map: Dictionary[String, int] = {} #		Times a port is written to	| Gate + Port (String)	-> Tiems (int)
	#var orig_merged_map: Dictionary[String, int] = {} #	Old Gate to New Gate Map	| Gate ID (int) 		-> Gate ID (int)
	var dependency: Dictionary[int, Array] = {} #			Dependendencies of a Gates	| Gate ID (int) 		-> Gate IDs (Array(int))
	var reverse_dependency: Dictionary[int, Array] = {} #	Dependants of a Gate		| Gate ID (int) 		-> Gate IDs (Array(int))
	var removed_list: Array[String] = [] #					Removed Gates				| Gate IDs (Array(String))
	#var stat_list: Array[int] = [] #						Static Gates				| Gate IDs (Array(int))
	#var loop_list: Array[int] = [] # 						Looped Gates				| Gate IDs (Array(int))
	var complex: float = 0.0 #								Complexity fo the Circuit	| (float)
	
	# Step  2 : Flatten the Circuit
	var f_res: Array[Dictionary] = from.flatten_recursive()
	var f_gates: Dictionary[String, GateDescription] = f_res[0]
	var f_conns: Dictionary[String, Connection] = f_res[1]
	
	# Step  3 : Convert to cached class and build dependency graph
	# Step  4.1 : Prune disconnected Gates (no input, no output) | Check handled inside connection loop
	# Step  5.1 : Conflict input detection | Check handled inside connection loop
	var id_count: int = 0
	var in_connected_gates: Array[int] = []
	var out_connected_gates: Array[int] = []
	ticks = 1
	for gate: GateDescription in f_gates.values():
		var cached = CachedGate.from_description(gate)
		cached.id = id_count
		gate_map[id_count] = cached
		dependency[id_count] = []
		reverse_dependency[id_count] = []
		gate_id_map[gate.id] = id_count
		old_gate_map[id_count] = gate.id
		ticks = max(ticks, gate.ticks)
		if gate.type == "IN":
			rank_map[id_count] = 0
			in_connected_gates.append(id_count)
		elif gate.type == "OUT":
			rank_map[id_count] = 255
			out_connected_gates.append(id_count)
		id_count += 1
	
	id_count = 0
	for connection: Connection in f_conns.values():
		var cached = CachedConnection.new()
		cached.id = id_count
		cached.from_gate = gate_map[gate_id_map[connection.gate_in]]
		cached.from_port = connection.port_in
		cached.to_gate = gate_map[gate_id_map[connection.gate_out]]
		cached.to_port = connection.port_out
		conn_map[id_count] = cached
		old_conn_map[id_count] = connection.id
		dependency[cached.to_gate].append(cached.from_gate)
		reverse_dependency[cached.from_gate].append(cached.to_gate)
		conn_id_map[connection.id] = id_count
		id_count += 1
		# Step 4.1 here
		if not cached.from_gate in out_connected_gates:
			out_connected_gates.append(cached.from_gate)
		if not cached.to_gate in in_connected_gates:
			in_connected_gates.append(cached.to_gate)
		# Step 5.1 here
		var string: String = str(cached.to_gate.id) + "|" + str(cached.to_port)
		if port_write_map.has(string):
			port_write_map[string] += 1
		else:
			port_write_map[string] = 0
	
	# Step  4.2 : Prune disconnected Gates (no input, no output) | Removal
	var changed: bool = true
	var removed_new_list: Array[int] = []
	while changed == true:
		changed = false
		for id: int in gate_map.keys():
			if id in in_connected_gates and id in out_connected_gates: # Gate wont be pruned
				continue
			in_connected_gates.erase(id)
			out_connected_gates.erase(id)
			removed_list.append(old_gate_map[id])
			removed_new_list.append(id)
			gate_map.erase(id)
			dependency.erase(id)
			reverse_dependency.erase(id)
			gate_id_map.erase(old_gate_map[id])
			old_gate_map.erase(id)
			changed = true
	
	# Step 4.3 : Check connection validity
	changed = true
	while changed == true:
		changed = false
		for connection: CachedConnection in conn_map.values():
			if not(connection.from_gate.id in removed_new_list) and not(connection.to_gate.id in removed_new_list):
				continue
			var id: int = connection.id
			conn_map.erase(id)
			conn_id_map.erase(old_conn_map[id])
			old_conn_map.erase(id)
			if dependency.has(connection.to_gate):
				dependency[connection.to_gate.id].erase(connection.from_gate.id)
			if reverse_dependency.has(connection.from_gate):
				reverse_dependency[connection.from_gate.id].erase(connection.to_gate.id)
			changed = true
	
	# Step 5.2 : Check if any port has multiple writes
	if port_write_map.values().max() > 1:
		valid = false
		printerr("Invalid Circuit! Conflicting port writes")
	
	# Step  6 : Merging identical gate sequences (same result)
	# <skipped for now>
	# uses : orig_merged_map
	
	# Step  7 : Constant Propagation and Collapsing
	# <skipped for now>
	# uses : stat_list
	
	# Step  8 : Combinational Loops (DFS cycle detection)
	# <skipped for now>
	# uses : loop_list
	
	# Step  9 : Rank calculation (forward dependency depth), the lower, the sooner it will be simulated
	for id: int in gate_map.keys():
		gate_map[id].rank = _compute_rank(id, [], rank_map, dependency)
	
	# Step 10 : LUT optimization
	# <skipped for now>
	
	# Step 11 : Sort gates by rank:
	var sorted_gates: Array[CachedGate] = gate_map.values().duplicate()
	sorted_gates.sort_custom(func(a, b): return a.rank < b.rank)
	
	# Step 12 : Group gats by rank for parallel execution (thread partitioning
	for gate: CachedGate in sorted_gates:
		if not ranked_map.has(gate.rank):
			ranked_map[gate.rank] = [gate]
			continue
		ranked_map[gate.rank].append(gate)
	 
	# Step 13 : Calculate Complexity
	for rank: int in ranked_map.keys():
		complex += log(1.0 + len(ranked_map[rank]))
	complex = float(complex) / float(ranked_map.size() - 1) # complexity = complexity / number of ranks
	complex = max(complex * 0.75, 1.0) # down scaled by 75%
	
	# Step 14 : Make final cached circuit
	gates = gate_map
	connections = conn_map
	parallel_schedule = ranked_map
	complexity = complex
	data = {
		"gate_map": gate_map,
		"conn_map": conn_map,
		"rank_map": rank_map,
		"ranked_map": ranked_map,
		"gate_id_map": gate_id_map,
		"conn_id_map": conn_id_map,
		"old_gate_map": old_gate_map,
		"old_conn_map": old_conn_map,
		"dependency": dependency,
		"reverse_dependency": reverse_dependency,
		"removed_list": removed_list,
		"complexity": complex,
		"ticks": ticks,
		"valid": valid
	}

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions
func _compute_rank(id: int, visited: Array[int], map: Dictionary[int, int], dependency: Dictionary[int, Array]) -> int:
		if id in map.keys():
			return map[id]
		if id in visited:
			return 0
		var max_rank = 0
		for p_id: int in dependency[id]:
			max_rank = max(max_rank, _compute_rank(p_id, visited, map, dependency))
		max_rank += 1
		map[id] = max_rank
		return max_rank

# subclasses
