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
var parallel_schedule: Array[Array]

## The Static Gate IDs of the Circuit
var static_gates: Array[int]

## The Dirty Gate IDs of the Circuit
var dirty_gates: Array[int]

# private variables

# @onready variables

# optional built-in _init() function
func _init(from: Circuit, threads: int = 4) -> void:
	# Step  1 : Create tempoary variables
	var gate_map: Dictionary[int, CachedGate] = {} # 		GateID to Gate Map 			| Gate ID (int) 		-> Gate (CachedGate)
	var conn_map: Dictionary[int, CachedConnection] = {} # 	ConnID to Conn Map			| Conn ID (int) 		-> Connection (CachedConnection)
	var rank_map: Dictionary[int, int] = {} #				Rank Map					| Gate ID (int) 		-> Rank (int)
	var ranked_map: Dictionary[int, Array] = {} #			Ranked Gates				| Rank (int) 			-> Gates (Array(CachedGate))
	var gate_id_map: Dictionary[String, int] = {} #			Old ID to New ID Map		| Gate ID (String) 		-> Gate ID (int)
	var conn_id_map: Dictionary[String, int] = {} #			Old ID to New ID Map		| Conn ID (String) 		-> Conn ID (int)
	#var orig_merged_map: Dictionary[String, int] = {} #		Old Gate to New Gate Map	| Gate ID (int) 		-> Gate ID (int)
	var dependency: Dictionary[int, Array] = {} #			Dependendencies of a Gates	| Gate ID (int) 		-> Gate IDs (Array(int))
	var reverse_dependency: Dictionary[int, Array] = {} #	Dependants of a Gate		| Gate ID (int) 		-> Gate IDs (Array(int))
	#var stat_list: Array[int] = [] #						Static Gates				| Gate IDs (Array(int))
	#var loop_list: Array[int] = [] # 						Looped Gates				| Gate IDs (Array(int))
	
	# Step  2 : Flatten the Circuit
	var f_res: Array[Dictionary] = from.flatten_recursive()
	var f_gates: Dictionary[String, GateDescription] = f_res[0]
	var f_conns: Dictionary[String, Connection] = f_res[1]
	
	# Step  3 : Convert to cached class and build dependency graph
	# Step  4.1 : Prune disconnected Gates (no input, no output) | Check handled inside connection loop
	var id_count: int = 0
	for gate: GateDescription in f_gates.values():
		var cached = CachedGate.from_description(gate)
		cached.id = id_count
		gate_map[id_count] = cached
		dependency[id_count] = []
		reverse_dependency[id_count] = []
		gate_id_map[gate.id] = id_count
		dirty_gates
		id_count += 1
	
	id_count = 0
	var connected_gates: Array[int] = []
	for connection: Connection in f_conns.values():
		var cached = CachedConnection.new()
		cached.id = id_count
		cached.from_gate = gate_id_map[connection.gate_in]
		cached.from_port = connection.port_in
		cached.to_gate = gate_id_map[connection.gate_out]
		cached.to_port = connection.port_out
		conn_map[id_count] = cached
		dependency[cached.to_gate].append(cached.from_gate)
		reverse_dependency[cached.from_gate].append(cached.to_gate)
		conn_id_map[connection.id] = id_count
		id_count += 1
		# Step 4 here
		if not cached.from_gate in connected_gates:
			connected_gates.append(cached.from_gate)
		if not cached.to_gate in connected_gates:
			connected_gates.append(cached.to_gate)
	
	# Step  4.2 : Prune disconnected Gates (no input, no output) | Removal 
	for id: int in gate_map.keys():
		if id in connected_gates: # Gate wont be pruned
			continue
		gate_map.erase(id)
		dependency.erase(id)
		reverse_dependency.erase(id)
		gate_id_map.erase(id)
	
	# Step  5 : Merging duplicated gates (identical io)
	# <skipped for now>
	# uses : orig_merged_map
	
	# Step  6 : Constant Propagation and Collapsing
	# <skipped for now>
	# uses : stat_list
	
	# Step  7 : Combinational Loops (DFS cycle detection)
	# <skipped for now>
	# uses : loop_list
	
	# Step  8 : Rank calculation (forward dependency depth), the lower, the sooner it will be simulated
	for id: int in gate_map.keys():
		gate_map[id].rank = _compute_rank(id, [], rank_map, dependency)
	
	# Step  9 : LUT optimization
	# <skipped for now>
	
	# Step 10 : Sort gates by rank:
	var sorted_gates: Array[CachedGate] = gate_map.values().duplicate()
	sorted_gates.sort_custom(func(a, b): return a.rank < b.rank)
	
	# Step 11 : Group gats by rank for parallel execution (thread partitioning
	for gate: CachedGate in sorted_gates:
		if not ranked_map.has(gate.rank):
			ranked_map[gate.rank] = [gate]
			continue
		ranked_map[gate.rank].append(gate)
	 
	# Step 12 : Make final cached circuit
	

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
