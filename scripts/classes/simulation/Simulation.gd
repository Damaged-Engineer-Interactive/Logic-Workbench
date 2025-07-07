class_name Simulation
extends Node

@warning_ignore_start("unused_signal")
signal sim_started(mode: SimMode, steps: int)	# beginning of the sim			| mode			| (optional) steps
signal sim_stopped(ticks: int)					# end of the sim				| tick number
signal tick_started(tick: int)					# beginning of a new tick 		| tick number
signal tick_ended(tick: int)					# end of the last tick			| tick number
signal tps(event: TPSEvent)						# low / normal / high TPS
signal error(tick: int, gates: Array[int])		# error (sim stop)				| tick number	| gate ids
@warning_ignore_restore("unused_signal")

enum TPSEvent { LOW, NORMAL, HIGH }

enum SimMode {
	STOP,				# Do Nothing
	ACTIVE,				# Run
	STEP				# Run for a number of steps
}

var circuit: CachedCircuit

var needed_threads: int
var task_id: int

var mode: SimMode

## How many sub-ticks are needed per tick to fully simulate the circuit
var sub_ticks: int

## Used by SimMode.STEP
var steps: int

## Current Tick
var tick: int

## Current gates to simulate
var gates: Array[CachedGate]

func _enter_tree() -> void:
	process_priority = -1
	process_mode = Node.PROCESS_MODE_ALWAYS
	process_thread_group = Node.PROCESS_THREAD_GROUP_SUB_THREAD
	process_thread_group_order = -1
	process_thread_messages = Node.FLAG_PROCESS_THREAD_MESSAGES
	name = "SIMULATION"

## The Simulation can only be initialised from a valid CachedCircuit
func setup(from: CachedCircuit) -> void:
	circuit = from
	
	needed_threads = max(1, roundi(circuit.complexity))
	task_id = -1
	
	mode = SimMode.STOP
	sub_ticks = circuit.ticks * circuit.parallel_schedule.keys().size() # amount of ticks needed per step * amount of ranks
	steps = 0
	tick = 0
	
	# Set all values to their default
	gates = circuit.gates.values()
	task_id = WorkerThreadPool.add_group_task(_simulate_gate, gates.size(), -1, true, "Set Defaults")
	# small delay, makes it a coroutine | 50 msec realtime
	print("Set Defaults :")
	while not WorkerThreadPool.is_group_task_completed(task_id):
		print("Count : [%s]" % str(WorkerThreadPool.get_group_processed_element_count(task_id)))
		await get_tree().create_timer(0.05, true, true, true).timeout
	# cleanup
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	print()

func _process(_delta: float) -> void:
	match mode:
		SimMode.ACTIVE:
			simulate_tick()
		SimMode.STEP:
			if steps > 0:
				simulate_tick()
			else:
				mode = SimMode.STOP

## Update the Simulation with a new Circuit
func update(new: CachedCircuit) -> void:
	setup(new)

## Run the Simulation (mode: TPS)
func run() -> void:
	if circuit.valid and mode == SimMode.STOP:
		mode = SimMode.ACTIVE
		sim_started.emit(mode, -1)

## Run the Simulation (mode: STEP)
func step(amount: int) -> void:
	if circuit.valid and mode == SimMode.STOP:
		steps = amount
		mode = SimMode.STEP
		sim_started.emit(mode, amount)

## Stop the Simulation
func stop() -> void:
	if mode != SimMode.STOP:
		mode = SimMode.STOP
		sim_stopped.emit(tick)

func simulate_tick():
	print("Simulate Tick : [%s]" % tick)
	for rank: int in circuit.parallel_schedule.keys():
		await _simulate_rank(rank)
		await _simulate_connections()
	tick += 1
	if mode == SimMode.STEP:
		steps -= 1

func _simulate_rank(rank: int) -> void:
	gates = circuit.parallel_schedule[rank]
	task_id = WorkerThreadPool.add_group_task(_simulate_gate, gates.size(), -1, true, "Simulate Rank [%s]" % str(rank))
	# small delay, makes it a coroutine | 50 msec realtime
	print("Simulate Rank : [%s] <%s>" % [str(rank), str(gates.size())])
	while not WorkerThreadPool.is_group_task_completed(task_id):
		#print("Count : [%s]" % str(WorkerThreadPool.get_group_processed_element_count(task_id)))
		await get_tree().create_timer(0.01, true, true, true).timeout
	# cleanup
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	print()

func _simulate_connections() -> void:
	var connections: Array[CachedConnection] = circuit.connections.values()
	task_id = WorkerThreadPool.add_group_task(_simulate_connection, connections.size(), -1, true, "Simulate Connections")
	# small delay, makes it a coroutine | 50 msec realtime
	print("Simulate Connections :")
	while not WorkerThreadPool.is_group_task_completed(task_id):
		print("Count : [%s]" % str(WorkerThreadPool.get_group_processed_element_count(task_id)))
		await get_tree().create_timer(0.05, true, true, true).timeout
	# cleanup
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	print()

# Called by the threads on every Connection
func _simulate_connection(id: int):
	var connection: CachedConnection = circuit.connections[id]
	connection.to_gate.inputs[connection.to_port] = connection.from_gate.outputs[connection.from_port].copy()

# Called by the threads on every Gate
# same for connections, but without ranks
func _simulate_gate(id: int):
	var gate: CachedGate = gates[id]
	gate.mutex.lock()
	print("simulating gate : %s [%s]" % [gate.id, gate.type])
	
	gate.mutex.unlock()
