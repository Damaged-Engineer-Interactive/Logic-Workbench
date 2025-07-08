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
var tick_old: int

## Current gates to simulate
var gates: Array

var connections: Array

var start: int = -1

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
	tick_old = 0
	
	# Set all values to their default
	gates = circuit.gates.values()
	task_id = WorkerThreadPool.add_group_task(_simulate_gate, gates.size(), gates.size(), true, "Set Defaults")
	# cleanup
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	#print()

func _process(_delta: float) -> void:
	match mode:
		SimMode.ACTIVE:
			simulate_tick()

## Update the Simulation with a new Circuit
func update(new: CachedCircuit) -> void:
	setup(new)

## Run the Simulation (mode: TPS)
func run() -> void:
	if circuit.valid and mode == SimMode.STOP:
		mode = SimMode.ACTIVE
		sim_started.emit(mode, -1)
		start = Time.get_ticks_usec()

## Run the Simulation (mode: STEP)
func step(amount: int) -> void:
	if circuit.valid and mode == SimMode.STOP:
		steps = amount
		mode = SimMode.STEP
		start = Time.get_ticks_usec()
		sim_started.emit(mode, amount)
		while steps > 0:
			await simulate_tick()
			steps -= 1
		stop()

## Stop the Simulation
func stop() -> void:
	mode = SimMode.STOP
	sim_stopped.emit(tick)
	var end: int = Time.get_ticks_usec()
	var ticks: int = tick-tick_old
	tick_old = tick
	print("Sim ended!")
	print("Time Taken    : %10s usec" % Global.format_int(end-start))
	print("Ticks run     : %10s ticks" % Global.format_int(ticks))
	@warning_ignore("integer_division")
	print("Time per Tick : %10s usec" % Global.format_int((end-start)/(ticks*1)))

func simulate_tick():
	for rank: int in circuit.parallel_schedule.keys():
		_simulate_rank(rank)
		_simulate_connections()
	tick += 1

func _simulate_rank(rank: int) -> void:
	gates = circuit.parallel_schedule[rank]
	task_id = WorkerThreadPool.add_group_task(_simulate_gate, gates.size(), gates.size(), true, "Simulate Rank [%s]" % str(rank))
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	return

func _simulate_connections() -> void:
	connections = circuit.connections.values()
	task_id = WorkerThreadPool.add_group_task(_simulate_connection, connections.size(), connections.size(), true, "Simulate Connections")
	# cleanup
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	return

# Called by the threads on every Connection
func _simulate_connection(id: int):
	var connection: CachedConnection = connections[id]
	connection.to_gate.inputs[connection.to_port] = connection.from_gate.outputs[connection.from_port].copy()

# Called by the threads on every Gate
# same for connections, but without ranks
func _simulate_gate(id: int):
	var gate: CachedGate = gates[id]
	gate.mutex.lock()

	# sim here
	match gate.type:
		"COMBINATIONAL.AND.#":
			pass

	gate.mutex.unlock()
