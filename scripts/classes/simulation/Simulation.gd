class_name Simulation
extends Node

@warning_ignore_start("unused_signal")
signal sim_started							# beginning of the sim			| mode			| (optional) steps
signal sim_stopped							# end of the sim				| tick number
signal error(tick: int, gates: Array[int])	# error (sim stop)				| tick number	| gate ids
@warning_ignore_restore("unused_signal")


enum TPSEvent {LOW, NORMAL, HIGH }


## The Simulated Circuit
var circuit: CachedCircuit

## The Sim Manager's Thread
var thread: Thread

## WorkerThreadPool Group Task ID
var task_id: int = -1

## Current Mode of the Sim
var active: bool = false

## Steps left to simulate | SimMode.STEP
var steps: int = 0

## Current Tick and last stopped tick
var tick: int = 0
var tick_old: int = 0

## Start time of the Sim | Performance Testing
var start_time: int = -1

## Performance Stats
var stats: Dictionary[String, int] = {}

## Current gates to simulate
var gates: Array
var connections: Array

func _init(from: CachedCircuit) -> void:
	circuit = from
	
	# Set all values to their default
	gates = circuit.gates.values()
	task_id = WorkerThreadPool.add_group_task(_thread_simulate_gate, gates.size(), gates.size(), true, "Set Defaults")
	# cleanup
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	
	## Start the Thread Manager
	thread = Thread.new()

func clean() -> void:
	active = false
	steps = 0
	if thread.is_started():
		thread.wait_to_finish()
	thread = null

func run(_steps: int) -> void:
	if active:
		printerr("Sim already running!")
		return
	
	steps = _steps
	stats = {}
	active = true
	start_time = Time.get_ticks_usec()
	thread.start(_thread_manager, Thread.PRIORITY_NORMAL)
	sim_started.emit()

func _thread_manager():
	print("thread manager started")
	while true:
		if active:
			if steps > 0:
				_thread_simulate_tick()
				steps -= 1
			else:
				print("stopped ( STEPS  ) [%d] <%d>" % [steps, tick])
				call_deferred(&"_sim_stop")
				return
		else:
			print("not active")
			print("stopped [%d] ( ACTIVE ) <%d>" % [steps, tick])
			call_deferred(&"_sim_stop")
			return

func _thread_simulate_tick():
	for rank: int in circuit.parallel_schedule.keys():
		_thread_simulate_rank(rank)
		_thread_simulate_connections()
	tick += 1

func _thread_simulate_rank(rank: int) -> void:
	gates = circuit.parallel_schedule[rank]
	task_id = WorkerThreadPool.add_group_task(_thread_simulate_gate, gates.size(), gates.size(), true, "Simulate Rank [%s]" % str(rank))
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	return

func _thread_simulate_connections() -> void:
	connections = circuit.connections.values()
	task_id = WorkerThreadPool.add_group_task(_thread_simulate_connection, connections.size(), connections.size(), true, "Simulate Connections")
	# cleanup
	WorkerThreadPool.wait_for_group_task_completion(task_id)
	task_id = -1
	return

# Called by the threads on every Connection
func _thread_simulate_connection(id: int):
	var connection: CachedConnection = connections[id]
	connection.to_gate.inputs[connection.to_port] = connection.from_gate.outputs[connection.from_port].copy()

# Called by the threads on every Gate
# same for connections, but without ranks
func _thread_simulate_gate(id: int):
	var gate: CachedGate = gates[id]
	gate.mutex.lock()

	# sim here
	match gate.type:
		"COMBINATIONAL.AND.#":
			pass

	gate.mutex.unlock()

# Called by the thread manager
func _sim_stop() -> void:
	var end_time: int = Time.get_ticks_usec()
	active = false
	thread.wait_to_finish()
	thread = Thread.new() # new thread
	var ticks: int = tick - tick_old
	@warning_ignore("integer_division")
	stats = {
		"time_taken": end_time-start_time,
		"ticks_run": ticks,
		"time_per_tick": (end_time-start_time)/(ticks*1)
	}
	tick_old = tick
	sim_stopped.emit()
	print("Sim ended!")
	print("Time Taken    : %10s usec" % Global.format_int(stats["time_taken"]))
	print("Ticks run     : %10s ticks" % Global.format_int(stats["ticks_run"]))
	print("Time per Tick : %10s usec" % Global.format_int(stats["time_per_tick"]))
