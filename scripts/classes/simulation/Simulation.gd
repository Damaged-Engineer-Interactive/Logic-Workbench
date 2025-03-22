# The name of the Class
class_name Simulation
# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums
enum Sizes {
	BIT_1 = 1,
	BIT_2 = 2,
	BIT_4 = 4,
	BIT_8 = 8,
	BIT_16 = 16,
	BIT_32 = 32
}

enum States {
	ERROR = 0,
	LOW = 1,
	HIGH = 2,
	UNKNOWN = 3
}

enum IO_TYPES {
	UNKNOWN = 0,
	INPUT = 1,
	OUTPUT = 2,
	BUS = 3
}

enum GATE_TYPES {
	UNKNOWN =   0,
	AND,
	NAND,
	OR,
	NOR,
	NOT,
	XOR,
	XNOR,
	STATE,
	
	ON,
	OFF,
	TRI,
	ERROR,
	
	SELECTOR,

	CUSTOM  = 255
}

# Constants
const STATE_TO_LETTER: Dictionary[States, String] = {
	States.ERROR: "!",
	States.LOW: "L",
	States.HIGH: "H",
	States.UNKNOWN: "?",
}

const STATE_TO_COLOR: Dictionary[States, Color] = {
	States.ERROR: Color.RED,
	States.LOW: Color.DARK_GREEN,
	States.HIGH: Color.GREEN,
	States.UNKNOWN: Color.BLUE,
}

const MAX_IO_COUNT: int = 128

const THEME_PANEL: StyleBoxFlat = preload("res://styles/simulation/Panel.stylebox")
const THEME_TITLE: StyleBoxFlat = preload("res://styles/simulation/Titlebar.stylebox")
const THEME_TITLE_SELECTED: StyleBoxFlat = preload("res://styles/simulation/Titlebar_selected.stylebox")

# @export variables
@export var allow_simulate: bool = false

# public variables
var gates: Array[Gate] = []
var connections: Array[Connection] = []

var can_simulate: bool = false

var GATES: Dictionary[Simulation.GATE_TYPES, Variant] = {
	GATE_TYPES.AND: AndGate,
	GATE_TYPES.NAND: NandGate,
	GATE_TYPES.OR: OrGate,
	GATE_TYPES.NOR: NorGate,
	GATE_TYPES.NOT: NotGate,
	GATE_TYPES.XOR: XorGate,
	GATE_TYPES.XNOR: XnorGate,
	GATE_TYPES.STATE: TriStateGate,
	
	GATE_TYPES.ON: OnGate,
	GATE_TYPES.OFF: OffGate,
	GATE_TYPES.TRI: TriGate,
	GATE_TYPES.ERROR: ErrorGate,
	
	GATE_TYPES.SELECTOR: SelectorGate
}

# private variables
var _next_gate_id: Array[int] = [0]
var _amount_of_gates: int = 0

var _next_connection_id: Array[int] = [0]


var _sim_counter: int = -1 # Frames until next simulate() call
var _is_simulating: bool = false # False : Dispatch new instance | True : Get Results
var _invalid_run: bool = false # Invalid if gates / connections changed

var mutex: Mutex
var semaphore: Semaphore

var thread_count: int
var threads: Array[Thread]

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	gates.resize(32)
	connections.resize(32)

	_prepare_simulation()

# remaining built-in functions
func _process(_d: float) -> void:
	if can_simulate:
		_sim_counter -= 1
		if _sim_counter == 0:
			simulate()
			_sim_counter += 20

# virtual functions to override
func _exit_tree() -> void:
	for thread: Thread in threads:
		thread.wait_to_finish()

# public functions
func get_gate(id: int) -> Gate:
	if id < gates.size():
		return gates[id]
	return null

func add_gate(gate: Gate) -> void:
	var id = _next_gate_id.pop_back() # Get the newest gate_id available
	if _next_gate_id.size() == 0: # Push new id if none available
		_next_gate_id.append(id + 1)
	
	if id >= gates.size(): # Array is full, make more space
		gates.resize(gates.size() + 16)
	
	_amount_of_gates += 1
	
	gate.gate_id = id
	
	gates[id] = gate
	
	_invalid_run = true

func remove_gate(id: int) -> Gate:
	var gate: Gate = gates[id]
	gates[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_gate_id.append(id)
	_amount_of_gates -= 1
	_invalid_run = true
	return gate # Returns the gate as reference, so it isn't immediately gone

func get_connection(con_id: String) -> Connection:
	for connection: Connection in connections:
		if not connection:
			break
		if con_id == connection.get_con_id():
			return connection
	return null

func add_connection(connection: Connection) -> void:
	var id = _next_connection_id.pop_back() # Get the newest connection_id available
	if _next_connection_id.size() == 0: # Push new id if none available
		_next_connection_id.append(id + 1)
	
	if id >= connections.size(): # Array is full, make more space
		connections.resize(connections.size() + 16)
	
	connection.id = id
	
	connection.name = connection.get_con_id()
	
	connections[id] = connection
	
	_invalid_run = true

func remove_connection(id: int) -> Connection:
	var connection: Connection = connections[id]
	connections[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_connection_id.append(id)
	_invalid_run = true
	return connection # Returns the gate as reference, so it isn't immediately gone

func simulate() -> void:
	if not allow_simulate:
		return
	_simulate_single_thread()
	_sim_counter = 1
	_invalid_run = false
	return
	
	@warning_ignore("unreachable_code")
	if _is_simulating:
		_simulate_end()
		_sim_counter = 1 # 1 Frame until next simulate() call 
	else:
		_simulate_begin()
		_sim_counter = 1 # 1 Frame until next simulate() call
	_invalid_run = false

# private functions
func _prepare_simulation() -> void:
	print("prepare_simulation")
	thread_count = floori(OS.get_processor_count() / 4.0)
	print("Logical Processors Available : %2s" % OS.get_processor_count())
	print("Logical Processors Usable    : %2s" % thread_count)
	
	can_simulate = true
	_sim_counter = 60 # 60 Frames until first simulate() call

func _simulate_begin() -> void:
	assert(false, "Not Implemented!")
	
	_is_simulating = true

func _simulate_end() -> void:
	assert(false, "Not Implemented!")
	
	_is_simulating = false

func _simulate_single_thread() -> void:
	# Gate Simulation
	for i: int in  range(0, _amount_of_gates):
		var gate: Gate = gates[i]
		
		match gate.gate_type:
			GATE_TYPES.AND:
				var result: States = States.HIGH
				for j in range(0, gate.input_amount):
					var value: States = gate.input_values[j][0]
					if value != States.HIGH:
						result = value
						break
				gate.output_values[0][0] = result
			
			GATE_TYPES.NAND:
				var result: States = States.HIGH
				for j in range(0, gate.input_amount):
					var value: States = gate.input_values[j][0]
					if value != States.HIGH:
						result = value
						break
				if result == States.HIGH:
					result = States.LOW
				elif result == States.LOW:
					result = States.HIGH
				gate.output_values[0][0] = result
			
			GATE_TYPES.OR:
				var result: States = States.LOW
				for j in range(0, gate.input_amount):
					var value: States = gate.input_values[j][0]
					if value == States.HIGH:
						result = value
					elif value == States.ERROR:
						result = value
						break
					elif value == States.UNKNOWN:
						result = value
						break
				gate.output_values[0][0] = result
			
			GATE_TYPES.NOR:
				var result: States = States.LOW
				for j in range(0, gate.input_amount):
					var value: States = gate.input_values[j][0]
					if value == States.HIGH:
						result = value
					elif value == States.ERROR:
						result = value
						break
					elif value == States.UNKNOWN:
						result = value
						break
				if result == States.HIGH:
					result = States.LOW
				elif result == States.LOW:
					result = States.HIGH
				gate.output_values[0][0] = result
			
			GATE_TYPES.NOT:
				var result: States = States.UNKNOWN
				var value: States = gate.input_values[0][0]
				if value == States.HIGH:
					result = States.LOW
				elif value == States.LOW:
					result = States.HIGH
				else:
					result = value
				gate.output_values[0][0] = result
			
			GATE_TYPES.XOR:
				var result: States = States.HIGH
				var value1: States = gate.input_values[0][0]
				var value2: States = gate.input_values[0][0]
				if value1 == States.ERROR or value2 == States.ERROR:
					result = States.ERROR
				elif value1 == States.UNKNOWN or value2 == States.UNKNOWN:
					result = States.UNKNOWN
				elif value1 == value2:
					result = States.LOW
				gate.output_values[0][0] = result
			
			GATE_TYPES.XNOR:
				var result: States = States.LOW
				var value1: States = gate.input_values[0][0]
				var value2: States = gate.input_values[0][0]
				if value1 == States.ERROR or value2 == States.ERROR:
					result = States.ERROR
				elif value1 == States.UNKNOWN or value2 == States.UNKNOWN:
					result = States.UNKNOWN
				elif value1 == value2:
					result = States.HIGH
				gate.output_values[0][0] = result
			
			GATE_TYPES.STATE:
				var result: States = States.UNKNOWN
				var value: States = gate.input_values[0][0]
				var enable: States = gate.input_values[1][0]
				if value == States.ERROR or enable == States.ERROR:
					result = States.ERROR
				elif value == States.UNKNOWN or enable == States.UNKNOWN:
					result = States.UNKNOWN
				elif enable == States.LOW:
					result = States.UNKNOWN
				else:
					result = enable
				gate.output_values[0][0] = result
			
			GATE_TYPES.ON:
				gate.output_values[0][0] = States.HIGH
			
			GATE_TYPES.OFF:
				gate.output_values[0][0] = States.LOW
			
			GATE_TYPES.TRI:
				gate.output_values[0][0] = States.UNKNOWN
			
			GATE_TYPES.ERROR:
				gate.output_values[0][0] = States.ERROR
			
			GATE_TYPES.SELECTOR:
				gate.output_values[0][0] = gate.state
		
		# Connection Simulation
		for connection: Connection in connections:
			if not connection:
				break
			var output_gate: Gate = gates[connection.gate_in]
			var output_values: Array[States] = output_gate.output_values[connection.port_in]
			var input_gate: Gate = gates[connection.gate_out]
			input_gate.input_values[connection.port_out] = output_values
		
		gate.redraw()

# subclasses
 
