# The name of the Class
class_name Circuit
# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums
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
const MAX_IO_COUNT: int = 128

const THEME_PANEL: StyleBoxFlat = preload("res://styles/simulation/panel.stylebox")
const THEME_TITLE: StyleBoxFlat = preload("res://styles/simulation/titlebar.stylebox")
const THEME_TITLE_SELECTED: StyleBoxFlat = preload("res://styles/simulation/titlebar_selected.stylebox")

# @export variables
@export var allow_simulate: bool = false

# public variables
var gates: Array[Gate] = []
var connections: Array[Connection] = []

var can_simulate: bool = false

static var GATES: Dictionary[GATE_TYPES, Variant] = {
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
var _amount_of_connections: int = 0

var _invalid_run: bool = false # Invalid if gates / connections changed


# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	gates.resize(32)
	connections.resize(32)

# remaining built-in functions

# virtual functions to override

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
	for connection: Connection in gate.connections.values():
		remove_connection(connection.id)
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
	var id: int = _next_connection_id.pop_back() # Get the newest connection_id available
	if _next_connection_id.size() == 0: # Push new id if none available
		_next_connection_id.append(id + 1)
	
	if id >= connections.size(): # Array is full, make more space
		connections.resize(connections.size() + 16)
	
	_amount_of_connections += 1
	
	connection.id = id
	
	connection.name = connection.get_con_id()
	
	connections[id] = connection
	
	var gate: Gate
	gate = get_gate(connection.gate_in)
	gate.connections[connection.get_con_id()] = connection
	gate = get_gate(connection.gate_out)
	gate.connections[connection.get_con_id()] = connection
	
	_invalid_run = true

func remove_connection(id: int) -> Connection:
	var connection: Connection = connections[id]
	var gate: Gate
	gate = get_gate(connection.gate_in)
	gate.connections.erase(connection.get_con_id())
	gate = get_gate(connection.gate_out)
	gate.connections.erase(connection.get_con_id())
	connections[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_connection_id.append(id)
	_amount_of_connections += 1
	_invalid_run = true
	return connection # Returns the gate as reference, so it isn't immediately gone

func simulate() -> void:
	_simulate_single_thread()

# private functions
func _simulate_single_thread() -> void:
	# Gate Simulation
	for i: int in  range(0, _amount_of_gates):
		var gate: Gate = gates[i]
		
		match gate.gate_type:
			GATE_TYPES.AND:
				var result: Value.States = Value.States.HIGH
				for j in range(0, gate.input_amount):
					var state: Value.States = gate.input_values[j].get_bit(0)
					if state == Value.States.LOW:
						result = state
					elif state == Value.States.ERROR:
						result = state
						break
					elif state == Value.States.UNKNOWN:
						result = state
						break
				gate.output_values[0].set_bit(0, result)
			
			GATE_TYPES.NAND:
				var result: Value.States = Value.States.HIGH
				for j in range(0, gate.input_amount):
					var state: Value.States = gate.input_values[j].get_bit(0)
					if state == Value.States.LOW:
						result = state
					elif state == Value.States.ERROR:
						result = state
						break
					elif state == Value.States.UNKNOWN:
						result = state
						break
				if result == Value.States.HIGH:
					result = Value.States.LOW
				elif result == Value.States.LOW:
					result = Value.States.HIGH
				gate.output_values[0].set_bit(0, result)
			
			GATE_TYPES.OR:
				var result: Value.States = Value.States.LOW
				for j in range(0, gate.input_amount):
					var state: Value.States = gate.input_values[j].get_bit(0)
					if state == Value.States.HIGH:
						result = state
					elif state == Value.States.ERROR:
						result = state
						break
					elif state == Value.States.UNKNOWN:
						result = state
						break
				gate.output_values[0].set_bit(0, result)
			
			GATE_TYPES.NOR:
				var result: Value.States = Value.States.LOW
				for j in range(0, gate.input_amount):
					var state: Value.States = gate.input_values[j].get_bit(0)
					if state == Value.States.HIGH:
						result = state
					elif state == Value.States.ERROR:
						result = state
						break
					elif state == Value.States.UNKNOWN:
						result = state
						break
				if result == Value.States.HIGH:
					result = Value.States.LOW
				elif result == Value.States.LOW:
					result = Value.States.HIGH
				gate.output_values[0].set_bit(0, result)
			
			GATE_TYPES.NOT:
				var result: Value.States = Value.States.UNKNOWN
				var value: Value.States = gate.input_values[0].get_bit(0)
				if value == Value.States.HIGH:
					result = Value.States.LOW
				elif value == Value.States.LOW:
					result = Value.States.HIGH
				else:
					result = value
				gate.output_values[0].set_bit(0, result)
			
			GATE_TYPES.XOR:
				var result: Value.States = Value.States.HIGH
				var value1: Value.States = gate.input_values[0].get_bit(0)
				var value2: Value.States = gate.input_values[1].get_bit(0)
				if value1 == Value.States.ERROR or value2 == Value.States.ERROR:
					result = Value.States.ERROR
				elif value1 == Value.States.UNKNOWN or value2 == Value.States.UNKNOWN:
					result = Value.States.UNKNOWN
				elif value1 == value2:
					result = Value.States.LOW
				else:
					result = Value.States.HIGH
				gate.output_values[0].set_bit(0, result)
			
			GATE_TYPES.XNOR:
				var result: Value.States = Value.States.HIGH
				var value1: Value.States = gate.input_values[0].get_bit(0)
				var value2: Value.States = gate.input_values[1].get_bit(0)
				if value1 == Value.States.ERROR or value2 == Value.States.ERROR:
					result = Value.States.ERROR
				elif value1 == Value.States.UNKNOWN or value2 == Value.States.UNKNOWN:
					result = Value.States.UNKNOWN
				elif value1 == value2:
					result = Value.States.LOW
				else:
					result = Value.States.HIGH
				if result == Value.States.HIGH:
					result = Value.States.LOW
				elif result == Value.States.LOW:
					result = Value.States.HIGH
				gate.output_values[0].set_bit(0, result)
			
			GATE_TYPES.STATE:
				var result: Value.States = Value.States.UNKNOWN
				var value: Value.States = gate.input_values[0].get_bit(0)
				var enable: Value.States = gate.input_values[1].get_bit(0)
				if value == Value.States.ERROR or enable == Value.States.ERROR:
					result = Value.States.ERROR
				elif value == Value.States.UNKNOWN or enable == Value.States.UNKNOWN:
					result = Value.States.UNKNOWN
				elif enable == Value.States.LOW:
					result = Value.States.UNKNOWN
				else:
					result = value
				gate.output_values[0].set_bit(0, result)
			
			GATE_TYPES.ON:
				gate.output_values[0].set_bit(0, Value.States.HIGH)
			
			GATE_TYPES.OFF:
				gate.output_values[0].set_bit(0, Value.States.LOW)
			
			GATE_TYPES.TRI:
				gate.output_values[0].set_bit(0, Value.States.UNKNOWN)
			
			GATE_TYPES.ERROR:
				gate.output_values[0].set_bit(0, Value.States.ERROR)
			
			GATE_TYPES.SELECTOR:
				gate.output_values[0].set_bit(0, gate.state)
		
		gate.redraw()
	
	# Connection Simulation
	for i in range(_amount_of_connections):
		var connection: Connection = connections[i]
		var output_gate: Gate = gates[connection.gate_in]
		var output_value: Value = output_gate.output_values[connection.port_in].copy()
		var input_gate: Gate = gates[connection.gate_out]
		input_gate.input_values[connection.port_out] = output_value

# subclasses
 
