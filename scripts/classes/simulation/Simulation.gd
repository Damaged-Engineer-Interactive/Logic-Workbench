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
	TRI,

	CUSTOM  = 255
}

# Constants
const STATE_TO_COLOR: Dictionary = {
	States.ERROR: Color.RED,
	States.LOW: Color.DARK_GREEN,
	States.HIGH: Color.GREEN,
	States.UNKNOWN: Color.BLUE,
}

const MAX_IO_COUNT: int = 128

# @export variables

# public variables
var gates: Array[Gate] = []
var connections: Array[Connection] = []

# private variables
var _next_gate_id: Array[int] = [0]

var _next_connection_id: Array[int] = [0]

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
func add_gate(gate: Gate) -> void:
	var id = _next_gate_id.pop_back() # Get the newest gate_id available
	if _next_gate_id.size() == 0: # Push new id if none available
		_next_gate_id.append(id + 1)
	
	if id >= gates.size(): # Array is full, make more space
		gates.resize(gates.size() + 16)
	
	gate.id = id
	
	gates[id] = gate

func remove_gate(id: int) -> Gate:
	var gate: Gate = gates[id]
	gates[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_gate_id.append(id)
	return gate # Returns the gate as reference, so it isn't immediately gone


func add_connection(connection: Connection) -> void:
	var id = _next_connection_id.pop_back() # Get the newest connection_id available
	if _next_connection_id.size() == 0: # Push new id if none available
		_next_connection_id.append(id + 1)
	
	if id >= connections.size(): # Array is full, make more space
		connections.resize(connections.size() + 16)
	
	connection.id = id
	
	connections[id] = connection

func remove_connection(id: int) -> Connection:
	var connection: Connection = connections[id]
	connections[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_connection_id.append(id)
	return connection # Returns the gate as reference, so it isn't immediately gone

# private functions

# subclasses
 