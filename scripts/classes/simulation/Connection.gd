# The name of the Class
class_name Connection
# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
## The ID of the Connection
var id: String = GateRegistry.make_uuid()

#region Output
## The ID of the first Gate
var gate_in: String = ""

## The Port of the first gate
var port_in: int = -1
#endregion

#region Input
## The ID of the second Gate
var gate_out: String = ""

## The Port of the second gate
var port_out: int = -1
#endregion

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Constructor
static func create(from_gate: String, from_port: int, to_gate: String, to_port: int) -> Connection:
	var connection: Connection = Connection.new()
	connection.gate_in = from_gate
	connection.port_in = from_port
	connection.gate_out = to_gate
	connection.port_out = to_port
	return connection

func copy() -> Connection:
	return create(gate_in, port_in, gate_out, port_out)

# private functions

# subclasses
