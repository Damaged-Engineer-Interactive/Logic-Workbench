# The name of the Class
class_name Connection
# The class this class extends
extends Object
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants
const IO_TYPES: Array[String] = [
	"IO.INPUT.#",
	"IO.OUTPUT.#",
	"ROUTING.TUNNEL_IN.#",
	"ROUTING.TUNNEL_OUT.#"
]

# @export variables

# public variables
## The ID of the Connection
var id: String = GateRegistry.make_uuid()

#region Output
## The ID of the first Gate
var from_gate: String = ""

## The Port of the first gate
var from_port: int = -1
#endregion

#region Input
## The ID of the second Gate
var to_gate: String = ""

## The Port of the second gate
var to_port: int = -1
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
static func create(_from_gate: String, _from_port: int, _to_gate: String, _to_port: int) -> Connection:
	var connection: Connection = Connection.new()
	connection.from_gate = _from_gate
	connection.from_port = _from_port
	connection.to_gate = _to_gate
	connection.to_port = _to_port
	return connection

func copy() -> Connection:
	return create(from_gate, from_port, to_gate, to_port)

func uses_from_io() -> bool: # outputs
	var res: bool = false
	for type: String in IO_TYPES:
		res = res or from_gate.begins_with(type)
	return res

func uses_to_io() -> bool: # inputs
	var res: bool = false
	for type: String in IO_TYPES:
		res = res or to_gate.begins_with(type)
	return res

func uses_io() -> bool:
	return uses_from_io() or uses_to_io()

func template() -> String:
	return from_gate + "#" + str(from_port) + ":" + to_gate + "#" + str(to_port)

func save() -> Dictionary:
	return {
		"id": id,
		"from_gate": from_gate,
		"from_port": from_port,
		"to_gate": to_gate,
		"to_port": to_port
	}

static func load(from: Dictionary) -> Connection:
	var res := Connection.new()
	res.id = from["id"]
	res.from_gate = from["from_gate"]
	res.from_port = from["from_port"]
	res.to_gate = from["to_gate"]
	res.to_port = from["to_port"]
	return res

# private functions

# subclasses
