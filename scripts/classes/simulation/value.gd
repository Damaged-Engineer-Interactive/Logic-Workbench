# The name of the Class
class_name Value
# The class this class extends
extends Resource
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

# Constants
const STATE_TO_STRING: Dictionary[States, String] = {
	States.ERROR: "!",
	States.LOW: "L",
	States.HIGH: "H",
	States.UNKNOWN: "?"
}

# @export variables

# public variables
var size: Sizes = Sizes.BIT_1

var data: Array[States] = []

# private variables

# @onready variables

# optional built-in _init() function
func _init(_size: Sizes, default: States = States.UNKNOWN) -> void:
	size = _size
	data.resize(int(_size))
	data.fill(default)

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override
func _to_string() -> String:
	var result = ""
	for state: States in data:
		result += STATE_TO_STRING.get(state, "-")
	return result

# public functions
## Alternate constructor
static func from_int(_size: Sizes, base: int) -> Value:
	var value := Value.new(_size)
	for i in range(0, int(_size)):
		var state: States = States.HIGH if (base >> i) & 1 == 1 else States.LOW
		value.set_bit(i, state)
	return value

func copy() -> Value:
	var value := Value.new(size)
	value.data = self.data.duplicate()
	return value

func clear(state: States = States.UNKNOWN) -> void:
	data.fill(state)

func get_bit(index: int) -> States:
	if index in range(0, size):
		return data[index]
	return States.ERROR

func set_bit(index: int, value: States) -> void:
	if index in range(0, size):
		data[index] = value

## Get the overall state of the Value. Behaves like an AND Gate
func get_state() -> States:
	var result: States = States.HIGH
	for state: States in data:
		if state == States.LOW:
			result = state
		elif state == States.ERROR:
			result = state
			break
		elif state == States.UNKNOWN:
			result = state
			break
	return result

# private functions

# subclasses
