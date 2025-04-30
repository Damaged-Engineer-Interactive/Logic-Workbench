# The name of the Class
class_name Value
# The class this class extends
extends Resource
# Docstring
## Wrapper for every value used by the simulation
## 
## Long desciption goes here

# Signals

# Enums
enum States {
	ERROR = 0,
	LOW = 1,
	HIGH = 2,
	UNKNOWN = 3
}

# Constants
const STATE_CHAR_MAP: Dictionary[States, String] = {
	States.ERROR: "!",
	States.LOW: "0",
	States.HIGH: "1",
	States.UNKNOWN: "?"
}

const CHAR_STATE_MAP: Dictionary[String, States] = {
	"!": States.ERROR,
	"0": States.LOW,
	"1": States.HIGH,
	"?": States.UNKNOWN
}

# @export variables

# public variables
## The Size of the Value[br]
## [code]-1[/code] signals an unknown size
var size: int = -1

## The Data of the Value[br]
## Not meant to be manually used, only through methods of this class
var data: Array[States] = []

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override
func _to_string() -> String:
	var result = ""
	for state: States in data:
		result += STATE_CHAR_MAP.get(state, "-")
	return result

# public functions
#region Constructors
static func from_default(_size: int, default := States.UNKNOWN) -> Value:
	if _size <= 0:
		return null
	var value := Value.new()
	value.size = _size
	value.data.resize(_size)
	value.data.fill(default)
	return value

static func from_int(_size: int, base: int) -> Value:
	if _size <= 0:
		return null
	var value := Value.new()
	value.size = _size
	value.data.resize(_size)
	for i in range(0, _size):
		var state: States = States.HIGH if (base >> i) & 1 == 1 else States.LOW
		value.data[i] = state
	return value

## Use to load strings, created by Value.save()
static func from_string(string: String) -> Value:
	var _size: int = int(string.split("_")[0])
	var value := Value.new()
	value.size = _size
	value.data.resize(_size)
	value.data.fill(States.UNKNOWN)
	var _value_string: String = string.split("_")[1]
	for i in range(0, int(_size)):
		var _state: String = _value_string[i]
		value.set_bit(i, CHAR_STATE_MAP.get(_state, States.ERROR))
	return value

#endregion

#region Utility
func copy() -> Value:
	var value := Value.new()
	value.size = size
	value.data = data.duplicate()
	return value

func clear(default := States.UNKNOWN) -> void:
	data.fill(default)

#endregion

#region Bit
func set_bit(index: int, value: States) -> void:
	if index <= 0 or index >= size:
		return
	data[index] = value

func get_bit(index: int) -> States:
	if index <= 0 or index >= size:
		return States.ERROR
	return data[index]

#endregion

#region Raw
func set_raw(values: Array[States]) -> void:
	if values.size() != data.size():
		return
	data = values

func get_raw() -> Array[States]:
	return data.duplicate()

#endregion

#region Saving
func save() -> String:
	return str(size) + "_" + to_string() # {bit_size}_{values}

#endregion

# private functions

# subclasses
