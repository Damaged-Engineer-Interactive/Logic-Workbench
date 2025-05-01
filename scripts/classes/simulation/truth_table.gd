# The name of the Class
class_name TruthTable
# The class this class extends
extends Object
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
var bitmap: BitMap

## The Bit Size of the Truth Table (columns)
var bit_size: int

## Amount of Outputs in the Truth Table (rows per section)
var amount_of_outputs: int

## Number of sections (possible input states) in the Truth Table
var amount_of_sections: int

## Input State (String) -> Section ID (int)
var state_id_map: Dictionary[String, int] = {}

# private variables
var _next_state_id: int = 0

# @onready variables

# optional built-in _init() function
func _init(_bit_size: int, _amount_of_outputs: int, _amount_of_sections: int) -> void:
	bit_size = _bit_size
	amount_of_outputs = _amount_of_outputs
	amount_of_sections = _amount_of_sections
	
	bitmap = BitMap.new()
	var size := Vector2i(bit_size, amount_of_outputs * amount_of_sections)
	bitmap.create(size)

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
func add_state(inputs: Array[Value], outputs: Array[Value]) -> void:
	var state: String = _make_state_string(inputs)
	var id: int = state_id_map.get(state, -1)
	if id == -1:
		id = _next_state_id
		_next_state_id += 1
	
	

# private functions
func _make_state_string(from: Array[Value]) -> String:
	var res: String = ""
	for value: Value in from:
		res += str(value)
		res += ":"
	return res

# subclasses
