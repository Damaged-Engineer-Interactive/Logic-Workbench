# The name of the Class
class_name ISA
# The class this class extends
extends Resource
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
@export_group("Debug", "debug_")
@export var debug_instruction_queue: Array[Instruction] = []

var bit_size: Value.Sizes = Value.Sizes.BIT_1

var instructions: Dictionary[String, Instruction] = {}

var keyword_to_value: Dictionary[String, Value] = {}

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	for instruction: Instruction in debug_instruction_queue:
		add_instruction(instruction)

# remaining built-in functions

# virtual functions to override

# public functions
func add_instruction(instruction: Instruction) -> void:
	instructions[instruction.keyword] = instruction
	keyword_to_value[instruction.keyword] = instruction.value

func get_instruction(keyword: String) -> Instruction:
	return instructions.get(keyword)

func remove_instruction(keyword: String) -> Instruction:
	var instruction: Instruction = instructions.get(keyword)
	if instruction != null:
		instructions[keyword] = null
		instructions.erase(keyword)
		keyword_to_value[keyword] = null
		keyword_to_value.erase(keyword)
		return instruction
	return null



func save(path: String) -> void:
	var config := ConfigFile.new()
	
	
	
	config.save(path)

func load(path: String) -> void:
	pass

# private functions

# subclasses
 
