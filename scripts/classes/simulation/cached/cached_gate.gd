# The name of the Class
class_name CachedGate
# The class this class extends
extends Object
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants
var TYPE_FUNCTION_MAP: Dictionary[String, Callable] = {
	"IO.INPUT.#": _simulate_nothing,
	"IO.OUTPUT.#": _simulate_nothing,
	
	"ROUTING.MUX.#": _simulate_routing_mux,
	"ROUTING.DEMUX.#": _simulate_routing_demux,
	"ROUTING.CONVERTER.#-A": _simulate_routing_converter_a,
	"ROUTING.CONVERTER.#-B": _simulate_routing_converter_b,
	
	"COMBINATIONAL.AND.#": _simulate_combinational_and,
	"COMBINATIONAL.NAND.#": _simulate_combinational_nand,
	"COMBINATIONAL.OR.#": _simulate_combinational_or,
	"COMBINATIONAL.NOR.#": _simulate_combinational_nor,
	"COMBINATIONAL.NOT.#": _simulate_combinational_not,
	"COMBINATIONAL.XOR.#": _simulate_combinational_xor,
	"COMBINATIONAL.XNOR.#": _simulate_combinational_xnor,
}

# @export variables

# public variables
## The ID of the Gate
var id: int

## The type of the Gate
var type: String

## The Rank of the Gate (simulation order, low to high)
var rank: int

## If this Gate is a constant
var is_const: bool

## If this Gate has a static output
var is_static: bool

## How many ticks are needed to fully simulate it
var ticks: int

## The Input States of the Gate
var inputs: Array[Value]
## The Output States of the Gate
var outputs: Array[Value]

var sim_function: Callable

var data: Dictionary

# private variables

# @onready variables

# optional built-in _init() function
func _init(from: GateDescription) -> void:
	type = from.type
	
	is_const = true if from.type == "CONST" else false
	is_static = is_const
	ticks = from.ticks
	data = from.data
	
	for input: PinDescription in from.inputs:
		inputs.append(ValueHelper.value(input.state.size))
	
	for output: PinDescription in from.outputs:
		outputs.append(ValueHelper.value(output.state.size))
	
	var type_suffix: String = ""
	if type == "ROUTING.CONVERTER.#":
		type_suffix = "-A" if inputs.size() == 1 else "-B"
	
	sim_function = TYPE_FUNCTION_MAP.get(type + type_suffix)

# simulate function
func _simulate_nothing() -> void:
	return

func _simulate_routing_mux() -> void:
	if inputs[0].get_bit_tri(0) == 1:
		outputs[0].tri()
	elif inputs[0].get_bit_value(0) == 1:
		outputs[0].from(inputs[2])
	else:
		outputs[0].from(inputs[1])

func _simulate_routing_demux() -> void:
	if inputs[0].get_bit_tri(0) == 1:
		outputs[0].tri()
		outputs[1].tri()
	elif inputs[0].get_bit_value(0) == 1:
		outputs[1].from(inputs[1])
		outputs[0].low()
	else:
		outputs[0].from(inputs[1])
		outputs[1].low()

# high bitsize -> low bitsize
func _simulate_routing_converter_a() -> void:
	var value: Value = inputs[0]
	var offset: int = 0
	var size: int = data["outputbits"]
	for i: int in range(outputs.size()):
		var output: Value = outputs[i]
		for bit: int in range(size):
			var j: int = bit + offset
			output.set_bit_value(bit, value.get_bit_value(j))
			output.set_bit_tri(bit, value.get_bit_tri(j))
		offset += size

# low bitsize -> high bitsize
func _simulate_routing_converter_b() -> void:
	var value: Value = outputs[0]
	var offset: int = 0
	var size: int = data["inputbits"]
	for i: int in range(inputs.size()):
		var input: Value = inputs[i]
		for bit: int in range(size):
			var j: int = bit + offset
			value.set_bit_value(j, input.get_bit_value(bit))
			value.set_bit_tri(j, input.get_bit_tri(bit))

func _simulate_combinational_and() -> void:
	outputs[0].arithmetic_and(inputs[0], inputs[1])

func _simulate_combinational_nand() -> void:
	outputs[0].arithmetic_nand(inputs[0], inputs[1])

func _simulate_combinational_or() -> void:
	outputs[0].arithmetic_or(inputs[0], inputs[1])

func _simulate_combinational_nor() -> void:
	outputs[0].arithmetic_nor(inputs[0], inputs[1])

func _simulate_combinational_not() -> void:
	outputs[0].arithmetic_not(inputs[0])

func _simulate_combinational_xor() -> void:
	outputs[0].arithmetic_xor(inputs[0], inputs[1])

func _simulate_combinational_xnor() -> void:
	outputs[0].arithmetic_xnor(inputs[0], inputs[1])
