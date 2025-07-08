# The name of the Class
class_name GateDescription
# The class this class extends
extends Object
# Docstring
## Basically the "blueprint" for a gate

# Signals

# Enums

# Constants

# @export variables

# public variables
## The Name of the Gate
var name: String

## The Type of the Gate
var type: String

## The Size of the Gate
var size: Vector2i

## The Color of the Gate
var color: Color

## If this Gate is maintained by the GateRegistry
var registry: bool = false

## How many ticks this Gate need, to fully simulate
var ticks: int = 1

## The priority when loading the gate | 0 = builtin, 1+ = custom
var priority: int = 0

#region AT RUNTIME
## The ID of the Gate
var id: String = GateRegistry.make_uuid()

## The Position of the Gate[br]
## x, y, (layer)
var position: Vector2 = Vector2.ZERO

#endregion

var inputs: Array[PinDescription]
var outputs: Array[PinDescription]

var data: Dictionary = {}

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Make gate from data
func from_data(from: Dictionary) -> GateDescription:
	var gate: GateDescription = copy()
	gate.data = from
	
	match type:
		"IO.INPUT.#":
			gate.outputs[0].state.size = from["bitsize"]
		"IO.OUTPUT.#":
			gate.inputs[0].state.size = from["bitsize"]
		"ROUTING.TUNNEL_IN.#":
			gate.inputs[0].state.size = from["bitsize"]
		"ROUTING.TUNNEL_OUT.#":
			gate.outputs[0].state.size = from["bitsize"]
		
		"ROUTING.MUX.#": # MUX Gate
			var bits: int = from["bitsize"]
			gate.inputs[0].state.size = bits
			gate.inputs[1].state.size = bits
			gate.inputs[2].state.size = bits
			gate.outputs[0].state.size = bits
		"ROUTING.DEMUX.#": # DEMUX Gate
			var bits: int = from["bitsize"]
			gate.inputs[1].state.size = bits
			gate.outputs[0].state.size = bits
			gate.outputs[1].state.size = bits
		"ROUTING.CONVERTER.#": # CONVERTER Gate
			var size_a: int = from["inputbits"]
			var size_b: int = from["outputbits"]
			var size_t: int = max(size_a, size_b) # total bitsize
			@warning_ignore_start("integer_division")
			var count_a: int = size_t / size_a # total / count (a)
			var count_b: int = size_t / size_b # total / count (b)
			@warning_ignore_restore("integer_division")
			
			var text_a: String = "IN" if count_a == 1 else "IN {i}"
			var text_b: String = "OUT" if count_b == 1 else "OUT {i}"
			
			for i: int in range(0,count_a):
				gate.inputs.append(PinDescription.create(text_a.format({"i": i}), size_a))
			for i: int in range(0,count_b):
				gate.outputs.append(PinDescription.create(text_b.format({"i": i}), size_b))
		
		"COMBINATIONAL.AND.#": # AND Gate
			var bits: int = from["bitsize"]
			var count: int = from["inputcount"]
			gate.outputs[0].state.size = bits
			for i in range(0, count):
				gate.inputs.append(PinDescription.create("IN %s" % str(i), bits))
		"COMBINATIONAL.NAND.#": # NAND Gate
			var bits: int = from["bitsize"]
			var count: int = from["inputcount"]
			gate.outputs[0].state.size = bits
			for i in range(0, count):
				gate.inputs.append(PinDescription.create("IN %s" % str(i), bits))
		"COMBINATIONAL.OR.#": # OR Gate
			var bits: int = from["bitsize"]
			var count: int = from["inputcount"]
			gate.outputs[0].state.size = bits
			for i in range(0, count):
				gate.inputs.append(PinDescription.create("IN %s" % str(i), bits))
		"COMBINATIONAL.NOR.#": # NOR Gate
			var bits: int = from["bitsize"]
			var count: int = from["inputcount"]
			gate.outputs[0].state.size = bits
			for i in range(0, count):
				gate.inputs.append(PinDescription.create("IN %s" % str(i), bits))
		"COMBINATIONAL.NOT.#": # NOT Gate
			var bits: int = from["bitsize"]
			gate.inputs[0].state.size = bits
			gate.outputs[0].state.size = bits
		"COMBINATIONAL.XOR.#": # XOR Gate
			var bits: int = from["bitsize"]
			var count: int = from["inputcount"]
			gate.outputs[0].state.size = bits
			for i in range(0, count):
				gate.inputs.append(PinDescription.create("IN %s" % str(i), bits))
		"COMBINATIONAL.XNOR.#": # XNOR Gate
			var bits: int = from["bitsize"]
			var count: int = from["inputcount"]
			gate.outputs[0].state.size = bits
			for i in range(0, count):
				gate.inputs.append(PinDescription.create("IN %s" % str(i), bits))
	
	return gate

## Returns a copy of this GateDescription, with the new id
func copy() -> GateDescription:
	var res: GateDescription = GateDescription.new()
	res.name = name
	res.type = type
	res.size = size
	res.color = color
	res.ticks = ticks
	res.priority = priority
	res.position = Vector2(position)
	
	res.inputs = []
	res.outputs = []
	
	for pin: PinDescription in inputs:
		res.inputs.append(pin.copy())
	for pin: PinDescription in outputs:
		res.outputs.append(pin.copy())
	
	res.data = data.duplicate()
	return res

# private functions

# subclasses
