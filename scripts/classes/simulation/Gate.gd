# The name of the Class
class_name Gate
# The class this class extends
extends GraphNode
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables
#region Gate
@export_group("Gate", "gate_")
## The Name of the Gate
@export var gate_name: String = ""

## The type of the Gate (used in the simulation)
@export var gate_type: int = 0

## The number of the Gate (used in the workspace and simulation to reference the gate)
## Limited to 255
@export var gate_id: int = 0

## The position of the Gate
@export var gate_position := Vector2(0,0)
#endregion

#region Input
@export_group("Input", "input_")
## The Number of inputs, the gate has
@export var input_amount: int = 0:
	set(value):
		input_amount = value
		input_sizes.resize(value)
		input_values.resize(value)

## The Size of every Input
## Array[Simulation.Sizes]
@export var input_sizes: Array[Simulation.Sizes] = []

## The Values of every Input
## Array[Array[Simulation.States]]
@export var input_values: Array[Array] = []
#endregion

#region Output
@export_group("Output", "output_")
## The Number of outputs, the gate has
@export var output_amount: int = 0:
	set(value):
		output_amount = value
		output_sizes.resize(value)
		output_values.resize(value)

## The Size of every output
## Array[Simulation.Sizes]
@export var output_sizes: Array[Simulation.Sizes] = []

## The Values of every output
## Array[Array[Simulation.States]]
@export var output_values: Array[Array] = []
#endregion

#region Bus
@export_group("Bus", "bus_")
## The Number of buses, the gate has
@export var bus_amount: int = 0:
	set(value):
		bus_amount = value
		bus_sizes.resize(value)
		bus_values.resize(value)

## The Size of every bus
## Array[Simulation.Sizes]
@export var bus_sizes: Array[Simulation.Sizes] = []

## The Values of every bus
## Array[Array[Simulation.States]]
@export var bus_values: Array[Array] = []
#endregion
# public variables

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Adds an IO port to the gate
func add_io(type: Simulation.IO_TYPES, bits: Simulation.Sizes) -> void:
	var val: Array[Simulation.States] = []
	val.resize(bits)
	val.fill(Simulation.States.UNKNOWN)
	match type:
		Simulation.IO_TYPES.INPUT:
			input_amount += 1
			input_sizes.append(bits)
			input_values.append(val)
		Simulation.IO_TYPES.OUTPUT:
			output_amount += 1
			output_sizes.append(bits)
			output_values.append(val)
		Simulation.IO_TYPES.BUS:
			bus_amount += 1
			bus_sizes.append(bits)
			bus_values.append(val)
			

## Creates the textures of this Gate
## Result : [gate: Image, input: Image, output: Image, bus: Image]
func create_textures() -> Array[Image]:
	# The maximum amount of IO + normal gate data
	var img_size: int = Simulation.MAX_IO_COUNT + 1
	# First pixel : R = gate_id, G = input_amount, B = output_amount, A = bus_amount
	# Rest pixels (aligns with each input/..) : R = unused, G = input_size, B = output_size, A = bus_size
	var gate: Image = Image.create_empty(img_size, 1, false, Image.Format.FORMAT_RGBA8)
	var color: Color = Color.BLACK
	color.r8 = gate_id
	color.g8 = input_amount
	color.b8 = output_amount
	color.a8 = bus_amount
	gate.set_pixel(0, 0, color)
	
	# 2 Bits = 1 State (LL = Err, LH = Low, HL = High, HH = Tri) = 2 * 8 * 4 = 64 (bits used) / 2 = 32 (BIT_32)
	var input: Image = Image.create_empty(img_size + 1, 1, false, Image.Format.FORMAT_RGBA8)
	for x: int in range(0, input_amount): # loop for every input = x position, y = 1
		var data: int = 0 # The Data of the input, that will be written to the pixel
		var p: int = 0 # Write Pointer
		for i: int in range(0, input_sizes[x]): # loop for every value in the input
			# write value | p += 2 | i = index
			var val: int = input_values[x][i] as int # Get the state of the bit
			val <<= p # Shift the value by the index
			data |= val # OR the existing data with the new data
			p += 2
		# Store values
		color = Color.BLACK
		color.r8 = data       & 0xFF # 1. Byte
		color.g8 = data <<  8 & 0xFF # 2. Byte
		color.b8 = data << 16 & 0xFF # 3. Byte
		color.a8 = data << 24 & 0xFF # 4. Byte
		input.set_pixel(x + 1, 0, color)
		# Store size
		color = gate.get_pixel(x + 1, 0)
		color.g8 = input_sizes[x]
		gate.set_pixel(x + 1, 0, color)
	
	# 2 Bits = 1 State (LL = Err, LH = Low, HL = High, HH = Tri) = 2 * 8 * 4 = 64 (bits used) / 2 = 32 (BIT_32)
	var output: Image = Image.create_empty(img_size + 1, 1, false, Image.Format.FORMAT_RGBA8)
	for x: int in range(0, output_amount): # loop for every output = x position, y = 1
		var data: int = 0 # The Data of the output, that will be written to the pixel
		var p: int = 0 # Write Pointer
		for i: int in range(0, output_sizes[x]): # loop for every value in the output
			# write value | p += 2 | i = index
			var val: int = output_values[x][i] as int # Get the state of the bit
			val <<= p # Shift the value by the index
			data |= val # OR the existing data with the new data
			p += 2
		color = Color.BLACK
		color.r8 = data       & 0xFF # 1. Byte
		color.g8 = data <<  8 & 0xFF # 2. Byte
		color.b8 = data << 16 & 0xFF # 3. Byte
		color.a8 = data << 24 & 0xFF # 4. Byte
		output.set_pixel(x + 1, 0, color)
		# Store size
		color = gate.get_pixel(x + 1, 0)
		color.b8 = output_sizes[x]
		gate.set_pixel(x + 1, 0, color)
	
	# 2 Bits = 1 State (LL = Err, LH = Low, HL = High, HH = Tri) = 2 * 8 * 4 = 64 (bits used) / 2 = 32 (BIT_32)
	var bus: Image = Image.create_empty(img_size + 1, 1, false, Image.Format.FORMAT_RGBA8)
	for x: int in range(0, bus_amount): # loop for every bus = x position, y = 1
		var data: int = 0 # The Data of the bus, that will be written to the pixel
		var p: int = 0 # Write Pointer
		for i: int in range(0, bus_sizes[x]): # loop for every value in the bus
			# write value | p += 2 | i = index
			var val: int = bus_values[x][i] as int # Get the state of the bit
			val <<= p # Shift the value by the index
			data |= val # OR the existing data with the new data
			p += 2
		color = Color.BLACK
		color.r8 = data       & 0xFF # 1. Byte
		color.g8 = data <<  8 & 0xFF # 2. Byte
		color.b8 = data << 16 & 0xFF # 3. Byte
		color.a8 = data << 24 & 0xFF # 4. Byte
		bus.set_pixel(x + 1, 0, color)
		# Store size
		color = gate.get_pixel(x + 1, 0)
		color.a8 = bus_sizes[x]
		gate.set_pixel(x + 1, 0, color)
	
	return [gate, input, output, bus] 

## Loads the supplied textures for this Gate
## textures : [gate: Image, input: Image, output: Image, bus: Image]
## refresh : bool = false # False = load only output values | True = load everything
func load_textures(textures: Array[Image], refresh: bool = false) -> void:
	var color: Color = Color.BLACK
	
	# First pixel : R = gate_id, G = input_amount, B = output_amount, A = bus_amount
	# Rest pixels (aligns with each input/..) : R = unused, G = input_size, B = output_size, A = bus_size
	var gate: Image = textures[0]

	if refresh:
		color = gate.get_pixel(0, gate_id)
		gate_id = color.r8
		input_amount = color.g8
		output_amount = color.b8
		bus_amount = color.a8
	
		# 2 Bits = 1 State (LL = Err, LH = Low, HL = High, HH = Tri) = 2 * 8 * 4 = 64 (bits used) / 2 = 32 (BIT_32)
		var input: Image = textures[1]
		for x: int in range(0, input_amount): # loop for every input, x = input idx, y = gate_id
			var p: int = 0 # Read Pointer
			var sze: Simulation.Sizes = gate.get_pixel(x + 1, gate_id).g8 as Simulation.Sizes
			input_sizes[x] = sze
			color = input.get_pixel(x + 1, gate_id)
			var val: int = 0
			val += color.r8       # 1. Byte
			val += color.g8 >>  8 # 2. Byte
			val += color.b8 >> 16 # 3. Byte
			val += color.a8 >> 24 # 4. Byte
			var states: Array[Simulation.States] = []
			states.resize(sze)
			for i: int in range(0, sze): # loop for every input
				var data: Simulation.States = Simulation.States.UNKNOWN
				data = val >> p & 0b11 as Simulation.States
				states[i] = data
				p += 2
			input_values[x] = states
	
	# 2 Bits = 1 State (LL = Err, LH = Low, HL = High, HH = Tri) = 2 * 8 * 4 = 64 (bits used) / 2 = 32 (BIT_32)
	var output: Image = textures[2]
	for x: int in range(0, output_amount): # loop for every output, x = output idx, y = gate_id
		var p: int = 0 # Read Pointer
		if refresh:
			output_sizes[x] = gate.get_pixel(x + 1, gate_id).b8 as Simulation.Sizes
		color = output.get_pixel(x + 1, gate_id)
		var val: int = 0
		val += color.r8       # 1. Byte
		val += color.g8 >>  8 # 2. Byte
		val += color.b8 >> 16 # 3. Byte
		val += color.a8 >> 24 # 4. Byte
		var states: Array[Simulation.States] = []
		states.resize(output_sizes[x])
		for i: int in range(0, output_sizes[x]): # loop for every input
			var data: Simulation.States = Simulation.States.UNKNOWN
			data = val >> p & 0b11 as Simulation.States
			states[i] = data
			p += 2
		output_values[x] = states

# private functions

# subclasses
