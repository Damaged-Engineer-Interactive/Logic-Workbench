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
## Normal [code]State : Color[/code] map. Used by Inputs and Outputs
const COLOR_NORMAL: Dictionary = {
	Simulation.States.ERROR: Color.RED,
	Simulation.States.LOW: Color.DARK_BLUE,
	Simulation.States.HIGH: Color.BLUE,
	Simulation.States.UNKNOWN: Color.MEDIUM_PURPLE
}

## Bi [code]State : Color[/code] map. Used by Buses
const COLOR_BI: Dictionary = {
	Simulation.States.ERROR: Color.RED,
	Simulation.States.LOW: Color.DARK_GREEN,
	Simulation.States.HIGH: Color.GREEN,
	Simulation.States.UNKNOWN: Color.YELLOW_GREEN
}

const STYLE_IN: StyleBoxFlat = preload("res://styles/simulation/Input.stylebox")
const STYLE_OUT: StyleBoxFlat = preload("res://styles/simulation/Output.stylebox")

# @export variables
#region Gate
@export_group("Gate", "gate_")
## The Name of the Gate
@export var gate_name: String = "":
	set(value):
		gate_name = value
		title = value

## The type of the Gate (used in the simulation)
@export var gate_type: int = 0

## The number of the Gate (used in the workspace and simulation to reference the gate)
## Limited to 255
@export var gate_id: int = 0

## The position of the Gate
@export var gate_position := Vector2(0,0):
	set(value):
		gate_position = value
		position_offset = value
#endregion

#region Input
@export_group("Input", "input_")
## The Number of inputs, the gate has
@export var input_amount: int = 0:
	set(value):
		input_amount = value
		input_sizes.resize(value)
		input_values.resize(value)
		input_names.resize(value)

## The Size of every Input
## Array[Simulation.Sizes]
@export var input_sizes: Array[Simulation.Sizes] = []

## The Values of every Input
## Array[Array[Simulation.States]]
@export var input_values: Array[Array] = []

## The Names of every Input
## Array[String]
@export var input_names: Array[String] = []
#endregion

#region Output
@export_group("Output", "output_")
## The Number of outputs, the gate has
@export var output_amount: int = 0:
	set(value):
		output_amount = value
		output_sizes.resize(value)
		output_values.resize(value)
		output_names.resize(value)

## The Size of every output
## Array[Simulation.Sizes]
@export var output_sizes: Array[Simulation.Sizes] = []

## The Values of every output
## Array[Array[Simulation.States]]
@export var output_values: Array[Array] = []

## The Names of every Output
## Array[String]
@export var output_names: Array[String] = []
#endregion

#region Bus
@export_group("Bus", "bus_")
## The Number of buses, the gate has
@export var bus_amount: int = 0:
	set(value):
		bus_amount = value
		bus_sizes.resize(value)
		bus_values.resize(value)
		bus_names.resize(value)

## The Size of every bus
## Array[Simulation.Sizes]
@export var bus_sizes: Array[Simulation.Sizes] = []

## The Values of every bus
## Array[Array[Simulation.States]]
@export var bus_values: Array[Array] = []

## The Names of every Bus
## Array[String]
@export var bus_names: Array[String] = []
#endregion
# public variables

# private variables
var _is_redraw_queued: bool = false
var _is_redraw_queued_full: bool = false

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function
func _enter_tree() -> void:
	custom_minimum_size = Vector2(200, 50)
	title = gate_name
	name = "GATE_%s_%s" % [gate_name, str(gate_id)]

	set("theme_override_styles/panel", Simulation.THEME_PANEL.duplicate())
	set("theme_override_styles/panel_selected", Simulation.THEME_PANEL.duplicate())
	set("theme_override_styles/titlebar", Simulation.THEME_TITLE.duplicate())
	set("theme_override_styles/titlebar_selected", Simulation.THEME_TITLE_SELECTED.duplicate())

# optional built-in _ready() function
func _ready() -> void:
	redraw(true)

# remaining built-in functions

# virtual functions to override

# public functions
## Adds an IO port to the gate
func add_io(type: Simulation.IO_TYPES, bits: Simulation.Sizes, nme: String) -> void:
	var val: Array[Simulation.States] = []
	val.resize(bits)
	val.fill(Simulation.States.UNKNOWN)
	match type:
		Simulation.IO_TYPES.INPUT:
			input_amount += 1
			input_sizes[-1] = bits
			input_values[-1] = val
			input_names[-1] = nme
		Simulation.IO_TYPES.OUTPUT:
			output_amount += 1
			output_sizes[-1] = bits
			output_values[-1] = val
			output_names[-1] = nme
		Simulation.IO_TYPES.BUS:
			bus_amount += 1
			bus_sizes[-1] = bits
			bus_values[-1] = val
			bus_names[-1] = nme
	redraw(true)

## Creates the textures of this Gate
## Result : [gate: Image, input: Image, output: Image, bus: Image]
func create_textures() -> Array[PackedByteArray]:
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
	
	return [gate.data["data"], input.data["data"], output.data["data"], bus.data["data"]] 

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
	
	redraw()

## Queue redraw() and prevent it from being called multiple times
func redraw(full: bool = false) -> void:
	print("redraw(%s)" % str(full))
	if not _is_redraw_queued:
		if full and not _is_redraw_queued_full:
			_is_redraw_queued_full = true
			call_deferred(&"_redraw", true)
		else:
			call_deferred(&"_redraw", false)
		_is_redraw_queued = true

# private functions
## Updates the Visual representation of the Gate.[br]
## Full # True : removes everything and starts from scratch | False : Only updates colors
func _redraw(full: bool = false) -> void:
	print("_redraw(%s) - Start" % str(full))
	_is_redraw_queued = false
	_is_redraw_queued_full = false
	if full:
		# Remove Childs
		for child in get_children():
			remove_child(child)
			child.queue_free()

		# Make new Childs
		var io: int = max(input_amount, output_amount)
		for i in range(0, io):
			var slot: HBoxContainer = HBoxContainer.new()
			slot.name = "SLOT_" + str(i)
			add_child(slot)

			if input_amount > i: # input exists, render it
				var state = _get_combined_state(input_values[i])
				var color = COLOR_NORMAL.get(state, Color.BLACK)

				var label: Label = Label.new()
				label.name = "IN_%s_%s" % [str(i), input_names[i]]
				label.text = input_names[i]

				var style: StyleBoxFlat = STYLE_IN.duplicate()
				style.border_color = color
				label.set("theme_override_styles/normal", style)

				slot.add_child(label)

				set_slot_enabled_left(i, true)
				set_slot_color_left(i, color)
				set_slot_type_left(i, input_sizes[i])
			else:
				set_slot_enabled_left(i, false)
			
			# Spacer between Input and Output
			var spacer: Label = Label.new()
			spacer.name = "SEP_%s" % str(i)
			spacer.size_flags_horizontal = Control.SIZE_EXPAND_FILL
			slot.add_child(spacer)

			if output_amount > i: # output exists, render it
				var state = _get_combined_state(output_values[i])
				var color = COLOR_NORMAL.get(state, Color.BLACK)

				var label: Label = Label.new()
				label.name = "OUT_%s_%s" % [str(i), output_names[i]]
				label.text = output_names[i]

				var style: StyleBoxFlat = STYLE_OUT.duplicate()
				style.border_color = color
				label.set("theme_override_styles/normal", style)

				slot.add_child(label)

				set_slot_enabled_right(i, true)
				set_slot_color_right(i, color)
				set_slot_type_right(i, output_sizes[i])
			else:
				set_slot_enabled_right(i, false)
		self.queue_redraw()
		print("_redraw(%s) - End" % str(full))
		return
	
	for i in range(0, input_amount): # loop for every input
		var state = _get_combined_state(input_values[i])
		var color = COLOR_NORMAL.get(state, Color.BLACK)

		set_slot_color_left(i, color)
		var label: Label = find_child("IN_%s_%s" % [str(i), input_names[i]])
		var style: StyleBoxFlat = label.get("theme_override_styles/normal")
		style.border_color = color
		label.set("theme_override_styles/normal", style)
	
	for i in range(0, output_amount): # loop for every output
		var state = _get_combined_state(output_values[i])
		var color = COLOR_NORMAL.get(state, Color.BLACK)

		set_slot_color_right(i, color)
		var label: Label = find_child("OUT_%s_%s" % [str(i), output_names[i]])
		var style: StyleBoxFlat = label.get("theme_override_styles/normal")
		style.border_color = color
		label.set("theme_override_styles/normal", style)
	
	print("_redraw(%s) - End" % str(full))
	return


func _get_combined_state(states: Array[Simulation.States]) -> Simulation.States:
	for state: Simulation.States in states:
		match state:
			Simulation.States.ERROR: return state
			Simulation.States.UNKNOWN: return state
			Simulation.States.LOW: return state
			# it must be high, so continue
	# everything was high, return HIGH
	return Simulation.States.HIGH

# subclasses
