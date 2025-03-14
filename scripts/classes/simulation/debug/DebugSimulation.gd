# The name of the Class
class_name DebugSimulation
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
@export var simulation: Simulation
@export var allow: bool = true

# public variables

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function
func _enter_tree() -> void:
	if !allow:
		print_rich("[color=cyan]DebugSimulation[/color] : [color=orange]DISABLED[/color]")
	elif simulation:
		simulation._debug_simulation_begin.connect(debug_simulation_begin)
		simulation._debug_simulation_end.connect(debug_simulation_end)
		print_rich("[color=cyan]DebugSimulation[/color] : [color=green]READY[/color]")
	else:
		print_rich("[color=cyan]DebugSimulation[/color] : [color=red]INVALID[/color]")
		push_error("Invalid simulation!")

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions
# [gate, input, output, bus, connection]
func debug_simulation_begin(data: Array[Image]) -> void:
	print_rich("[color=cyan]DebugSimulation[/color] : [color=purple]debug_simulation_begin[/color]")
	
	# Test for : height, width, amount of images
	var basic_image_test: bool = true
	if data.size() != 5:
		basic_image_test = false
		print_rich("[color=blue]Basic Image Tests[/color] : [color=red]Fail[/color] | [color=white]Invalid amount of Images[/color]")
	for i in range(0, data.size()):
		var image: Image = data[i]
		if image.get_width() != Simulation.MAX_IO_COUNT + 1:
			basic_image_test = false
			print_rich("[color=blue]Basic Image Tests[/color] - [color=cyan][%s][/color] : [color=red]Fail[/color] | [color=white]Wrong image width[/color]" % str(i))
		if image.get_height() != simulation._amount_of_gates:
			basic_image_test = false
			print_rich("[color=blue]Basic Image Tests[/color] - [color=cyan][%s][/color] : [color=red]Fail[/color] | [color=white]Wrong image height[/color]" % str(i))
	
	if basic_image_test:
		print_rich("[color=blue]Basic Image Tests[/color] : [color=green]Pass[/color]")
	
	# Test for : Each gate
	for y in range(0, simulation._amount_of_gates):
		var res_gate: bool = true # Gate texture
		var res_input: bool = true # Input texture
		var res_output: bool = true # Output texture
		
		var image: Image
		var color: Color
		
		image = data[0]
		color = image.get_pixel(0, y)
		if color.r8 != simulation.gates[y].gate_type:
			res_gate = false
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][Gate@%s][/color] : [color=red]Fail[/color] | [color=white]Wrong Gate Type[/color]" % str(y))
		if color.g8 != simulation.gates[y].input_amount:
			res_gate = false
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][Gate@%s][/color] : [color=red]Fail[/color] | [color=white]Wrong Input Amount[/color]" % str(y))
		if color.b8 != simulation.gates[y].output_amount:
			res_gate = false
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][Gate@%s][/color] : [color=red]Fail[/color] | [color=white]Wrong Output Amount[/color]" % str(y))
		if color.a8 != simulation.gates[y].bus_amount:
			res_gate = false
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][Gate@%s][/color] : [color=red]Fail[/color] | [color=white]Wrong Bus Amount[/color]" % str(y))
		
		image = data[1]
		for i in range(0, simulation.gates[y].input_amount):
			color = image.get_pixel(i + 1, y)
			var val: int = 0
			val |= color.r8       # 1. Byte
			val |= color.g8 <<  8 # 2. Byte
			val |= color.b8 << 16 # 3. Byte
			val |= color.a8 << 24 # 4. Byte
			var p: int = 0
			for j: int in range(0, simulation.gates[y].input_sizes[i]): # loop for every input
				var d: Simulation.States = Simulation.States.UNKNOWN
				d = val >> p & 0b11 as Simulation.States
				if d != simulation.gates[y].input_values[i][j]:
					res_input = false
					print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=red]Fail[/color] | [color=purple]Input [/color] | [color=white]Invalid State[/color] | %s != %s" % [str(y), d, simulation.gates[y].input_values[i][j]])
				else:
					print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=green]Pass[/color] | [color=purple]Input [/color] | %s : %s" % [str(y), d, simulation.gates[y].input_values[i][j]])
				p += 2
		
		image = data[2]
		for i in range(0, simulation.gates[y].output_amount):
			color = image.get_pixel(i + 1, y)
			var val: int = 0
			val |= color.r8       # 1. Byte
			val |= color.g8 <<  8 # 2. Byte
			val |= color.b8 << 16 # 3. Byte
			val |= color.a8 << 24 # 4. Byte
			var p: int = 0
			for j: int in range(0, simulation.gates[y].output_sizes[i]): # loop for every output
				var d: Simulation.States = Simulation.States.UNKNOWN
				d = val >> p & 0b11 as Simulation.States
				if d != simulation.gates[y].output_values[i][j]:
					res_output = false
					print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=red]Fail[/color] | [color=purple]Output[/color] |[color=white]Invalid State[/color] | %s != %s" % [str(y), d, simulation.gates[y].output_values[i][j]])
				else:
					print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=green]Pass[/color] | [color=purple]Output[/color] | %s : %s" % [str(y), d, simulation.gates[y].output_values[i][j]])
				p += 2
		
		if res_gate and res_input and res_output:
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=green]Pass[/color]\n" % str(y))
		else:
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=red]Fail[/color]\n" % str(y))
	print("\n")
	
	pass # Breakpoint

# [gate, input, output, bus]
func debug_simulation_end(data: Array[Image]) -> void:
	print_rich("[color=cyan]DebugSimulation[/color] : [color=purple]debug_simulation_end[/color]")
	
	# Test for : height, width, amount of images
	var basic_image_test: bool = true
	if data.size() != 4:
		basic_image_test = false
		print_rich("[color=blue]Basic Image Tests[/color] : [color=red]Fail[/color] | [color=white]Invalid amount of Images[/color]")
	for i in range(0, data.size()):
		var image: Image = data[i]
		if image.get_width() != Simulation.MAX_IO_COUNT + 1:
			basic_image_test = false
			print_rich("[color=blue]Basic Image Tests[/color] - [color=cyan][%s][/color] : [color=red]Fail[/color] | [color=white]Wrong image width[/color]" % str(i))
		if image.get_height() != simulation._amount_of_gates:
			basic_image_test = false
			print_rich("[color=blue]Basic Image Tests[/color] - [color=cyan][%s][/color] : [color=red]Fail[/color] | [color=white]Wrong image height[/color]" % str(i))
	
	if basic_image_test:
		print_rich("[color=blue]Basic Image Tests[/color] : [color=green]Pass[/color]")
	
	# Test for : Each gate
	for y in range(0, simulation._amount_of_gates):
		var res_gate: bool = true # Gate texture
		
		var image: Image
		var color: Color
		
		image = data[0]
		color = image.get_pixel(0, y)
		if color.r8 != simulation.gates[y].gate_type:
			res_gate = false
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][Gate@%s][/color] : [color=red]Fail[/color] | [color=white]Wrong Gate Type[/color]" % str(y))
		if color.g8 != simulation.gates[y].input_amount:
			res_gate = false
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][Gate@%s][/color] : [color=red]Fail[/color] | [color=white]Wrong Input Amount[/color]" % str(y))
		if color.b8 != simulation.gates[y].output_amount:
			res_gate = false
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][Gate@%s][/color] : [color=red]Fail[/color] | [color=white]Wrong Output Amount[/color]" % str(y))
		if color.a8 != simulation.gates[y].bus_amount:
			res_gate = false
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][Gate@%s][/color] : [color=red]Fail[/color] | [color=white]Wrong Bus Amount[/color]" % str(y))
		
		image = data[1]
		for i in range(0, simulation.gates[y].input_amount):
			color = image.get_pixel(i + 1, y)
			var val: int = 0
			val |= color.r8       # 1. Byte
			val |= color.g8 <<  8 # 2. Byte
			val |= color.b8 << 16 # 3. Byte
			val |= color.a8 << 24 # 4. Byte
			var p: int = 0
			for j: int in range(0, simulation.gates[y].input_sizes[i]): # loop for every input
				var d: Simulation.States = Simulation.States.UNKNOWN
				d = val >> p & 0b11 as Simulation.States
				print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=green]Pass[/color] | [color=purple]Input [/color] | %s : %s" % [str(y), d, simulation.gates[y].input_values[i][j]])
		
		image = data[2]
		for i in range(0, simulation.gates[y].output_amount):
			color = image.get_pixel(i + 1, y)
			var val: int = 0
			val |= color.r8       # 1. Byte
			val |= color.g8 <<  8 # 2. Byte
			val |= color.b8 << 16 # 3. Byte
			val |= color.a8 << 24 # 4. Byte
			var p: int = 0
			for j: int in range(0, simulation.gates[y].output_sizes[i]): # loop for every output
				var d: Simulation.States = Simulation.States.UNKNOWN
				d = val >> p & 0b11 as Simulation.States
				print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=green]Pass[/color] | [color=purple]Output[/color] | %s : %s" % [str(y), d, simulation.gates[y].output_values[i][j]])
		
		if res_gate:
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=green]Pass[/color]\n" % str(y))
		else:
			print_rich("[color=blue]Gate Test[/color] - [color=cyan][%s][/color] : [color=red]Fail[/color]\n" % str(y))
	print("\n")
	
	pass # Breakpoint

# subclasses
 
