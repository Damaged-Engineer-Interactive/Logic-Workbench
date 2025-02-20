# The name of the Class

# The class this class extends
extends VBoxContainer
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables
var shader_file := preload("res://shaders/simulation.glsl")

# public variables

# private variables
#region dragging
var _drag_area: bool = false
var _drag_active: bool = false
var _drag_start := Vector2(0,0)
#endregion

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	var gate: Gate = $Content/Gate
	
	gate.input_amount = 2
	gate.input_sizes.append(Simulation.Sizes.BIT_1)
	gate.input_values.append([Simulation.States.UNKNOWN])
	gate.input_sizes.append(Simulation.Sizes.BIT_1)
	gate.input_values.append([Simulation.States.UNKNOWN])
	
	gate.output_amount = 1
	gate.output_sizes.append(Simulation.Sizes.BIT_1)
	gate.output_values.append([Simulation.States.UNKNOWN])
	
	var data: Dictionary = gate.create_textures()
	
	print("- - - Data")
	for key: String in data.keys():
		print("\n\n")
		print(key)
		var image: Image = data[key] as Image
		print(image)
		print(image.get_size())
		print(image.get_format())
		print()
		print(image.data)
		print("\n\n")
	
	# Computing
	print("- - - Compute")
	var rd := RenderingServer.create_local_rendering_device() # Local RenderingDevice
	print(rd)
	print()
	
	# Load shader
	var shader_spirv: RDShaderSPIRV = shader_file.get_spirv()
	var shader: RID = rd.shader_create_from_spirv(shader_spirv)
	
	print(shader)
	print()
	
	# Providing Data
	var gate_uniform = _create_uniform(rd, 0, data["gate"])
	var input_uniform = _create_uniform(rd, 1, data["input"])
	var output_uniform = _create_uniform(rd, 2, data["output"])
	var bus_uniform = _create_uniform(rd, 3, data["bus"])
	
	var uniform_set: RID = rd.uniform_set_create([gate_uniform, input_uniform, output_uniform, bus_uniform], shader, 0)
	
	print(uniform_set)
	print()
	
	# Defining the Workgroups (final workgroups : x = gate_count / 16, y = 1, z = 1)
	var gate_count: int = 1 
	
	# Creating the pipeline
	var pipeline: RID = rd.compute_pipeline_create(shader)
	var compute_list: int = rd.compute_list_begin()
	rd.compute_list_bind_compute_pipeline(compute_list, pipeline)
	rd.compute_list_bind_uniform_set(compute_list, uniform_set, 0)
	rd.compute_list_dispatch(compute_list, maxi(1, ceili(gate_count / 16.0)), 1, 1)
	rd.compute_list_end()
	
	print("submit")
	# Running the Shader
	rd.submit()
	rd.sync()
	
	print("done")
	
	# Getting the Result
	var res_gate: Image = Image.new()
	res_gate.copy_from(data["gate"])
	res_gate.data["data"] = rd.texture_get_data(gate_uniform.get_ids()[-1], 0)
	
	var res_input: Image = Image.new()
	res_input.copy_from(data["input"])
	res_input.data["data"] = rd.texture_get_data(input_uniform.get_ids()[-1], 0)
	
	var res_output: Image = Image.new()
	res_output.copy_from(data["output"])
	res_output.data["data"] = rd.texture_get_data(output_uniform.get_ids()[-1], 0)
	
	var res_bus: Image = Image.new()
	res_bus.copy_from(data["bus"])
	res_bus.data["data"] = rd.texture_get_data(bus_uniform.get_ids()[-1], 0)
	
	var result: Dictionary = {
		"gate": res_gate,
		"input": res_input,
		"output": res_output,
		"bus": res_bus
	}
	
	print("- - - Result")
	for key: String in result.keys():
		print("\n\n")
		print(key)
		var image: Image = result[key] as Image
		print(image)
		print(image.get_size())
		print(image.get_format())
		print()
		print(image.data)
		print("\n\n")

# remaining built-in functions

# virtual functions to override
func _input(event: InputEvent) -> void:
	if event is InputEventMouse:
		if event is InputEventMouseButton:
			if _drag_area:
				_input_drag_click(event)
		
		if event is InputEventMouseMotion:
			if _drag_active:
				_input_drag_move(event)

# public functions

# private functions
#region dragging
func _top_bar_mouse_entered() -> void:
	_drag_area = true

func _top_bar_mouse_exited() -> void:
	_drag_area = false

func _input_drag_click(event: InputEventMouseButton):
	if event.button_index != MOUSE_BUTTON_LEFT:
		return
	
	if event.pressed:
		_drag_active = true
		_drag_start = get_local_mouse_position()
	else:
		_drag_active = false
		_drag_start = Vector2(0,0)

@warning_ignore("unused_parameter")
func _input_drag_move(event: InputEventMouseMotion):
	var pos = Vector2(DisplayServer.window_get_position())
	pos -= _drag_start
	pos += get_global_mouse_position()
	
	DisplayServer.window_set_position(Vector2i(pos))#
#endregion

func _create_uniform(rd: RenderingDevice, binding: int, image: Image) -> RDUniform:
	var textureFormat: RDTextureFormat = RDTextureFormat.new()
	textureFormat.width = image.get_width()
	textureFormat.height = image.get_height()
	textureFormat.usage_bits += RenderingDevice.TEXTURE_USAGE_CAN_UPDATE_BIT
	textureFormat.usage_bits += RenderingDevice.TEXTURE_USAGE_STORAGE_BIT
	textureFormat.usage_bits += RenderingDevice.TEXTURE_USAGE_SAMPLING_BIT
	textureFormat.usage_bits += RenderingDevice.TEXTURE_USAGE_CAN_COPY_FROM_BIT
	textureFormat.format = RenderingDevice.DATA_FORMAT_R8G8B8A8_UNORM
	
	print(image.get_data())
	var data: PackedByteArray = image.get_data()
	
	var texture: RID = rd.texture_create(textureFormat, RDTextureView.new())
	rd.texture_update(texture, 0, data)
	
	var uniform: RDUniform = RDUniform.new()
	uniform.uniform_type = RenderingDevice.UniformType.UNIFORM_TYPE_IMAGE
	uniform.binding = binding
	uniform.add_id(texture)
	
	return uniform

func _home_button_pressed() -> void:
	var view: View = get_parent()
	var controller: ViewController = view.controller
	controller.quit()

# subclasses
