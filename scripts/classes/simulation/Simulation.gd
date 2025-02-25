# The name of the Class
class_name Simulation
# The class this class extends
extends Node
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

enum IO_TYPES {
	UNKNOWN = 0,
	INPUT = 1,
	OUTPUT = 2,
	BUS = 3
}

enum GATE_TYPES {
	UNKNOWN =   0,
	AND,
	NAND,
	OR,
	NOR,
	NOT,
	XOR,
	XNOR,
	TRI,

	CUSTOM  = 255
}

# Constants
const STATE_TO_COLOR: Dictionary = {
	States.ERROR: Color.RED,
	States.LOW: Color.DARK_GREEN,
	States.HIGH: Color.GREEN,
	States.UNKNOWN: Color.BLUE,
}

const MAX_IO_COUNT: int = 128

# @export variables
@export var can_simulate: bool = false

# public variables
var gates: Array[Gate] = []
var connections: Array[Connection] = []

# private variables
var _next_gate_id: Array[int] = [0]

var _next_connection_id: Array[int] = [0]

#region Shader
var _sim_counter: int = -1 # Frames until next simulate() call
var _is_simulating: bool = false # False : Dispatch new instance | True : Get Results
# [gate, input, output, bus]
var _data: Array[Image] = []

var _rd: RenderingDevice
var _shader: RID

var _uniform_gate: RDUniform
var _uniform_input: RDUniform
var _uniform_output: RDUniform
var _uniform_bus: RDUniform
var _uniform_set: RID

var pipeline: RID

#endregion

# @onready variables
var shader = preload("res://shaders/simulation.gd")

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	gates.resize(32)
	connections.resize(32)

	_prepare_simulation()

# remaining built-in functions
func _process(_d: float) -> void:
	if can_simulate:
		_sim_counter -= 1
		if _sim_counter == 0:
			simulate()
			_sim_counter = 10 # 10 Frames until next simulate() call

# virtual functions to override

# public functions
func add_gate(gate: Gate) -> void:
	var id = _next_gate_id.pop_back() # Get the newest gate_id available
	if _next_gate_id.size() == 0: # Push new id if none available
		_next_gate_id.append(id + 1)
	
	if id >= gates.size(): # Array is full, make more space
		gates.resize(gates.size() + 16)
	
	gate.id = id
	
	gates[id] = gate

func remove_gate(id: int) -> Gate:
	var gate: Gate = gates[id]
	gates[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_gate_id.append(id)
	return gate # Returns the gate as reference, so it isn't immediately gone


func add_connection(connection: Connection) -> void:
	var id = _next_connection_id.pop_back() # Get the newest connection_id available
	if _next_connection_id.size() == 0: # Push new id if none available
		_next_connection_id.append(id + 1)
	
	if id >= connections.size(): # Array is full, make more space
		connections.resize(connections.size() + 16)
	
	connection.id = id
	
	connections[id] = connection

func remove_connection(id: int) -> Connection:
	var connection: Connection = connections[id]
	connections[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_connection_id.append(id)
	return connection # Returns the gate as reference, so it isn't immediately gone

func simulate() -> void:
	if _is_simulating:
		_simulate_end()
	else:
		_simulate_begin()

# private functions
func _prepare_simulation() -> void:
	_rd = RenderingServer.create_local_rendering_device()

	var spirv: RDShaderSPIRV = shader_file.get_spirv()
	_shader = _rd.shader_create_from_spirv(spirv)

	can_simulate = true
	_sim_counter = 60 # 60 Frames until first simulate() call

func _simulate_begin() -> void:
	# invalid data, will be fixed later

	_uniform_gate = _simulate_create_uniform(0, _data[0])
	_uniform_input = _simulate_create_uniform(1, _data[1])
	_uniform_output = _simulate_create_uniform(2, _data[2])
	_uniform_bus = _simulate_create_uniform(3, _data[3])

	_uniform_set = rd.uniform_set_create([_uniform_gate, _uniform_input, _uniform_output, _uniform_bus], _shader, 0)

	var pipeline: RID = _rd.compute_pipeline_create(_shader)
	var compute_list: int = _rd.compute_list_begin()
	_rd.compute_list_bind_compute_pipeline(compute_list, pipeline)
	_rd.compute_list_bind_uniform_set(compute_list, _uniform_set, 0)
	_rd.compute_list_dispatch(compute_list, maxi(1, ceili(gates.size() / 16.0)), 1, 1)
	_rd.compute_list_end()

	_rd.submit()
	
func _simulate_end() -> void:
	_rd.sync() # Sync to prevent reading before the shader finishes

	# Getting the Result
	var res_gate: Image = Image.new()
	res_gate.copy_from(data[0])
	res_gate.data["data"] = _rd.texture_get_data(_uniform_gate.get_ids()[-1], 0)
	
	var res_input: Image = Image.new()
	res_input.copy_from(data[1])
	res_input.data["data"] = _rd.texture_get_data(_uniform_input.get_ids()[-1], 0)
	
	var res_output: Image = Image.new()
	res_output.copy_from(data[2])
	res_output.data["data"] = _rd.texture_get_data(_uniform_output.get_ids()[-1], 0)
	
	var res_bus: Image = Image.new()
	res_bus.copy_from(data[3])
	res_bus.data["data"] = _rd.texture_get_data(_uniform_bus.get_ids()[-1], 0)

	# [gate, input, output, bus]
	var data: Array[Image] = [res_gate, res_input, res_output, res_bus]

	# Handle connections here

func _simulate_create_uniform(binding: int, image: Image) -> RDUniform:
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
	
	var texture: RID = _rd.texture_create(textureFormat, RDTextureView.new())
	_rd.texture_update(texture, 0, data)
	
	var uniform: RDUniform = RDUniform.new()
	uniform.uniform_type = RenderingDevice.UniformType.UNIFORM_TYPE_IMAGE
	uniform.binding = binding
	uniform.add_id(texture)
	
	return uniform

# subclasses
 