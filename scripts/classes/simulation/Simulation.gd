# The name of the Class
class_name Simulation
# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals
#region Debug
signal _debug_simulation_begin(textures: Array[Image])
signal _debug_simulation_end(textures: Array[Image])

#endregion

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
	ON,
	OFF,

	CUSTOM  = 255
}

# Constants
const STATE_TO_LETTER: Dictionary[States, String] = {
	States.ERROR: "!",
	States.LOW: "L",
	States.HIGH: "H",
	States.UNKNOWN: "?",
}

const STATE_TO_COLOR: Dictionary[States, Color] = {
	States.ERROR: Color.RED,
	States.LOW: Color.DARK_GREEN,
	States.HIGH: Color.GREEN,
	States.UNKNOWN: Color.BLUE,
}

const MAX_IO_COUNT: int = 128

const THEME_PANEL: StyleBoxFlat = preload("res://styles/simulation/Panel.stylebox")
const THEME_TITLE: StyleBoxFlat = preload("res://styles/simulation/Titlebar.stylebox")
const THEME_TITLE_SELECTED: StyleBoxFlat = preload("res://styles/simulation/Titlebar_selected.stylebox")

# @export variables
@export var allow_simulate: bool = false

# public variables
var gates: Array[Gate] = []
var connections: Array[Connection] = []

var can_simulate: bool = false

var GATES: Dictionary[Simulation.GATE_TYPES, Variant] = {
	Simulation.GATE_TYPES.AND: AndGate,
	Simulation.GATE_TYPES.NAND: NandGate,
	Simulation.GATE_TYPES.OR: OrGate,
	Simulation.GATE_TYPES.NOR: NorGate,
	Simulation.GATE_TYPES.NOT: NotGate,
	Simulation.GATE_TYPES.XOR: XorGate,
	Simulation.GATE_TYPES.XNOR: XnorGate,
	Simulation.GATE_TYPES.TRI: TriStateGate,
	Simulation.GATE_TYPES.ON: OnGate,
	Simulation.GATE_TYPES.OFF: OffGate
}

# private variables
var _next_gate_id: Array[int] = [0]
var _amount_of_gates: int = 0

var _next_connection_id: Array[int] = [0]

#region Shader
var _sim_counter: int = -1 # Frames until next simulate() call
var _is_simulating: bool = false # False : Dispatch new instance | True : Get Results
var _invalid_run: bool = false # Invalid if gates / connections changed
# [gate, input, output, bus, connection]
var _data: Array[Image] = []

var _rd: RenderingDevice
var _simulate_shader: RID
var _connection_shader: RID

var _uniform_gate: RDUniform
var _uniform_input: RDUniform
var _uniform_output: RDUniform
var _uniform_bus: RDUniform
var _uniform_connection: RDUniform
var _uniform_sim_set: RID
var _uniform_con_set: RID

#endregion

# @onready variables
var _simulate_file = preload("res://shaders/simulation.glsl")

var _connection_file = preload("res://shaders/connection.glsl")

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
			_sim_counter += 20

# virtual functions to override

# public functions
func get_gate(id: int) -> Gate:
	if id < gates.size():
		return gates[id]
	return null

func add_gate(gate: Gate) -> void:
	var id = _next_gate_id.pop_back() # Get the newest gate_id available
	if _next_gate_id.size() == 0: # Push new id if none available
		_next_gate_id.append(id + 1)
	
	if id >= gates.size(): # Array is full, make more space
		gates.resize(gates.size() + 16)
	
	_amount_of_gates += 1
	
	gate.gate_id = id
	
	gates[id] = gate
	
	_invalid_run = true

func remove_gate(id: int) -> Gate:
	var gate: Gate = gates[id]
	gates[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_gate_id.append(id)
	_amount_of_gates -= 1
	_invalid_run = true
	return gate # Returns the gate as reference, so it isn't immediately gone

func get_connection(con_id: String) -> Connection:
	for connection: Connection in connections:
		if not connection:
			break
		if con_id == connection.get_con_id():
			return connection
	return null

func add_connection(connection: Connection) -> void:
	var id = _next_connection_id.pop_back() # Get the newest connection_id available
	if _next_connection_id.size() == 0: # Push new id if none available
		_next_connection_id.append(id + 1)
	
	if id >= connections.size(): # Array is full, make more space
		connections.resize(connections.size() + 16)
	
	connection.id = id
	
	connection.name = connection.get_con_id()
	
	connections[id] = connection
	
	_invalid_run = true

func remove_connection(id: int) -> Connection:
	var connection: Connection = connections[id]
	connections[id] = null # Same effect as pop_at() or erase(), but without resizing
	_next_connection_id.append(id)
	_invalid_run = true
	return connection # Returns the gate as reference, so it isn't immediately gone

func simulate() -> void:
	if not allow_simulate:
		return
	if _is_simulating:
		_simulate_end()
		_sim_counter = 1 # 1 Frame until next simulate() call 
	else:
		_simulate_begin()
		_sim_counter = 3 # 3 Frames until next simulate() call
	_invalid_run = false

# private functions
func _prepare_simulation() -> void:
	print("prepare_simulation")
	_rd = RenderingServer.create_local_rendering_device()

	var sim_spirv: RDShaderSPIRV = _simulate_file.get_spirv()
	_simulate_shader = _rd.shader_create_from_spirv(sim_spirv)
	
	var con_spirv: RDShaderSPIRV = _connection_file.get_spirv()
	_connection_shader = _rd.shader_create_from_spirv(con_spirv)

	can_simulate = true
	_sim_counter = 60 # 60 Frames until first simulate() call

func _simulate_begin() -> void:
	#print("simulate_begin")
	if _amount_of_gates == 0:
		return
	
	# Prepare Buffers
	var data_gate: PackedByteArray = PackedByteArray()
	var data_input: PackedByteArray = PackedByteArray()
	var data_output: PackedByteArray = PackedByteArray()
	var data_bus: PackedByteArray = PackedByteArray()
	var image_connection: Image = Image.create_empty(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8)

	# Loop through the gates
	for i: int in range(0, _amount_of_gates):
		var gate: Gate = gates[i]
		var gate_data: Array[PackedByteArray] = gate.create_textures()
		data_gate.append_array(gate_data[0])
		data_input.append_array(gate_data[1])
		data_output.append_array(gate_data[2])
		data_bus.append_array(gate_data[3])
	
	# Loop through the connections
	for connection: Connection in connections:
		if not connection:
			continue
		if connection.size_in != connection.size_out:
			continue
		var x: int = connection.gate_in
		var y: int = connection.port_in + 1
		var color: Color = Color.BLACK
		color.r = connection.gate_out
		color.g = connection.port_out + 1
		image_connection.set_pixel(x, y, color)

	# Make Images
	_data.resize(0)
	_data.resize(5)
	_data[0] = Image.create_from_data(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8, data_gate)
	_data[1] = Image.create_from_data(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8, data_input)
	_data[2] = Image.create_from_data(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8, data_output)
	_data[3] = Image.create_from_data(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8, data_bus)
	_data[4] = image_connection

	_uniform_gate = _simulate_create_uniform(0, _data[0])
	_uniform_input = _simulate_create_uniform(1, _data[1])
	_uniform_output = _simulate_create_uniform(2, _data[2])
	_uniform_bus = _simulate_create_uniform(3, _data[3])
	_uniform_connection = _simulate_create_uniform(4, _data[4])

	_uniform_sim_set = _rd.uniform_set_create([_uniform_gate, _uniform_input, _uniform_output, _uniform_bus, _uniform_connection], _simulate_shader, 0)
	_uniform_con_set = _rd.uniform_set_create([_uniform_gate, _uniform_input, _uniform_output, _uniform_bus, _uniform_connection], _connection_shader, 0)

	var sim_pipeline: RID = _rd.compute_pipeline_create(_simulate_shader)
	var con_pipeline: RID = _rd.compute_pipeline_create(_connection_shader)
	var compute_list: int = _rd.compute_list_begin()

	_rd.compute_list_bind_compute_pipeline(compute_list, sim_pipeline)
	_rd.compute_list_bind_uniform_set(compute_list, _uniform_sim_set, 0)
	_rd.compute_list_dispatch(compute_list, maxi(1, ceili(gates.size() / 16.0)), 1, 1)

	_rd.compute_list_bind_compute_pipeline(compute_list, con_pipeline)
	_rd.compute_list_bind_uniform_set(compute_list, _uniform_con_set, 0)
	_rd.compute_list_dispatch(compute_list, maxi(1, ceili(gates.size() / 16.0)), 1, 1)
	
	_rd.compute_list_end()

	_rd.submit()
	
	_is_simulating = true
	
	_debug_simulation_begin.emit(_data)
	
func _simulate_end() -> void:
	#print("simulate_end")
	_rd.sync() # Sync to prevent reading before the shader finishes

	if _invalid_run:
		_is_simulating = false
		return

	# Getting the Result
	var res_gate: Image = Image.create_from_data(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8, _rd.texture_get_data(_uniform_gate.get_ids()[-1], 0))
	
	var res_input: Image = Image.create_from_data(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8, _rd.texture_get_data(_uniform_input.get_ids()[-1], 0))
	
	var res_output: Image = Image.create_from_data(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8, _rd.texture_get_data(_uniform_output.get_ids()[-1], 0))
	
	var res_bus: Image = Image.create_from_data(MAX_IO_COUNT + 1, _amount_of_gates, false, Image.Format.FORMAT_RGBA8, _rd.texture_get_data(_uniform_bus.get_ids()[-1], 0))

	# [gate, input, output, bus]
	var data: Array[Image] = [res_gate, res_input, res_output, res_bus]
	
	_debug_simulation_end.emit(data)

	get_tree().call_group(&"Gates", &"load_textures", data)
	
	_is_simulating = false

func _simulate_create_uniform(binding: int, image: Image) -> RDUniform:
	var textureFormat: RDTextureFormat = RDTextureFormat.new()
	textureFormat.width = image.get_width()
	textureFormat.height = image.get_height()
	textureFormat.usage_bits += RenderingDevice.TEXTURE_USAGE_CAN_UPDATE_BIT
	textureFormat.usage_bits += RenderingDevice.TEXTURE_USAGE_STORAGE_BIT
	textureFormat.usage_bits += RenderingDevice.TEXTURE_USAGE_SAMPLING_BIT
	textureFormat.usage_bits += RenderingDevice.TEXTURE_USAGE_CAN_COPY_FROM_BIT
	textureFormat.format = RenderingDevice.DATA_FORMAT_R8G8B8A8_UNORM
	
	var data: PackedByteArray = image.get_data()
	
	var texture: RID = _rd.texture_create(textureFormat, RDTextureView.new())
	_rd.texture_update(texture, 0, data)
	
	var uniform: RDUniform = RDUniform.new()
	uniform.uniform_type = RenderingDevice.UniformType.UNIFORM_TYPE_IMAGE
	uniform.binding = binding
	uniform.add_id(texture)
	
	return uniform

# subclasses
 
