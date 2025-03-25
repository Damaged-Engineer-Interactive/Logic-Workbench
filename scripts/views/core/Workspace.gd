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

# public variables

# private variables
#region dragging
var _drag_area: bool = false
var _drag_active: bool = false
var _drag_start := Vector2(0,0)
#endregion

# @onready variables
@onready var simulation: Simulation = $Content/HBoxContainer/Workspace/Simulation
@onready var workspace: GraphEdit = $Content/HBoxContainer/Workspace

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

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

func _home_button_pressed() -> void:
	var view: View = get_parent()
	var controller: ViewController = view.controller
	controller.quit()

func _workspace_connection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	var gate_in: int = from_node.split("_")[-1] as int
	var gate_out: int = to_node.split("_")[-1] as int
	
	var connection := Connection.new()
	connection.gate_in = gate_in
	connection.port_in = from_port
	connection.type_in = Simulation.IO_TYPES.INPUT
	connection.size_in = Simulation.Sizes.BIT_1
	
	connection.gate_out = gate_out
	connection.port_out = to_port
	connection.type_out = Simulation.IO_TYPES.OUTPUT
	connection.size_out = Simulation.Sizes.BIT_1
	
	simulation.add_connection(connection)
	workspace.connect_node(from_node, from_port, to_node, to_port)


func _workspace_disconnection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	var con_id: String = Connection.make_con_id(from_node, from_port, to_node, to_port)
	var connection: Connection = simulation.get_connection(con_id)
	simulation.remove_connection(connection.id)
	workspace.disconnect_node(from_node, from_port, to_node, to_port)

func _workspace_delete_nodes_request(nodes: Array[StringName]) -> void:
	for node_name: StringName in nodes:
		var node: Gate = workspace.find_child(node_name, true, false) as Gate
		var id = node_name.split("_")[-1]
		
		for connection: Connection in simulation.connections:
			if connection.uses_gate(node):
				simulation.remove_connection(connection.id)
				workspace.queue_redraw()
		
		simulation.remove_gate(int(id))
		workspace.remove_child(node)

func _gate_button_down(type: Simulation.GATE_TYPES) -> void:
	if type == Simulation.GATE_TYPES.UNKNOWN:
		return
	var gate_type: Variant = simulation.GATES.get(type)
	var gate: Gate = gate_type.new()
	
	simulation.add_gate(gate)
	workspace.add_child(gate)

# subclasses
