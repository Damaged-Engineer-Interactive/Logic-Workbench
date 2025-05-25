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
#region Path
# Data Saving Related
const SAVE_DIR: String = "user://saved/"
#endregion

#region Extensions
## General Logic Workbench Data
const EXT_CIRCUIT: String = ".lwc"

const EXT_DATA: String = ".lw"
#endregion

# @export variables

# public variables

# private variables
#region dragging
var _drag_area: bool = false
var _drag_active: bool = false
var _drag_start := Vector2(0,0)
#endregion

#region other
var current_project: String
var current_ids = []
var gate_position: PackedVector2Array = PackedVector2Array([Vector2i(0,0)])
var position_ids: PackedStringArray
#endregion

# @onready variables
@onready var simulation: Simulation = %Simulation
@onready var workspace: GraphEdit = %Workspace

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _process(_delta: float) -> void:
	if is_visible_in_tree():
		current_project = FileAccess.open("user://data/selected.lw", FileAccess.READ).get_as_text().trim_suffix("\n")
		load_project(current_project)
		set_process(false)

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
	
	DisplayServer.window_set_position(Vector2i(pos))
#endregion

func _home_button_pressed() -> void:
	print(save_project(current_project))
	var view: View = get_parent()
	var controller: ViewController = view.controller
	controller.quit()

func _workspace_connection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	var gate_in: int = from_node.split("_")[-1] as int
	var gate_out: int = to_node.split("_")[-1] as int
	
	var connection := Connection.new()
	connection.gate_in = gate_in
	connection.port_in = from_port
	connection.type_in = Gate.IOTypes.INPUT
	connection.size_in = Value.Sizes.BIT_1
	
	connection.gate_out = gate_out
	connection.port_out = to_port
	connection.type_out = Gate.IOTypes.OUTPUT
	connection.size_out = Value.Sizes.BIT_1
	
	simulation.add_connection(connection)
	workspace.connect_node(from_node, from_port, to_node, to_port)


func _workspace_disconnection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	var con_id: String = Connection.make_con_id(from_node, from_port, to_node, to_port)
	var connection: Connection = simulation.get_connection(con_id)
	simulation.remove_connection(connection.id)
	workspace.disconnect_node(from_node, from_port, to_node, to_port)

func _workspace_delete_nodes_request(nodes: Array[StringName]) -> void:
	for node_name: StringName in nodes:
		var node: Gate = workspace.find_child(node_name, false, false) as Gate
		var id = node_name.split("_")[-1]
		
		simulation.remove_gate(int(id))
		workspace.remove_child(node)
		workspace.queue_redraw()

func save_project(selected_project: String) -> String:
	var workspace_exists = false
	var extra_exists = false
	if FileAccess.file_exists(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT):
		workspace_exists = true
	if FileAccess.file_exists(SAVE_DIR + "Projects/" + selected_project + "/extra" + EXT_DATA):
		extra_exists = true
	
	print(workspace_exists, " ", extra_exists)
	
	if workspace_exists and extra_exists:
		return "Saved " + selected_project + " Successfully!"
	elif workspace_exists:
		FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/extra" + EXT_DATA, FileAccess.WRITE).store_string("")
		return "Saved " + selected_project + ", workspace successfully but failed to save extra."
	elif extra_exists:
		FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT, FileAccess.WRITE).store_string("")
		return "Saved " + selected_project + ", extra successfully but failed to save workspace."
	else:
		FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT, FileAccess.WRITE).store_string("")
		FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/exta" + EXT_DATA, FileAccess.WRITE).store_string("")
		return "Couldn't save " + selected_project + ", both workspace and extra files are missing."
	

func load_project(selected_project: String) -> String:
	if selected_project == "":
		return "Pick a project 🦆"
	
	load_extra_data(selected_project)
	load_gates(selected_project)
	load_connections(selected_project)
	return "Loaded: " + selected_project + " Successfully!"

func load_gates(selected_project: String) -> void:
	if FileAccess.file_exists(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT):
		if FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT, FileAccess.READ).get_length() != 0:
			var projectData = FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT, FileAccess.READ)
			var lines = []
			while not projectData.eof_reached():
				lines.append(projectData.get_line())
			
			for logic_gate: String in lines:
				var gate: PackedStringArray = logic_gate.split(",")
				for i: String in gate:
					print(i)
					var gate_type = gate[0]
					print(gate_type)
					var gate_id = gate[1].trim_suffix(":")
					
					while gate_type.begins_with(" "):
						gate_type = gate_type.trim_prefix(" ")
					
					var is_int: String = gate_type.right(1)
					while is_int.is_valid_int():
						gate_type = gate_type.trim_suffix(is_int)
						is_int = gate_type.right(1)
					
					
					if gate_position != PackedVector2Array() && position_ids != PackedStringArray([""]):
						gate_data(Circuit.GATE_TYPES.get(gate_type), int(gate_id), gate_type, gate_position[position_ids.find(gate_id)])
		else:
			FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT, FileAccess.WRITE).store_string("")

func load_connections(selected_project: String) -> void:
	if FileAccess.file_exists(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT):
		if FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT, FileAccess.READ).get_length() != 0:
			var projectData = FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT, FileAccess.READ)
			var lines: PackedStringArray = []
			var default_back: int = 1
			var lines_back: int = 1
			while not projectData.eof_reached():
				lines.append(projectData.get_line())
			
			for logic_gate: String in lines:
				var child_gate: String
				var parent_gate: String
				
				if logic_gate.begins_with(" "):
					child_gate = logic_gate
					# AND0:  - Parent
					#  OR01  - Child
					#  XOR12 - Child
					
					# Find the parent gate by checking for the line that's one indentation back.
					if lines[lines.find(logic_gate)-default_back].ends_with(":") == false:
						default_back = lines_back + 1
						lines_back = default_back
						parent_gate = lines[lines.find(logic_gate)-default_back]
						default_back = 1
					elif lines[lines.find(logic_gate)-default_back].ends_with(":") == true:
						default_back = 1
						lines_back = 1
						parent_gate = lines[lines.find(logic_gate)-default_back]
						print(parent_gate)
				
				while child_gate.begins_with(" "):
					child_gate = child_gate.trim_prefix(" ")
				
				var parent_type
				var parent_id
				var parent_port
				
				if parent_gate.length() != 0:
					var parent_details: PackedStringArray = parent_gate.split(",")
					parent_id = parent_details[1].trim_suffix(":")
					parent_port = parent_details[0].right(1)
					print(parent_port)
					parent_type = parent_details[0].erase(parent_details[0].length()-1)
					if parent_type.right(1).is_valid_int():
						parent_type = parent_type.erase(parent_type.length()-1)
					print(parent_type)
					while parent_type.begins_with(" "):
						parent_type = parent_type.trim_prefix(" ")
				
				var child_type
				var child_id = logic_gate.trim_suffix(":").right(1)
				var child_port
				
				if child_gate.length() != 0:
					var child_details: PackedStringArray = child_gate.split(",")
					print(child_details)
					if child_details[0].right(2).is_valid_int():
						child_port = child_details[0].right(2)
						child_type = child_details[0].erase(child_details[0].length()-2, 2)
					else:
						child_port = child_details[0].right(1)
						child_type = child_details[0].erase(child_details[0].length()-1)
					print(child_port)
				
				if child_type != null || parent_type != null:
					print("parent type: ", parent_type, " parent id: ", parent_id, " parent port: ", parent_port, " child type: ", child_type, " child id: ", child_id, " child port: ", child_port)
					_workspace_connection_request("GATE_" + parent_type + "_" + parent_id, int(parent_port), "GATE_" + child_type + "_" + child_id, int(child_port))
		else:
			FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/workspace" + EXT_CIRCUIT, FileAccess.WRITE).store_string("")

func gate_data(type: Circuit.GATE_TYPES, id: int, g_name: String, g_position: Vector2) -> void:
	if type == Circuit.GATE_TYPES.UNKNOWN:
		return
	
	if !current_ids.find(id) == -1:
		return
	
	var gate_type: Variant = Circuit.GATES.get(type)
	var gate: Gate = gate_type.new()
	gate.gate_position = g_position
	
	current_ids.push_back(id)
	gate.gate_id = id
	
	gate.gate_name = g_name
	
	simulation.add_gate(gate)
	workspace.add_child(gate)

func load_extra_data(selected_project: String) -> void:
	if FileAccess.file_exists(SAVE_DIR + "Projects/" + selected_project + "/extra" + EXT_DATA) and FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/extra" + EXT_DATA, FileAccess.READ).get_length() != 0:
		var projectData = FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/extra" + EXT_DATA, FileAccess.READ)
		var lines: PackedStringArray = []
		while not projectData.eof_reached():
			lines.append(projectData.get_line())
		
		print(lines)
		if lines != PackedStringArray([""]):
			for position_data: String in lines:
				var split_data: PackedStringArray = position_data.split(": ")
				print(split_data)
				var vector_x: int = 0
				var vector_y: int = 0
				vector_x = int(split_data[1].trim_prefix("(").trim_suffix(")").split(",")[0])*100
				vector_y = int(split_data[1].trim_prefix("(").trim_suffix(")").split(",")[1])*100
				position_ids.append(split_data[0])
				gate_position.append(Vector2i(vector_x, vector_y))
		else:
			FileAccess.open(SAVE_DIR + "Projects/" + selected_project + "/extra" + EXT_DATA, FileAccess.WRITE).store_string("")

func _gate_button_down(type: Circuit.GATE_TYPES) -> void:
	if type == Circuit.GATE_TYPES.UNKNOWN:
		return
	var gate_type: Variant = Circuit.GATES.get(type)
	var gate: Gate = gate_type.new()
	simulation.add_gate(gate)
	workspace.add_child(gate)
# subclasses
