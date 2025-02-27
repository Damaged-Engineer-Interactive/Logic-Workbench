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
	var gate: Gate = $Content/HBoxContainer/Workspace/GATE_AND_0
	var simulation: Simulation = $Content/HBoxContainer/Workspace/Simulation
	simulation.add_gate(gate)

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

# subclasses
