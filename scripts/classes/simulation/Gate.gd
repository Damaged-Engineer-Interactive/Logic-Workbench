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
enum IOTypes {
	UNKNOWN = 0,
	INPUT = 1,
	OUTPUT = 2,
	BUS = 3
}

# Constants
## Normal [code]State : Color[/code] map. Used by Inputs and Outputs
const COLOR_NORMAL: Dictionary[Value.States, Color] = {
	Value.States.ERROR: Color.RED,
	Value.States.LOW: Color.DARK_BLUE,
	Value.States.HIGH: Color.BLUE,
	Value.States.UNKNOWN: Color.BLACK
}

## Bi [code]State : Color[/code] map. Used by Buses
const COLOR_BI: Dictionary[Value.States, Color] = {
	Value.States.ERROR: Color.RED,
	Value.States.LOW: Color.DARK_GREEN,
	Value.States.HIGH: Color.GREEN,
	Value.States.UNKNOWN: Color.WHITE
}

const STYLE_IN: StyleBoxFlat = preload("res://styles/simulation/input.stylebox")
const STYLE_OUT: StyleBoxFlat = preload("res://styles/simulation/output.stylebox")

# @export variables
#region Gate
@export_group("Gate", "gate_")
## The Name of the Gate
@export var gate_name: String = "":
	set(value):
		gate_name = value
		title = value

## The type of the Gate (used in the simulation)
## Limited to 255
@export var gate_type: int = 0

## The number of the Gate (used in the workspace and simulation to reference the gate)
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
		input_values.resize(value)
		input_names.resize(value)

## The Names of every Input
## Array[String]
@export var input_names: Array[String] = []

## The Values of every Input
## Array[Value]
@export var input_values: Array[Value] = []
#endregion

#region Output
@export_group("Output", "output_")
## The Number of outputs, the gate has
@export var output_amount: int = 0:
	set(value):
		output_amount = value
		output_values.resize(value)
		output_names.resize(value)

## The Names of every Output
## Array[String]
@export var output_names: Array[String] = []

## The Values of every Input
## Array[Value]
@export var output_values: Array[Value] = []
#endregion

#region Bus
@export_group("Bus", "bus_")
## The Number of buses, the gate has
@export var bus_amount: int = 0:
	set(value):
		bus_amount = value
		bus_values.resize(value)
		bus_names.resize(value)

## The Names of every Bus
## Array[String]
@export var bus_names: Array[String] = []

## The Values of every bus
## Array[Value]
@export var bus_values: Array[Value] = []
#endregion

var connections: Dictionary[String, Connection] = {}

# public variables
var buttons: Array[Dictionary] = []

# private variables
var _is_redraw_queued: bool = false
var _is_redraw_queued_full: bool = false

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function
func _enter_tree() -> void:
	custom_minimum_size = Vector2(200, 50)
	title = gate_name
	add_to_group(&"Gates")
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
func add_io(type: IOTypes, bit_size: Value.Sizes, nme: String) -> void:
	var value: Value = Value.new(bit_size)

	match type:
		IOTypes.INPUT:
			input_amount += 1
			input_values[-1] = value
			input_names[-1] = nme
		IOTypes.OUTPUT:
			output_amount += 1
			output_values[-1] = value
			output_names[-1] = nme
		IOTypes.BUS:
			bus_amount += 1
			bus_values[-1] = value
			bus_names[-1] = nme
	redraw(true)

## Adds a Button to the Gate
func add_button(id: String, label: String, callable: Callable) -> void:
	buttons.append({"id": id, "label": label, "callable": callable})
	redraw(true)

## Queue redraw() and prevent it from being called multiple times
func redraw(full: bool = false) -> void:
	#print("redraw(%s)" % str(full))
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
	#print("_redraw(%s) - Start" % str(full))
	_is_redraw_queued = false
	_is_redraw_queued_full = false
	if full:
		# Remove Childs
		for child in get_children():
			remove_child(child)
			child.queue_free()

		# Make new Childs
		var io: int = max(input_amount, output_amount)
		for i in range(0, io + buttons.size()):
			var slot: HBoxContainer = HBoxContainer.new()
			slot.name = "SLOT_" + str(i)
			add_child(slot)

			if input_amount > i: # input exists, render it
				var state: Value.States = input_values[i].get_state()
				var color: Color = COLOR_NORMAL.get(state, Color.BLACK)

				var label: Label = Label.new()
				label.name = "IN_%s_%s" % [str(i), input_names[i]]
				label.text = input_names[i]

				var style: StyleBoxFlat = STYLE_IN.duplicate()
				style.border_color = color
				label.set("theme_override_styles/normal", style)

				slot.add_child(label)

				set_slot_enabled_left(i, true)
				set_slot_color_left(i, color)
				set_slot_type_left(i, input_values[i].size)
			else:
				set_slot_enabled_left(i, false)
			
			# Spacer between Input and Button / Output
			var spacer: Label = Label.new()
			spacer.name = "SEP_%s_left" % str(i)
			spacer.size_flags_horizontal = Control.SIZE_EXPAND_FILL
			slot.add_child(spacer)
			
			if i >= io:
				var button: Button = _create_button(buttons[i - io])
				slot.add_child(button)
				# Spacer between Button and Output
				var spacer2: Label = Label.new()
				spacer2.name = "SEP_%s_right" % str(i)
				spacer2.size_flags_horizontal = Control.SIZE_EXPAND_FILL
				slot.add_child(spacer2)

			if output_amount > i: # output exists, render it
				var state: Value.States = output_values[i].get_state()
				var color: Color = COLOR_NORMAL.get(state, Color.BLACK)

				var label: Label = Label.new()
				label.name = "OUT_%s_%s" % [str(i), output_names[i]]
				label.text = output_names[i]

				var style: StyleBoxFlat = STYLE_OUT.duplicate()
				style.border_color = color
				label.set("theme_override_styles/normal", style)

				slot.add_child(label)

				set_slot_enabled_right(i, true)
				set_slot_color_right(i, color)
				set_slot_type_right(i, output_values[i].size)
			else:
				set_slot_enabled_right(i, false)
			
			if i == io - 1:
				var separator := HSeparator.new()
				separator.name = "SEP_IO_BTN"
				add_child(separator)
		self.queue_redraw()
		#print("_redraw(%s) - End" % str(full))
		return
	
	for i in range(0, input_amount): # loop for every input
		var state: Value.States = input_values[i].get_state()
		var color = COLOR_NORMAL.get(state, Color.BLACK)

		set_slot_color_left(i, color)
		var label: Label = get_node("SLOT_%s/IN_%s_%s" % [i, i, input_names[i]])
		var style: StyleBoxFlat = label.get("theme_override_styles/normal")
		style.border_color = color
		label.set("theme_override_styles/normal", style)
	
	for i in range(0, output_amount): # loop for every output
		var state: Value.States = output_values[i].get_state()
		var color = COLOR_NORMAL.get(state, Color.BLACK)

		set_slot_color_right(i, color)
		#var label: Label = find_child("OUT_%s_%s" % [str(i), output_names[i]])
		var label: Label = get_node("SLOT_%s/OUT_%s_%s" % [i, i, output_names[i]])
		var style: StyleBoxFlat = label.get("theme_override_styles/normal")
		style.border_color = color
		label.set("theme_override_styles/normal", style)
	
	return

func _create_button(data: Dictionary) -> Button:
	var button: Button = Button.new()
	button.text = data["label"]
	button.name = data["id"]
	button.button_down.connect(data["callable"])
	return button

# subclasses
