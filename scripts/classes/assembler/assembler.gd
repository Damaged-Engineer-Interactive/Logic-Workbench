# The name of the Class

# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants
const THEME_INSTRUCTION_BORDER: StyleBoxFlat = preload("res://styles/assembler/instruction_border.tres")
const THEME_INSTRUCTION_ICON: Texture2D = preload("res://assets/images/Edit-round-icon.png")

# @export variables
@export var isa: ISA

# public variables

# private variables
var _editing_in_progress: bool = false
var _editing_instruction: Instruction = null

# @onready variables
@onready var popup: PopupPanel = %InstructionPopup

@onready var edit: CodeEdit = %CodeEdit

@onready var instruction_container: VBoxContainer = %InstructionContainer

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	if isa:
		isa._ready()
	redraw_instructions()

# remaining built-in functions

# virtual functions to override

# public functions
func redraw_instructions() -> void:
	for node: Node in instruction_container.get_children():
		instruction_container.remove_child(node)
	
	for instruction: Instruction in isa.instructions.values():
		instruction_container.add_child(_create_instruction_button(instruction))

# private functions
func _create_instruction_button(instruction: Instruction) -> PanelContainer:
	var panel := PanelContainer.new()
	panel.name = instruction.keyword
	panel.set("theme_override_styles/panel", THEME_INSTRUCTION_BORDER)
	
	var container := HBoxContainer.new()
	panel.add_child(container)
	
	var label := Label.new()
	label.text = instruction.keyword
	label.set("theme_override_styles/normal", THEME_INSTRUCTION_BORDER)
	label.size_flags_horizontal = Control.SIZE_EXPAND_FILL
	container.add_child(label)
	
	var button := Button.new()
	button.flat = true
	button.icon = THEME_INSTRUCTION_ICON.duplicate()
	button.icon_alignment = HORIZONTAL_ALIGNMENT_CENTER
	button.expand_icon = true
	button.custom_minimum_size = Vector2(20, 20)
	button.pressed.connect(_edit_instruction_button.bind(instruction.keyword))
	container.add_child(button)
	
	return panel

func _edit_instruction_button(keyword: String = "") -> void:
	printt("instruction : ", keyword)
	_editing_instruction = isa.instructions.get(keyword, null)
	_editing_in_progress = true
	if _editing_instruction:
		popup.find_child("Title").text = "Edit Instruction"
		popup.find_child("Keyword").text = _editing_instruction.keyword
		popup.find_child("Value").text = str(_editing_instruction.value)
	else:
		popup.find_child("Title").text = "New Instruction"
		popup.find_child("Keyword").text = ""
		popup.find_child("Value").text = ""
	
	popup.popup_centered()

func _confirm_instruction_pressed() -> void:
	_editing_in_progress = false
	
	var keyword: String = popup.find_child("Keyword").text
	var value: Value = Value.from_string(Value.Sizes.BIT_1, popup.find_child("Value").text)


func _cancel_instruction_pressed() -> void:
	_editing_in_progress = false
	_editing_instruction = null
	popup.hide()

# subclasses
