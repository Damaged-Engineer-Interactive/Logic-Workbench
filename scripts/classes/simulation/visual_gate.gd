class_name VisualGate
extends GraphNode

const STYLE_PANEL: StyleBoxFlat = preload("res://styles/panel.tres")
const STYLE_TITLE_N: StyleBoxFlat = preload("res://styles/title_normal.tres")  # Normal
const STYLE_TITLE_S: StyleBoxFlat = preload("res://styles/title_selected.tres") # Selected
const STYLE_SLOT: StyleBoxFlat = preload("res://styles/slot.tres") 
const STYLE_SLOT_L: StyleBoxFlat = preload("res://styles/slot_left.tres")  # Left Label
const STYLE_SLOT_R: StyleBoxFlat = preload("res://styles/slot_right.tres") # Right Label

const SNAP: float = 25.0
const MIN: Vector2 = Vector2(250.0, 125.0)

var from: GateDescription

func _init(_from: GateDescription) -> void:
	from = _from
	
	# set constant & styles
	title = from.name + " @ " + from.id
	name = from.id
	position_offset = from.position
	
	var color: Color = from.color
	set("theme_override_constants/separation", 10)
	
	var style: StyleBoxFlat
	style = STYLE_PANEL.duplicate()
	style.border_color = color
	style.bg_color = Color(color)
	style.bg_color.a = 0.1
	set("theme_override_styles/panel", style)
	set("theme_override_styles/panel_selected", style)
	style = STYLE_TITLE_N.duplicate()
	style.border_color = color
	set("theme_override_styles/titlebar", style)
	style = STYLE_TITLE_S.duplicate()
	style.border_color = color
	style.bg_color = color
	set("theme_override_styles/titlebar_selected", style)
	style = STYLE_SLOT.duplicate()
	style.border_color = color
	set("theme_override_styles/slot", style)
	
	var style_left: StyleBoxFlat = STYLE_SLOT_L.duplicate()
	style_left.border_color = color
	var style_right: StyleBoxFlat = STYLE_SLOT_R.duplicate()
	style_right.border_color = color
	
	
	# create slots
	var count: int = max(from.inputs.size(), from.outputs.size())
	for i in range(0, count):
		var slot := HBoxContainer.new()
		slot.name = "SLOT_" + str(i)
		
		if i < from.inputs.size(): # slot exists, make label
			var label := Label.new()
			label.name = "LEFT"
			label.text = from.inputs[i].name + " | " + str(from.inputs[i].state.size)
			label.set("theme_override_styles/normal", style_left)
			label.custom_minimum_size = Vector2(0.0, 35.0)
			label.size_flags_horizontal = Control.SIZE_EXPAND_FILL
			label.horizontal_alignment = HORIZONTAL_ALIGNMENT_LEFT
			label.vertical_alignment = VERTICAL_ALIGNMENT_CENTER
			slot.add_child(label)
		else: # slot doesnt exist, make spacer
			var control := Control.new()
			control.name = "LEFT"
			control.custom_minimum_size = Vector2(0.0, 35.0)
			control.size_flags_horizontal = Control.SIZE_EXPAND_FILL
			slot.add_child(control)
		
		# Middle Spacer
		if true:
			var control := Control.new()
			control.name = "CENTER"
			control.custom_minimum_size = Vector2(0.0, 35.0)
			control.size_flags_horizontal = Control.SIZE_EXPAND_FILL
			slot.add_child(control)
		
		if i < from.outputs.size(): # slot exists, make label
			var label := Label.new()
			label.name = "RIGHT"
			label.text = str(from.outputs[i].state.size) + " | " + from.outputs[i].name
			label.set("theme_override_styles/normal", style_right)
			label.custom_minimum_size = Vector2(0.0, 35.0)
			label.size_flags_horizontal = Control.SIZE_EXPAND_FILL
			label.horizontal_alignment = HORIZONTAL_ALIGNMENT_RIGHT
			label.vertical_alignment = VERTICAL_ALIGNMENT_CENTER
			slot.add_child(label)
		else: # slot doesnt exist, make spacer
			var control := Control.new()
			control.name = "RIGHT"
			control.custom_minimum_size = Vector2(0.0, 35.0)
			control.size_flags_horizontal = Control.SIZE_EXPAND_FILL
			slot.add_child(control)
		
		add_child(slot)
		set_slot_draw_stylebox(i, false)
		if i < from.inputs.size(): # slot exists
			set_slot_enabled_left(i, true)
			set_slot_color_left(i, color)
			set_slot_type_left(i, from.inputs[i].state.size)
		if i < from.outputs.size(): # slot exists
			set_slot_enabled_right(i, true)
			set_slot_color_right(i, color)
			set_slot_type_right(i, from.outputs[i].state.size)
		
	if from.type in Connection.IO_TYPES:
		var label: Label = Label.new()
		label.name = "LABEL_IO"
		label.text = from.data["name"]
		label.vertical_alignment = VERTICAL_ALIGNMENT_CENTER
		label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
		label.custom_minimum_size = Vector2(0, 35)
		add_child(label)


func _ready() -> void:
	custom_minimum_size.x = ceilf(size.x / SNAP) * SNAP
	custom_minimum_size.y = ceilf(size.y / (SNAP * 2.0)) * (SNAP * 2.0)
	custom_minimum_size = MIN.max(custom_minimum_size)
