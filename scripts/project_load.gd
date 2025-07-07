extends PanelContainer

const THEME: Theme = preload("res://styles/ui/theme.tres")

func _ready() -> void:
	for project: Project in Global.available_projects.values():
		var container: PanelContainer = PanelContainer.new()
		container.size_flags_horizontal = Control.SIZE_EXPAND_FILL
		%Grid.add_child(container)
		container.theme = THEME
		var box: VBoxContainer = VBoxContainer.new()
		box.set(&"theme_override_constants/separation", 8)
		container.add_child(box)
		var label: Label = Label.new()
		label.name = "Title"
		label.text = project.name
		label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
		label.vertical_alignment = VERTICAL_ALIGNMENT_CENTER
		label.custom_minimum_size = Vector2(0, 35)
		box.add_child(label)
		var sep: HSeparator = HSeparator.new()
		sep.theme = THEME
		box.add_child(sep)
		label = Label.new()
		label.name = "Description"
		label.text = project.description
		label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
		label.vertical_alignment = VERTICAL_ALIGNMENT_CENTER
		label.autowrap_mode = TextServer.AUTOWRAP_WORD_SMART
		label.custom_minimum_size = Vector2(0, 35)
		box.add_child(label)
		sep = sep.duplicate()
		box.add_child(sep)
		var scroll: ScrollContainer = ScrollContainer.new()
		scroll.name = "Tags"
		scroll.custom_minimum_size = Vector2(0, 51)
		scroll.size_flags_horizontal = Control.SIZE_EXPAND_FILL
		scroll.vertical_scroll_mode = ScrollContainer.SCROLL_MODE_DISABLED
		var cont: HBoxContainer = HBoxContainer.new()
		cont.set(&"theme_override_constants/separation", 8)
		cont.alignment = BoxContainer.ALIGNMENT_CENTER
		cont.size_flags_horizontal = Control.SIZE_EXPAND_FILL
		cont.size_flags_vertical = Control.SIZE_EXPAND_FILL
		box.add_child(cont)
		for tag: String in project.tags:
			var panel: PanelContainer = PanelContainer.new()
			panel.name = tag
			panel.theme = THEME
			panel.theme_type_variation = "BorderedPanel"
			cont.add_child(panel)
			label = Label.new()
			label.text = tag
			label.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
			label.vertical_alignment = VERTICAL_ALIGNMENT_CENTER
			panel.add_child(label)
		var button: Button = Button.new()
		button.name = "Open"
		button.text = "Open Project"
		button.custom_minimum_size = Vector2(0, 35)
		button.theme = THEME
		box.add_child(button)
		button.pressed.connect(Global.load_project.bind(project.name))

func _main_menu_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/main.tscn")
