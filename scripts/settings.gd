extends PanelContainer

func _settings_category(cat: int) -> void:
	%TabContainer.current_tab = cat

func _main_menu_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/main.tscn")
