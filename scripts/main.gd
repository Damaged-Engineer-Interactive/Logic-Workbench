extends PanelContainer

func _new_project_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/project_new.tscn")


func _load_project_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/project_load.tscn")


func _settings_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/settings.tscn")

func _credits_other_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/credits.tscn")

func _quit_pressed() -> void:
	get_tree().quit()
