extends PanelContainer

func _new_project_pressed() -> void:
	pass # Replace with function body.


func _load_project_pressed() -> void:
	pass # Replace with function body.


func _settings_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/settings.tscn")

func _credits_other_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/credits.tscn")

func _quit_pressed() -> void:
	get_tree().quit()
