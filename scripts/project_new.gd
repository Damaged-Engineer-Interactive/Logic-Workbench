extends PanelContainer



func _main_menu_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/main.tscn")


func _create_pressed() -> void:
	var project: Project = Project.new()
	if not (%Name.text or %Description.text):
		printerr("Name cant be empty")
		return
	
	project.name = %Name.text
	project.description = %Description.text
	Global.available_projects[project.name] = project
	Global.load_project(project.name)
