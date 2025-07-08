extends Node

var active_project: Project = null
var available_projects: Dictionary[String, Project] = {}

func _init() -> void:
	await GateRegistry.loaded
	
	if not DirAccess.dir_exists_absolute("user://saved"):
		DirAccess.make_dir_recursive_absolute("user://saved")
	
	var dir = DirAccess.open("user://saved")
	if dir:
		dir.list_dir_begin()
		var file_name = dir.get_next()
		while file_name != "":
			if dir.current_is_dir():
				if FileAccess.file_exists("user://saved/%s/manifest.lwp" % file_name):
					print("Found Project [user://saved/%s/manifest.lwp]" % file_name)
					var project: Project = Project.load("user://saved/%s/manifest.lwp" % file_name)
					available_projects[project.name] = project
					print("Loaded Project [%s]" % project.name)
			file_name = dir.get_next()
	else:
		print("An error occurred when trying to access the path.")

func load_project(_name: String) -> void:
	get_tree().change_scene_to_file("res://scenes/workspace.tscn")
	active_project = available_projects[_name]
	GateRegistry.reset()
	for gate: Circuit in active_project.gates.values():
		GateRegistry.add_gate(gate)

func dir_remove_recursive(directory: String) -> void:
	for dir_name in DirAccess.get_directories_at(directory):
		dir_remove_recursive(directory.path_join(dir_name))
	for file_name in DirAccess.get_files_at(directory):
		DirAccess.remove_absolute(directory.path_join(file_name))
	DirAccess.remove_absolute(directory)

func format_int(value: int) -> String:
	var string = str(value)
	var result = ""
	var count = 0

	for i in range(string.length() - 1, -1, -1):
		result = string[i] + result
		count += 1
		if count % 3 == 0 and i != 0:
			result = " " + result

	return result
