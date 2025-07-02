extends Node

var active_project: Project = null
var available_projects: Array[Project] = []

func _init() -> void:
	active_project = Project.new()
	active_project.name = "DEV_EMPTY"
	active_project.description = "DEV ONLY Project. Default project when running only the workspace"
	active_project.tags = PackedStringArray(["DEV", "NO SAVE"])
