# The name of the Class

# The class this class extends
extends VBoxContainer
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants
#region Path
# Data Saving Related
const SAVE_DIR: String = "user://saved/"
#endregion

#region Extensions
## General Logic Workbench Data
const EXT_DATA: String = ".lw"
#endregion

# @export variables
# public variables

# private variables
#region dragging
var _drag_area: bool = false
var _drag_active: bool = false
var _drag_start := Vector2(0,0)
#endregion

var selectedProject: String = FileAccess.open("user://data/selected.lw", FileAccess.READ).get_as_text().trim_suffix("\n")

# @onready variables
@onready var projectList: ItemList = $Content/Columns/ProjectSubCategorie/LoadProject/Left/List
@onready var projectName: Label = $Content/Columns/ProjectSubCategorie/LoadProject/Right/Name
@onready var projectTags: Label = $Content/Columns/ProjectSubCategorie/LoadProject/Right/Tags
@onready var projectDescription: Label = $Content/Columns/ProjectSubCategorie/LoadProject/Right/Description
@onready var projecetLoadButton: Button = $Content/Columns/SubCategories/Projects/Load
@onready var projectCancelButton: Button = $Content/Columns/ProjectSubCategorie/LoadProject/Right/Cancel
@onready var projectsButton: Button = $Content/Columns/MainCategories/VBoxContainer/Projects

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	if DirAccess.dir_exists_absolute(SAVE_DIR + "Projects"):
		for project in DirAccess.get_directories_at(SAVE_DIR + "Projects"):
			projectList.add_item(project.get_basename())
	
	$Content/Columns/SubCategories.hide()
	$Content/Columns/SubCategories/Projects.hide()
	$Content/Columns/SubCategories/Settings.hide()
	$Content/Columns/SubCategories/Credits.hide()
	
	$Content/Columns/ProjectSubCategorie.hide()

# remaining built-in functions

# virtual functions to override
func _input(event: InputEvent) -> void:
	if event is InputEventMouse:
		if event is InputEventMouseButton:
			if _drag_area:
				_input_drag_click(event)
		
		if event is InputEventMouseMotion:
			if _drag_active:
				_input_drag_move(event)

# public functions

# private functions
#region dragging
func _top_bar_mouse_entered() -> void:
	_drag_area = true

func _top_bar_mouse_exited() -> void:
	_drag_area = false

func _input_drag_click(event: InputEventMouseButton):
	if event.button_index != MOUSE_BUTTON_LEFT:
		return
	
	if event.pressed:
		_drag_active = true
		_drag_start = get_local_mouse_position()
	else:
		_drag_active = false
		_drag_start = Vector2(0,0)

func _input_drag_move(_event: InputEventMouseMotion):
	var pos = Vector2(DisplayServer.window_get_position())
	pos -= _drag_start
	pos += get_global_mouse_position()
	
	DisplayServer.window_set_position(Vector2i(pos))
#endregion

# subclasses


func _close_button_pressed() -> void:
	var view: View = get_parent()
	var controller: ViewController = view.controller
	controller.quit()


func _projects_toggled(state: bool) -> void:
	print("projects : %s" % str(state))
	$Content/Columns/SubCategories/Projects.visible = state
	$Content/Columns/SubCategories.visible = false
	if state:
		$Content/Columns/MainCategories/VBoxContainer/Settings.button_pressed = false
		$Content/Columns/MainCategories/VBoxContainer/Credits.button_pressed = false
		$Content/Columns/SubCategories.visible = true


func _settings_toggled(state: bool) -> void:
	print("settings : %s" % str(state))
	$Content/Columns/SubCategories/Settings.visible = state
	$Content/Columns/SubCategories.visible = false
	if state:
		$Content/Columns/MainCategories/VBoxContainer/Projects.button_pressed = false
		$Content/Columns/MainCategories/VBoxContainer/Credits.button_pressed = false
		$Content/Columns/SubCategories.visible = true


func _credits_toggled(state: bool) -> void:
	print("credits : %s" % str(state))
	$Content/Columns/SubCategories/Credits.visible = state
	$Content/Columns/SubCategories.visible = false
	if state:
		$Content/Columns/MainCategories/VBoxContainer/Settings.button_pressed = false
		$Content/Columns/MainCategories/VBoxContainer/Projects.button_pressed = false
		$Content/Columns/SubCategories.visible = true


func _home_pressed() -> void:
	var view: View = get_parent()
	var controller: ViewController = view.controller
	controller.quit()


func _text_meta_clicked(meta: Variant) -> void:
	print(meta)
	OS.shell_open(str(meta))

func _quit_pressed() -> void:
	var view: View = get_parent()
	var controller: ViewController = view.controller
	controller.quit()

func _projects_load_toggled(state: bool) -> void:
	$Content/Columns/ProjectSubCategorie.visible = state

func _projects_sub_load_button_down() -> void:
	var view: View = get_parent()
	var controller: ViewController = view.controller
	print(selectedProject)
	FileAccess.open("user://data/selected.lw", FileAccess.WRITE).store_string(selectedProject)
	controller.switch_view("workspace")

func _list_item_clicked(index: int, _at_position: Vector2, _mouse_button_index: int) -> void:
	var projects = DirAccess.get_directories_at(SAVE_DIR + "Projects")
	for project in range(projects.size()):
		var projectData = FileAccess.open(SAVE_DIR + "Projects/" + projects[project] + "/info" + EXT_DATA, FileAccess.READ)
		if projectList.get_item_text(index) == projectData.get_line():
			projectName.text = "Name : " + projectList.get_item_text(index)
			projectTags.text = "Tags : " + projectData.get_line()
			projectDescription.text = "Description : " + projectData.get_line()
	selectedProject = projectList.get_item_text(index)

func _cancel_toggled(state: bool) -> void:
	if state:
		projecetLoadButton.button_pressed = false
		projectsButton.button_pressed = false
		projectCancelButton.button_pressed = false
		$Content/Columns/ProjectSubCategorie.visible = false
		$Content/Columns/SubCategories.visible = false
