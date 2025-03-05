# The name of the Class
class_name ViewController
# The class this class extends
extends Control
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals
## Internal Signal
signal _finished_loading

# Enums

# Constants
#region Path
## Location of the Views to load
const VIEW_DIR: String = "res://scenes/views"

## Location of the Packs to load
const PACK_DIR: String = "user://packs/"

## Data Saving Related
const SAVE_DIR: String = "user://saved/"

## Data Saving Related
const DATA_DIR: String = "user://data/"
#endregion

#region Extensions
## General Logic Workbench Data
const EXT_DATA: String = ".lw"

## Saved Logic Workbench View
const EXT_VIEW: String = ".lwv"

## Logic Workbench Circuit
const EXT_CIRCUIT: String = ".lwc"
#endregion

#region File
## Separator between Files (when shared)
const FILE_SEP: String = "92226282"

## Seperator between Categories
const FILE_CAT: String = "00"

## File Beginning for Data-Related stuff
const FILE_DAT: String = "35EDE72F"

## File Beginning for Simulation-Related stuff
const FILE_SIM: String = "C81049B1"

#endregion

# @export variables
## The available Views
## id : View
@export var views: Dictionary = {}

## The ViewNodes
## id : ViewNode
@export var nodes: Dictionary = {}

## The Projects
## id : Project
@export var projects: Dictionary = {}

@export_group("View", "view_")
## The "404" View ID
@export var view_404: String = "404"
## The first View
@export var view_index: String = "index"

# public variables
## The currently opened Project
var current_project: Project = null

# private variables
var _checked_save_dir: bool = false

var _current_view: View = null

var _waiting_to_finish: int = 0:
	set(value):
		_waiting_to_finish = value
		if value == 0:
			_finished_loading.emit()

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	check_save_dir(true)

	load_packs()
	
	load_projects()
	
	load_views()
	
	await _finished_loading
	
	switch_view(view_404) # Switch to 404
	
	switch_view(view_index) # Switch to index, if index is not found, 404 will stay

# remaining built-in functions

# virtual functions to override

# public functions
func check_save_dir(force: bool = false) -> void:
	if _checked_save_dir and not force:
		return # already checked
	DirAccess.make_dir_recursive_absolute(PACK_DIR)
	DirAccess.make_dir_recursive_absolute(VIEW_DIR)
	DirAccess.make_dir_recursive_absolute(DATA_DIR)
	_checked_save_dir = true

func load_packs() -> void:
	check_save_dir()
	var dir: DirAccess = DirAccess.open(PACK_DIR)
	if not dir:
		return # Error
	
	dir.list_dir_begin()
	var entry: String = dir.get_next()
	while entry != "":
		if !dir.current_is_dir():
			load_pack(PACK_DIR + "/" + entry)
		# skip directories
		entry = dir.get_next()
	dir.list_dir_end()


func load_pack(path: String) -> void:
	# Perform checks here
	ProjectSettings.load_resource_pack(path)

#region Project
## Adds a ViewNode
func add_project(proj: Project) -> void:
	projects[proj.id] = proj
	add_child(proj)

## Rmoves the ViewNode
func remove_project(proj: Project) -> void:
	projects.erase(proj.id)
	remove_child(proj)

## Returns the ViewNode
func return_project(id: String) -> Project:
	return projects.get(id, null) as Project

## Sets the currently active project
func set_active_project(proj: Project) -> void:
	current_project = proj

## Gets the currently active project
func get_active_project() -> Project:
	return current_project

## Loads all projects
func load_projects() -> void:
	check_save_dir()
	var dir := DirAccess.open(DATA_DIR)
	if not dir:
		return
	
	dir.list_dir_begin()
	var entry: String = dir.get_next()
	while entry != "":
		if dir.current_is_dir():
			if dir.file_exists(DATA_DIR + "/" + entry + "/project.lwd"):
				# Correct file structure, begin parsing the file
				import_project(DATA_DIR + "/" + entry)
#			load_views(path + "/" + entry) # Recursive Loading
		# skip non-directories
		entry = dir.get_next()
	dir.list_dir_end()

## Load / Import a specific project
func import_project(path: String) -> Project:
	var proj := Project.new()
	add_child(proj)
	proj.load_from_path(path + "/project.lwd")
	remove_child(proj)
	add_project(proj)
	return proj
#endregion

#region ViewNode
## Adds a ViewNode
func add_node(node: ViewNode) -> void:
	nodes[node.id] = node
	add_child(node)

## Rmoves the ViewNode
func remove_node(node: ViewNode) -> void:
	nodes.erase(node.id)
	remove_child(node)

## Returns teh ViewNode
func return_node(id: String) -> ViewNode:
	return nodes.get(id, null) as ViewNode
#endregion

#region View
func switch_view(id: String) -> void:
	if not views.has(id):
		printerr("View with id [%s] does not exist!" % id)
		return
	
	if _current_view:
		_current_view.is_active = false
		_current_view.process_mode = Node.PROCESS_MODE_DISABLED
		_current_view = null
	
	_current_view = views.get(id) as View
	_current_view.process_mode = Node.PROCESS_MODE_INHERIT
	_current_view.is_active = true

func load_views(path: String = VIEW_DIR) -> void:
	var dir := DirAccess.open(path)
	if not dir:
		return
	
	dir.list_dir_begin()
	var entry: String = dir.get_next()
	while entry != "":
		if dir.current_is_dir():
			load_views(path + "/" + entry) # Recursive Loading
		else:
			if entry.get_extension() == "scn":
				load_view(path + "/" + entry)
		entry = dir.get_next()
	dir.list_dir_end()

func load_view(path: String) -> void:
	# incrementing counter
	_waiting_to_finish += 1
	
	# Creating the View
	var view := View.new(path)
	
	# Add the View as a child so it can process
	add_child(view)
	
	# Initialize the View
	view.initialize()
	
	# Add the View to the Controller
	views[view.id] = view
	
	# waiting for the View to finish initialiting
	await view.initialized
	
	# instantiating the view
	view.instantiate()
	
	# done
	_waiting_to_finish -= 1
	
	view.process_mode = Node.PROCESS_MODE_DISABLED
	
	return

func unload_view(id: String) -> void:
	# Check if View exists
	if not views.has(id):
		return
	
	# unload view
	var view := views.get(id) as View
	view.unload()
	views.erase(id)
	remove_child(view)
	view.free()
	
	# Done
	return
#endregion

func quit() -> void:
	for id: String in views.keys():
		unload_view(id)
	
	for node: ViewNode in nodes.values():
		remove_node(node)
		node.free()
	
	get_tree().quit()

# private functions

# subclasses
