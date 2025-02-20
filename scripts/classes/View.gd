# The name of the Class
class_name View
# The class this class extends
extends CenterContainer
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals
signal initialized

signal available

# Enums

# Constants

# @export variables
## Reference 
@export var controller: ViewController = null

## The ID of the View
@export var id: String = "<unknown>":
	set(value):
		id = value
		name = id

## The original path of the Scene
@export var path: String = "<unknown>":
	set(value):
		path = value
		if path == "<unknown":
			id = path
		id = path.get_basename().split("/")[-1] # The Filename

## If the View should be able to process even when it is not displayed
@export var background_process: bool = false

## If the View is currently active
@export var is_active: bool = false:
	set(value):
		is_active = value
		visible = value
		if get_child(0):
			if get_child(0).has_method(&"active"):
				get_child(0).active(value) # Let the Scene know if it is active or not
		
		if not background_process:
			get_child(0).set_process(value)

# public variables
## If the View is currently loaded
var is_loaded: bool = false

## If the View is available
var is_available: bool = false

# private variables
## The loaded scebe
var _scene: PackedScene = null

## If the scene is currently loading
var _is_loading: bool = false

# @onready variables

# optional built-in _init() function
func _init(_path: String = "<unknown>") -> void:
	path = _path
	size_flags_horizontal = Container.SIZE_EXPAND_FILL
	size_flags_vertical = Container.SIZE_EXPAND_FILL

# optional built-in _enter_tree() function
func _enter_tree() -> void:
	controller = get_parent()
	size = controller.size
	visible = is_active

# optional built-in _ready() function

# remaining built-in functions
func _process(_d: float) -> void:
	if _is_loading:
		match ResourceLoader.load_threaded_get_status(path):
			ResourceLoader.ThreadLoadStatus.THREAD_LOAD_IN_PROGRESS:
				# Loading still in progress
				pass
			ResourceLoader.ThreadLoadStatus.THREAD_LOAD_FAILED:
				# Loading Failed
				_is_loading = false
				is_loaded = false
				is_available = false
			ResourceLoader.ThreadLoadStatus.THREAD_LOAD_LOADED:
				# Loading done
				_is_loading = false
				is_loaded = true
				initialize() # call again

# virtual functions to override

# public functions
## Called when the view should initialize itself
func initialize() -> void:
	if is_available or _is_loading:
		return
	
	if not is_loaded:
		if FileAccess.file_exists(_get_save_path()):
			path = _get_save_path()
		elif not FileAccess.file_exists(path):
			return # No valid file
		
		var err = ResourceLoader.load_threaded_request(path)
		if err != OK:
			return
		
		_is_loading = true
		return # Returns early, will be called again
	
	# Resource expected to be loaded
	var resource: Resource = ResourceLoader.load_threaded_get(path)
	if resource is not PackedScene:
		return # Invalid type
	_scene = resource
	
	initialized.emit()

## Called when the View should instantiate it's scene
func instantiate() -> void:
	# Checking if the scene is loaded and available
	if not is_loaded or not _scene:
		return
	
	# Checking if the scene can instantiate
	if not _scene.can_instantiate():
		return
	
	# Instantiating the Scene
	var node: Control = _scene.instantiate(PackedScene.GEN_EDIT_STATE_DISABLED)
	
	# Adding the Scene as a child
	add_child(node)
	
	node.custom_minimum_size = controller.size
	
	# Setting variable
	is_available = true
	
	# Emiting signal
	available.emit()
	
	return

## Called when the View should save itself
func save() -> void:
	if controller:
		controller.check_save_dir()
	
	# Pack the scene
	var scene := PackedScene.new()
	scene.pack(get_child(0))
	
	# Save the scene
	var flags: int = 0
	flags += ResourceSaver.FLAG_OMIT_EDITOR_PROPERTIES
	flags += ResourceSaver.FLAG_BUNDLE_RESOURCES
	ResourceSaver.save(scene, _get_save_path(), flags)

## Called when the view should unload everything
func unload(_save: bool = true) -> void:
	# Should it save before unloading?
	if _save:
		save()
	
	# Set variables
	is_available = false
	is_loaded = false
	
	# remove the scene
	var child = get_child(0)
	child.set_process(false)
	remove_child(child)
	child.queue_free()
	
	# the rest does the ViewController
	return

# private functions
func _get_save_path() -> String:
	return ViewController.SAVE_DIR + id + ViewController.EXT_VIEW

# subclasses
