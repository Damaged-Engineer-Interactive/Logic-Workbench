# The name of the Class
class_name Project
# The class this class extends
extends Object
# Docstring
## Data of a Project
## 
## Holds every propertie of a Project

# Signals

# Enums

# Constants
const INVALID_CHARS: Array[String] = [":", "/", "\\", "?", "*", "\"", "|", "%", "<", ">"]
const INVALID_CHARS_PATTERN: String = r'[:\\/\\?\\*"\|%<>]'
const INVALID_CHARS_STRING: String = r': / \ ? * " | % < >'

# @export variables

# public variables
## The Name of the Project
var name: String = ""

## The Description of the Project
var description: String = ""

## The Tags of the Project
var tags: PackedStringArray = []

## The Gates of the Project
var gates: Dictionary[String, GateDescription] = {}

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
func save() -> void:
	if "NO SAVE" in tags and not OS.has_feature("editor"):
		printerr("[NO SAVE] Tag found! Save aborted.")
		return
	
	var path: String = "user://saved/" + name
	var file: FileAccess
	if not DirAccess.dir_exists_absolute(path):
		DirAccess.make_dir_recursive_absolute(path)
	
	file = FileAccess.open(path + "/manifest.lwp", FileAccess.WRITE)
	file.store_string("version_1\n")
	file.store_string("[DATA]\n")
	file.store_string('name: {name}\ndescription: {desc}\n'.format({"name": name, "desc": description.replace("\n", " ")}))
	file.store_string("[TAGS]\n")
	for tag: String in tags:
		file.store_line(tag)
	file.store_string("[CIRCUITS]\n")
	for gate: GateDescription in gates.values():
		file.store_line(gate.type)
	file.store_string("[END]")
	file.close()
	
	if DirAccess.dir_exists_absolute(path + "/circuits"):
		Global.dir_remove_recursive(path + "/circuits")
	
	for gate: Circuit in gates.values():
		gate.save(path + "/circuits") # load path : path + "/circuits/%s.lwc % type

static func load(path: String) -> Project: # only loads metadata & circuits. doesnt modify registry
	GateRegistry.reset()
	if not path.get_extension() == "lwp":
		printerr("File [%s] has an invalid extension! aborting load" % path)
		return
	if not FileAccess.file_exists(path):
		printerr("File [%s] does not exist! aborting load" % path)
		return
	
	var file := FileAccess.open(path, FileAccess.ModeFlags.READ)
	var res := Project.new()
	
	if not file.get_line() != "version_1\n":
		printerr("File [%s] has an invalid version! aborting load" % path)
		return
	if not file.get_line() != "[DATA]\n":
		printerr("File [%s] has an invalid format! aborting load" % path)
		return
	# data
	res.name = file.get_line().get_slice(": ",1)
	res.description = file.get_line().get_slice(": ",1)
	if not file.get_line() != "[TAGS]\n":
		printerr("File [%s] has an invalid format! aborting load" % path)
		return
	# tags
	var text: String = file.get_line()
	if text != "[CIRCUITS]":
		while true:
			res.tags.append(text)
			text = file.get_line()
			if text == "[CIRCUITS]":
				break
	# circuits
	text = file.get_line()
	Global.active_project = res
	if text != "[END]":
		while true:
			var gate: Circuit = Circuit.load(path.get_base_dir() + "/circuits/%s.lwc" % text)
			if not gate: continue
			gate = gate.to_description()
			res.gates[gate.type] = gate
			GateRegistry.add_gate(gate)
			text = file.get_line()
			if text == "[END]":
				break
	Global.active_project = null
	GateRegistry.reset()
	return res

# private functions

# subclasses
