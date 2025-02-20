# The name of the Class
class_name Project
# The class this class extends
extends Node
# Docstring
## Data of a Project
## 
## Holds every propertie of a Project

# Signals

# Enums

# Constants

# @export variables

# public variables
## The ID of the Project
var id: String = ""

## The Name of the Project
var nme: String = ""

## The Tags of the Project
var tags: PackedStringArray = []

## The Description of the Project
var description: String = ""

# private variables
# The origin of the File
var _origin: String = ""

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
func load_from_path(path: String) -> Project:
	if not FileAccess.file_exists(path):
		return null
	
	var buffer: PackedByteArray = FileAccess.get_file_as_bytes(path)
	buffer = buffer.slice(0,5) # Should match ViewController.FILE_DAT + ViewController.FILE_CAT
	if not buffer.hex_encode().to_upper() == ViewController.FILE_DAT + ViewController.FILE_CAT:
		return null
	
	# File correct, keep loading
	var separator = int(ViewController.FILE_CAT)
	
	buffer = FileAccess.get_file_as_bytes(path).slice(5) # skip the identifier
	var file_usage_idx: int = buffer.find(separator) # next separator, should mark the end of the file usage
	var proj_id_idx: int = buffer.find(separator, file_usage_idx + 1) # next separator, should mark the end of the project id
	var proj_name_idx: int = buffer.find(separator, proj_id_idx + 1) # next separator, should mark the end of the project name
	var proj_tag_idx: int = buffer.find(separator, proj_name_idx + 1) # next separator, should mark the end of the project tags
	var proj_desc_idx: int = buffer.find(separator, proj_tag_idx + 1) # last separator, should mark the end of the project description
	
	var file_usage: String = buffer.slice(0, file_usage_idx).get_string_from_utf8()
	if file_usage != "PROJECT":
		return null
	
	id = buffer.slice(file_usage_idx + 1, proj_id_idx).get_string_from_utf8()
	nme = buffer.slice(proj_id_idx + 1, proj_name_idx).get_string_from_utf8()
	tags = buffer.slice(proj_name_idx + 1, proj_tag_idx).get_string_from_utf8().split(",")
	description = buffer.slice(proj_tag_idx + 1, proj_desc_idx).get_string_from_utf8()
	
	_origin = path
	
	name = id
	
	return self

# private functions

# subclasses
