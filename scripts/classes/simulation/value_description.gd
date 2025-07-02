# The name of the Class
class_name ValueDescription
# The class this class extends
extends Resource
# Docstring
## Placeholder Wrapper for every value used when building the circuit in the visualiser
## 
## Long desciption goes here

# Signals

# Enums

# @export variables

# public variables
## The Size of the Value[br]
## [code]-1[/code] signals an unknown size
var size: int = -1

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions
func _init(_size: int) -> void:
	size = _size

# virtual functions to override

# public functions
func copy() -> ValueDescription:
	return ValueDescription.new(size)

func save() -> Dictionary:
	return {"size": size}

static func load(from: Dictionary) -> ValueDescription:
	return ValueDescription.new(from["size"])

# private functions

# subclasses
