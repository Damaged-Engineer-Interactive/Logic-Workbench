# The name of the Class
class_name Instruction
# The class this class extends
extends Resource
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
## The Keyword that will be replaced by the value
@export var keyword: String = ""

## The Description of the Instruction
@export var description: String = ""

## The Value of the Instruction
@export var value: Value = null

## The Categorie of the Instruction
@export var categorie: String = "Unknown Categorie"

# private variables

# @onready variables

# optional built-in _init() function
func _init(kwd: String = "unknown", val := Value.new(), desc: String = "", cat: String = "") -> void:
	keyword = kwd
	description = desc
	value = val
	categorie = cat if !cat.is_empty() else categorie

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions

# subclasses
 
