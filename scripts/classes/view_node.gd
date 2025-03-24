# The name of the Class
class_name ViewNode
# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables
## The ID of the ViewNode
@export var id: String = "<undefined>"

## The Owner of the ViewNode
@export var owner_id: String = "Global"

## If the View should process
@export var process: bool = true:
	set(value):
		process = value
		set_process(value)
		set_physics_process(value)

# public variables

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions

# subclasses
