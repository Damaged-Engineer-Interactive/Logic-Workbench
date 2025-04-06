# The name of the Class
class_name VirtualClient
# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Holds information about a specific VirtualData client.

# Signals

# Enums

# Constants

# @export variables

# public variables
var type := VirtualHost.Types.UNKNOWN

var id: int = -1
var descriptor: String = "<unknown>"

# private variables

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Returns the size of the VirtualClient
func get_size() -> int:
	return -1

## Read a value at a specific address
func read(_address: Array[Simulation.States]) -> Array[Simulation.States]:
	push_error("Not implemented!")
	return []

## Write a value at a specific address
func write(_address: Array[Simulation.States], _value: Array[Simulation.States]) -> void:
	push_error("Not implemented!")
	return


## read every value of the whole client
func read_raw() -> Array[Array]:
	push_error("Not implemented!")
	return []

## writes a bunch of values to the whole client
func write_raw(_values: Array[Array]) -> void:
	push_error("Not implemented!")
	return

# private functions

# subclasses
