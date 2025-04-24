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
## (re-)initialise the Virtual Client, returns true if it succeeded
func initialise() -> bool:
	push_error("Not implemented!")
	return false

## Returns the size of the VirtualClient
func get_size() -> int:
	return -1

## Read a value
func read() -> Value:
	push_error("Not implemented!")
	return null

## Write a value
func write(_value: Value) -> void:
	push_error("Not implemented!")
	return


## read every value of the whole client
func read_raw() -> Array[Value]:
	push_error("Not implemented!")
	return []

## writes a bunch of values to the whole client
func write_raw(_values: Array[Value]) -> void:
	push_error("Not implemented!")
	return

# private functions

# subclasses
