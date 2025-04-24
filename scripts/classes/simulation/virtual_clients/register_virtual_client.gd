# The name of the Class
class_name RegisterVirtualClient
# The class this class extends
extends VirtualClient
# Docstring
## short description goes here 
## 
## Holds information about a specific VirtualData client.

# Signals

# Enums

# Constants

# @export variables

# public variables
var size: Value.Sizes

var data: Value

# private variables

# @onready variables

# optional built-in _init() function
func _init() -> void:
	type = VirtualHost.Types.REGISTER

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## (re-)initialise the Virtual Client, returns true if it succeeded
func initialise() -> bool:
	if not size:
		return false
	data = Value.new(size)
	return true

## Returns the size of the VirtualClient
func get_size() -> int:
	return size

## Read a value at a specific address
func read(_address: Value) -> Value:
	return data.copy()

## Write a value at a specific address
func write(_address: Value, value: Value) -> void:
	data = value.copy()
	return


## read every value of the whole client
func read_raw() -> Array[Value]:
	return [data.copy()]

## writes a bunch of values to the whole client
func write_raw(values: Array[Value]) -> void:
	data = values[0].copy()
	return

# private functions

# subclasses
