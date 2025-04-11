# The name of the Class
class_name RamVirtualClient
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
var address_size := Value.Sizes.BIT_1
var data_size := Value.Sizes.BIT_1

var data: Dictionary[String, Value] = {}

# private variables

# @onready variables

# optional built-in _init() function
func _init() -> void:
	type = VirtualHost.Types.RAM

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Returns the size of the VirtualClient
func get_size() -> int:
	return int(data_size) * ( 2 ** ( address_size + 1 ) ) # Data Bits * 2^(address_bits + 1)

## Read a value at a specific address
func read(address: Value) -> Value:
	var value: Value = data.get(str(address))
	if value == null:
		value = Value.new(data_size)
	return value.copy()

## Write a value at a specific address
func write(address: Value, value: Value) -> void:
	data[str(address)] = value.copy()
	return


## read every value of the whole client
func read_raw() -> Array[Value]:
	return data.values()

## writes a bunch of values to the whole client
func write_raw(values: Array[Value]) -> void:
	for i in range(0, (2 ** int(address_size) + 1)):
		var address: Value = Value.from_int(address_size, i)
		data[str(address)] = values[i].copy()
	return

# private functions

# subclasses
