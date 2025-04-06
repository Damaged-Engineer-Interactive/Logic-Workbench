# The name of the Class
class_name RomVirtualClient
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
var address_size := Simulation.Sizes.BIT_1
var data_size := Simulation.Sizes.BIT_1

var data: Dictionary[String, Array] = {}

# private variables

# @onready variables

# optional built-in _init() function
func _init() -> void:
	type = VirtualHost.Types.ROM

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions
## Returns the size of the VirtualClient
func get_size() -> int:
	return int(data_size) * ( 2 ** ( address_size + 1 ) ) # Data Bits * 2^(address_bits + 1)

## Read a value at a specific address
func read(address: Array[Simulation.States]) -> Array[Simulation.States]:
	var value: Array[Simulation.States] = data.get(str(address), [])
	if value == []:
		value.resize(int(data_size))
		value.fill(Simulation.States.UNKNOWN)
	return value


## read every value of the whole client
func read_raw() -> Array[Array]:
	var full_memory: Array[Array] = []
	full_memory.resize(2 ** int(address_size) + 1)
	
	for i in range(0, full_memory.size()):
		var address: Array[Simulation.States] = []
		address.resize(address_size)
		var bits: int = i
		for j in range(0, address_size + 1):
			address[j] = bits & 1
			bits >>= 1
		var value: Array[Simulation.States] = data.get(str(address), [])
		if value == []:
			value.resize(int(data_size))
			value.fill(Simulation.States.UNKNOWN)
		full_memory[i] = value
	return full_memory

## writes a bunch of values to the whole client
func write_raw(values: Array[Array]) -> void:
	for i in range(0, (2 ** int(address_size) + 1)):
		var address: Array[Simulation.States] = []
		address.resize(address_size)
		var bits: int = i
		for j in range(0, address_size + 1):
			address[j] = bits & 1
			bits >>= 1
		data[str(address)] = values[i]
	return

# private functions

# subclasses
