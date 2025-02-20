# The name of the Class
class_name Connection
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

# public variables
## The ID of the Connection
var id: int = 0

## The ID of the first Gate
var gate_in: int = -1

## The Port of the first gate
var port_in: int = -1

## The Type of the first gate
var type_in: Simulation.IO_TYPES = Simulation.IO_TYPES.UNKNOWN

## The ID of the second Gate
var gate_out: int = -1

## The Port of the second gate
var port_out: int = -1

## The Type of the second gate
var type_out: Simulation.IO_TYPES = Simulation.IO_TYPES.UNKNOWN

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
