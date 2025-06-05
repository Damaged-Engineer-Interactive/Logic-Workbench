# The name of the Class
class_name CachedConnection
# The class this class extends
extends Object
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants

# @export variables

# public variables
## The ID of the connection
var id: int

#region Output
## The ID of the first Gate
var from_gate: CachedGate

## The Port of the first gate
var from_port: int = -1
#endregion

#region Input
## The ID of the second Gate
var to_gate: CachedGate

## The Port of the second gate
var to_port: int = -1
#endregion

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
