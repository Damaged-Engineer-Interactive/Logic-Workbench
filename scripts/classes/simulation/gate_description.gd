# The name of the Class
class_name GateDescription
# The class this class extends
extends Object
# Docstring
## Basically the "blueprint" for a gate

# Signals

# Enums

# Constants

# @export variables

# public variables
## The Name of the Gate
var name: String

## The Type of the Gate
var type: String

## The group, that this gate is part of
var group: String

## The Size of the Gate
var size: Vector2i

## The Color of the Gate
var color: Color

#region AT RUNTIME
## The ID of the Gate
var id: String

## The Position of the Gate[br]
## x, y, layer
var position: Vector3i

#endregion

var inputs: Array[PinDescription]
var outputs: Array[PinDescription]
var buttons: Array[ButtonDescription]

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
