# The name of the Class
class_name CachedGate
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
## The ID of the Gate
var id: String

## The type of the Gate
var type: String

## The Rank of the Gate (simulation order, low to high)
var rank: int

## If this Gate has a static output
var is_static: bool

## If this Gate is a constant
var is_const: bool


## The Input States of the Gate
var inputs: Array[Value]
## The Output States of the Gate
var outputs: Array[Value]

# private variables

# @onready variables

# optional built-in _init() function
static func from_description(from: GateDescription) -> CachedGate:
	var gate: CachedGate = CachedGate.new()
	gate.type = from.type
	
	gate.is_const = true if from.type == "CONST" else false
	gate.is_static = gate.is_const
	
	gate.inputs.resize(from.inputs.size())
	gate.outputs.resize(from.outputs.size())
	return gate

# optional built-in _enter_tree() function

# optional built-in _ready() function

# remaining built-in functions

# virtual functions to override

# public functions

# private functions

# subclasses
