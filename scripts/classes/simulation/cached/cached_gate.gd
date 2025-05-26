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
var id: int

## The type of the Gate
var type: String

## The Rank of the Gate (simulation order, low to high)
var rank: int

## If this Gate is a constant
var is_const: bool

## If this Gate has a static output
var is_static: bool

## How many ticks are needed to fully simulate it
var ticks: int

## The Input States of the Gate
var inputs: Array[Value]
## The Output States of the Gate
var outputs: Array[Value]

var mutex: Mutex

# private variables

# @onready variables

# optional built-in _init() function
static func from_description(from: GateDescription) -> CachedGate:
	var gate: CachedGate = CachedGate.new()
	gate.type = from.type
	gate.mutex = Mutex.new()
	
	gate.is_const = true if from.type == "CONST" else false
	gate.is_static = gate.is_const
	gate.ticks = from.ticks
	
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
