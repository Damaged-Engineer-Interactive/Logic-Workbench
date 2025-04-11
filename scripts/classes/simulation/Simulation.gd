# The name of the Class
class_name Simulation
# The class this class extends
extends Node
# Docstring
## short description goes here 
## 
## Long desciption goes here

# Signals

# Enums

# Constants
const THEME_PANEL: StyleBoxFlat = preload("res://styles/simulation/panel.stylebox")
const THEME_TITLE: StyleBoxFlat = preload("res://styles/simulation/titlebar.stylebox")
const THEME_TITLE_SELECTED: StyleBoxFlat = preload("res://styles/simulation/titlebar_selected.stylebox")

# @export variables
@export var allow_simulate: bool = false

# public variables
var can_simulate: bool = false
static var thread_count: int = -1

var circuit: Circuit

# private variables
var _sim_counter: int = -1 # Frames until next simulate() call
var _is_simulating: bool = false # False : Dispatch new instance | True : Get Results

# @onready variables

# optional built-in _init() function

# optional built-in _enter_tree() function

# optional built-in _ready() function
func _ready() -> void:
	_prepare_simulation()

# remaining built-in functions
func _process(_d: float) -> void:
	if can_simulate:
		_sim_counter -= 1
		if _sim_counter == 0:
			simulate()
			_sim_counter += 20

# virtual functions to override

# public functions
static func get_thread_count() -> int:
	return thread_count

func get_gate(id: int) -> Gate:
	return circuit.get_gate(id)

func add_gate(gate: Gate) -> void:
	circuit.add_gate(gate)

func remove_gate(id: int) -> Gate:
	return circuit.remove_gate(id) # Returns the gate as reference, so it isn't immediately gone

func get_connection(con_id: String) -> Connection:
	return circuit.get_connection(con_id)

func add_connection(connection: Connection) -> void:
	circuit.add_connection(connection)

func remove_connection(id: int) -> Connection:
	return circuit.remove_connection(id) # Returns the gate as reference, so it isn't immediately gone

func simulate() -> void:
	if not allow_simulate:
		return
	circuit.simulate()
	_sim_counter = 1

# private functions
func _prepare_simulation() -> void:
	print("prepare_simulation")
	thread_count = floori(OS.get_processor_count() / 4.0)
	print("Logical Processors Available : %2s" % OS.get_processor_count())
	print("Logical Processors Usable    : %2s" % thread_count)
	
	circuit = Circuit.new()
	
	can_simulate = true
	_sim_counter = 60 # 60 Frames until first simulate() call

# subclasses
 
