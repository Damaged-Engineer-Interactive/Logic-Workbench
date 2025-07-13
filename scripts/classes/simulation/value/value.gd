class_name Value
extends RefCounted

var size: int

func _init(_size: int) -> void:
	size = _size

func get_bit_value(_offset: int) -> int:
	push_error("Must be implemented by sub-class!")
	return 0

func get_bit_tri(_offset: int) -> int:
	push_error("Must be implemented by sub-class!")
	return 0

func get_range_value(_offset: int, _size: int) -> int:
	push_error("Must be implemented by sub-class!")
	return 0

func get_range_tri(_offset: int, _size: int) -> int:
	push_error("Must be implemented by sub-class!")
	return 0

func from(from: Value) -> void:
	push_error("Must be implemented by sub-class!")
	return

func low() -> void:
	push_error("Must be implemented by sub-class!")
	return

func high() -> void:
	push_error("Must be implemented by sub-class!")
	return

func tri() -> void:
	push_error("Must be implemented by sub-class!")
	return

#region Arithmetic

func arithmetic_and(_a: Value, _b: Value) -> void:
	push_error("Must be implemented by sub-class!")
	return

func arithmetic_nand(_a: Value, _b: Value) -> void:
	push_error("Must be implemented by sub-class!")
	return

func arithmetic_or(_a: Value, _b: Value) -> void:
	push_error("Must be implemented by sub-class!")
	return

func arithmetic_nor(_a: Value, _b: Value) -> void:
	push_error("Must be implemented by sub-class!")
	return

func arithmetic_not(_with: Value) -> void:
	push_error("Must be implemented by sub-class!")
	return

func arithmetic_xor(_a: Value, _b: Value) -> void:
	push_error("Must be implemented by sub-class!")
	return

func arithmetic_xnor(_a: Value, _b: Value) -> void:
	push_error("Must be implemented by sub-class!")
	return

#endregion
