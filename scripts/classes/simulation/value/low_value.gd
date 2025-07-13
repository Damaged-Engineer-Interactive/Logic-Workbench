class_name LowValue
extends Value

const RANGE_MASKS: Dictionary[int, int] = {
	 1: 0x0001,
	 2: 0x0003,
	 3: 0x0007,
	 4: 0x000F,
	 5: 0x001F,
	 6: 0x003F,
	 7: 0x007F,
	 8: 0x00FF,
	 9: 0x01FF,
	10: 0x03FF,
	11: 0x07FF,
	12: 0x0FFF,
	13: 0x1FFF,
	14: 0x3FFF,
	15: 0x7FFF,
	16: 0xFFFF,
}

const BIT_VALUE_MASK: int = 1
const BIT_TRI_MASK: int = 1 << 16

const STATE_LOW: int = 0
const STATE_HIGH: int = 0xFFFF
const STATE_TRI: int = 0xFFFF << 16

var _state: int

func _init(_size: int) -> void:
	size = _size
	_state = 0
	assert(_size > 0 and _size <= 16, "Invalid Size for LowValue")

func get_type() -> Value.TYPES:
	return Value.TYPES.LOW

func get_bit(offset: int) -> int:
	var value: int = get_bit_value(offset)
	var state: int = get_bit_tri(offset)
	return (state << 1) | value

func get_bit_value(offset: int) -> int:
	return (_state & (BIT_VALUE_MASK << offset)) >> offset

func get_bit_tri(offset: int) -> int:
	return (_state & (BIT_TRI_MASK << offset)) >> offset + 16

func set_bit(offset: int, value: int) -> void:
	set_bit_value(offset, value & 1)
	set_bit_tri(offset, (value >> 1) & 1)

func set_bit_value(offset: int, value: int) -> void:
	if value == 1:
		_state |= (1 << offset)
	else:
		_state &= ~(1 << offset)

func set_bit_tri(offset: int, value: int) -> void:
	if value == 1:
		_state |= (1 << (offset + 16))
	else:
		_state &= ~(1 << (offset + 16))

func get_range_value(_offset: int, _size: int) -> int:
	var mask: int = RANGE_MASKS.get(_size, 0) << _offset
	return (_state & mask) >> _offset

func get_range_tri(_offset: int, _size: int) -> int:
	var mask: int = RANGE_MASKS.get(_size, 0) << _offset + 16
	return (_state & mask) >> _offset + 16

func get_full_value() -> int:
	return _state & 0xFFFF

func get_full_tri() -> int:
	return (_state >> 16) & 0xFFFF

func set_full_value(value: int) -> void:
	_state = (_state & (0xFFFF << 16)) | (value & 0xFFFF)

func set_full_tri(value: int) -> void:
	_state = (_state & 0xFFFF) | ((value & 0xFFFF) << 16)

func from(_from: Value) -> void:
	_state = _from._state

func low() -> void:
	_state = STATE_LOW

func high() -> void:
	_state = STATE_HIGH

func tri() -> void:
	_state = STATE_TRI

#region Arithmetic

func arithmetic_and(a: Value, b: Value) -> void:
	set_full_value(a.get_full_value() & b.get_full_value())
	set_full_tri(a.get_full_tri() | b.get_full_tri())

func arithmetic_nand(a: Value, b: Value) -> void:
	set_full_value(~(a.get_full_value() & b.get_full_value()))
	set_full_tri(a.get_full_tri() | b.get_full_tri())

func arithmetic_or(a: Value, b: Value) -> void:
	set_full_value(a.get_full_value() | b.get_full_value())
	set_full_tri(a.get_full_tri() | b.get_full_tri())

func arithmetic_nor(a: Value, b: Value) -> void:
	set_full_value(~(a.get_full_value() | b.get_full_value()))
	set_full_tri(a.get_full_tri() | b.get_full_tri())

func arithmetic_not(with: Value) -> void:
	set_full_value(~(with.get_full_value()))
	set_full_tri(with.get_full_tri())

func arithmetic_xor(a: Value, b: Value) -> void:
	set_full_value(a.get_full_value() ^ b.get_full_value())
	set_full_tri(a.get_full_tri() | b.get_full_tri())

func arithmetic_xnor(a: Value, b: Value) -> void:
	set_full_value(~(a.get_full_value() | b.get_full_value()))
	set_full_tri(a.get_full_tri() | b.get_full_tri())

#endregion
