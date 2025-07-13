class_name ValueHelper
extends Object

## Make a new value, chooses the correct type for the size
static func value(size: int) -> Value:
	if size <= 0:
		return null
	elif size <= 16:
		return LowValue.new(size)
	return null
