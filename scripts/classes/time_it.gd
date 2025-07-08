# time_it.gd
class_name TimeIt
extends RefCounted

var _timestamps: Dictionary[String, Array] = {}  # Dictionary<String, Array[PackedInt64Array]> -> [[start, stop]]

func register_start(identifier: String) -> void:
	_timestamps[identifier] = [Time.get_ticks_usec()]

func register_stop(identifier: String) -> void:
	_timestamps[identifier].append(Time.get_ticks_usec())

func result(clear: bool = true) -> void:
	print("TimeIt Results:")
	for id in _timestamps.keys():
		var time: Array = _timestamps[id]
		print("%s | %s - %s | %s" % [time[-1] - time[0], time[0], time[-1], id])
	if clear:
		_timestamps.clear()
