## The [LokINIAccessStrategy] class is responsible for
## implementing [code]INI[/code] data accessing.
## 
## This class inherits from the [LokAccessStrategy] in order to implement
## its [method _save_partition] and [method _load_partition] methods and,
## with that, provide saving and loading functionalities for
## [code]INI[/code] data. [br]
## [br]
## [b]Version[/b]: 1.0.0[br]
## [b]Author[/b]: [url]github.com/nadjiel[/url]
class_name LokINIAccessStrategy
extends LokAccessStrategy

## The [method _save_partition] method overrides its super counterpart
## [method LokAccessStrategy._save_partition] in order to provide [param data]
## saving in the [code]INI[/code] format. [br]
## When finished, this method returns a [Dictionary] with the data it
## saved. [br]
## To read more about the parameters of this method, see
## [method LokAccessStrategy._save_partition].
func _save_partition(
	partition_path: String,
	data: Dictionary,
	replace: bool = false
) -> Dictionary:
	var result: Dictionary = create_result()
	
	var error: Error = LokFileSystemUtil.create_file_if_not_exists(
		partition_path
	)
	
	# If partition wasn't created, cancel
	if error != Error.OK:
		result["status"] = error
		return result
	
	var load_result: Dictionary = {}
	
	if not replace:
		load_result = _load_partition(partition_path)
	
	# Merge previous and new datas
	result["data"] = data.merged(load_result.get("data", {}))
	
	var config := ConfigFile.new()
	for key: String in result["data"].keys():
		config.set_value("Data", key, result["data"])
	
	error = LokFileSystemUtil.write_or_create_file(
		partition_path, config.encode_to_text()
	)
	
	if error != Error.OK:
		result["status"] = error
	
	return result

## The [method _load_partition] method overrides its super counterpart
## [method LokAccessStrategy._load_partition] in order to provide data
## loading in the [code]INI[/code] format. [br]
## When finished, this method returns a [Dictionary] with the data it
## loaded. [br]
## To read more about the parameters of this method and the format of
## its return, see [method LokAccessStrategy._load_partition].
func _load_partition(
	partition_path: String
) -> Dictionary:
	var result: Dictionary = create_result()
	
	# Abort if partition doesn't exist
	if not LokFileSystemUtil.file_exists(partition_path):
		result["status"] = Error.ERR_FILE_NOT_FOUND
		return result
	
	var loaded_content: String = LokFileSystemUtil.read_file(
		partition_path
	)
	
	var config := ConfigFile.new()
	var err: int = config.parse(loaded_content)
	
	# Cancel if no data could be parsed
	if err != OK:
		result["status"] = err
	
	var loaded_data: Dictionary = {}
	for key: String in config.get_section_keys("Data"):
		loaded_data[key] = config.get_value("Data", key)
	
	result["data"] = loaded_data
	
	return result

# Returns a simple String representing this AccessStrategy
func _to_string() -> String:
	return "INI"
