class_name ProjectAccessorVersion1
extends LokStorageAccessorVersion

func _retrieve_data(deps: Dictionary) -> Dictionary:
	var project: Project = deps.get(&"project") as Project
	
	
