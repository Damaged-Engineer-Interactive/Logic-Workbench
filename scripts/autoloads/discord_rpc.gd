extends Node

func _ready():
	DiscordRPC.app_id = 1237469970208784394 # Application ID
	DiscordRPC.details = '"What should I do now?"'
	DiscordRPC.state = 'Doing something'
	DiscordRPC.large_image = "icon" # Image key from "Art Assets"
	DiscordRPC.start_timestamp = int(Time.get_unix_time_from_system()) # "02:46 elapsed"
	DiscordRPC.refresh() # Always refresh after changing the values!
