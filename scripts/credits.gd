extends PanelContainer


func _ajax_youtube_pressed() -> void:
	OS.shell_open("https://www.youtube.com/@ajax123z")


func _ajax_discord_pressed() -> void:
	OS.shell_open("https://discord.gg/gNtqTZXr4u")


func _main_menu_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/main.tscn")
