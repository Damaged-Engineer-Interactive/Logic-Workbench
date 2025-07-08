extends PanelContainer

var registry_tree_type_map: Dictionary[TreeItem, String] = {} # Tree item to gate type map
var registry_type_tree_map: Dictionary[String, TreeItem] = {} # gate type to tree item map

var visualiser_tree_type_map: Dictionary[TreeItem, String] = {} # Tree item to gate type map

var selected: GateDescription = null
var registry_selected: String = ""

var circuit: Circuit
var simulation: Simulation

var clear_confirmation: bool = false

var gate_visual_map: Dictionary[StringName, VisualGate] = {}
var visual_gate_map: Dictionary[StringName, GateDescription] = {}

func _ready() -> void:
	%Workspace.snapping_distance = int(VisualGate.SNAP)
	_make_gate_tree()
	_clear_gate_data()
	circuit = Circuit.new()
	redraw_io()
	_clear_pressed()
	GateRegistry.updated.connect(_make_gate_tree)

#region VISUALISATION
func _make_gate_tree() -> void:
	print("Make Visualiser Tree")
	var tree: Tree = %GateTree
	tree.clear()
	visualiser_tree_type_map.clear()
	var root: TreeItem = tree.create_item()
	root.set_text(0, "GateRegistry")
	
	var grouped := GateRegistry._grouped
	
	for group: String in grouped.keys():
		print(group)
		var group_item: TreeItem = root.create_child()
		group_item.set_text(0, group)
		group_item.set_selectable(0, false)
		for gate: String in grouped[group].keys():
			print("- " + gate)
			var gate_item: TreeItem = group_item.create_child()
			gate_item.set_text(0, gate)
			if grouped[group][gate] == ["#"]:
				print("| #")
				var type: String = "%s.%s.#" % [group, gate]
				var version: GateDescription = GateRegistry.get_gate(type)
				gate_item.set_custom_bg_color(0, version.color, true)
				gate_item.set_custom_color(0, version.color)
				visualiser_tree_type_map[gate_item] = version.type
			else:
				for version: String in grouped[group][gate]:
					print("| - " + version)
					gate_item.set_selectable(0, false)
					var version_item: TreeItem = gate_item.create_child()
					version_item.set_text(0, version)
					var type: String = "%s.%s.%s" % [group, gate, version]
					var desc: GateDescription = GateRegistry.get_gate(type)
					version_item.set_custom_bg_color(0, desc.color, true)
					version_item.set_custom_color(0, desc.color)
					visualiser_tree_type_map[version_item] = type
	print()
	
	tree = %RegistryTree
	tree.clear()
	registry_tree_type_map.clear()
	root = tree.create_item()
	root.set_text(0, "GateRegistry")
	
	var item: TreeItem = root.create_child()
	item.set_text(0, "NEW GATE")
	registry_tree_type_map[item] = ".."
	registry_type_tree_map[".."] = item
	var color: Color = _make_random_color()
	%RegistryGateColor.color = color
	item.set_custom_bg_color(0, color, false)
	
	for group: String in grouped.keys():
		var group_item: TreeItem = root.create_child()
		group_item.set_text(0, group)
		group_item.set_selectable(0, false)
		for gate: String in grouped[group].keys():
			var gate_item: TreeItem = group_item.create_child()
			gate_item.set_text(0, gate)
			if grouped[group][gate] == ["#"]:
				var type: String = "%s.%s.#" % [group, gate]
				var version: GateDescription = GateRegistry.get_gate(type)
				gate_item.set_custom_bg_color(0, version.color, true)
				gate_item.set_custom_color(0, version.color)
				gate_item.set_selectable(0, true)
				registry_tree_type_map[gate_item] = version.type
				registry_type_tree_map[type] = gate_item
			else:
				for version: String in grouped[group][gate]:
					gate_item.set_selectable(0, false)
					var version_item: TreeItem = gate_item.create_child()
					version_item.set_text(0, version)
					var type: String = "%s.%s.%s" % [group, gate, version]
					var desc: GateDescription = GateRegistry.get_gate(type)
					version_item.set_custom_bg_color(0, desc.color, true)
					version_item.set_custom_color(0, desc.color)
					version_item.set_selectable(0, true)
					registry_tree_type_map[version_item] = type
					registry_type_tree_map[type] = version_item

func _gate_tree_item_selected() -> void:
	var selected_item: TreeItem = %GateTree.get_next_selected(null)
	selected = GateRegistry.get_gate(visualiser_tree_type_map.get(selected_item, ""))
	if selected == null:
		return
	
	%GateName.text = selected.name
	%GateType.text = selected.type
	
	_clear_gate_data()
	
	match selected.type:
		"IO.INPUT.#": # Universal Input
			%GateData.show()
			%GateData.get_node(^"BitSize").show()
			%GateData.get_node(^"InputName").show()
		"IO.OUTPUT.#": # Universal Output
			%GateData.show()
			%GateData.get_node(^"BitSize").show()
			%GateData.get_node(^"OutputName").show()
		"ROUTING.TUNNEL_IN.#": # Universal Input
			%GateData.show()
			%GateData.get_node(^"BitSize").show()
			%GateData.get_node(^"InputName").show()
		"ROUTING.TUNNEL_OUT.#": # Universal Output
			%GateData.show()
			%GateData.get_node(^"BitSize").show()
			%GateData.get_node(^"OutputName").show()
		
		"ROUTING.ROUTING.MUX.#": # Mux
			%GateData.show()
			%GateData.get_node(^"BitSize").show()
		"ROUTING.ROUTING.DEMUX.#": # DEMUX
			%GateData.show()
			%GateData.get_node(^"BitSize").show()
		"ROUTING.CONVERTER.#": # Bit Converter
			%GateData.show()
			%GateData.get_node(^"InputBits").show()
			%GateData.get_node(^"OutputBits").show()
		
		"COMBINATIONAL.AND.#": # AND Gate
			%GateData.show()
			%GateData.get_node(^"InputCount").show()
			%GateData.get_node(^"BitSize").show()
		"COMBINATIONAL.NAND.#": # NAND Gate
			%GateData.show()
			%GateData.get_node(^"InputCount").show()
			%GateData.get_node(^"BitSize").show()
		"COMBINATIONAL.OR.#": # OR Gate
			%GateData.show()
			%GateData.get_node(^"InputCount").show()
			%GateData.get_node(^"BitSize").show()
		"COMBINATIONAL.NOR.#": # NOR Gate
			%GateData.show()
			%GateData.get_node(^"InputCount").show()
			%GateData.get_node(^"BitSize").show()
		"COMBINATIONAL.NOT.#": # NOT Gate
			%GateData.show()
			%GateData.get_node(^"BitSize").show()
		"COMBINATIONAL.XOR.#": # XOR Gate
			%GateData.show()
			%GateData.get_node(^"InputCount").show()
			%GateData.get_node(^"BitSize").show()
		"COMBINATIONAL.XNOR.#": # XNOR Gate
			%GateData.show()
			%GateData.get_node(^"InputCount").show()
			%GateData.get_node(^"BitSize").show()

func _clear_gate_data() -> void:
	%GateData.hide()
	%GateData.get_node(^"BitSize").hide()
	%GateData.get_node(^"BitSize/Value").value = 1
	%GateData.get_node(^"InputName").hide()
	%GateData.get_node(^"InputName/Value").text = ""
	%GateData.get_node(^"InputCount").hide()
	%GateData.get_node(^"InputCount/Value").value = 2
	%GateData.get_node(^"InputBits").hide()
	%GateData.get_node(^"InputBits/Value").value = 1
	%GateData.get_node(^"OutputName").hide()
	%GateData.get_node(^"OutputName/Value").text = ""
	%GateData.get_node(^"OutputCount").hide()
	%GateData.get_node(^"OutputCount/Value").value = 2
	%GateData.get_node(^"OutputBits").hide()
	%GateData.get_node(^"OutputBits/Value").value = 1

func _is_gate_data_correct() -> bool:
	if not selected:
		return false
	
	match selected.type:
		"IO.INPUT.#":
			if %GateData.get_node(^"InputName/Value").text == "":
				return false
		"IO.OUTPUT.#":
			if %GateData.get_node(^"OutputName/Value").text == "":
				return false
		"ROUTING.TUNNEL_IN.#":
			if %GateData.get_node(^"InputName/Value").text == "":
				return false
		"ROUTING.TUNNEL_OUT.#":
			if %GateData.get_node(^"OutputName/Value").text == "":
				return false
		
		"ROUTING.CONVERTER.#":
			var a: int = %GateData.get_node(^"InputBits/Value").value
			var b: int = %GateData.get_node(^"OutputBits/Value").value
			return a % b == 0 or b % a == 0
	
	return true

func _make_gate_data() -> Dictionary:
	match selected.type:
		"IO.INPUT.#":
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"name": %GateData.get_node(^"InputName/Value").text
			}
		"IO.OUTPUT.#":
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"name": %GateData.get_node(^"OutputName/Value").text
			}
		"ROUTING.TUNNEL_IN.#":
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"name": %GateData.get_node(^"InputName/Value").text
			}
		"ROUTING.TUNNEL_OUT.#":
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"name": %GateData.get_node(^"OutputName/Value").text
			}
		
		"ROUTING.MUX.#": # MUX Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value
			}
		"ROUTING.DEMUX.#": # DEMUX Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value
			}
		"ROUTING.CONVERTER.#": # CONVERTER Gate
			return {
				"inputbits": %GateData.get_node(^"InputBits/Value").value,
				"outputbits": %GateData.get_node(^"OutputBits/Value").value
			}
		
		"COMBINATIONAL.AND.#": # AND Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"inputcount": %GateData.get_node(^"InputCount/Value").value
			}
		"COMBINATIONAL.NAND.#": # NAND Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"inputcount": %GateData.get_node(^"InputCount/Value").value
			}
		"COMBINATIONAL.OR.#": # OR Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"inputcount": %GateData.get_node(^"InputCount/Value").value
			}
		"COMBINATIONAL.NOR.#": # NOR Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"inputcount": %GateData.get_node(^"InputCount/Value").value
			}
		"COMBINATIONAL.NOT.#": # NOT Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value
			}
		"COMBINATIONAL.XOR.#": # XOR Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"inputcount": %GateData.get_node(^"InputCount/Value").value
			}
		"COMBINATIONAL.XNOR.#": # XNOR Gate
			return {
				"bitsize": %GateData.get_node(^"BitSize/Value").value,
				"inputcount": %GateData.get_node(^"InputCount/Value").value
			}
	
	return {}

func _add_gate_pressed() -> void:
	# check if the gate metadata is correct
	var correct: bool = _is_gate_data_correct()
	
	# continue normally
	if correct:
		var data: Dictionary = _make_gate_data()
		
		var gate: GateDescription = selected.from_data(data)
		circuit.add_gate(gate)
		#call_deferred(&"update_workspace")
		
		#region temporary testing
		var v_gate := VisualGate.new(gate)
		%Workspace.add_child(v_gate)
		if gate.type in Connection.IO_TYPES:
			call_deferred(&"redraw_io")
		gate_visual_map[gate.id] = v_gate
		visual_gate_map[v_gate.name] = gate
		#endregion
	else:
		var stylebox: StyleBoxFlat = %AddGate.get("theme_override_styles/disabled")
		var color: Color = stylebox.bg_color
	
		%AddGate.disabled = true
		var tween: Tween = get_tree().create_tween()
		tween.tween_property(%AddGate, ^"text", "Invalid Gate Data", 0.25)
		tween.parallel().tween_property(stylebox, "bg_color", Color.CRIMSON, 1)
		# wait 2.5 seconds
		await get_tree().create_timer(2.5).timeout
		# return to normal
		if tween:
			tween.kill()
		tween = %AddGate.create_tween()
		tween.tween_property(%AddGate, ^"text", "Add Gate to Circuit", 0.5)
		tween.parallel().tween_property(stylebox, "bg_color", color, 1)
		await tween.finished
		%AddGate.disabled = false

func _main_menu_pressed() -> void:
	get_tree().change_scene_to_file("res://scenes/main.tscn")
	Global.active_project.save()

func _gate_pressed() -> void:
	$VBoxContainer/TabContainer.current_tab = 0

func _visualiser_pressed() -> void:
	$VBoxContainer/TabContainer.current_tab = 1

func _simulator_pressed() -> void:
	$VBoxContainer/TabContainer.current_tab = 2

func _registry_tree_item_selected() -> void:
	var selected_item: TreeItem = %RegistryTree.get_next_selected(null)
	registry_selected = registry_tree_type_map.get(selected_item, "")
	if registry_selected == "":
		%LoadGate.disabled = true
		return
	elif registry_selected == "..":
		%LoadGate.disabled = true
		%RegistryCategoryName.text = ""
		%RegistryGateName.text = ""
		%RegistryGateVersion.text = ""
		var color: Color = _make_random_color()
		selected_item.set_custom_bg_color(0, color, false)
		%RegistryGateColor.color = color
		return
	
	var split: PackedStringArray = registry_selected.split(".", false, 3)
	%RegistryCategoryName.text = split[0]
	%RegistryGateName.text = split[1]
	%RegistryGateVersion.text = split[2]
	%RegistryGateColor.color = selected_item.get_custom_bg_color(0)
	registry_type_tree_map[".."].set_custom_bg_color(0, selected_item.get_custom_bg_color(0))
	
	_check_if_loadable()


func _make_random_color() -> Color:
	var color: Color = Color(0.8,0.2,0.2,1)
	color.h = randf_range(0,1)
	return color

func _save_gate_pressed() -> void:
	var correct: bool = true
	var reason: String = "Unable to save!"
	
	if %RegistryCategoryName.text == "":
		correct = false
		reason = "Category must not be empty"
	elif %RegistryGateName.text == "":
		correct = false
		reason = "Gate Name must not be empty"
	elif %RegistryGateVersion.text == "":
		correct = false
		reason = "Gate Version must not be empty"
	elif registry_selected in GateRegistry.builtin_gates:
		correct = false
		reason = "Can't override a Builtin Gate!"
	elif circuit.is_empty():
		correct = false
		reason = "Can't save an empty circuit!"
	elif registry_selected == "..":
		correct = false
		reason = "Can't override this!"
	
	if correct: # save gate
		circuit.name = %RegistryGateName.text
		circuit.type = "%s.%s.%s" % [%RegistryCategoryName.text, %RegistryGateName.text, %RegistryGateVersion.text]
		circuit.color = %RegistryGateColor.color
		for gate: GateDescription in circuit.gates.values():
			gate.position = gate_visual_map[gate.id].position_offset
		GateRegistry.add_gate(circuit.to_description())
		call_deferred(&"_make_gate_tree")
	else: # cant save
		var stylebox: StyleBoxFlat = %SaveGate.get("theme_override_styles/disabled")
		var color: Color = stylebox.bg_color
	
		%SaveGate.disabled = true
		var tween: Tween = get_tree().create_tween()
		tween.tween_property(%SaveGate, ^"text", reason, 0.25)
		tween.parallel().tween_property(stylebox, "bg_color", Color.CRIMSON, 1)
		# wait 2.5 seconds
		await get_tree().create_timer(2.5).timeout
		# return to normal
		if tween:
			tween.kill()
		tween = %SaveGate.create_tween()
		tween.tween_property(%SaveGate, ^"text", "Save or Update Gate", 0.5)
		tween.parallel().tween_property(stylebox, "bg_color", color, 1)
		await tween.finished
		%SaveGate.disabled = false

func redraw_io() -> void:
	# Inputs - remove
	var node: Node = %InputInfo.get_node(^"RegistryInputContainer")
	for child: Node in node.get_children():
		if not child.name.begins_with("INPUT_"):
			continue
		node.remove_child(child)
	# Inputs - check
	%InputInfo.visible = not circuit.input_config.is_empty()
	# Inputs - add
	var slot: int = 0
	for id: String in circuit.input_config.keys():
		var container := HBoxContainer.new()
		container.name = "INPUT_%s" % str(slot)
		node.add_child(container)
		
		var slt := LineEdit.new()
		slt.name = "Slot"
		slt.placeholder_text = str(slot)
		slt.alignment = HORIZONTAL_ALIGNMENT_CENTER
		slt.editable = false
		slt.custom_minimum_size = Vector2(70, 35)
		slt.set("theme_override_styles/focus", StyleBoxEmpty.new())
		container.add_child(slt)
		
		var gid := LineEdit.new()
		gid.name = "GateID"
		gid.placeholder_text = id
		gid.alignment = HORIZONTAL_ALIGNMENT_CENTER
		gid.editable = false
		gid.custom_minimum_size = Vector2(180, 35)
		gid.set("theme_override_styles/focus", StyleBoxEmpty.new())
		container.add_child(gid)
		
		var bs := LineEdit.new()
		bs.name = "BitSize"
		bs.placeholder_text = str(circuit.input_config[id].state.size)
		bs.alignment = HORIZONTAL_ALIGNMENT_CENTER
		bs.editable = false
		bs.custom_minimum_size = Vector2(120, 35)
		bs.set("theme_override_styles/focus", StyleBoxEmpty.new())
		container.add_child(bs)
		
		var pn := LineEdit.new()
		pn.name = "PinName"
		pn.placeholder_text = str(circuit.input_config[id].name)
		pn.alignment = HORIZONTAL_ALIGNMENT_CENTER
		pn.editable = false
		pn.custom_minimum_size = Vector2(0, 35)
		pn.size_flags_horizontal = Control.SIZE_EXPAND_FILL
		pn.set("theme_override_styles/focus", StyleBoxEmpty.new())
		container.add_child(pn)
		
		slot += 1
	
	# Outputs - remove
	node = %OutputInfo.get_node(^"RegistryOutputContainer")
	for child: Node in node.get_children():
		if not child.name.begins_with("OUTPUT_"):
			continue
		node.remove_child(child)
	# Outputs - check
	%OutputInfo.visible = not circuit.output_config.is_empty()
	# Outputs - add
	slot = 0
	for id: String in circuit.output_config.keys():
		var container := HBoxContainer.new()
		container.name = "OUTPUT_%s" % str(slot)
		node.add_child(container)
		
		var slt := LineEdit.new()
		slt.name = "Slot"
		slt.placeholder_text = str(slot)
		slt.alignment = HORIZONTAL_ALIGNMENT_CENTER
		slt.editable = false
		slt.custom_minimum_size = Vector2(70, 35)
		slt.set("theme_override_styles/focus", StyleBoxEmpty.new())
		container.add_child(slt)
		
		var gid := LineEdit.new()
		gid.name = "GateID"
		gid.placeholder_text = id
		gid.alignment = HORIZONTAL_ALIGNMENT_CENTER
		gid.editable = false
		gid.custom_minimum_size = Vector2(180, 35)
		gid.set("theme_override_styles/focus", StyleBoxEmpty.new())
		container.add_child(gid)
		
		var bs := LineEdit.new()
		bs.name = "BitSize"
		bs.placeholder_text = str(circuit.output_config[id].state.size)
		bs.alignment = HORIZONTAL_ALIGNMENT_CENTER
		bs.editable = false
		bs.custom_minimum_size = Vector2(120, 35)
		bs.set("theme_override_styles/focus", StyleBoxEmpty.new())
		container.add_child(bs)
		
		var pn := LineEdit.new()
		pn.name = "PinName"
		pn.placeholder_text = str(circuit.output_config[id].name)
		pn.alignment = HORIZONTAL_ALIGNMENT_CENTER
		pn.editable = false
		pn.custom_minimum_size = Vector2(0, 35)
		pn.size_flags_horizontal = Control.SIZE_EXPAND_FILL
		pn.set("theme_override_styles/focus", StyleBoxEmpty.new())
		container.add_child(pn)
		
		slot += 1


func update_workspace() -> void:
	pass


func _workspace_connection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	var connection := Connection.create(from_node, from_port, to_node, to_port)
	circuit.add_connection(connection)
	%Workspace.connect_node(from_node, from_port, to_node, to_port, false)


func _workspace_disconnection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	var template := Connection.create(from_node, from_port, to_node, to_port)
	circuit.remove_connection(template)
	%Workspace.disconnect_node(from_node, from_port, to_node, to_port)


func _workspace_delete_nodes_request(nodes: Array[StringName]) -> void:
	for node: StringName in nodes:
		var v_gate: VisualGate = %Workspace.get_node(NodePath(node))
		visual_gate_map.erase(v_gate.name)
		gate_visual_map.erase(v_gate.from.id)
		var conns: Array = circuit.remove_gate(v_gate.from.id)[1]
		for connection: Connection in conns:
			%Workspace.disconnect_node(connection.from_gate, connection.from_port, connection.to_gate, connection.to_port)
		%Workspace.remove_child(v_gate)
		if v_gate.from.type in Connection.IO_TYPES:
			call_deferred(&"redraw_io")


func _registry_gate_color_color_changed(color: Color) -> void:
	%RegistryTree.get_root().get_child(0).set_custom_bg_color(0, color, false)


func clear_workspace() -> void:
	for conn: Dictionary in %Workspace.connections:
		%Workspace.disconnect_node(conn.from_node, conn.from_port, conn.to_node, conn.to_port)
	
	for node: Node in %Workspace.get_children().duplicate():
		if node.name.split("#").size() == 4:
			%Workspace.remove_child(node)
	
	circuit = Circuit.new()
	%Workspace.scroll_offset = Vector2.ZERO
	%Workspace.zoom = 1.0
	call_deferred(&"redraw_io")

func _new_gate_pressed() -> void:
	if clear_confirmation:
		clear_workspace()
		%NewGate.text = "New Workspace made!"
		return
	
	clear_confirmation = true
	%NewGate.text = "Are you sure?"
	await get_tree().create_timer(2.5).timeout
	clear_confirmation = false
	%NewGate.text = "New Gate"

func _check_if_loadable() -> bool:
	var type: String = ""
	type += "%s." % %RegistryCategoryName.text
	type += "%s." % %RegistryGateName.text
	type += "%s" % %RegistryGateVersion.text
	registry_selected = type
	if GateRegistry.has_gate(type) and type not in GateRegistry.builtin_gates:
		%LoadGate.disabled = false
		%DeleteGate.disabled = false
		%RegistryTree.set_selected(registry_type_tree_map[type], 0)
		return true
	else:
		%LoadGate.disabled = true
		%DeleteGate.disabled = true
		return false


func _load_gate_pressed() -> void:
	# clear old circuit
	clear_workspace()
	
	# make new gates
	circuit = GateRegistry.get_gate(registry_selected)
	for gate: GateDescription in circuit.gates.values():
		#call_deferred(&"update_workspace")
		
		#region temporary testing
		var v_gate := VisualGate.new(gate)
		%Workspace.add_child(v_gate)
		gate_visual_map[gate.id] = v_gate
		visual_gate_map[v_gate.name] = gate
		if gate.type in Connection.IO_TYPES:
			call_deferred(&"redraw_io")
		#endregion
	
	# make new connections
	for connection: Connection in circuit.connections.values():
		%Workspace.connect_node(connection.from_gate, connection.from_port, connection.to_gate, connection.to_port, false)

#endregion

#region SIMULATION
func _generate_pressed() -> void:
	if not circuit or circuit.is_empty():
		return
	
	%Generate.disabled = true
	%Clear.disabled = false
	%Checker.disabled = false
	%SimControls.show()
	
	var cached: CachedCircuit = CachedCircuit.new(circuit.to_description())
	simulation = Simulation.new()
	simulation.sim_stopped.connect(_sim_stopped)
	add_child(simulation)
	simulation.setup(cached)


func _clear_pressed() -> void:
	%Generate.disabled = false
	%Clear.disabled = true
	%Checker.disabled = true
	%SimControls.hide()
	
	if simulation:
		remove_child(simulation)
		simulation = null


func _delete_gate_pressed() -> void:
	GateRegistry.remove_gate(registry_selected)
	_check_if_loadable()


func _sim_mode_stop_pressed() -> void:
	%SimModeStep.disabled = false
	%SimModeTPS.disabled = false
	%SimModeStop.disabled = true
	
	simulation.stop()


func _simulate_step_pressed() -> void:
	%SimModeStep.disabled = true
	%SimModeTPS.disabled = true
	%SimModeStop.disabled = false
	
	%StepPanel.hide()
	simulation.step(%Steps.value)


func _simulate_tps_pressed() -> void:
	%SimModeStep.disabled = true
	%SimModeTPS.disabled = true
	%SimModeStop.disabled = false
	
	%TPSPanel.hide()
	#simulation.tps = %TPS.value
	simulation.run()


func _sim_mode_step_pressed() -> void:
	%StepPanel.show()


func _sim_mode_tps_pressed() -> void:
	%TPSPanel.show()


func _cancel_pressed() -> void:
	%StepPanel.hide()
	%TPSPanel.hide()

func _sim_stopped(_ticks: int) -> void:
	%SimModeStep.disabled = false
	%SimModeTPS.disabled = false
	%SimModeStop.disabled = true
