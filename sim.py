# Python-based simulation of a digital circuit using Cached structures

from collections import defaultdict

class CachedGate:
    def __init__(self, gate_id, gate_type):
        self.id = gate_id
        self.type = gate_type
        self.input_ports = {}  # dict of port_id -> value
        self.output_ports = {}  # dict of port_id -> value
        self.rank = 0
        self.is_static = False

        if gate_type == "AND":
            self.input_ports[0] = False
            self.input_ports[1] = False
            self.output_ports[0] = False
        elif gate_type == "OR":
            self.input_ports[0] = False
            self.input_ports[1] = False
            self.output_ports[0] = False
        elif gate_type == "NOT":
            self.input_ports[0] = False
            self.output_ports[0] = True
        elif gate_type == "CONST_0":
            self.output_ports[0] = False
            self.is_static = True
        elif gate_type == "CONST_1":
            self.output_ports[0] = True
            self.is_static = True
        elif gate_type == "CLOCK":
            self.output_ports[0] = False
        
        

    def compute(self):
        # For demonstration: implement basic logic gates
        if self.type == "AND":
            self.output_ports[0] = all(self.input_ports.values())
        elif self.type == "OR":
            self.output_ports[0] = any(self.input_ports.values())
        elif self.type == "NOT":
            self.output_ports[0] = not list(self.input_ports.values())[0]
        elif self.type == "CONST_1":
            self.output_ports[0] = True
        elif self.type == "CONST_0":
            self.output_ports[0] = False
        elif self.type == "CLOCK":
            self.output_ports[0] = not self.output_ports[0]

class CachedConnection:
    def __init__(self, from_gate_id, from_port, to_gate_id, to_port):
        self.from_gate_id = from_gate_id
        self.from_port = from_port
        self.to_gate_id = to_gate_id
        self.to_port = to_port

class CachedCircuit:
    def __init__(self, gates, connections):
        self.gates = {g.id: g for g in gates}
        self.connections = connections
        self.sorted_gates = []
        self.build_schedule()

    def build_schedule(self):
        # Build dependency graph and assign ranks
        dependency = defaultdict(set)
        reverse_dependency = defaultdict(set)

        for conn in self.connections:
            dependency[conn.from_gate_id].add(conn.to_gate_id)
            reverse_dependency[conn.to_gate_id].add(conn.from_gate_id)

        rank_map = {}

        def compute_rank(gate_id, visited=set()):
            if gate_id in rank_map:
                return rank_map[gate_id]
            if gate_id in visited:
                return 0  # prevent cycle crash
            visited.add(gate_id)
            rank = 1 + max((compute_rank(parent, visited.copy()) for parent in reverse_dependency[gate_id]), default=0)
            rank_map[gate_id] = rank
            return rank

        for gid in self.gates:
            self.gates[gid].rank = compute_rank(gid)

        self.sorted_gates = sorted(self.gates.values(), key=lambda g: g.rank)

    def simulate(self, ticks=1):
        for _ in range(ticks):
            # Update inputs via connections
            for conn in self.connections:
                from_gate = self.gates[conn.from_gate_id]
                to_gate = self.gates[conn.to_gate_id]
                value = from_gate.output_ports.get(conn.from_port, False)
                to_gate.input_ports[conn.to_port] = value

            # Compute gates in order
            for gate in self.sorted_gates:
                if not gate.is_static:
                    gate.compute()

    def print_state(self):
        for gate in self.sorted_gates:
            print(f"Gate {gate.id} (Type: {gate.type}, Rank: {gate.rank}) -> Outputs: {gate.output_ports}")

# --- Expanded Example Circuit ---

gates = []
connections = []

gid_counter = 0

def gid():
    gid_counter += 1
    return gid_counter

### Create Circuit

# CLOCK
gates.append(CachedGate(gid(), "CLOCK")) # GID 0

# Constants
gates.append(CachedGate(gid(), "CONST_0")) # GID 1
gates.append(CachedGate(gid(), "CONST_1")) # GID 2

# Registers
register_ids = []
for i in range(4):
    

### Simulate Circuit
circuit = CachedCircuit(gates, connections)
circuit.simulate(ticks=1)
circuit.print_state()
