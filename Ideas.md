## Simulation
#### Cached Circuit (will be the actual class that gets simulated, very performant)
**Requirements / Tasks :**
 - Created from a normal Circuit
 - Turns every Gate and Connection into their respective Cached\* classes
 - Flattens the entire Circuit
 - Gives every Gate and Connection their rank (order of simulation), depending on:
   - how much depends on them later on
   - if it is a standalone / static Circuit (should be simulated at the very beginning)
   - if some things are repetitive and have the same output value:
     - Merge it
	 - Simulate it once
	 - Set connection references

** Used Classes**
 - GateDescription 			| Everything about a normal Gate
 - ConnectionDescription 	| Everything about a normal Connection
 - Circuit					| A normal Circuit that can be edited
 - CachedGate				| A stripped down version of GateDescription, designed to be performant
 - CachedConnection			| A stripped down version of ConnectionDescription, designed to be performant
 - CachedCircuit 			| A stripped down but powerful version of Circuit. 