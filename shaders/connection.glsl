#[compute]
#version 450

layout(local_size_x = 16, local_size_y = 16) in;



/* - - - Textures - - - */

// Gate texture
// First pixel : R = gate_id, G = input_amount, B = output_amount, A = bus_amount
// Other pixel (aligns with each input/..) : R = unused, G = input_size, B = output_size, A = bus_size
layout(set = 0, binding = 0, rgba8) uniform image2D gate_texture;

// Input texture (Read-only)
// 2 Bits = 1 State (LL = LOW, LH = HIGH, HL = TRI, HH = ERR) = 2 * 8 * 4 = 64 (bits used) / 2 = 32 (MAX_IO_SIZE)
layout(set = 0, binding = 1, rgba8) uniform image2D input_texture;

// Output texture
// 2 Bits = 1 State (LL = LOW, LH = HIGH, HL = TRI, HH = ERR) = 2 * 8 * 4 = 64 (bits used) / 2 = 32 (MAX_IO_SIZE)
layout(set = 0, binding = 2, rgba8) uniform image2D output_texture;

// Bus texture
// 2 Bits = 1 State (LL = LOW, LH = HIGH, HL = TRI, HH = ERR) = 2 * 8 * 4 = 64 (bits used) / 2 = 32 (MAX_IO_SIZE)
layout(set = 0, binding = 3, rgba8) uniform image2D bus_texture;

// Connection texture
// Aligns with each pixel : R = source_gate, G = source_port, B = unused, A = unused
layout(set = 0, binding = 3, rgba8) uniform readonly image2D connection_texture;



/* - - - Main - - - */
void main() {
    ivec2 destination = ivec2(gl_GlobalInvocationID.xy);
    if (destination.x == 0) { return; } // Invalid connection_texture

    vec4 data = imageLoad(connection_texture, destination);

    ivec2 source = ivec2(int(data.r * 255.0), int(data.g * 255.0));

    vec4 color = imageLoad(output_texture, source);

    imageStore(input_texture, destination, color);
}
