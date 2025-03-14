#[compute]
#version 450

layout(local_size_x = 1, local_size_y = 16) in;



/* - - - Definitions - - - */

// Constants : Textures
const uint TEX_IN           =   1;
const uint TEX_OUT          =   2;
const uint TEX_BUS          =   3;

// Constants : Errors
const uint ERR_OK           =   0;  // No Error
const uint ERR_COUNT        =   1;  // Invalid amount of IO
const uint ERR_SIZE         =   2;  // Invalid size of IO
const uint ERR_VALUE        =   3;  // Invalid value in IO (STATE_ERR)

// Constants : Tri-State
const uint STATE_ERR        =   0;  // ERROR
const uint STATE_LOW        =   1;  // LOW
const uint STATE_HIGH       =   2;  // HIGH
const uint STATE_TRI        =   3;  // UNKNOWN

// Constants : Gate Types
const uint GATE_UNKNOWN     =   0;
const uint GATE_AND         =   1;
const uint GATE_NAND        =   2;
const uint GATE_OR          =   3;
const uint GATE_NOR         =   4;
const uint GATE_NOT         =   5;
const uint GATE_XOR         =   6;
const uint GATE_XNOR        =   7;
const uint GATE_TRI         =   8;
const uint GATE_ON          =   9;
const uint GATE_OFF         =  10;

const uint GATE_CUSTOM      = 255;

// Constants : Maximum
const uint MAX_IO_COUNT     = 128;
const uint MAX_IO_SIZE      =  32;



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
// Aligns with each pixel : R = destination_gate, G = destination_port, B = unused, A = unused
layout(set = 0, binding = 4, rgba8) uniform readonly image2D connection_texture;



/* - - - Helper - - - */

// Read a multi-bit value from a texture
uint[MAX_IO_SIZE] read_value(uint tex, ivec2 pos) {
    uint values[MAX_IO_SIZE];
    for (int i = 0; i < MAX_IO_SIZE; i++) {
        values[i] = STATE_ERR;
    }
    uint size = 0;
    vec4 data;
    switch (tex) {
        case TEX_IN:
            data = imageLoad(input_texture, pos);
            size = uint(imageLoad(gate_texture, pos).g * 255.0);
            break;
        case TEX_BUS:
            data = imageLoad(bus_texture, pos);
            size = uint(imageLoad(gate_texture, pos).g * 255.0);
            break;
    };
    // uint raw_value = uint(data.r * 255.0) | (uint(data.g * 255.0) << 8) | (uint(data.b * 255.0) << 16) | (uint(data.a * 255.0) << 24);
    uint raw_value = 0;
    raw_value |= uint(data.r * 255.0);
    raw_value |= uint(data.g * 255.0) << 8;
    raw_value |= uint(data.b * 255.0) << 16;
    raw_value |= uint(data.a * 255.0) << 24;
    
    for (int i = 0; i < size; i++) {
        values[i] = (raw_value >> (i * 2)) & 0x3;
    }
    return values;
}

// Write a multi-bit value from a texture
void write_value(uint tex, ivec2 pos, uint values[MAX_IO_SIZE]) {
    uint packed_value = 0;
    for (int i = 0; i < MAX_IO_SIZE; i++) {
        packed_value |= (values[i] & 0x3) << (i * 2);
    }
    vec4 data = vec4(
        float((packed_value      ) & 0xFF) / 255.0,
        float((packed_value >>  8) & 0xFF) / 255.0,
        float((packed_value >> 16) & 0xFF) / 255.0,
        float((packed_value >> 24) & 0xFF) / 255.0
    );
    switch (tex) {
        case TEX_OUT:
            imageStore(output_texture, pos, data);
            break;
        case TEX_BUS:
            imageStore(bus_texture, pos, data);
            break;
    };
}



/* - - - Main - - - */
void main() {
    uint id = uint(gl_GlobalInvocationID.x);
    
    vec4 gate_data = imageLoad(gate_texture, ivec2(id, 0));
    uint gate_type = uint(gate_data.r * 255.0);

    uint input_count = uint(gate_data.g * 255.0);
    uint input_sizes[MAX_IO_COUNT];
    uint input_values[MAX_IO_COUNT][MAX_IO_SIZE];

    uint output_count = uint(gate_data.b * 255.0);
    uint output_sizes[MAX_IO_COUNT];
    uint output_values[MAX_IO_COUNT][MAX_IO_SIZE];

    uint bus_count = uint(gate_data.a * 255.0);
    uint bus_sizes[MAX_IO_COUNT];
    uint bus_values[MAX_IO_COUNT][MAX_IO_SIZE];

    uint err = ERR_OK;

    // Read inputs
    for (uint i = 0; i < input_count; i++) {
        input_sizes[i] = uint(imageLoad(gate_texture, ivec2(i + 1, id)).g * 255.0);
        input_values[i] = read_value(TEX_IN, ivec2(i + 1, id));
    }

    // Read outputs (sizes only)
    for (uint i = 0; i < output_count; i++) {
        output_sizes[i] = uint(imageLoad(gate_texture, ivec2(i + 1, id)).b * 255.0);
    }

    // Read buses (acting as inputs)
    for (uint i = 0; i < bus_count; i++) {
        bus_sizes[i] = uint(imageLoad(gate_texture, ivec2(i + 1, id)).a * 255.0);
        bus_values[i] = read_value(TEX_BUS, ivec2(i + 1, id));
    }

    // Simulate logic gate
    switch (gate_type) {
        case GATE_AND: {
            if (input_count < 2) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };

            uint res = STATE_ERR;


            for (uint i = 0; i < input_count; i++) {
                if (input_sizes[i] != 1) {
                    res = STATE_ERR;
                    err = ERR_SIZE;
                    break;
                }
                uint value = input_values[i][0];
                switch (value) {
                    case STATE_ERR:
                        res = STATE_ERR;
                        err = ERR_VALUE;
                        break;
                    case STATE_TRI:
                        res = STATE_TRI;
                        break;
                    case STATE_LOW:
                        res = STATE_LOW;
                }
            }
            output_values[0][0] = res;
            break; }
        
        case GATE_NAND: {
            if (input_count < 2) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };

            uint res = STATE_ERR;


            for (uint i = 0; i < input_count; i++) {
                if (input_sizes[i] != 1) {
                    res = STATE_ERR;
                    err = ERR_SIZE;
                    break;
                }
                uint value = input_values[i][0];
                switch (value) {
                    case STATE_ERR:
                        res = STATE_ERR;
                        err = ERR_VALUE;
                        break;
                    case STATE_TRI:
                        res = STATE_TRI;
                        break;
                    case STATE_LOW:
                        res = STATE_LOW;
                }
            }
            if (res == STATE_LOW) {
            	res = STATE_HIGH;
            } else if (res == STATE_HIGH) {
            	res = STATE_LOW;
            }
            output_values[0][0] = res;
            break; }
        
        case GATE_OR: {
            if (input_count < 2) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };

            uint res = STATE_ERR;


            for (uint i = 0; i < input_count; i++) {
                if (input_sizes[i] != 1) {
                    res = STATE_ERR;
                    err = ERR_SIZE;
                    break;
                }
                uint value = input_values[i][0];
                switch (value) {
                    case STATE_ERR:
                        res = STATE_ERR;
                        err = ERR_VALUE;
                        break;
                    case STATE_TRI:
                        res = STATE_TRI;
                        break;
                    case STATE_HIGH:
                        res = STATE_HIGH;
                }
            }
            output_values[0][0] = res;
            break; }
        
        case GATE_NOR: {
            if (input_count < 2) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };

            uint res = STATE_ERR;


            for (uint i = 0; i < input_count; i++) {
                if (input_sizes[i] != 1) {
                    res = STATE_ERR;
                    err = ERR_SIZE;
                    break;
                }
                uint value = input_values[i][0];
                switch (value) {
                    case STATE_ERR:
                        res = STATE_ERR;
                        err = ERR_VALUE;
                        break;
                    case STATE_TRI:
                        res = STATE_TRI;
                        break;
                    case STATE_HIGH:
                        res = STATE_HIGH;
                }
            }
            if (res == STATE_LOW) {
            	res = STATE_HIGH;
            } else if (res == STATE_HIGH) {
            	res = STATE_LOW;
            }
            output_values[0][0] = res;
            break; }
        
        case GATE_NOT: {
            if (input_count != 1) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };
            
            uint res = STATE_ERR;

            uint value = input_values[0][0];
            
            switch (value) {
                case STATE_LOW: res = STATE_HIGH;
                case STATE_HIGH: res = STATE_LOW;
                default: res = value;
            }
            
            output_values[0][0] = res;
            break; }
        
        case GATE_XOR: {
            if (input_count != 2) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };
            
            uint res = STATE_HIGH;

            uint value1 = input_values[0][0];
            uint value2 = input_values[1][0];
            
            if (value1 == STATE_TRI || value2 == STATE_TRI) {
                res = STATE_TRI;
            } else if (value1 == STATE_ERR || value2 == STATE_ERR) {
                res = STATE_ERR;
            } else if (value1 == value2) {
                res = STATE_LOW;
            }
            
            output_values[0][0] = res;
            break; }
        
        case GATE_XNOR: {
            if (input_count != 2) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };
            
            uint res = STATE_LOW;

            uint value1 = input_values[0][0];
            uint value2 = input_values[1][0];
            
            if (value1 == STATE_TRI || value2 == STATE_TRI) {
                res = STATE_TRI;
            } else if (value1 == STATE_ERR || value2 == STATE_ERR) {
                res = STATE_ERR;
            } else if (value1 == value2) {
                res = STATE_HIGH;
            }
            
            output_values[0][0] = res;
            break; }
        
        case GATE_TRI: {
            if (input_count != 2) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };
            
            uint res = STATE_HIGH;

            uint value = input_values[0][0];
            uint enable = input_values[1][0];
            
            if (value == STATE_TRI || enable == STATE_TRI) {
                res = STATE_TRI;
            } else if (value == STATE_ERR || enable == STATE_ERR) {
                res = STATE_ERR;
            } else if (enable == STATE_HIGH) {
                res = value;
            } else if (enable == STATE_LOW) {
                res = STATE_TRI;
            }
            
            output_values[0][0] = res;
            break; }
        
        case GATE_ON: {
            if (input_count != 0) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };
            
            uint res = STATE_HIGH;

            
            output_values[0][0] = res;
            break; }

        case GATE_OFF: {
            if (input_count != 0) { break; };
            if (output_count != 1) { break; };
            if (output_sizes[0] != 1) { break; };
            if (bus_count != 0) { break; };
            
            uint res = STATE_LOW;

            
            output_values[0][0] = res;
            break; }

        default: break;
    }
    

    // Write outputs
    for (uint i = 0; i < output_count; i++) {
        write_value(TEX_OUT, ivec2(i + 1, id), output_values[i]);
    }

    // Write buses (acting as outputs)
    for (uint i = 0; i < bus_count; i++) {
        write_value(TEX_BUS, ivec2(i + 1, id), bus_values[i]);
    }

    imageStore(output_texture, ivec2(0, id), vec4(float(err / 255.0), 0, 0, 0));
}
