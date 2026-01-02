#import "Compiler";
#import "Basic";
#import "File";
#import "String";

NUM_REGISTERS :: 16;
REGISTER_NAME : [NUM_REGISTERS] string : #run BuildRegisterNames();

BuildRegisterNames :: () -> [NUM_REGISTERS] string {
    register_names : [NUM_REGISTERS] string;

    // 8 bit
    register_names[0b0000] = "al";
    register_names[0b0001] = "cl";
    register_names[0b0010] = "dl";
    register_names[0b0011] = "bl";
    register_names[0b0100] = "ah";
    register_names[0b0101] = "ch";
    register_names[0b0110] = "dh";
    register_names[0b0111] = "bh";

    // 16 bit
    register_names[0b1000] = "ax";
    register_names[0b1001] = "cx";
    register_names[0b1010] = "dx";
    register_names[0b1011] = "bx";
    register_names[0b1100] = "sp";
    register_names[0b1101] = "bp";
    register_names[0b1110] = "si";
    register_names[0b1111] = "di";

    return register_names;
}

NUM_EFFECTIVE_ADDRESSES :: 8;
EFFECTIVE_ADDRESS : [NUM_EFFECTIVE_ADDRESSES] string : #run BuildEffectiveAddresses();

BuildEffectiveAddresses :: () -> [NUM_EFFECTIVE_ADDRESSES] string {
    effective_addresses : [NUM_EFFECTIVE_ADDRESSES] string;
    effective_addresses[0b000] = "bx + si";
    effective_addresses[0b001] = "bx + di";
    effective_addresses[0b010] = "bp + si";
    effective_addresses[0b011] = "bp + di";
    effective_addresses[0b100] = "si";
    effective_addresses[0b101] = "di";
    effective_addresses[0b110] = "bp";
    effective_addresses[0b111] = "bx";
    return effective_addresses;
}

MAX_INSTRUCTION_BYTES :: 6;
InstructionBuffer :: struct {
    buffer : [MAX_INSTRUCTION_BYTES] byte;
    num_bytes : u8;
}

byte :: #type,isa u8;

BytePrinter :: (builder: *String_Builder, any: Any, struct_printer_data: *void) -> bool {
    if any.type.type == {
        case .VARIANT; {
            ti_variant := cast(*Type_Info_Variant) any.type;
            if ti_variant.name == "byte" {
                print_integer(builder, .{value=any, base=2, minimum_digits=8});
                return true;
            }
        }
        case .STRUCT; {
            ti_struct := cast(*Type_Info_Struct) any.type;
            if ti_struct.name == {
                case "InstructionBuffer"; {
                    instruction_buffer := cast(*InstructionBuffer) any.value_pointer;
                    using instruction_buffer.*;
                    append(builder, sprint("{% bytes: [", num_bytes));
                    for 0..5 {
                        if it > 0 append(builder, " ");
                        append(builder, ifx it < num_bytes sprint("%", buffer[it]) else "--------");
                    }
                    append(builder, sprint("]}",));
                    return true;
                }
            }
        }
    }

    return false;
}

ReadBytesIntoInstructionBuffer :: (file : string, cursor : *int, inst : *InstructionBuffer, count : u8) {
    using inst.*;
    assert(count > 0 && count <= MAX_INSTRUCTION_BYTES);
    assert(count <= MAX_INSTRUCTION_BYTES - num_bytes);

    // this might need to be a for loop - maybe not??
    memcpy(*(buffer[num_bytes]), *(file[cursor.*]), count);
    cursor.* += count;
    num_bytes += count;
}

ReadInstruction :: (file : string, cursor : *int, print_instruction_buffer : bool = false) -> asm : string {
    // inst : u16; // this needs to change - read 8 bits first, determine if we need more
    // we know 6 bytes will contain every possible instruction, so...
    using instruction : InstructionBuffer;

    full_asm : String_Builder;  // full string to return including comments
    asm : String_Builder;       // string containing only asm code
    init_string_builder(*full_asm, 256);
    init_string_builder(*asm, 64);
    printed_to_asm := false;

    // read first byte
    ReadBytesIntoInstructionBuffer(file, cursor, *instruction, 1);

    // determine what opcode we are doing and whether we need to read more bytes
    // the first 4 bits give us a clue if we need to read more
    if buffer[0] >> 4 == { // high nibble

        case 0b1000; {
            if buffer[0] >> 2 == 0b100010 { // MOV (R/M to/from register)
                append(*full_asm, "; MOV (R/M to/from register)\n");

                // read at least 1 more byte to know mode value
                ReadBytesIntoInstructionBuffer(file, cursor, *instruction, 1);

                DEST_MASK   : u8 : 0b00000010;  // buffer[0]
                WORD_MASK   : u8 : 0b00000001;
                MODE_MASK   : u8 : 0b11000000;  // buffer[1]
                REG_MASK    : u8 : 0b00111000;
                RM_MASK     : u8 : 0b00000111;

                dest_flag : u8 = (buffer[0] & DEST_MASK) >> 1;
                word_flag : u8 = buffer[0] & WORD_MASK;

                mode : u8 = (buffer[1] & MODE_MASK) >> 6;
                reg : u8 = (buffer[1] & REG_MASK) >> 3;
                r_m : u8 = (buffer[1] & RM_MASK);
                
                word_reg : u8 = (word_flag << 3) | reg;  // we will need this

                dest, source : string; // ultimately we need to figure these out

                // helper function -> "[base (+ displacement)]"
                FormatEffectiveAddress :: (base : string, displacement : u16 = 0) -> string {
                    return sprint("[%]", ifx displacement != 0 then sprint("% + %", base, displacement) else base);
                }

                // read any more displacement bytes we might need to
                // get (formatted) source and dest
                if mode == {
                    case 0b00; { // Memory Mode - no displacement* (still must read 1 byte if R/M = 110)
                        base_effective_address : string;
                        if r_m == 0b110 {
                            ReadBytesIntoInstructionBuffer(file, cursor, *instruction, 2);
                            base_effective_address = sprint("%", cast(u8) buffer[2]); // not sure how to directly cast u16 to string. too tired to figure it out
                        } else {
                            base_effective_address = EFFECTIVE_ADDRESS[r_m];
                        }

                        dest = ifx dest_flag then REGISTER_NAME[word_reg] else FormatEffectiveAddress(base_effective_address);
                        source = ifx dest_flag then FormatEffectiveAddress(base_effective_address) else REGISTER_NAME[word_reg];
                    }
                    case 0b01; { // Memory mode - 8 bit displacement
                        ReadBytesIntoInstructionBuffer(file, cursor, *instruction, 1);

                        effective_address := FormatEffectiveAddress(EFFECTIVE_ADDRESS[r_m], buffer[2]);

                        dest = ifx dest_flag then REGISTER_NAME[word_reg] else effective_address;
                        source = ifx dest_flag then effective_address else REGISTER_NAME[word_reg];
                    }
                    case 0b10; { // Memory mode - 16 bit displacement
                        ReadBytesIntoInstructionBuffer(file, cursor, *instruction, 2);

                        effective_address := FormatEffectiveAddress(EFFECTIVE_ADDRESS[r_m], (cast(*u16) *(buffer[2])).*);

                        dest = ifx dest_flag then REGISTER_NAME[word_reg] else effective_address;
                        source = ifx dest_flag then effective_address else REGISTER_NAME[word_reg];
                    }
                    case 0b11; { // register to register - no displacement
                        word_rm : u8 = (word_flag << 3) | r_m;
                        dest = REGISTER_NAME[ifx dest_flag then word_reg else word_rm];
                        source = REGISTER_NAME[ifx dest_flag then word_rm else word_reg];
                    }
                }

                append(*asm, sprint("mov %, %\n", dest, source));
                printed_to_asm = true;
            }
        }

        case 0b1011; { // MOV (immediate move)
            WORD_MASK   :: 0b00001000;
            REG_MASK    :: 0b00000111;
            word_flag := buffer[0] & WORD_MASK;

            append(*full_asm, sprint("; MOV (immediate to register - %)\n", ifx word_flag then "16 bits" else "8 bits"));

            // read 1 or 2 more bytes depending on w
            ReadBytesIntoInstructionBuffer(file, cursor, *instruction, xx ifx word_flag then 2 else 1);

            // do shit
            append(*asm, sprint("mov %, %\n",
                REGISTER_NAME[buffer[0] & (WORD_MASK | REG_MASK)],
                (cast(*u16) *(buffer[1])).*)
            );
            printed_to_asm = true;
        }

    }

    if print_instruction_buffer append(*full_asm, sprint("; instruction_buffer: %\n", instruction));
    append(*full_asm, ifx printed_to_asm builder_to_string(*asm) else "Unknown instruction code.\n");

    return builder_to_string(*full_asm);
}

#run {
    set_build_options_dc(.{do_output=false});
    context.print_style.struct_printer = BytePrinter;

    options := get_build_options();
    args := options.compile_time_command_line;

    if (args.count < 1) {
        print("Incorrect number of arguments. Expected:\n");
        print("\tjai 8086_inst_decoder.jai - <input_filename>\n");
        print("Optional arguments:\n");
        print("\tprint_instruction_buffer - prints instruction buffer when processing an instruction\n");
        print("Exiting.\n");
        return;
    }

    print_instruction_buffer : bool = array_find(args, "print_instruction_buffer");

    file, ok := read_entire_file(args[0]);
    if !ok {
        print("Exiting.\n");
        return;
    }

    asm : String_Builder;
    init_string_builder(*asm);

    filename := ifx begins_with(args[0], "./") then slice(args[0], 2, args[0].count-2) else args[0];
    append(*asm, sprint("; %\n", filename));
    append(*asm, "bits 16\n\n");

    cursor := 0;
    while cursor < file.count {
        append(*asm, sprint("%\n", ReadInstruction(file, *cursor, print_instruction_buffer=print_instruction_buffer)));
    }

    print("%\n", builder_to_string(*asm));
}
