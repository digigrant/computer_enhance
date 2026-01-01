#import "Compiler";
#import "Basic";
#import "File";
#import "String";

OPCODE_SHIFT    : u8 : 2;
D_SHIFT         : u8 : 1;
// no W_SHIFT
MOD_SHIFT       : u8 : 14;
REG_SHIFT       : u8 : 11;
RM_SHIFT        : u8 : 8;

Mask :: ($size : u8) -> u16 {
    #if size == 1   return 1;
    return (0b1 << size) - 1;
}

OPCODE_MASK     : u16 : #run Mask(6) << OPCODE_SHIFT;
D_MASK          : u16 : #run Mask(1) << D_SHIFT;
W_MASK          : u16 : #run Mask(1);
MOD_MASK        : u16 : #run Mask(2) << MOD_SHIFT;
REG_MASK        : u16 : #run Mask(3) << REG_SHIFT;
RM_MASK         : u16 : #run Mask(3) << RM_SHIFT;

Opcode  :: (inst : u16) -> u16 { return inst & OPCODE_MASK >> OPCODE_SHIFT; }
D       :: (inst : u16) -> u16 { return inst & D_MASK >> D_SHIFT; }
W       :: (inst : u16) -> u16 { return inst & W_MASK; }
Mod     :: (inst : u16) -> u16 { return inst & MOD_MASK >> MOD_SHIFT; }
Reg     :: (inst : u16) -> u16 { return inst & REG_MASK >> REG_SHIFT; }
Rm      :: (inst : u16) -> u16 { return inst & RM_MASK >> RM_SHIFT; }

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

ReadInstruction :: (file : string, cursor : *int) -> asm : string {
    inst : u16;
    memcpy(*inst, *(file[cursor.*]), 2); 
    cursor.* += 2;

    asm : String_Builder;
    init_string_builder(*asm, 64);

    if Opcode(inst) == {
        case 0b100010; append(*asm, "mov");
    }

    append(*asm, " ");

    w_reg := cast(u8) W(inst) << 3 | Reg(inst);
    w_rm := cast(u8) W(inst) << 3 | Rm(inst);

    append(*asm, REGISTER_NAME[ifx D(inst) then w_reg else w_rm]);
    append(*asm, ", ");
    append(*asm, REGISTER_NAME[ifx D(inst) then w_rm else w_reg]);

    return builder_to_string(*asm);
}

#run { set_build_options_dc(.{do_output=false});
    context.print_style.default_format_int.base = 2; // print ints in binary

    options := get_build_options();
    args := options.compile_time_command_line;

    if (args.count < 1) {
        print("No arguments were given. Expected:\n");
        print("\tjai 8086_inst_decoder.jai - <input_filename>\n");
        print("Exiting.\n");
        return;
    }

    if (args.count > 1) {
        print("Too many arguments. Expected:\n");
        print("\tjai 8086_inst_decoder.jai - <input_filename>\n");
        print("Exiting.\n");
        return;
    }

    file, ok := read_entire_file(args[0]);
    if !ok {
        print("Exiting.\n");
        return;
    }

    asm : String_Builder;
    init_string_builder(*asm);

    filename := ifx begins_with(args[0], "./") then slice(args[0], 2, args[0].count-2) else args[0];
    append(*asm, sprint("; %\n", filename));
    append(*asm, "bits 16\n");

    cursor := 0;
    while cursor < file.count {
        append(*asm, sprint("%\n", ReadInstruction(file, *cursor)));
    }

    print("%\n", builder_to_string(*asm));
}
