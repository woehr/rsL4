use super::types::{Syscall};

fn swi_num_of(sc_num: u32) -> u32 {
    sc_num & 0x00FFFFFF
}

pub fn debugPutChar(c: u8) {
    unsafe {
        let sc_num = Syscall::DebugPutChar as u32;
//        let swi_num = swi_num_of(sc_num);
        asm!("
             mov r0, $0
             mov r7, $1
             swi 0x00FFFFF8
             "
            : // no outputs
            : "r"(c), "r"(sc_num) // input
            : "r0", "r7" //clobbers
            : "volatile" // options
            );
    }
}

