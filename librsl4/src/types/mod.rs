// This module corresponds to sel4/types.h. Unlike the sel4 header, it is not separated into
// separate files for different architectures.

pub use self::generated::*;
use self::constants::*;
//use self::errors::*;
use self::message_info::*;
//use self::object_type::*;

mod generated;
mod constants;
//mod errors;
mod message_info;
//mod object_type;

enum FaultType {
    NoFault = 0,
    CapFault,
    VMFault,
    UnknownSyscall,
    UserException,
    Interrupt,
}

enum LookupFailureType {
    NoFailure = 0,
    InvalidRoot,
    MissingCapability,
    DepthMismatch,
    GuardMismatch,
}

enum CapRights {
    CanWrite      = 0x01,
    CanRead       = 0x02,
    CanGrant      = 0x04,
    AllRights     = 0x07, /* seL4_CanWrite | seL4_CanRead | seL4_CanGrant */
    Transfer_Mint = 0x100,
}

const UntypedRetypeMaxObjects : isize = 256;
const GuardSizeBits           : isize = 5;
const GuardBits               : isize = 18;
const BadgeBits               : isize = 28;

#[repr(C)]
pub struct IPCBuffer {
    tag            : message_info::MessageInfo,
    msg            : [Word; constants::MsgMaxLength],
    userData       : Word,
    caps_or_badges : [Word; constants::MsgMaxExtraCaps],
    receiveCNode   : CPtr,
    receiveIndex   : CPtr,
    receiveDepth   : Word,
}

type CNode      = CPtr;
type IRQHandler = CPtr;
type IRQControl = CPtr;
type TCB        = CPtr;
type Untyped    = CPtr;
type DomainSet  = CPtr;

// TODO: Do something about this.
//#define seL4_NilData seL4_CapData_Badge_new(0)

// ===============================================================================================
// The following correspond to the arm architecture specific types.
// ===============================================================================================

// TODO: Conditional compilation on an arm flag
pub type Word = u32;
pub type CPtr = Word;

type ARM_Page          = CPtr;
type ARM_PageTable     = CPtr;
type ARM_PageDirectory = CPtr;
type ARM_ASIDControl   = CPtr;
type ARM_ASIDPool      = CPtr;

#[repr(C)]
struct UserContext {
    pc   : Word,
    sp   : Word,
    cpsr : Word,
    r0   : Word,
    r1   : Word,
    r8   : Word,
    r9   : Word,
    r10  : Word,
    r11  : Word,
    r12  : Word,
    r2   : Word,
    r3   : Word,
    r4   : Word,
    r5   : Word,
    r6   : Word,
    r7   : Word,
    r14  : Word,
}

enum ARM_VMAttributes {
    ARM_PageCacheable        = 0x01,
    ARM_ParityEnabled        = 0x02,
    ARM_Default_VMAttributes = 0x03,
    ARM_ExecuteNever         = 0x04,
}

