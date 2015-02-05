// Corresponds to the sel4/constants.h header.

pub const InvalidPriority : isize = -1;
pub const MinPriority     : isize = 0;
pub const MaxPriority     : isize = 255;
pub const MsgLengthBits   : usize = 7;
pub const MsgExtraCapBits : usize = 2;
pub const MsgMaxLength    : usize = 120;
pub const MsgMaxExtraCaps : usize = 3; // 2^MsgExtraCapBits-1

