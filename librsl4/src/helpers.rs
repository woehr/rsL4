use core::str::{StrExt};
use super::syscalls::{debugPutChar};

pub fn debugPutStr(s: &str) {
   for b in s.bytes() {
       debugPutChar(b);
   }
}

