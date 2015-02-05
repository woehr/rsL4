pub struct MessageInfo(u32);
pub struct CapData(u32);

const bits_label          : u32 = 20;
const bits_caps_unwrapped : u32 = 3;
const bits_extra_caps     : u32 = 2;
const bits_length         : u32 = 7;

const bits_guard_bits     : u32 = 18;
const bits_guard_size     : u32 = 5;

const bits_badge          : u32 = 28;

struct Rsl4MessageInfo {
    label          : u32,
    caps_unwrapped : u32,
    extra_caps     : u32,
    length         : u32,
}

fn rsl4_message_info_new(mi : MessageInfo) -> Rsl4MessageInfo {
    let MessageInfo(x) = mi;
    Rsl4MessageInfo {
        label          : (x >> 12) & ((1u32 << bits_label) -1),
        caps_unwrapped : (x >>  9) & ((1u32 << bits_caps_unwrapped) -1),
        extra_caps     : (x >>  7) & ((1u32 << bits_extra_caps) -1),
        length         : (x >>  0) & ((1u32 << bits_length) -1),
    }
}

impl Rsl4MessageInfo {
    fn kernel_repr(&self) -> MessageInfo {
        assert!(self.label          < (1u32 << bits_label));
        assert!(self.caps_unwrapped < (1u32 << bits_caps_unwrapped));
        assert!(self.extra_caps     < (1u32 << bits_extra_caps));
        assert!(self.length         < (1u32 << bits_length));
        MessageInfo(
          self.label          << 12
        | self.caps_unwrapped << 9
        | self.extra_caps     << 7
        | self.length
        )
    }

    fn get_label(&self) -> u32 {
        assert!(self.label < (1u32 << bits_label));
        self.label
    }

    fn get_caps_unwrapped(&self) -> u32 {
        assert!(self.caps_unwrapped < (1u32 << bits_caps_unwrapped));
        self.caps_unwrapped
    }

    fn get_extra_caps(&self) -> u32 {
        assert!(self.extra_caps< (1u32 << bits_extra_caps));
        self.extra_caps
    }

    fn get_length(&self) -> u32 {
        assert!(self.length < (1u32 << bits_length));
        self.length
    }
}

enum Rsl4CapData {
    Guard {
        guard_bits : u32,
        guard_size : u32,
    },
    Badge(u32),
}

