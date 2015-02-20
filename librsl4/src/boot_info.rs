use super::types::*;
//use super::functions::{set_user_data};

// Note: seL4 prefixes the variants with Cap so I've named the enum Cap.
enum Cap {
    Null                =  0, /* null cap */
    InitThreadTCB       =  1, /* initial thread's TCB cap */
    InitThreadCNode     =  2, /* initial thread's root CNode cap */
    InitThreadPD        =  3, /* initial thread's PD cap */
    IRQControl          =  4, /* global IRQ controller cap */
    ASIDControl         =  5, /* global ASID controller cap */
    InitThreadASIDPool  =  6, /* initial thread's ASID pool cap */
    IOPort              =  7, /* global IO port cap (null cap if not supported) */
    IOSpace             =  8, /* global IO space cap (null cap if no IOMMU support) */
    BootInfoFrame       =  9, /* bootinfo frame cap */
    InitThreadIPCBuffer = 10, /* initial thread's IPC buffer frame cap */
    Domain              = 11  /* global domain controller cap */
}

#[repr(C)]
struct SlotRegion {
    start : Word, /* first CNode slot position OF region */
    end   : Word, /* first CNode slot position AFTER region */
}

#[repr(C)]
struct DeviceRegion {
    basePaddr     : Word,       /* base physical address of device region */
    frameSizeBits : Word,       /* size (2^n bytes) of a device-region frame */
    frames        : SlotRegion, /* device-region frame caps */
}

// TODO: The constants 167 and 199 (num of untyped caps and device regions, respectively) are taken
// straight from the kernel configuration file. In the future these should not be hard coded but
// instead passed in as a configuration option.
#[repr(C)]
pub struct BootInfo<'a> {
    nodeID                  : Word,                /* ID [0..numNodes-1] of the seL4 node (0 if uniprocessor) */
    numNodes                : Word,                /* number of seL4 nodes (1 if uniprocessor) */
    numIOPTLevels           : Word,                /* number of IOMMU PT levels (0 if no IOMMU support) */
    ipcBuffer               : &'a IPCBuffer,       /* pointer to initial thread's IPC buffer */
    empty                   : SlotRegion,          /* empty slots (null caps) */
    sharedFrames            : SlotRegion,          /* shared-frame caps (shared between seL4 nodes) */
    userImageFrames         : SlotRegion,          /* userland-image frame caps */
    userImagePTs            : SlotRegion,          /* userland-image PT caps */
    untyped                 : SlotRegion,          /* untyped-object caps (untyped caps) */
    untypedPaddrList        : [Word; 167],         /* physical address of each untyped cap */
    untypedSizeBitsList     : [u8; 167],           /* size (2^n) bytes of each untyped cap */
    initThreadCNodeSizeBits : u8,                  /* initial thread's root CNode size (2^n slots) */
    numDeviceRegions        : Word,                /* number of device regions */
    deviceRegions           : [DeviceRegion; 199], /* device regions */
    initThreadDomain        : u32,                 /* Initial thread's domain ID */
}

static mut boot_info : *const BootInfo<'static> = 0 as *const BootInfo<'static>;

#[no_mangle]
pub extern "C" fn init_boot_info(bi : *const BootInfo<'static>) {
    unsafe { boot_info = bi };
}

// The assumption with the boot info is that it will be setup before jumping to the Rust entry
// point thus it is an error for it to be null.
pub fn get_boot_info() -> &'static BootInfo<'static> {
    assert!(unsafe { boot_info != (0 as *const BootInfo<'static>) });
    unsafe { &*boot_info }
}

