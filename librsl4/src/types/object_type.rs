enum ObjectType {
    UntypedObject,
    TCBObject,
    EndpointObject,
    AsyncEndpointObject,
    CapTableObject,
}

const NonArchObjectTypeCount : isize = 5;

// TODO: Add arm configuration selection
enum ArchObjectType {
    ARM_SmallPageObject = NonArchObjectTypeCount,
    ARM_LargePageObject,
    ARM_SectionObject,
    ARM_SuperSectionObject,
    ARM_PageTableObject,
    ARM_PageDirectoryObject,
    ObjectTypeCount,
}

#[static_assert]
static a: bool = ObjectType::UntypedObject as isize == 0;
#[static_assert]
static b: bool = ObjectType::CapTableObject as isize == 4;
#[static_assert]
static c: bool = ArchObjectType::ObjectTypeCount as isize == 11;

