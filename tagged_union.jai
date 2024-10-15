
/*
    Some custom extensions for the Tagged_Union type.
    Would also be nice to have some defined type coersions on the tagged union (e.g. float32 to float64)
    get_or_default(*u, float32, 5) ? can't do bc get returns a pointer 
*/

Tagged_Union :: struct (value_types: ..Type) {
    tag: Type;
    using value: union {
        #insert -> string {
            sb: String_Builder;
            for value_types  print_to_builder(*sb, "_%1: %1;", it);
            return builder_to_string(*sb);
        }
    }
    
    // placed inside the struct namespace so that this gets overloaded per instantiation
    is_valid_type :: (t: Type) -> bool {
        return array_find(value_types, t);
    }
}

// statically typed set
set :: (u: *Tagged_Union, value: $T) {
    #run assert(array_find(u.value_types, T));
    u.tag = T;
    memcpy(*u.value, *value, size_of(T));
}

// set from an Any
set_any :: (u: *Tagged_Union, value: Any) -> bool {
    if !u.is_valid_type(type_from_info(value.type)) return false;
    
    u.tag = type_from_info(value.type);
    memcpy(*u.value, value.value_pointer, value.type.runtime_size);
    
    return true;
}

// get the value of the tagged union as a certain type
get_as :: (u: Tagged_Union, $T: Type) -> *T {
    #run assert(is_valid_type(T));
    if u.tag == T  return cast(*T) *u.value;
    return null;
}

// get the value of the tagged union as an Any
// useful for printing the tagged union's value
get_as_any :: (using u: *Tagged_Union) -> Any {
    if !is_valid_type(u.tag)  return Any.{};
    return Any.{ 
        type = cast(*Type_Info) u.tag,
        value_pointer = *u.value,
    };
}

#scope_file

// get_biggest_size is used at compile-time by the #run in Tagged_Union.
get_biggest_size :: (types: [] Type) -> s64 {
    biggest: s64;

    // This cast to *Type_Info requires runtime_storageless_type_info to be disabled,
    // or for us to be running at compile-time. We know for sure in this case that we are
    // calling this routine at compile-time, but in runtime use, this kind of cast
    // presumes that Type_Info has been exported for all runtime types.
    for types  biggest = max(biggest, (cast(*Type_Info)(it)).runtime_size); 

    return biggest;
}

