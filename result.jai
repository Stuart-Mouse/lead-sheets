/*
    A generic result object that we can use to return either a value or an error.
    An error is convertible to any other result type.
    
    make_result and make_error procs are scoped into struct itself so that you can 
    `using Result(T)` in a procedure to get convenient access to the proper overloads.
    
    Maybe some of this is not strictly necessary...
    I've never really used result objects much since that's just not the way I've programmed, 
    but as this module gets more complex, it seems like something worth trying out.
    
    So far it already seems to me that just having multiple return values is better than using a result object, but maybe the two can coexist
    I still want to think that the result object could end up being more convenient specifically in cases where 
    like what I am doing with the typecheck_node, evaluate_node, etc procs
        maybe bc they are such large procedures that all need to handle so many error cases, and coordinate to return error strings tot he user
    although, maybe having to receive these rsult objects sort of sucks for the user, especially since the helpful macros and related procedures for handling the result object are probabyl namespaced with the rest of the LS stuff
    maybe this would be a nonissue if I factored this out to Utils and the user just used the utils un-namespaced, but still
    I think there's probably some additional friction in getting back some result object on the user end
    so maybe the relevant procedures should have overloads that just use multiple returns for the sake of user convenience...
    
    i dunno
    maybe having an operator overload for the most important functionality is actually more relevant here, since then the namespacing thing is not an issue
    can operator overload have multiple returns as well? could make the unboxing really simple actually
    
*/
Result :: struct(T: Type) {
    success:    bool;
    union {
        error:  string;
        value:  T;
    }
    
    make_result :: inline (value: T) -> Result(T) {
        return .{ success = true, value = value };
    }
    make_error :: inline (format: string, args: ..Any) -> Result(T) {
        return .{ error = sprint(format, ..args) };
    } @PrintLike
    make_error_temp :: inline (format: string, args: ..Any) -> Result(T) {
        return .{ error = tprint(format, ..args) };
    } @PrintLike
    
    // operator || :: or_default;
    // operator << :: make_result;
    
}

operator ! :: inline (result: Result) -> bool { return !result.success; }

// only for errors, since we have binary compatibility in that case
recast_error :: (result: Result($T), $to_type: Type) -> Result(to_type) {
    assert(result.success == false);
    return .{ success = false, error = result.error };
}

// this assumes that the Result object is the only return value.
or_return :: (result: Result($T), $R: Type) -> T #expand {
    if !result  `return recast_error(R, result); 
    return result.value;
}

// this assumes that the Result object is the only return value.
or_return :: (result: Result($T)) -> T #expand {
    if !result  `return result;
    return result.value;
}

or_default :: inline (result: Result($T), default: T) -> T {
    if !result  return default;
    return result.value;
}

// NOTE: I considered putting bool first so it's harder to ignore, 
// but on the other hand, then it's harder to reuse the ok variable in caller scope.
// ALSO NOTE: value or error will be junk data depending on result.success.
unbox :: inline (result: Result($T)) -> (T, string, bool) {
    return result.value, result.error, result.success;
}

// TODO: would like to be able to append to error string, maybe?

// TODO: triple left shift operator overload as an or_return? would be cool...
//       but jai does not allow #expand on operators I think. never actually tried though...


