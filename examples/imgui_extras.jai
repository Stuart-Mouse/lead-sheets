
// TODO: optionally pass *Context to callback?
InputTextDynamicCallbackUserData :: struct {
    buf                   : *[..] u8;
    ChainCallback         : ImGui.InputTextCallback;
    ChainCallbackUserData : *void;
}

InputTextDynamicCallback : ImGui.InputTextCallback : (data: *ImGui.InputTextCallbackData) -> s32 #c_call {
    new_context: #Context;
    push_context new_context {
        user_data := cast(*InputTextDynamicCallbackUserData) data.UserData;
        if data.EventFlag & .CallbackResize {
            new_len := cast(u32) data.BufTextLen;
            new_cap := max(8, user_data.buf.allocated, next_power_of_2(new_len));
            array_reserve(user_data.buf, new_cap);
            user_data.buf.count = new_len;
            data.Buf = user_data.buf.data;
            return 0;
        }
        if user_data.ChainCallback != null {
            data.UserData = user_data.ChainCallbackUserData;
            return user_data.ChainCallback(data);
        }
    }
    return 0;
}

InputTextDynamic :: (
    label     : *u8, 
    buf       : *[..] u8, 
    flags     : ImGui.InputTextFlags    = xx 0, 
    callback  : ImGui.InputTextCallback = null, 
    user_data : *void                   = null
) -> bool {
    if !buf.data {
        array_reserve(buf, 8);
        buf.data[0] = 0;
    }
    cb_user_data : InputTextDynamicCallbackUserData = .{
        buf                   = buf,
        ChainCallback         = callback,
        ChainCallbackUserData = user_data,
    };
    return ImGui.InputText(label, buf.data, cast(u64) buf.allocated, flags | .CallbackResize, InputTextDynamicCallback, *cb_user_data);
}

// InputTextWithHintDynamic :: (
//     label     : *u8, 
//     hint      : *u8, 
//     buf       : *[..] u8, 
//     flags     : ImGui.InputTextFlags    = xx 0, 
//     callback  : ImGui.InputTextCallback = null, 
//     user_data : *void                   = null
// ) -> bool {
//     cb_user_data : InputTextDynamicCallbackUserData = .{
//         buf                   = buf,
//         ChainCallback         = callback,
//         ChainCallbackUserData = user_data,
//     };
//     return InputTextWithHint(label, hint, *buf.*[0], buf.allocated, flags | .CallbackResize, InputTextDynamicCallback, *cb_user_data);
// }

// InputTextMultilineDynamic :: (
//     label     : *u8, 
//     buf       : *[..] u8, 
//     size      : Vector2                 = .{}, 
//     flags     : ImGui.InputTextFlags    = xx 0, 
//     callback  : ImGui.InputTextCallback = null, 
//     user_data : *void                   = null
// ) -> bool {
//     cb_user_data : InputTextDynamicCallbackUserData = .{
//         buf                   = buf,
//         ChainCallback         = callback,
//         ChainCallbackUserData = user_data,
//     };
//     return InputTextMultiline(label, *buf.*[0], buf.allocated, size, flags | .CallbackResize, InputTextDynamicCallback, *cb_user_data);
// }

ComboTypes :: (
    label            : *u8,
    value            : *Type,
    types            : [] Type, 
    combo_flags      : ImGui.ComboFlags      = xx 0, 
    selectable_flags : ImGui.SelectableFlags = xx 0
) {
    if ImGui.BeginCombo(label, tprint("%\0", value).data, combo_flags) {
        for types {
            if ImGui.Selectable(
                label    = tprint("%\0", it).data, 
                selected = value.* == it, 
                flags    = selectable_flags
            ) {
                value.* = it;
            }
        }
        ImGui.EndCombo();
    }
}

ComboEnum :: (
    label            : *u8,
    value            : *$T, 
    combo_flags      : ImGui.ComboFlags      = xx 0, 
    selectable_flags : ImGui.SelectableFlags = xx 0
) #modify {
    return (cast(*Type_Info)T).type == .ENUM;
} {
    enum_ti := cast(*Type_Info_Enum) type_info(T);
    
    current_value_index := 0;
    for enum_ti.values {
        if value.* == xx it {
            current_value_index = it_index;
            break;
        }
    }
    current_value_name := enum_ti.names[current_value_index];
    
    if ImGui.BeginCombo(label, current_value_name.data, combo_flags) {
        for name: enum_ti.names {
            if ImGui.Selectable(
                label    = name.data, 
                selected = value.* == xx enum_ti.values[it_index], 
                flags    = selectable_flags
            ) {
                value.* = xx enum_ti.values[it_index];
            }
        }
        ImGui.EndCombo();
    }
}


ComboEnumDynamic :: (
    label            : *u8,
    value            : Any, 
    combo_flags      : ImGui.ComboFlags      = xx 0, 
    selectable_flags : ImGui.SelectableFlags = xx 0
) {
    if value.type.type != .ENUM  return;
    ti_enum := cast(*Type_Info_Enum) value.type;
    
    s64_value: s64;
    if ti_enum.runtime_size == {
        case 1; s64_value = xx,no_check (cast(*u8 )(value.value_pointer)).*;
        case 2; s64_value = xx,no_check (cast(*u16)(value.value_pointer)).*;
        case 4; s64_value = xx,no_check (cast(*u32)(value.value_pointer)).*;
        case 8; s64_value = xx,no_check (cast(*u64)(value.value_pointer)).*;
    }
    
    found, current_value_index := array_find(ti_enum.values, s64_value);
    if !found then current_value_index = 0;
    
    current_value_name  := ti_enum.names[current_value_index];
    
    if ImGui.BeginCombo(label, current_value_name.data, combo_flags) {
        for name: ti_enum.names {
            if ImGui.Selectable(
                label    = name.data, 
                selected = s64_value == xx ti_enum.values[it_index], 
                flags    = selectable_flags
            ) {
                s64_value = xx ti_enum.values[it_index];
            }
        }
        ImGui.EndCombo();
    }
    
    if ti_enum.runtime_size == {
        case 1; s64_value = xx,no_check (cast(*u8 )(value.value_pointer)).*;
        case 2; s64_value = xx,no_check (cast(*u16)(value.value_pointer)).*;
        case 4; s64_value = xx,no_check (cast(*u32)(value.value_pointer)).*;
        case 8; s64_value = xx,no_check (cast(*u64)(value.value_pointer)).*;
    }
}


TreeNodeAny :: (
    label : *u8,
    value : Any,
    flags : ImGui.TreeNodeFlags = .None
) {
    if value.type.type == {
      case .STRUCT;
        if ImGui.TreeNodeEx(label, flags) {
            ti_struct := cast(*Type_Info_Struct) value.type;
            for ti_struct.members {
                if it.flags & .CONSTANT  continue;
                TreeNodeAny(it.name.data, Any.{ it.type, cast(*u8) value.value_pointer + it.offset_in_bytes }, flags);
            }
            ImGui.TreePop();
        }
        
      case .ARRAY;
        if ImGui.TreeNodeEx(label, flags) {
            ti_array := cast(*Type_Info_Array) value.type;
            
            data       : *void = ---;
            elem_count : int   = ---;
                    
            if ti_array.array_type == .FIXED {
                data       = value.value_pointer;
                elem_count = ti_array.array_count;
            } else {
                raw_array := cast(*Resizable_Array) value.value_pointer;
                data       = raw_array.data;
                elem_count = raw_array.count;
            }
            
            member_any := Any.{ ti_array.element_type, data };
            for 0..elem_count-1 {
                TreeNodeAny(tprint("%[%]\0", string.{c_style_strlen(label), label}, it).data, flags);
                member_any.value_pointer = cast(*u8) member_any.value_pointer + ti_array.element_type.runtime_size;
            }
            ImGui.TreePop();
        }
        
      case .ENUM;
        ComboEnumDynamic(label, value);
        
      case .INTEGER;
        s32_value: s32;
        dynamic_int_cast(s32_value, value);
        ImGui.InputInt(label, *s32_value);
        dynamic_int_cast(value, s32_value);
        return;
        
      case .FLOAT;
        f32_value: float32;
        dynamic_int_cast(f32_value, value);
        ImGui.InputFloat(label, *f32_value);
        dynamic_int_cast(value, f32_value);
        return;
        
      case .BOOL;
        bool_value: bool;
        dynamic_int_cast(bool_value, value);
        if (ImGui.RadioButton(label, bool_value)) { 
            bool_value = !bool_value;
        } 
        dynamic_int_cast(value, bool_value);
        return;
        
      case;
        ImGui.BulletText("%: \"%\"", label, value);
        return;

    }
}

// TreeNodePoly :: (
//     label : *u8,
//     value : Any,
//     flags : ImGui.TreeNodeFlags = .None
// ) {
//     TI :: type_info(T);
    
//     #if value.type.type == {
//       case .STRUCT;
//         if ImGui.TreeNodeEx(label, flags) {
//             ti_struct := cast(*Type_Info_Struct) value.type;
//             for ti_struct.members {
//                 if it.flags & .CONSTANT  continue;
//                 TreeNodeAny(it.name.data, Any.{ it.type, cast(*u8) value.value_pointer + it.offset_in_bytes }, flags);
//             }
//             ImGui.TreePop();
//         }
        
//       case .ARRAY;
//         if ImGui.TreeNodeEx(label, flags) {
//             ti_array := cast(*Type_Info_Array) value.type;
            
//             data       : *void = ---;
//             elem_count : int   = ---;
                    
//             if ti_array.array_type == .FIXED {
//                 data       = value.value_pointer;
//                 elem_count = ti_array.array_count;
//             } else {
//                 raw_array := cast(*Resizable_Array) value.value_pointer;
//                 data       = raw_array.data;
//                 elem_count = raw_array.count;
//             }
            
//             member_any := Any.{ ti_array.element_type, data };
//             for 0..elem_count-1 {
//                 TreeNodeAny(tprint("%[%]\0", string.{c_style_strlen(label), label}, it).data, flags);
//                 member_any.value_pointer = cast(*u8) member_any.value_pointer + ti_array.element_type.runtime_size;
//             }
//             ImGui.TreePop();
//         }
        
//       case .ENUM;
//         ComboEnumDynamic(label, value);
        
//       case .INTEGER;
//         s32_value: s32;
//         dynamic_int_cast(s32_value, value);
//         ImGui.InputInt(label, *s32_value);
//         dynamic_int_cast(value, s32_value);
//         return;
        
//       case .FLOAT;
//         f32_value: float32;
//         dynamic_int_cast(f32_value, value);
//         ImGui.InputFloat(label, *f32_value);
//         dynamic_int_cast(value, f32_value);
//         return;
        
//       case .BOOL;
//         bool_value: bool;
//         dynamic_int_cast(bool_value, value);
//         if (ImGui.RadioButton(label, bool_value)) { 
//             bool_value = !bool_value;
//         } 
//         dynamic_int_cast(value, bool_value);
//         return;
        
//       case;
//         ImGui.BulletText("%: \"%\"", label, value);
//         return;

//     }
// }



#scope_file

next_power_of_2 :: (num: u32) -> u32 {
    if num != 0 {
        num -= 1;
        num |= (num >> 1);  // OR first 2 bits
        num |= (num >> 2);  // OR next  2 bits
        num |= (num >> 4);  // OR next  4 bits
        num |= (num >> 8);  // OR next  8 bits
        num |= (num >> 16); // OR next 16 bits
        num += 1;
    }
    return num;
}

dynamic_int_cast :: (dst: Any, src: Any, enforce_size: bool = false) -> bool {
    if enforce_size && src.type.runtime_size > dst.type.runtime_size {
        return false;
    }
    
    if src.type.type == {
        case .INTEGER;
        case .ENUM;
        case .BOOL;
             
        case; return false;
    }
    if dst.type.type == {
        case .INTEGER;
        case .ENUM;
        case .BOOL;
             
        case; return false;
    }
    
    s64_value: s64;
  
    if src.type.runtime_size == {
        case  1; s64_value = xx (cast(*s8  )src.value_pointer).*;
        case  2; s64_value = xx (cast(*s16 )src.value_pointer).*;
        case  4; s64_value = xx (cast(*s32 )src.value_pointer).*;
        case  8; s64_value = xx (cast(*s64 )src.value_pointer).*;
        // case 16; s64_value = xx (cast(*i128)src.data).*;
    }
  
    if dst.type.runtime_size == {
        case  1; (cast(*s8  )dst.value_pointer).* = xx s64_value;
        case  2; (cast(*s16 )dst.value_pointer).* = xx s64_value;
        case  4; (cast(*s32 )dst.value_pointer).* = xx s64_value;
        case  8; (cast(*s64 )dst.value_pointer).* = xx s64_value;
        // case 16; (cast(*i128)dst.data).* = xx s64_value;
    }
  
    return true;
}

dynamic_float_cast :: (dst: Any, src: Any) -> bool {
    if dst.type.type != .FLOAT  return false;
    
    f64_value: float64;
    
    if src.type.type == {
        case .FLOAT;
            if src.type.runtime_size == {
                // case 2; f64_value = xx (cast(*f16)src.value_pointer).*;
                case 4; f64_value = xx (cast(*float32)src.value_pointer).*;
                case 8; f64_value = xx (cast(*float64)src.value_pointer).*;
            }
        case .INTEGER;
            if src.type.runtime_size == {
                case  1; f64_value = xx (cast(*s8  )src.value_pointer).*;
                case  2; f64_value = xx (cast(*s16 )src.value_pointer).*;
                case  4; f64_value = xx (cast(*s32 )src.value_pointer).*;
                case  8; f64_value = xx (cast(*s64 )src.value_pointer).*;
                // case 16; f64_value = xx (cast(*s128)src.value_pointer).*;
            }
        case;
            return false;
    }
    
    if dst.type.runtime_size == {
        // case 2; (cast(*f16)dst.data).* = xx f64_value;
        case 4; (cast(*float32)dst.value_pointer).* = xx f64_value;
        case 8; (cast(*float64)dst.value_pointer).* = xx f64_value;
    }
    
    return true;
}