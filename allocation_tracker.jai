
/*
    This is intended to be a super basic tracking allocator. 
    Not intended for debugging purposes, just to make sure we can free any data that is allocated by the module so that the user does not have to worry about it.
*/

Allocation_Tracker :: struct {
    allocator:   Allocator;
    allocations: *[..] Allocation;
    
    Allocation :: struct {
        allocator:  Allocator;
        memory:     *void;
        // size:       int;
    }
    
    allocator_proc :: (mode: Allocator_Mode, size: s64, old_size: s64, old_memory: *void, allocator_data: *void) -> *void {
        tracker := allocator_data.(*Allocation_Tracker);
        
        if tracker == null {
            log("tracker == null");
            return null;
        }
        
        if tracker.allocations == null {
            log("tracker.allocations == null");
            return null;
        }
        
        if tracker.allocator.proc == null {
            log("tracker.allocator.proc == null");
            return null;
        }
        
        if tracker.allocations.allocator.proc == null {
            tracker.allocations.allocator = Context_Base.default_allocator;
        }
        
        if mode == {
          case .ALLOCATE;
            memory := tracker.allocator.proc(mode, size, old_size, old_memory, tracker.allocator.data);
            array_add(tracker.allocations, .{ tracker.allocator, memory });
            return memory;
            
          case .RESIZE;
            memory := tracker.allocator.proc(mode, size, old_size, old_memory, tracker.allocator.data);
            if memory != old_memory {
                for (tracker.allocations.*) {
                    if it.memory == old_memory then remove;
                }
                array_add(tracker.allocations, .{ tracker.allocator, memory });
            }
            return memory;
            
          case .FREE;
            memory := tracker.allocator.proc(mode, size, old_size, old_memory, tracker.allocator.data);
            for (tracker.allocations.*) {
                if it.memory == old_memory then remove;
            }
            return memory;
            
          case;
            return tracker.allocator.proc(mode, size, old_size, old_memory, tracker.allocator.data);
        }
        
        return null;
    }
}

wrap_allocator_for_tracking :: (allocator: Allocator, allocations: *[..] Allocation_Tracker.Allocation) -> Allocator #expand {
    tracker := Allocation_Tracker.{ allocator, allocations };
    return .{ Allocation_Tracker.allocator_proc, *tracker };
}

list_all_allocations :: (allocations: [..] Allocation_Tracker.Allocation) {
    log("Tracked % allocations:");
    for allocations {
        log("\t% allocated by %", it.memory, it.allocator.proc);
    }
}


