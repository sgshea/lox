use std::any::Any;
use std::collections::HashMap;
use std::marker::PhantomData;

use crate::chunk::Value;

/// Trait for objects that can be traced by the garbage collector
pub trait GcTrace {
    /// Report all GC-managed references this object holds
    fn trace(&self, gc: &mut Gc);

    /// Downcast to Any for type-safe dereferencing
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

/// A typed reference to a GC-managed object
/// This is just an index into the GC's object array
pub struct GcRef<T: GcTrace> {
    index: usize,
    _marker: PhantomData<T>,
}

impl<T: GcTrace> GcRef<T> {
    fn new(index: usize) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }
}

// Manual Clone implementation - doesn't require T: Clone
impl<T: GcTrace> Clone for GcRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

// Manual Copy implementation - just copies the index
impl<T: GcTrace> Copy for GcRef<T> {}

impl<T: GcTrace> std::fmt::Debug for GcRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GcRef({})", self.index)
    }
}

impl<T: GcTrace> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T: GcTrace> Eq for GcRef<T> {}

impl<T: GcTrace> std::hash::Hash for GcRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

/// Internal object header with GC metadata
struct GcObject {
    is_marked: bool,
    data: Box<dyn GcTrace>,
}

/// Error type for GC operations
#[derive(Debug, Clone, Copy)]
pub struct GcError;

impl std::fmt::Display for GcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Out of memory")
    }
}

impl std::error::Error for GcError {}

/// The garbage collector
///
/// This is a mark-sweep garbage collector that manages all heap-allocated objects.
/// Objects are stored in a vector and referenced by index through GcRef handles.
pub struct Gc {
    /// All allocated objects (Some) and freed slots (None)
    objects: Vec<Option<GcObject>>,
    /// Stack of freed indices that can be reused
    free_slots: Vec<usize>,
    /// Interned strings for deduplication
    strings: HashMap<String, GcRef<String>>,
    /// Gray stack for the mark phase (worklist of objects to trace)
    grey_stack: Vec<usize>,
    /// Total bytes allocated
    bytes_allocated: usize,
    /// Threshold for triggering next GC
    next_gc: usize,
}

impl Gc {
    /// Growth factor for determining next GC threshold
    const HEAP_GROW_FACTOR: usize = 2;

    /// Initial GC threshold (1 MB)
    const INITIAL_GC_THRESHOLD: usize = 1024 * 1024;

    /// Create a new garbage collector
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            free_slots: Vec::new(),
            strings: HashMap::new(),
            grey_stack: Vec::new(),
            bytes_allocated: 0,
            next_gc: Self::INITIAL_GC_THRESHOLD,
        }
    }

    /// Allocate a new object on the GC heap
    pub fn alloc<T: GcTrace + 'static>(&mut self, object: T) -> Result<GcRef<T>, GcError> {
        // Estimate size
        let size = std::mem::size_of::<T>();
        self.bytes_allocated += size;

        let gc_object = GcObject {
            is_marked: false,
            data: Box::new(object),
        };

        // Try to reuse free slot first
        let index = if let Some(free_index) = self.free_slots.pop() {
            self.objects[free_index] = Some(gc_object);
            free_index
        } else {
            self.objects.push(Some(gc_object));
            self.objects.len() - 1
        };

        Ok(GcRef::new(index))
    }

    /// Intern string, returning a reference to either an existing or new string
    pub fn intern(&mut self, s: &str) -> Result<GcRef<String>, GcError> {
        // Check if already interned
        if let Some(&existing) = self.strings.get(s) {
            return Ok(existing);
        }

        // Allocate new string
        let string_ref = self.alloc(s.to_string())?;
        self.strings.insert(s.to_string(), string_ref);
        Ok(string_ref)
    }

    /// Dereference a GC reference (immutable)
    pub fn deref<T: GcTrace + 'static>(&self, reference: GcRef<T>) -> &T {
        self.objects[reference.index]
            .as_ref()
            .expect("Dereferencing freed object")
            .data
            .as_any()
            .downcast_ref::<T>()
            .expect("Type mismatch in GC dereference")
    }

    /// Dereference a GC reference (mutable)
    pub fn deref_mut<T: GcTrace + 'static>(&mut self, reference: GcRef<T>) -> &mut T {
        self.objects[reference.index]
            .as_mut()
            .expect("Dereferencing freed object")
            .data
            .as_any_mut()
            .downcast_mut::<T>()
            .expect("Type mismatch in GC dereference")
    }

    /// Mark an object as reachable
    pub fn mark_object<T: GcTrace>(&mut self, obj: GcRef<T>) {
        if let Some(object) = self.objects[obj.index].as_mut() {
            if object.is_marked {
                return; // Already marked
            }

            object.is_marked = true;
            self.grey_stack.push(obj.index);
        }
    }

    /// Mark a value (which may contain object references)
    pub fn mark_value(&mut self, value: &Value) {
        match value {
            Value::String(s) => self.mark_object(*s),
            Value::Function(f) => self.mark_object(*f),
            Value::Closure(c) => self.mark_object(*c),
            Value::NativeFunction(n) => self.mark_object(*n),
            _ => {} // Numbers, bools, nil have no references
        }
    }

    /// Check if a GC should be triggered
    pub fn should_gc(&self) -> bool {
        self.bytes_allocated > self.next_gc
    }

    /// Run a garbage collection cycle
    ///
    /// performs a mark-sweep collection:
    /// 1. Trace through all reachable objects (mark phase)
    /// 2. Remove unreachable strings from intern table
    /// 3. Free all unmarked objects (sweep phase)
    /// 4. Adjust the next GC threshold
    pub fn collect_garbage(&mut self) {
        self.trace_references();
        self.remove_white_strings();
        self.sweep();

        // Adjust next GC threshold
        self.next_gc = self.bytes_allocated * Self::HEAP_GROW_FACTOR;
    }

    /// Trace through all gray objects, marking their references
    fn trace_references(&mut self) {
        while let Some(index) = self.grey_stack.pop() {
            self.blacken_object(index);
        }
    }

    /// Blacken an object by tracing its references
    fn blacken_object(&mut self, index: usize) {
        // We need to temporarily take the object out to call trace
        let object = self.objects[index].take();

        if let Some(obj) = object.as_ref() {
            obj.data.trace(self);
        }

        // Put it back
        self.objects[index] = object;
    }

    /// Remove white (unmarked) strings from the intern table
    fn remove_white_strings(&mut self) {
        self.strings.retain(|_, &mut string_ref| {
            self.objects[string_ref.index]
                .as_ref()
                .map_or(false, |obj| obj.is_marked)
        });
    }

    /// Sweep phase - free all unmarked objects
    fn sweep(&mut self) {
        for i in 0..self.objects.len() {
            if let Some(object) = self.objects[i].as_mut() {
                if object.is_marked {
                    // Keep it but clear mark for next cycle
                    object.is_marked = false;
                } else {
                    // Free it
                    self.free(i);
                }
            }
        }
    }

    /// Free an object at the given index
    fn free(&mut self, index: usize) {
        if let Some(object) = self.objects[index].take() {
            // Estimate size
            let size = std::mem::size_of_val(&*object.data);
            self.bytes_allocated = self.bytes_allocated.saturating_sub(size);
            self.free_slots.push(index);
        }
    }
}

impl Default for Gc {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl GcTrace for i32 {
        fn trace(&self, _gc: &mut Gc) {}
        fn as_any(&self) -> &dyn Any { self }
        fn as_any_mut(&mut self) -> &mut dyn Any { self }
    }

    #[test]
    fn test_alloc_and_deref() {
        let mut gc = Gc::new();
        let obj_ref = gc.alloc(42i32).unwrap();
        assert_eq!(*gc.deref(obj_ref), 42);
    }

    #[test]
    fn test_intern_strings() {
        let mut gc = Gc::new();
        let s1 = gc.intern("hello").unwrap();
        let s2 = gc.intern("hello").unwrap();

        // Should be the same reference
        assert_eq!(s1, s2);
        assert_eq!(gc.strings.len(), 1);
    }

    #[test]
    fn test_gc_threshold() {
        let gc = Gc::new();
        assert!(!gc.should_gc()); // Initially under threshold
    }
}
