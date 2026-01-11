use std::collections::HashSet;
use std::rc::Rc;

/// String interner for deduplicating strings
pub struct StringInterner {
    strings: HashSet<Rc<String>>,
}

impl StringInterner {
    /// Creates a new empty string interner
    pub fn new() -> Self {
        Self {
            strings: HashSet::new(),
        }
    }

    /// Interns a string, returning an Rc to either an existing interned string
    /// or a newly created one if this string hasn't been seen before
    pub fn intern(&mut self, s: &str) -> Rc<String> {
        // Try to find an existing interned string
        for existing in self.strings.iter() {
            if existing.as_str() == s {
                return Rc::clone(existing);
            }
        }

        // Create new interned string
        let rc = Rc::new(s.to_string());
        self.strings.insert(Rc::clone(&rc));
        rc
    }

    /// Returns the number of unique interned strings
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Returns true if the interner is empty
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern_same_string_returns_same_rc() {
        let mut interner = StringInterner::new();
        let s1 = interner.intern("hello");
        let s2 = interner.intern("hello");

        // Should point to the same allocation
        assert!(Rc::ptr_eq(&s1, &s2));
    }

    #[test]
    fn test_intern_different_strings() {
        let mut interner = StringInterner::new();
        let s1 = interner.intern("hello");
        let s2 = interner.intern("world");

        // Should be different strings
        assert_ne!(s1, s2);
    }

    #[test]
    fn test_intern_count() {
        let mut interner = StringInterner::new();
        interner.intern("hello");
        interner.intern("world");
        interner.intern("hello"); // Duplicate

        assert_eq!(interner.len(), 2);
    }
}
