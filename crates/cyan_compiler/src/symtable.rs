use std::collections::HashMap;

use crate::types::Type;

pub struct SymtableEntry {
  pub typ: Type,
  pub is_defined: bool,
  pub stack_frame_size: isize,
}

pub struct Symtable {
  entries: HashMap<String, SymtableEntry>,
}

impl Symtable {
  pub fn new() -> Self {
    Self {
      entries: HashMap::new(),
    }
  }

  pub fn is_defined(&self, name: &str) -> bool {
    self.entries.contains_key(name)
  }

  pub fn add_var(&mut self, name: &str, typ: Type) {
    self.entries.insert(
      name.to_string(),
      SymtableEntry {
        typ,
        is_defined: false,
        stack_frame_size: 0,
      },
    );
  }

  pub fn add_func(&mut self, name: &str, typ: Type, is_defined: bool) {
    self.entries.insert(
      name.to_string(),
      SymtableEntry {
        typ,
        is_defined,
        stack_frame_size: 0,
      },
    );
  }

  pub fn get(&self, name: &str) -> Option<&SymtableEntry> {
    self.entries.get(name)
  }

  pub fn set_stack_frame_size(&mut self, name: &str, stack_frame_size: isize) {
    if let Some(entry) = self.entries.get_mut(name) {
      entry.stack_frame_size = stack_frame_size;
    }
  }
}
