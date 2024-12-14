use std::path::{Path, PathBuf};

pub trait WithoutFileExtension {
  /// Removes the file extension from a `Path` and returns a new `PathBuf`.
  fn without_extension(&self) -> PathBuf;
}

impl WithoutFileExtension for Path {
  fn without_extension(&self) -> PathBuf {
    if let Some(stem) = self.file_stem() {
      if let Some(parent) = self.parent() {
        parent.join(stem)
      } else {
        PathBuf::from(stem)
      }
    } else {
      self.to_path_buf()
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_without_extension() {
    let path = Path::new("example/file.txt");
    assert_eq!(path.without_extension(), PathBuf::from("example/file"));

    let path_no_extension = Path::new("example/file");
    assert_eq!(
      path_no_extension.without_extension(),
      PathBuf::from("example/file")
    );

    let path_root = Path::new("file.txt");
    assert_eq!(path_root.without_extension(), PathBuf::from("file"));

    let path_no_parent = Path::new("file");
    assert_eq!(path_no_parent.without_extension(), PathBuf::from("file"));
  }
}
