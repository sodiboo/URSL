use tree_sitter::{Node, TreeCursor};

use crate::defs::Position;

pub trait CursorExt<'a> {
    fn skip_comment(&mut self) -> bool;
    fn next(&mut self) -> bool;
    fn down(&mut self) -> bool;
    fn up(&mut self) -> bool;
}

impl<'a> CursorExt<'a> for TreeCursor<'a> {
    fn skip_comment(&mut self) -> bool {
        while self.node().kind() == "comment" {
            if !self.goto_next_sibling() {
                return false;
            }
        }
        true
    }

    fn next(&mut self) -> bool {
        if self.goto_next_sibling() {
            self.skip_comment()
        } else {
            false
        }
    }

    fn down(&mut self) -> bool {
        if self.goto_first_child() {
            self.skip_comment()
        } else {
            false
        }
    }

    fn up(&mut self) -> bool {
        if self.goto_parent() {
            self.next()
        } else {
            false
        }
    }
}

pub trait NodeExt {
    fn pos(&self) -> Position;
    fn end_pos(&self) -> Position;
    fn text<'a>(&self, source: &'a str) -> &'a str;
    fn field(&self, name: &str) -> Self;
}

impl NodeExt for Node<'_> {
    fn pos(&self) -> Position {
        self.start_position().into()
    }

    fn end_pos(&self) -> Position {
        self.end_position().into()
    }

    fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.byte_range()]
    }

    fn field(&self, name: &str) -> Self {
        self.child_by_field_name(name).expect("Badly formatted syntax tree")
    }
}
