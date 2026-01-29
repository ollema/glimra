//// Pure Gleam implementation of vscode-textmate's StateStack
////
//// The StateStack is an immutable linked-list that tracks the parser state
//// during tokenization. Each frame represents a grammar rule that has been
//// entered but not yet exited (e.g., a begin/end pair).
////
//// This module also includes AttributedScopeStack for scope names with
//// theme metadata, and ScopeStack for simple scope hierarchies.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Simple linked-list of scope names without theme metadata.
/// Used internally for scope path tracking.
pub type ScopeStack {
  /// Root scope (no parent)
  ScopeRoot(scope_name: String)
  /// Child scope extending a parent
  ScopeChild(parent: ScopeStack, scope_name: String)
}

/// Create a root scope stack with a single scope name
pub fn scope_stack_root(scope_name: String) -> ScopeStack {
  ScopeRoot(scope_name)
}

/// Push a new scope onto the stack
pub fn scope_stack_push(stack: ScopeStack, scope_name: String) -> ScopeStack {
  ScopeChild(stack, scope_name)
}

/// Get the depth of the scope stack (1-indexed, root = 1)
pub fn scope_stack_depth(stack: ScopeStack) -> Int {
  scope_stack_depth_impl(stack, 1)
}

fn scope_stack_depth_impl(stack: ScopeStack, acc: Int) -> Int {
  case stack {
    ScopeRoot(_) -> acc
    ScopeChild(parent, _) -> scope_stack_depth_impl(parent, acc + 1)
  }
}

/// Attributed scope stack - scope names with encoded token metadata.
///
/// Each node in the linked list contains:
/// - The scope path (hierarchical scope names)
/// - Encoded token attributes (32-bit packed metadata for theming)
/// - Cached scope names list (avoids repeated traversal)
///
/// This is used during tokenization to track both the scope hierarchy
/// and the theme attributes that should be applied.
pub type AttributedScopeStack {
  /// Root attributed scope
  AttributedRoot(
    scope_path: ScopeStack,
    token_attributes: Int,
    /// Cached list of scope names from root to this node
    cached_names: List(String),
  )
  /// Child attributed scope extending a parent
  AttributedChild(
    parent: AttributedScopeStack,
    scope_path: ScopeStack,
    token_attributes: Int,
    /// Cached list of scope names from root to this node
    cached_names: List(String),
  )
}

/// Create a root attributed scope stack
fn attributed_root(
  scope_name: String,
  token_attributes: Int,
) -> AttributedScopeStack {
  AttributedRoot(
    scope_path: ScopeRoot(scope_name),
    token_attributes: token_attributes,
    cached_names: [scope_name],
  )
}

/// Push a new scope name onto the attributed scope stack
/// This properly extends the existing scope path hierarchy
///
/// TextMate grammars can have space-separated scope names in a single "name" field.
/// For example: "meta.definition.variable.js variable.other.constant.js"
/// In this case, we need to push each scope individually.
pub fn attributed_push_scope(
  stack: AttributedScopeStack,
  scope_name: String,
  token_attributes: Int,
) -> AttributedScopeStack {
  // Check for space-separated scopes (common in TextMate grammars)
  case string.contains(scope_name, " ") {
    False -> {
      // Fast path: single scope name (most common case)
      let current_path = attributed_get_scope_path(stack)
      let new_path = scope_stack_push(current_path, scope_name)
      let parent_names = attributed_get_scope_names(stack)
      AttributedChild(
        parent: stack,
        scope_path: new_path,
        token_attributes: token_attributes,
        cached_names: list.append(parent_names, [scope_name]),
      )
    }
    True -> {
      // Split by spaces and push each scope individually
      let scopes = string.split(scope_name, " ")
      attributed_push_scopes(stack, scopes, token_attributes)
    }
  }
}

/// Push multiple scope names onto the attributed scope stack
fn attributed_push_scopes(
  stack: AttributedScopeStack,
  scopes: List(String),
  token_attributes: Int,
) -> AttributedScopeStack {
  case scopes {
    [] -> stack
    [scope, ..rest] -> {
      let current_path = attributed_get_scope_path(stack)
      let new_path = scope_stack_push(current_path, scope)
      let parent_names = attributed_get_scope_names(stack)
      let new_stack =
        AttributedChild(
          parent: stack,
          scope_path: new_path,
          token_attributes: token_attributes,
          cached_names: list.append(parent_names, [scope]),
        )
      attributed_push_scopes(new_stack, rest, token_attributes)
    }
  }
}

/// Get the scope path from an attributed scope stack
fn attributed_get_scope_path(stack: AttributedScopeStack) -> ScopeStack {
  case stack {
    AttributedRoot(scope_path: path, ..) -> path
    AttributedChild(scope_path: path, ..) -> path
  }
}

/// Get all scope names from an attributed scope stack (from root to top)
///
/// Returns the cached scope names list for O(1) access.
pub fn attributed_get_scope_names(stack: AttributedScopeStack) -> List(String) {
  case stack {
    AttributedRoot(cached_names: names, ..) -> names
    AttributedChild(cached_names: names, ..) -> names
  }
}

/// Get the token attributes for the current scope
pub fn attributed_get_token_attributes(stack: AttributedScopeStack) -> Int {
  case stack {
    AttributedRoot(token_attributes: attrs, ..) -> attrs
    AttributedChild(token_attributes: attrs, ..) -> attrs
  }
}

/// The main state stack - tracks parser state during tokenization.
///
/// This is an immutable linked-list where each frame represents a grammar
/// rule that has been entered. The tokenizer uses this to track:
/// - Which rules are currently active
/// - The scope names that should be applied
/// - End patterns for begin/end rules
/// - Position information for anchor matching
///
/// Operations on StateStack return new stacks, never modifying the original.
pub type StateStack {
  /// The NULL/initial state - used as the starting point for tokenization
  StateStackNull
  /// A state frame representing an active grammar rule
  StateStackFrame(
    /// Parent state (previous frame in the stack)
    parent: StateStack,
    /// The ID of the rule that was entered
    rule_id: Int,
    /// Position in the line where this state was entered
    enter_pos: Int,
    /// Anchor position for \G matching
    anchor_pos: Int,
    /// Whether the begin pattern captured EOL
    begin_rule_captured_eol: Bool,
    /// Dynamically resolved end pattern (for back-references)
    /// None if the rule doesn't have an end pattern or no back-refs
    end_rule: Option(String),
    /// Scope names pushed when entering this rule
    name_scopes: Option(AttributedScopeStack),
    /// Scope names for the content of this rule
    content_name_scopes: Option(AttributedScopeStack),
  )
}

/// Get the initial/null state stack
pub fn initial() -> StateStack {
  StateStackNull
}

/// Create an initial state stack with the root scope
/// This should be called when tokenizing with a null state to set up the grammar's root scope
pub fn create_root_state(scope_name: String, rule_id: Int) -> StateStack {
  let root_scope = attributed_root(scope_name, 0)
  StateStackFrame(
    parent: StateStackNull,
    rule_id: rule_id,
    enter_pos: 0,
    anchor_pos: -1,
    begin_rule_captured_eol: False,
    end_rule: None,
    name_scopes: Some(root_scope),
    content_name_scopes: Some(root_scope),
  )
}

/// Push a new frame onto the state stack
pub fn push(
  stack: StateStack,
  rule_id: Int,
  enter_pos: Int,
  anchor_pos: Int,
  begin_rule_captured_eol: Bool,
  end_rule: Option(String),
  name_scopes: Option(AttributedScopeStack),
  content_name_scopes: Option(AttributedScopeStack),
) -> StateStack {
  StateStackFrame(
    parent: stack,
    rule_id: rule_id,
    enter_pos: enter_pos,
    anchor_pos: anchor_pos,
    begin_rule_captured_eol: begin_rule_captured_eol,
    end_rule: end_rule,
    name_scopes: name_scopes,
    content_name_scopes: content_name_scopes,
  )
}

/// Pop the top frame from the state stack, returning the parent
/// Returns StateStackNull if already at the root
pub fn pop(stack: StateStack) -> StateStack {
  case stack {
    StateStackNull -> StateStackNull
    StateStackFrame(parent, ..) -> parent
  }
}

/// Safely pop - returns self if already at root (never returns Null from a Frame)
pub fn safe_pop(stack: StateStack) -> StateStack {
  case stack {
    StateStackNull -> StateStackNull
    StateStackFrame(StateStackNull, ..) -> stack
    StateStackFrame(parent, ..) -> parent
  }
}

/// Get the depth of the state stack (0 for null, 1 for first frame, etc.)
pub fn depth(stack: StateStack) -> Int {
  depth_impl(stack, 0)
}

fn depth_impl(stack: StateStack, acc: Int) -> Int {
  case stack {
    StateStackNull -> acc
    StateStackFrame(parent, ..) -> depth_impl(parent, acc + 1)
  }
}

/// Get the rule ID of the current frame
/// Returns None for the null state
pub fn get_rule_id(stack: StateStack) -> Option(Int) {
  case stack {
    StateStackNull -> None
    StateStackFrame(rule_id: id, ..) -> Some(id)
  }
}

/// Get the enter position of the current frame
pub fn get_enter_pos(stack: StateStack) -> Int {
  case stack {
    StateStackNull -> -1
    StateStackFrame(enter_pos: pos, ..) -> pos
  }
}

/// Reset enter_pos for all frames in the stack to -1
/// This should be called at the start of each new line to prevent
/// cross-line endless loop false positives
pub fn reset_enter_positions(stack: StateStack) -> StateStack {
  case stack {
    StateStackNull -> StateStackNull
    StateStackFrame(
      parent: parent,
      rule_id: rule_id,
      enter_pos: _,
      anchor_pos: anchor_pos,
      begin_rule_captured_eol: captured_eol,
      end_rule: end_rule,
      name_scopes: name_scopes,
      content_name_scopes: content_name_scopes,
    ) ->
      StateStackFrame(
        parent: reset_enter_positions(parent),
        rule_id: rule_id,
        enter_pos: -1,
        anchor_pos: anchor_pos,
        begin_rule_captured_eol: captured_eol,
        end_rule: end_rule,
        name_scopes: name_scopes,
        content_name_scopes: content_name_scopes,
      )
  }
}

/// Get the anchor position of the current frame
pub fn get_anchor_pos(stack: StateStack) -> Int {
  case stack {
    StateStackNull -> -1
    StateStackFrame(anchor_pos: pos, ..) -> pos
  }
}

/// Get the end rule pattern of the current frame
pub fn get_end_rule(stack: StateStack) -> Option(String) {
  case stack {
    StateStackNull -> None
    StateStackFrame(end_rule: rule, ..) -> rule
  }
}

/// Check if the current frame captured EOL in its begin pattern
pub fn get_begin_rule_captured_eol(stack: StateStack) -> Bool {
  case stack {
    StateStackNull -> False
    StateStackFrame(begin_rule_captured_eol: captured, ..) -> captured
  }
}

/// Get the name scopes of the current frame
pub fn get_name_scopes(stack: StateStack) -> Option(AttributedScopeStack) {
  case stack {
    StateStackNull -> None
    StateStackFrame(name_scopes: scopes, ..) -> scopes
  }
}

/// Get the content name scopes of the current frame
pub fn get_content_name_scopes(
  stack: StateStack,
) -> Option(AttributedScopeStack) {
  case stack {
    StateStackNull -> None
    StateStackFrame(content_name_scopes: scopes, ..) -> scopes
  }
}

/// Check if two state stacks have the same rule at their top.
/// Used for detecting infinite loops during tokenization.
pub fn has_same_rule_as(a: StateStack, b: StateStack) -> Bool {
  case a, b {
    StateStackFrame(rule_id: id_a, end_rule: end_a, ..),
      StateStackFrame(rule_id: id_b, end_rule: end_b, ..)
    -> id_a == id_b && end_a == end_b
    _, _ -> False
  }
}

/// Set the content name scopes for the current frame.
/// Returns a new state stack with the updated value.
pub fn set_content_name_scopes(
  stack: StateStack,
  content_name_scopes: Option(AttributedScopeStack),
) -> StateStack {
  case stack {
    StateStackNull -> StateStackNull
    StateStackFrame(
      parent: parent,
      rule_id: rule_id,
      enter_pos: enter_pos,
      anchor_pos: anchor_pos,
      begin_rule_captured_eol: begin_rule_captured_eol,
      end_rule: end_rule,
      name_scopes: name_scopes,
      content_name_scopes: _,
    ) ->
      StateStackFrame(
        parent: parent,
        rule_id: rule_id,
        enter_pos: enter_pos,
        anchor_pos: anchor_pos,
        begin_rule_captured_eol: begin_rule_captured_eol,
        end_rule: end_rule,
        name_scopes: name_scopes,
        content_name_scopes: content_name_scopes,
      )
  }
}

/// Get all scope names from the state stack by collecting from all frames.
/// Returns scope names from root to top.
pub fn get_scope_names(stack: StateStack) -> List(String) {
  get_scope_names_impl(stack, [])
  |> list.flatten
}

fn get_scope_names_impl(
  stack: StateStack,
  acc: List(List(String)),
) -> List(List(String)) {
  case stack {
    StateStackNull -> acc
    StateStackFrame(parent: parent, content_name_scopes: Some(scopes), ..) ->
      get_scope_names_impl(parent, [attributed_get_scope_names(scopes), ..acc])
    StateStackFrame(parent: parent, name_scopes: Some(scopes), ..) ->
      get_scope_names_impl(parent, [attributed_get_scope_names(scopes), ..acc])
    StateStackFrame(parent: parent, ..) -> get_scope_names_impl(parent, acc)
  }
}
