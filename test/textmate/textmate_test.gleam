//// Unit tests for pure Gleam TextMate implementation
////
//// These tests verify the grammar compilation and state management
//// without requiring OnigScanner initialization. Full tokenization
//// tests would require the OnigScanner to be set up.

import gleam/dict
import gleam/option.{None, Some}
import glimra/textmate/raw_grammar.{
  BaseReference, RawRule, RelativeReference, SelfReference, TopLevelReference,
  TopLevelRepositoryReference, begin_end_rule, include_rule, is_begin_end_rule,
  is_include_only_rule, is_match_rule, match_rule, new_raw_grammar, new_raw_rule,
  parse_include,
}
import glimra/textmate/state_stack.{
  StateStackNull, depth, pop, push, scope_stack_depth, scope_stack_push,
  scope_stack_root,
}
import startest.{describe, it}
import startest/expect

pub fn textmate_tests() {
  describe("textmate", [
    describe("parse_include", [
      it("parses $self reference", fn() {
        parse_include("$self")
        |> expect.to_equal(SelfReference)
      }),
      it("parses $base reference", fn() {
        parse_include("$base")
        |> expect.to_equal(BaseReference)
      }),
      it("parses relative reference", fn() {
        parse_include("#comments")
        |> expect.to_equal(RelativeReference("comments"))
      }),
      it("parses top level reference", fn() {
        parse_include("source.javascript")
        |> expect.to_equal(TopLevelReference("source.javascript"))
      }),
      it("parses top level repository reference", fn() {
        parse_include("source.javascript#expression")
        |> expect.to_equal(TopLevelRepositoryReference(
          "source.javascript",
          "expression",
        ))
      }),
    ]),
    describe("raw grammar", [
      it("creates new grammar with scope name", fn() {
        let grammar = new_raw_grammar("source.test")
        grammar.scope_name
        |> expect.to_equal("source.test")
        grammar.patterns
        |> expect.to_equal([])
      }),
      it("creates match rule", fn() {
        let rule = match_rule("\\bfn\\b", Some("keyword.function"), dict.new())
        rule.match_pattern
        |> expect.to_equal(Some("\\bfn\\b"))
        rule.name
        |> expect.to_equal(Some("keyword.function"))
      }),
      it("creates begin/end rule", fn() {
        let rule =
          begin_end_rule(
            "\"",
            "\"",
            Some("string.quoted.double"),
            None,
            dict.new(),
            dict.new(),
            [],
          )
        rule.begin
        |> expect.to_equal(Some("\""))
        rule.end
        |> expect.to_equal(Some("\""))
        rule.name
        |> expect.to_equal(Some("string.quoted.double"))
      }),
      it("creates include rule", fn() {
        let rule = include_rule("#comments")
        rule.include
        |> expect.to_equal(Some("#comments"))
      }),
    ]),
    describe("scope stack", [
      it("has depth 1 at root", fn() {
        let stack = scope_stack_root("source.test")
        scope_stack_depth(stack)
        |> expect.to_equal(1)
      }),
      it("increases depth on push", fn() {
        let stack =
          scope_stack_root("source.test")
          |> scope_stack_push("meta.function")
          |> scope_stack_push("entity.name.function")

        scope_stack_depth(stack)
        |> expect.to_equal(3)
      }),
    ]),
    describe("state stack", [
      it("has depth 0 when null", fn() {
        depth(StateStackNull)
        |> expect.to_equal(0)
      }),
      it("increases depth on push", fn() {
        let stack =
          StateStackNull
          |> push(1, 0, 0, False, None, None, None)

        depth(stack)
        |> expect.to_equal(1)
      }),
      it("decreases depth on pop", fn() {
        let stack =
          StateStackNull
          |> push(1, 0, 0, False, None, None, None)
          |> push(2, 5, 5, False, None, None, None)

        depth(stack)
        |> expect.to_equal(2)

        let popped = pop(stack)
        depth(popped)
        |> expect.to_equal(1)
      }),
    ]),
    describe("rule type checks", [
      it("identifies match rule", fn() {
        let rule = match_rule("\\bfn\\b", None, dict.new())
        is_match_rule(rule)
        |> expect.to_be_true()
      }),
      it("identifies begin/end rule", fn() {
        let rule =
          begin_end_rule("\"", "\"", None, None, dict.new(), dict.new(), [])
        is_begin_end_rule(rule)
        |> expect.to_be_true()
      }),
      it("identifies include-only rule", fn() {
        let rule = include_rule("#comments")
        is_include_only_rule(rule)
        |> expect.to_be_true()
      }),
      it("identifies include-only rule with patterns", fn() {
        let rule = RawRule(..new_raw_rule(), patterns: [include_rule("#a")])
        is_include_only_rule(rule)
        |> expect.to_be_true()
      }),
    ]),
  ])
}
