import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn markdown_tests() {
  describe("markdown expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.Dracula,
      )
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.GithubDark,
      )
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.Monokai,
      )
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.NightOwl,
      )
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Markdown, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.OneDarkPro,
      )
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.TokyoNight,
      )
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Markdown,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn markdown_ast_tests() {
  describe("markdown expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Markdown)
    }),
  ])
}

pub fn markdown_regex_plus_ast_tests() {
  describe("markdown expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Markdown)
    }),
  ])
}

pub fn markdown_generated_tests() {
  describe("markdown expected generated validation", [
    it("generates all patterns correctly", fn() {
      test_helpers.validate_expected_generated(languages.Markdown)
    }),
  ])
}

pub fn markdown_recursion_tests() {
  describe("markdown expected recursion validation", [
    it("processes all patterns correctly", fn() {
      test_helpers.validate_expected_recursion(languages.Markdown)
    }),
  ])
}
