import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn mojo_tests() {
  describe("mojo expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Mojo,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Mojo, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Mojo,
        "snippet",
        themes.GithubDark,
      )
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Mojo,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Mojo, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Mojo, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Mojo, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(
        languages.Mojo,
        "snippet",
        themes.OneDarkPro,
      )
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(
        languages.Mojo,
        "snippet",
        themes.TokyoNight,
      )
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Mojo,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn mojo_ast_tests() {
  describe("mojo expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Mojo)
    }),
  ])
}

pub fn mojo_regex_plus_ast_tests() {
  describe("mojo expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Mojo)
    }),
  ])
}

pub fn mojo_generated_tests() {
  describe("mojo expected generated validation", [
    it("generates all patterns correctly", fn() {
      test_helpers.validate_expected_generated(languages.Mojo)
    }),
  ])
}

pub fn mojo_recursion_tests() {
  describe("mojo expected recursion validation", [
    it("processes all patterns correctly", fn() {
      test_helpers.validate_expected_recursion(languages.Mojo)
    }),
  ])
}
