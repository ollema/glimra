import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn nix_tests() {
  describe("nix expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Nix,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Nix, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(languages.Nix, "snippet", themes.GithubDark)
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Nix,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Nix, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Nix, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Nix, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(languages.Nix, "snippet", themes.OneDarkPro)
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(languages.Nix, "snippet", themes.TokyoNight)
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Nix,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn nix_ast_tests() {
  describe("nix expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Nix)
    }),
  ])
}

pub fn nix_regex_plus_ast_tests() {
  describe("nix expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Nix)
    }),
  ])
}

pub fn nix_generated_tests() {
  describe("nix expected generated validation", [
    it("generates all patterns correctly", fn() {
      test_helpers.validate_expected_generated(languages.Nix)
    }),
  ])
}

pub fn nix_recursion_tests() {
  describe("nix expected recursion validation", [
    it("processes all patterns correctly", fn() {
      test_helpers.validate_expected_recursion(languages.Nix)
    }),
  ])
}
