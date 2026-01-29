import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn clojure_tests() {
  describe("clojure expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.Dracula,
      )
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.GithubDark,
      )
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.Monokai,
      )
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.NightOwl,
      )
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Clojure, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.OneDarkPro,
      )
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.TokyoNight,
      )
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Clojure,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn clojure_ast_tests() {
  describe("clojure expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Clojure)
    }),
  ])
}

pub fn clojure_regex_plus_ast_tests() {
  describe("clojure expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Clojure)
    }),
  ])
}

pub fn clojure_generated_tests() {
  describe("clojure expected generated validation", [
    it("generates all patterns correctly", fn() {
      test_helpers.validate_expected_generated(languages.Clojure)
    }),
  ])
}

pub fn clojure_recursion_tests() {
  describe("clojure expected recursion validation", [
    it("processes all patterns correctly", fn() {
      test_helpers.validate_expected_recursion(languages.Clojure)
    }),
  ])
}
