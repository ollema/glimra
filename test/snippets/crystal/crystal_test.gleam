import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn crystal_tests() {
  describe("crystal expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.Dracula,
      )
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.GithubDark,
      )
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.Monokai,
      )
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.NightOwl,
      )
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Crystal, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.OneDarkPro,
      )
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.TokyoNight,
      )
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Crystal,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn crystal_ast_tests() {
  describe("crystal expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Crystal)
    }),
  ])
}

pub fn crystal_regex_plus_ast_tests() {
  describe("crystal expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Crystal)
    }),
  ])
}

pub fn crystal_generated_tests() {
  describe("crystal expected generated validation", [
    it("generates all patterns correctly", fn() {
      test_helpers.validate_expected_generated(languages.Crystal)
    }),
  ])
}

pub fn crystal_recursion_tests() {
  describe("crystal expected recursion validation", [
    it("processes all patterns correctly", fn() {
      test_helpers.validate_expected_recursion(languages.Crystal)
    }),
  ])
}
