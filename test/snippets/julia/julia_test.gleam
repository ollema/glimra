import glimra/languages
import startest.{describe, it}
import test_helpers
import glimra/themes

pub fn julia_tests() {
  describe("julia expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.CatppuccinMocha)
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.GithubDark)
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.GruvboxDarkMedium)
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.OneDarkPro)
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.TokyoNight)
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(languages.Julia, "snippet", themes.VitesseDark)
    }),
  ])
}

pub fn julia_ast_tests() {
  describe("julia expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Julia)
    }),
  ])
}

pub fn julia_regex_plus_ast_tests() {
  describe("julia expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Julia)
    }),
  ])
}
