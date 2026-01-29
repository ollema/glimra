import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn php_tests() {
  describe("php expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Php,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Php, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(languages.Php, "snippet", themes.GithubDark)
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Php,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Php, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Php, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Php, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(languages.Php, "snippet", themes.OneDarkPro)
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(languages.Php, "snippet", themes.TokyoNight)
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Php,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn php_ast_tests() {
  describe("php expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Php)
    }),
  ])
}

pub fn php_regex_plus_ast_tests() {
  describe("php expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Php)
    }),
  ])
}

pub fn php_generated_tests() {
  describe("php expected generated validation", [
    it("generates all patterns correctly", fn() {
      test_helpers.validate_expected_generated(languages.Php)
    }),
  ])
}
