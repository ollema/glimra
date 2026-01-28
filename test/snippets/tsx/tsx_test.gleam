import glimra/languages
import startest.{describe, it}
import test_helpers
import glimra/themes

pub fn tsx_tests() {
  describe("tsx expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.CatppuccinMocha)
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.GithubDark)
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.GruvboxDarkMedium)
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.OneDarkPro)
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.TokyoNight)
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(languages.Tsx, "snippet", themes.VitesseDark)
    }),
  ])
}

pub fn tsx_ast_tests() {
  describe("tsx expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Tsx)
    }),
  ])
}

pub fn tsx_regex_plus_ast_tests() {
  describe("tsx expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Tsx)
    }),
  ])
}
