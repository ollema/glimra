import glimra/languages
import startest.{describe, it}
import test_helpers
import glimra/themes

pub fn typescript_tests() {
  describe("typescript expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.CatppuccinMocha)
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.GithubDark)
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.GruvboxDarkMedium)
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.OneDarkPro)
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.TokyoNight)
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(languages.Typescript, "snippet", themes.VitesseDark)
    }),
  ])
}

pub fn typescript_ast_tests() {
  describe("typescript expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Typescript)
    }),
  ])
}

pub fn typescript_regex_plus_ast_tests() {
  describe("typescript expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Typescript)
    }),
  ])
}
