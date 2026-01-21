import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn dart_tests() {
  describe("dart expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Dart,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Dart, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Dart,
        "snippet",
        themes.GithubDark,
      )
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Dart,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Dart, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Dart, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Dart, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(
        languages.Dart,
        "snippet",
        themes.OneDarkPro,
      )
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(
        languages.Dart,
        "snippet",
        themes.TokyoNight,
      )
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Dart,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn dart_ast_tests() {
  describe("dart expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Dart)
    }),
  ])
}
