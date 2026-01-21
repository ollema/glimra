import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn elixir_tests() {
  describe("elixir expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Elixir,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Elixir, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Elixir,
        "snippet",
        themes.GithubDark,
      )
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Elixir,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Elixir, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(
        languages.Elixir,
        "snippet",
        themes.NightOwl,
      )
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Elixir, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(
        languages.Elixir,
        "snippet",
        themes.OneDarkPro,
      )
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(
        languages.Elixir,
        "snippet",
        themes.TokyoNight,
      )
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Elixir,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn elixir_ast_tests() {
  describe("elixir expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Elixir)
    }),
  ])
}
