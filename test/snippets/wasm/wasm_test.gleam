import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn wasm_tests() {
  describe("wasm expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Wasm,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Wasm, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Wasm,
        "snippet",
        themes.GithubDark,
      )
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Wasm,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Wasm, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Wasm, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Wasm, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(
        languages.Wasm,
        "snippet",
        themes.OneDarkPro,
      )
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(
        languages.Wasm,
        "snippet",
        themes.TokyoNight,
      )
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Wasm,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn wasm_ast_tests() {
  describe("wasm expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Wasm)
    }),
  ])
}
