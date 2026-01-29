import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn cpp_tests() {
  describe("cpp expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Cpp,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Cpp, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(languages.Cpp, "snippet", themes.GithubDark)
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Cpp,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Cpp, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Cpp, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Cpp, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(languages.Cpp, "snippet", themes.OneDarkPro)
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(languages.Cpp, "snippet", themes.TokyoNight)
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Cpp,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn cpp_ast_tests() {
  describe("cpp expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Cpp)
    }),
  ])
}

pub fn cpp_regex_plus_ast_tests() {
  describe("cpp expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Cpp)
    }),
  ])
}

pub fn cpp_generated_tests() {
  describe("cpp expected generated validation", [
    it("generates all patterns correctly", fn() {
      test_helpers.validate_expected_generated(languages.Cpp)
    }),
  ])
}

pub fn cpp_recursion_tests() {
  describe("cpp expected recursion validation", [
    it("processes all patterns correctly", fn() {
      test_helpers.validate_expected_recursion(languages.Cpp)
    }),
  ])
}
