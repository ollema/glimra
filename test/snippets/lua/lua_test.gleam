import glimra/languages
import glimra/themes
import startest.{describe, it}
import test_helpers

pub fn lua_tests() {
  describe("lua expected tokens snippet validation", [
    it("validates with catppuccin-mocha theme", fn() {
      test_helpers.validate_snippet(
        languages.Lua,
        "snippet",
        themes.CatppuccinMocha,
      )
    }),
    it("validates with dracula theme", fn() {
      test_helpers.validate_snippet(languages.Lua, "snippet", themes.Dracula)
    }),
    it("validates with github-dark theme", fn() {
      test_helpers.validate_snippet(languages.Lua, "snippet", themes.GithubDark)
    }),
    it("validates with gruvbox-dark-medium theme", fn() {
      test_helpers.validate_snippet(
        languages.Lua,
        "snippet",
        themes.GruvboxDarkMedium,
      )
    }),
    it("validates with monokai theme", fn() {
      test_helpers.validate_snippet(languages.Lua, "snippet", themes.Monokai)
    }),
    it("validates with night-owl theme", fn() {
      test_helpers.validate_snippet(languages.Lua, "snippet", themes.NightOwl)
    }),
    it("validates with nord theme", fn() {
      test_helpers.validate_snippet(languages.Lua, "snippet", themes.Nord)
    }),
    it("validates with one-dark-pro theme", fn() {
      test_helpers.validate_snippet(languages.Lua, "snippet", themes.OneDarkPro)
    }),
    it("validates with tokyo-night theme", fn() {
      test_helpers.validate_snippet(languages.Lua, "snippet", themes.TokyoNight)
    }),
    it("validates with vitesse-dark theme", fn() {
      test_helpers.validate_snippet(
        languages.Lua,
        "snippet",
        themes.VitesseDark,
      )
    }),
  ])
}

pub fn lua_ast_tests() {
  describe("lua expected ast validation", [
    it("parses all patterns correctly", fn() {
      test_helpers.validate_expected_ast(languages.Lua)
    }),
  ])
}

pub fn lua_regex_plus_ast_tests() {
  describe("lua expected regex_plus_ast validation", [
    it("transforms all patterns correctly", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages.Lua)
    }),
  ])
}

pub fn lua_generated_tests() {
  describe("lua expected generated validation", [
    it("generates all patterns correctly", fn() {
      test_helpers.validate_expected_generated(languages.Lua)
    }),
  ])
}

pub fn lua_recursion_tests() {
  describe("lua expected recursion validation", [
    it("processes all patterns correctly", fn() {
      test_helpers.validate_expected_recursion(languages.Lua)
    }),
  ])
}
