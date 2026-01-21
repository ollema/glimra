//// Themes - Single source of truth for all theme definitions
////
//// This module defines the BundledTheme enum and all associated metadata.
//// All other modules should import from here rather than defining their own mappings.

/// Bundled themes for syntax highlighting.
/// Each variant represents a theme with bundled color scheme support.
pub type BundledTheme {
  CatppuccinMocha
  Dracula
  GithubDark
  GruvboxDarkMedium
  Monokai
  NightOwl
  Nord
  OneDarkPro
  TokyoNight
  VitesseDark
}

/// Metadata about a theme
pub type ThemeInfo {
  ThemeInfo(
    /// Constructor name (e.g., "CatppuccinMocha")
    constructor: String,
    /// Theme identifier used in APIs (e.g., "catppuccin-mocha")
    id: String,
    /// Theme filename (e.g., "catppuccin-mocha.json")
    theme_file: String,
  )
}

/// Get all metadata for a theme.
/// This is THE source of truth for theme information.
pub fn theme_info(theme: BundledTheme) -> ThemeInfo {
  case theme {
    CatppuccinMocha ->
      ThemeInfo("CatppuccinMocha", "catppuccin-mocha", "catppuccin-mocha.json")
    Dracula -> ThemeInfo("Dracula", "dracula", "dracula.json")
    GithubDark -> ThemeInfo("GithubDark", "github-dark", "github-dark.json")
    GruvboxDarkMedium ->
      ThemeInfo(
        "GruvboxDarkMedium",
        "gruvbox-dark-medium",
        "gruvbox-dark-medium.json",
      )
    Monokai -> ThemeInfo("Monokai", "monokai", "monokai.json")
    NightOwl -> ThemeInfo("NightOwl", "night-owl", "night-owl.json")
    Nord -> ThemeInfo("Nord", "nord", "nord.json")
    OneDarkPro -> ThemeInfo("OneDarkPro", "one-dark-pro", "one-dark-pro.json")
    TokyoNight -> ThemeInfo("TokyoNight", "tokyo-night", "tokyo-night.json")
    VitesseDark -> ThemeInfo("VitesseDark", "vitesse-dark", "vitesse-dark.json")
  }
}

/// All bundled themes
pub fn all_themes() -> List(BundledTheme) {
  [
    CatppuccinMocha, Dracula, GithubDark, GruvboxDarkMedium, Monokai, NightOwl,
    Nord, OneDarkPro, TokyoNight, VitesseDark,
  ]
}

/// Get the theme identifier (e.g., "catppuccin-mocha")
pub fn theme_id(theme: BundledTheme) -> String {
  theme_info(theme).id
}

/// Theme type - either a bundled theme or a custom theme with JSON
pub type Theme {
  /// Use a bundled theme
  Bundled(BundledTheme)
  /// Use a custom theme with a name and JSON content
  Custom(name: String, json: String)
}

/// Get the name/id of a theme
pub fn theme_name(theme: Theme) -> String {
  case theme {
    Bundled(t) -> theme_id(t)
    Custom(name, _) -> name
  }
}
