# R Configuration Options
options(
  conflicts.policy = "strict",
  max.print = 3000,
  repos = c(
    CRAN = "https://kite.cedar.roche.com/r/cedar_r4.4_bioc3.19-release/latest"
    # Uncomment and modify the desired repository below as needed:
    # BiocDevel = "https://kite.cedar.roche.com/r/cedar_r4.4_bioc3.20-devel/latest",
    # BiocRelease_May2023 = "https://kite.cedar.roche.com/r/cedar_r4.4_bioc3.18-release/2023_05_01",
    # BiocRelease_May2022 = "https://kite.cedar.roche.com/r/cedar_r4.4_bioc3.17-release/2022_05_01",
    # RUniverse = "https://gadenbuie.r-universe.dev"
  ),
  nwarnings = 50,
  renv.consent = TRUE,
  renv.config.install.transactional = FALSE,
  renv.config.dependencies.limit = Inf
)

Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
