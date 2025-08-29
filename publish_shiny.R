# pak::pak("agdamsbo/project.aid")

# merge_scripts(path=here::here("app/functions.R"),files=list.files("R/",pattern = ".R$",full.names = TRUE))

# styler::style_file("app/functions.R")

project.aid::merge_scripts(
  files = c(
    list.files("R", pattern = ".R$", full.names = TRUE),
    "https://raw.githubusercontent.com/agdamsbo/project.aid/refs/heads/main/R/chunks_of_n.R"
  ),
  dest = here::here("app/functions.R")
)

project.aid::merge_scripts(
  files = c(
    here::here("app/functions.R"),
    here::here("app/server_raw.R")
  ),
  dest = here::here("app/server.R")
)

shiny::runApp(appDir = "app",launch.browser = TRUE)

project.aid::deploy_shiny(
  files = c("server.R", "ui.R"),
  account.name = "agdamsbo",
  name.app = "banko",
  name.token = "rsconnect_agdamsbo_token",
  name.secret = "rsconnect_agdamsbo_secret"
)

# It also runs as shinylive app, but quite slow. But it runs!
# Will implement in vignette.


## Merging scripts for minimal live version

project.aid::merge_scripts(
  files = c(
    list.files("R/", pattern = ".R$", full.names = TRUE),
    "live/server_raw.R"
  ),
  dest = here::here("live/server.R")
)


shiny::runApp(appDir = "live",launch.browser = TRUE)

shinylive::export(appdir = "live", destdir = "docs")
#
httpuv::runStaticServer(dir = "docs")
