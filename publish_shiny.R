# pak::pak("agdamsbo/project.aid")

merge_scripts <- function(path,files,strip.roxygen=TRUE){
sink(path)

for(i in seq_along(files)){
  current_file <-  readLines(files[i])

  if (strip.roxygen){
  current_file <- sub("(?m)^\\#\\'.*\n?", "", current_file, perl=T)
  }

  cat("\n\n########\n#### Current file:",files[i],"\n########\n\n")
  cat(current_file, sep ="\n")
}
sink()
}

deploy_shiny <- function (path=here::here("app/"), files=NULL, account.name, name.app,
          name.token, name.secret){
  rsconnect::setAccountInfo(name = account.name, token = keyring::key_get(service = name.token),
                            secret = keyring::key_get(service = name.secret))
  rsconnect::deployApp(appDir = path,appFiles = files, lint = TRUE, appName = name.app)
}

merge_scripts(path=here::here("app/functions.R"),files=list.files("R/",pattern = ".R$",full.names = TRUE))

styler::style_file("app/functions.R")

merge_scripts(path=here::here("app/server.R"),files=c("app/functions.R","app/server_raw.R"))

deploy_shiny(
  files= c("server.R","ui.R"),
  account.name = "agdamsbo",
  name.app = "banko",
  name.token = "rsconnect_agdamsbo_token",
  name.secret = "rsconnect_agdamsbo_secret"
)

# shinylive::export(appdir = "app", destdir = "docs")
#
# httpuv::runStaticServer(dir = "docs")


