# pak::pak("agdamsbo/project.aid")

merge_scripts <- function(path,files){
sink(path)

for(i in seq_along(files)){
  current_file = readLines(files[i])
  cat("\n\n########\n\n#### Current file:",files[i],"\n\n########\n\n")
  cat(current_file, sep ="\n")
}

sink()
}

merge_scripts(path=here::here("app/functions.R"),files=list.files("R/",pattern = ".R$",full.names = TRUE))

project.aid::deploy_shiny(
  account.name = "agdamsbo",
  name.app = "banko",
  name.token = "rsconnect_agdamsbo_token",
  name.secret = "rsconnect_agdamsbo_secret"
)

# shinylive::export(appdir = "app", destdir = "docs")
#
# httpuv::runStaticServer(dir = "docs")
