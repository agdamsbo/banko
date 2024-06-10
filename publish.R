devtools::build_readme()

spelling::update_wordlist()

# publishing
# devtools::check_win_release()

rhub::rhub_check(platforms = c("linux","macos","macos-arm64","windows"))
