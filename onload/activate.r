options(warn = -1)
# library_names <- c(
#
# )

if ("renv" %in% utils::installed.packages()[, 1]) {

}


lapply(list.files("./onload/options", full.names = TRUE, pattern = "[.]r$"), source)
