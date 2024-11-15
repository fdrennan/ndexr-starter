#' @export
ui <- function(request) {
  box::use(shiny = shiny[tags], . / r / inputs / inputs)
  box::use(. / r / home)
  request <- as.list.environment(request)

  home$ui_home("home", request)
}
