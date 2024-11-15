#' @export
server <- function(input, output, session) {
  box::use(
    shiny = shiny[tags],
    . / site_shared,
    . / r / connections / postgres,
    . / r / inputs / inputs,
    . / r / home
  )

  session_home <- as.list.environment(session)

  home$server_home("home", session_home)
}
