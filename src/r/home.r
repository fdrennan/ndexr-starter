#' @export
ui_home <- function(id, request) {
  box::use(
    shiny = shiny[tags],
    . / head / head,
    . / inputs / inputs
  )

  ns <- shiny$NS(id)

  tags$html(
    head$ui_head(
      title = "genview",
      description = "genview",
      icon = "images/images.jpg",
      author = "Freddy Drennan",
      image = "./images/images.jpg",
      stylesheet = "./css/home.css",
      url = "",
      name = "genview",
      content = "genview"
    ),
    tags$body(
      shiny$uiOutput(ns("body"))
    )
  )
}

#' @export
server_home <- function(id, session_home) {
  box::use(
    shiny = shiny[tags],
    . / head / head,
    . / inputs / inputs,
    glue,
    ssh
  )

  renv_lockfile <- jsonlite::fromJSON("../renv.lock")
  request <- as.list.environment(session_home$request)
  request <- request[grepl("HTTP", names(request))]

  # Assuming you have the correct SSH credentials
  # con <- ssh$ssh_connect(host = "localhost", passwd = Sys.getenv("passwd"))

  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- shiny$renderUI({
      tags$div(
        class = "container",
        tags$div(
          class = "row",
          tags$div(
            class = "col-12",
            tags$h1("Welcome!")
          )
        )
      )
    })
  })
}
