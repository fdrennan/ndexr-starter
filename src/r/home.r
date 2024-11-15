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
  con <- ssh$ssh_connect(host = "localhost", passwd = Sys.getenv("passwd"))
  
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$body <- shiny$renderUI({
      
      tags$div(
        class = "container",
        tags$div(
          class = "row",
          tags$div(
            class = "col-12",
            inputs$card(
              title = "SSH Configuration Details",
              
              # Responsive table container
              tags$div(
                class = "table-responsive",
                
                # Bootstrap-styled table
                tags$table(
                  class = "table table-striped table-bordered table-hover",
                  
                  # Table Header
                  tags$thead(
                    tags$tr(
                      tags$th(""),
                      tags$th("location"),
                      tags$th("size")
                    )
                  ),
                  
                  tags$tbody(
                    tags$tr(
                      tags$td(tags$code("ssh shpc_sc1")),
                      tags$td("Santa Clara (SC1)"),
                      tags$td("7104 compute cores, 24 A100/40G GPUs, 80 A100/80G GPUs, 4.7 PB storage")
                    ),
                    tags$tr(
                      tags$td(tags$code("ssh shpc_ind")),
                      tags$td("Indianapolis (RID)"),
                      tags$td("6336 compute cores, 48 V100/32G GPUs, 4.7 PB storage")
                    ),
                    tags$tr(
                      tags$td(tags$code("ssh shpc_kau")),
                      tags$td("Kaiseraugst (KAU)"),
                      tags$td("3552 compute cores, 56 A100/40G GPUs, 4.7 PB storage")
                    )
                  )
                  
                )
              )
            ),
            
            inputs$card(
              title = "Links",
              tags$div(
                tags$h4("Google Docs"),
                tags$ul(
                  tags$li(
                    tags$a("Freddy Onboarding", target="_blank", href="https://docs.google.com/document/d/11yXQEYwsOyKUebl1nIZFdSxHKmIISVB7TZherUulye8/edit?tab=t.0"),
                    tags$a("Leo/Freddy", target="_blank", href="https://docs.google.com/document/d/1BYP8PoN6WdyvLR27jIdT20pmnGWHKSF_D7zLTY8ZfNY/edit?tab=t.0#heading=h.xzlrpqe7sp14")
                  )
                )
                # Package Managers and Environments
                tags$h4("Package Managers and Environments"),
                tags$ul(
                  tags$li(tags$a("GAPM", href = "https://gapm.cedar.roche.com", target = "_blank")),
                  tags$li(tags$a("Kite Reference", href = "https://code.roche.com/cedar/gran/kite-reference/-/blob/4beb0d68cc9f9995ea9a5818799ec834fb472a96/cedar_r4.5_bioc3.21-devel/git_repos.dcf", target = "_blank")),
                  tags$li(tags$a("FacileExplorer", href = "https://code.roche.com/cedar/facileexplorer", target = "_blank"))
                ),
                
                # Application Deployment
                tags$h4("Application Deployment"),
                tags$ul(
                  tags$li(tags$a("Connect Cloud", href = "https://connect-cloud.cedar.roche.com", target = "_blank")),
                  tags$li(tags$a("Connect On-Prem", href = "https://connect-onprem.cedar.roche.com", target = "_blank"))
                ),
                
                # sHPC Resources
                tags$h4("sHPC Resources"),
                tags$ul(
                  tags$li(tags$a("Roche sHPC Platform", href = "https://rochewiki.roche.com/confluence/display/SCICOMP/Roche+sHPC+Platform", target = "_blank")),
                  tags$li(tags$a("sHPC New User Guide", href = "https://rochewiki.roche.com/confluence/display/SCICOMP/sHPC+New+User+Guide", target = "_blank"))
                ),
                
                # Legacy Solutions
                tags$h4("Legacy Solutions"),
                tags$ul(
                  tags$li(tags$a("GRED Shiny Dev", href = "http://gred-shiny-p02.sc1.roche.com:3838/dev/", target = "_blank")),
                  tags$li(tags$a("GRED Shiny Prod", href = "http://gred-shiny-p01.sc1.roche.com:3838/prd/", target = "_blank")),
                  tags$li(tags$a("AD Group Request", href = "http://adgrouprequest.roche.com", target = "_blank")),
                  tags$li(tags$a("RStudio Connect", href = "https://rsconnect.rplatform.org/", target = "_blank"))
                ),
                
                # Additional Resources
                tags$h4("Additional Resources"),
                tags$ul(
                  tags$li(tags$a("My ABC Workflow", href = "https://code.roche.com/waddella/my_abc_workflow", target = "_blank")),
                  tags$li(tags$a("CropQuest", href = "https://code.roche.com/waddella/cropquest", target = "_blank")),
                  tags$li(tags$a("CEDAR Ops Guide", href = "https://connect-cloud.cedar.roche.com/cedar-ops-guide/", target = "_blank"))
                )
              )
            ),
            
            
            inputs$card(
              "Shiny Application Template",
              purrr::map(
                c("../package.json", "../compose.yml", "../.Rprofile",
                  "../onload/activate.r", fs::dir_ls("../onload/options"),
                  "../app.r", "ui.r", "server.r","./r/home.r"
                ),
                function(x) {
                  inputs$card(x, inputs$code(readr::read_file(x)))
                }
              )
            ),
            inputs$card(
              "Application Data",
              inputs$card(
                "Headers",
                purrr::imap(
                  request,
                  function(value, name) {
                    tags$div(tags$b(name), value)
                  }
                )
              ),
              inputs$card(
                "R Version",
                inputs$code(jsonlite::toJSON(renv_lockfile$R, pretty = TRUE))
              ),
              inputs$card(
                "Packages",
                inputs$code(jsonlite::toJSON(renv_lockfile$Packages, pretty = TRUE))
              )
            )
          )
        )
      )
      
    })
  })
}
