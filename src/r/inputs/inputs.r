#' @export
modal_transition <- function(message = "Please wait", easyClose = FALSE, content = NULL, spin_image = FALSE) {
  box::use(shiny = shiny[tags, modalDialog, showModal, removeModal])
  box::use(shinyjs)
  box::use(. / inputs)
  id <- uuid::UUIDgenerate()
  ns <- shiny$NS(id)
  shiny$removeModal()
  inputs$pan(message)

  message <- tags$p(message, class = "text-center lead")
  message <- tags$div(class = "lead mb-3", message)


  image <- fs::dir_ls("./images/spinners", regexp = ".sh$", invert = T)

  image <- sample(image, 1)

  shiny$showModal(
    shiny$modalDialog(
      easyClose = easyClose,
      footer = NULL,
      # size = "l", # Ensures modal is large enough to fit content
      tags$div(
        class = "container-fluid",
        tags$div(
          class = "row justify-content-center align-items-center h-100",
          tags$div(
            class = "col-8 d-flex flex-column justify-content-center align-items-center",
            style = "max-height: 100%;",
            tags$div(class = "d-flex justify-content-end", content),
            tags$img(
              src = image,
              class = "img img-fluid rounded",
              class = ifelse(spin_image, "spin-image", "")
            )
            # inputs$ui_spinner(ns("spinner"))
          )
        )
      )
    )
  )
}

#' @export
ui_spinner <- function(id, pattern = "gif$|png$|svg$|jpeg$") {
  box::use(shiny = shiny[tags])
  ns <- shiny$NS(id)

  tags$div(
    class = "container-fluid h-75 mt-5",
    tags$div(
      class = "row justify-content-center align-items-center h-100",
      tags$div(
        class = "col-12 d-flex flex-column justify-content-center align-items-center",
        style = "padding: 20px; max-height: 100%;",
        message,
        tags$img(
          src = "https://ndexr.io/images/rocks/palodc.jpg",
          class = "img img-fluid rounded"
        )
        # inputs$ui_spinner(ns("spinner"))
      )
    )
  )
}


#' @export
pan <- function(message) {
  cli::cli_inform(message)
  try(shiny::showNotification(message, closeButton = TRUE))
}


#' @export
create_dynamic_card <- function(ns, titles, values, buttons = list()) {
  box::use(shiny = shiny[tags], . / inputs)
  server_info <- purrr::map2(titles, values, ~ tags$p(tags$strong(.x), .y, class = "me-3 mb-0"))

  # Create buttons layout if provided
  buttons_layout <- purrr::map(buttons, function(btn) {
    inputs$actionButton(ns(btn$id), btn$label, class = paste("btn", btn$class, "btn-sm me-2"))
  })

  tags$div(
    class = "card",
    style = "max-width: 100%;",
    tags$div(
      class = "card-body",
      tags$div(
        class = "row",

        # Server Info
        tags$div(
          class = "col-12 d-flex flex-wrap align-items-center",
          server_info # Dynamically generated server information
        ),

        # Buttons (if any)
        tags$div(
          class = "col-12 d-flex align-items-center justify-content-md-end mt-3 mt-md-0",
          buttons_layout # Dynamically generated buttons
        )
      )
    )
  )
}


#' @export
actionButton <- function(id, label = NULL, icon = NULL, class = "btn-primary", value = 0, size = "sm", disabled = FALSE, ...) {
  box::use(shiny = shiny[tags])

  args <- list(...) # Capture the arguments as a list

  valid_sizes <- c("sm", "md", "lg")
  if (!size %in% valid_sizes) {
    stop("Invalid button size. Use 'sm', 'md', or 'lg'.")
  }

  # Add the button size class if size is specified
  size_class <- ifelse(size == "md", "", paste0("btn-", size))

  # If disabled, add the disabled attribute
  disabled_attr <- if (disabled) "disabled" else NULL


  # Create the button with Bootstrap styling and an optional icon
  tags$button(
    id = id,
    type = "button",
    class = paste("btn", class, size_class, "action-button"), # Combine button classes dynamically
    `data-val` = value, # Data attribute to track button clicks
    disabled = disabled_attr, # Add disabled attribute if TRUE
    tags$span(
      if (!is.null(icon)) {
        if (class(icon) != "shiny.tag") icon <- shiny::icon(icon)
        icon
      }, # If an icon is provided, add it
      if (!is.null(label)) label # Button label text, only if provided
    ),
    !!!args
  )
}


#' @export
passwordInput <- function(id, label = NULL, placeholder = NULL, value = "", size = "md", disabled = FALSE, ...) {
  box::use(shiny = shiny[tags])

  args <- list(...) # Capture the arguments as a list

  valid_sizes <- c("sm", "md", "lg")
  if (!size %in% valid_sizes) {
    stop("Invalid input size. Use 'sm', 'md', or 'lg'.")
  }

  # Add the input size class if size is specified
  size_class <- ifelse(size == "md", "", paste0("form-control-", size))

  # If disabled, add the disabled attribute
  disabled_attr <- if (disabled) "disabled" else NULL

  # Create the password input field with Bootstrap styling
  tags$div(
    class = "form-group",
    if (!is.null(label)) tags$label(`for` = id, label), # Add label if provided
    tags$input(
      id = id,
      type = "password",
      class = paste("form-control", size_class), # Combine input classes dynamically
      placeholder = placeholder, # Placeholder text if provided
      value = value, # Initial value
      disabled = disabled_attr, # Add disabled attribute if TRUE
      !!!args # Include any additional passed attributes
    )
  )
}


#' @export
create_tabset_panel <- function(..., sidebar = FALSE, reload_button = NULL, title = NULL, justify = c("start", "center", "end", "between", "fill", "justified"), panel_name = "Needs Name") {
  box::use(shiny = shiny[tags])

  justify <- match.arg(justify)

  tabs <- list(...)

  if (!is.null(tabs[[1]][[1]]$nav_item)) {
    tabs <- tabs[[1]]
  }

  uuid <- uuid::UUIDgenerate()

  # Check if there is an active tab; if not, activate the first tab by default
  has_active <- any(sapply(tabs, function(tab) {
    grepl("active", tab$nav_item$children[[1]]$attribs$class)
  }))

  if (!has_active && length(tabs) > 0) {
    tabs[[1]]$nav_item$children[[1]]$attribs$class <- "nav-link active"
    tabs[[1]]$content$attribs$class <- "tab-pane fade show active"
  }

  # Determine justification class
  justify_class <- switch(justify,
    start = "justify-content-start",
    center = "justify-content-center",
    end = "justify-content-end",
    fill = "nav-fill",
    between = "justify-content-between",
    justified = "nav-justified",
    "justify-content-start"
  )

  if (sidebar) {
    tags$div(
      class = "d-flex flex-column",
      tags$div(
        class = "row flex-grow-1",
        tags$nav(
          class = "col-auto d-none d-lg-block mt-3 p-1",
          tags$div(
            class = "d-flex flex-column align-items-center align-items-md-start",
            if (!is.null(title)) {
              tags$h4(class = "mt-3 text-underline", title)
            },
            tags$ul(
              class = paste("nav nav-pills flex-column mb-sm-auto mb-0 align-items-center align-items-md-start", justify_class),
              id = uuid,
              role = "tablist",
              lapply(tabs, function(tab) {
                tab$nav_item
              })
            )
          )
        ),
        tags$nav(
          class = "d-lg-none p-1",
          tags$div(
            class = "d-flex flex-column align-items-center align-items-md-start",
            if (!is.null(title)) {
              tags$h4(class = "mt-3 text-underline", title)
            },
            tags$ul(
              class = paste("nav nav-pills mb-sm-auto mb-0 align-items-center align-items-md-start", justify_class),
              id = uuid,
              role = "tablist",
              lapply(tabs, function(tab) {
                tab$nav_item
              })
            )
          )
        ),
        # Main content area
        tags$div(
          class = "col d-flex flex-column",
          tags$div(
            class = "container-fluid mt-3 h-100",
            tags$div(
              class = "tab-content h-100",
              lapply(tabs, `[[`, "content")
            )
          )
        )
      )
    )
  } else {
    # Horizontal tabset panel (default behavior)
    tags$div(
      class = "d-flex flex-column",
      # Add title if provided
      if (!is.null(title)) {
        tags$h4(class = "mt-3 text-underline", title)
      },
      tags$div(
        class = "d-flex justify-content-between align-items-center",
        tags$div(
          class = "title-box d-flex align-items-center px-3 py-2 m-0 rounded-end  d-none d-md-block",
          tags$h5(class = "m-0", panel_name)
        ),
        tags$ul(
          class = paste("nav nav-tabs flex-grow-1", justify_class),
          id = uuid,
          role = "tablist",
          style = "margin: 0; padding-left: 15px;", # Add padding to space out from title box
          lapply(tabs, function(tab) tab$nav_item)
        ),
        if (!is.null(reload_button)) {
          tags$div(
            class = "ms-2",
            reload_button
          )
        }
      ),
      tags$div(
        class = "tab-content",
        lapply(tabs, `[[`, "content")
      )
    )
  }
}

#' @export
create_tab <- function(tab_title, tab_content, icon = NULL, popup_message = NULL, active_title = FALSE, header_type = shiny::tags$h3) {
  box::use(shiny = shiny[tags])
  tab_id <- uuid::UUIDgenerate()

  # If icon is provided, include it in the tab title
  title_with_icon <- if (!is.null(icon)) {
    shiny$tags$span(
      shiny$tags$i(class = icon),
      # Text is displayed only on medium screens and up
      shiny$tags$span(class = "d-none d-md-inline ms-2", tab_title)
    )
  } else {
    tab_title
  }

  # Add tooltip attributes
  if (!is.null(popup_message)) {
    # Use Bootstrap tooltip with popup_message
    tooltip_attributes <- list(
      `data-bs-toggle` = "tooltip",
      `data-bs-placement` = "top",
      title = popup_message
    )
  } else {
    # Use default title attribute with tab_title
    tooltip_attributes <- list(
      title = tab_title
    )
  }

  list(
    nav_item = tags$li(
      class = "nav-item",
      role = "presentation",
      tags$a(
        class = ifelse(active_title, "nav-link active", "nav-link"),
        id = paste0(tab_id, "-tab"),
        href = paste0("#", tab_id),
        `data-bs-toggle` = "tab",
        role = "tab",
        `aria-controls` = tab_id,
        `aria-selected` = ifelse(active_title, "true", "false"),
        !!!tooltip_attributes,
        title_with_icon
      )
    ),
    content = tags$div(
      class = ifelse(active_title, "tab-pane fade show active h-100", "tab-pane fade"),
      id = tab_id,
      role = "tabpanel",
      `aria-labelledby` = paste0(tab_id, "-tab"),
      tab_content
    )
  )
}

#' @export
card <- function(title, ..., collapse = TRUE) {
  box::use(shiny = shiny[tags])
  id <- uuid::UUIDgenerate()

  content <- shiny$tagList(...)

  collapse_class <- if (collapse) "card-body collapse" else "card-body"
  tags$div(
    class = "card m-0 p-0 mb-1",
    tags$div(
      class = "card-header d-flex justify-content-between align-items-center",
      tags$span(title),
      tags$div(
        class = "card-controls",
        tags$button(
          class = "btn btn-sm btn-outline-secondary me-2",
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", id),
          tags$i(class = "fas fa-chevron-down")
        ),
        tags$button(
          class = "btn btn-sm btn-outline-secondary fullscreen-toggle",
          type = "button",
          onclick = paste0(
            "let card = document.getElementById('", id, "').closest('.card'); ",
            "if (!document.fullscreenElement) { ",
            "card.requestFullscreen ? card.requestFullscreen() : card.webkitRequestFullscreen(); ",
            "card.classList.remove('collapse'); ",
            "card.classList.add('fullscreen'); ",
            "this.querySelector('i').classList.replace('fa-expand', 'fa-compress'); ",
            "} else { ",
            "document.exitFullscreen(); ",
            "card.classList.remove('fullscreen'); ",
            "this.querySelector('i').classList.replace('fa-compress', 'fa-expand'); ",
            "}"
          ),
          tags$i(class = "fas fa-expand")
        )
      )
    ),
    tags$div(
      id = id,
      class = collapse_class,
      content
    )
  )
}

#' @export
custom_plot_theme <- function(...) {
  box::use(ggplot2)
  ggplot2$theme(
    plot.margin = ggplot2$unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.background = ggplot2$element_rect(fill = "#ffffff", color = NA),
    panel.background = ggplot2$element_rect(fill = "#ecf0f1", color = "#2c3e50"), # light background with dark border
    text = ggplot2$element_text(family = "Roboto", color = "#2c3e50"), # consistent with primary font and dark color
    axis.text = ggplot2$element_text(family = "Lato", color = "#2c3e50"), # consistent with secondary font
    axis.title = ggplot2$element_text(family = "Lato", color = "#2c3e50"),
    panel.grid.major = ggplot2$element_line(color = "#bdc3c7", size = 0.3), # soft grey for grid lines
    panel.grid.minor = ggplot2$element_line(color = "#dfe6e9", size = 0.15),
    axis.text.x = ggplot2$element_text(color = "#2c3e50", angle = 45, hjust = 1),
    axis.text.y = ggplot2$element_text(color = "#2c3e50"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.text = ggplot2$element_text(family = "Lato", color = "#2c3e50"),
    legend.title = ggplot2$element_text(family = "Lato", color = "#2c3e50"),
    plot.title = ggplot2$element_text(family = "Roboto", face = "bold", size = 14, color = "#3498db", hjust = 0.5), # Soft blue for title
    plot.subtitle = ggplot2$element_text(family = "Lato", size = 12, color = "#95a5a6", hjust = 0.5), # Soft grey for subtitle
    plot.caption = ggplot2$element_text(family = "Courier New", size = 10, color = "#7f8c8d", hjust = 1) # Code font for caption
  )
}


#' @export
ui_send_message <- function(id = "send_message") {
  box::use(.. / aws / sns / sns, shiny = shiny[tags], .. / inputs / inputs)
  ns <- shiny$NS(id)

  shiny::tagList(
    # Button to open the contact form
    inputs$actionButton(
      class = "btn action-button",
      id = ns("openform"),
      tags$i(class = "fas fa-envelope")
    ),
    # Contact form container, initially hidden
    tags$div(
      id = ns("contact-form"),
      class = "my-2 contact-form-container d-none",
      tags$h2(
        class = "d-flex justify-content-between align-items-center",
        "Contact Us",
        inputs$actionButton(
          type = "button",
          class = "fas fa-x",
          "aria-label" = "Close",
          id = ns("closeform")
        )
      ),
      tags$form(
        tags$div(
          class = "mb-2",
          tags$input(
            type = "text",
            class = "form-control",
            id = ns("name"),
            placeholder = "Your Name"
          )
        ),
        tags$div(
          class = "mb-2",
          tags$input(
            type = "email",
            class = "form-control",
            id = ns("email"),
            placeholder = "Your Email"
          )
        ),
        tags$div(
          class = "mb-2",
          tags$input(
            type = "tel",
            class = "form-control",
            id = ns("phone"),
            placeholder = "Your Phone Number"
          )
        ),
        tags$div(
          class = "mb-2",
          tags$textarea(
            class = "form-control",
            id = ns("message"),
            rows = "4",
            placeholder = "Your Message"
          )
        ),
        tags$button(
          type = "button",
          id = ns("submit"),
          class = "btn btn-primary action-button",
          "Submit"
        )
      )
    ),

    # Script to toggle the form visibility
    tags$script(shiny$HTML(paste0("
      $(document).ready(function() {
        $('#", ns("openform"), "').on('click', function() {
          $('#", ns("contact-form"), "').removeClass('d-none');
        });
        $('#", ns("closeform"), "').on('click', function() {
          $('#", ns("contact-form"), "').addClass('d-none');
        });
      });
    ")))
  )
}


#' @export
server_send_message <- function(id = "send_message") {
  box::use(.. / aws / sns / sns, shiny = shiny[tags], .. / inputs / inputs)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      shiny$observeEvent(input$submit, {
        sns$sns_send(
          ns_common_store_user = shiny::NS("fdrennan"),
          PhoneNumber = "2549318313",
          countryCode = "1",
          Message = paste0(c(input$name, input$email, input$phone, input$message), collapse = "\n")
        )
        inputs$pan("Message sent!")
      })
    }
  )
}


#' @export
if_is_null <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  x
}

#' @export
code <- function(txt) {
  box::use(shiny = shiny[tags])

  tags$pre(
    class = "bg-light p-1 rounded overflow-auto",
    tags$code(
      class = "text-monospace",
      paste0("\n", txt, collapse = "\n")
    )
  )
}
