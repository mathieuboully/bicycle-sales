rm(list = ls())

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(lubridate)
library(stringr)
library(leaflet)
library(plotly)
library(httr)

options(
  shiny.maxRequestSize = 100 * 1024 ^ 2,
  dplyr.summarise.inform = FALSE,
  stringsAsFactors = FALSE
)

# Sys.setlocale(category = "LC_ALL", locale = "French")
app_name = "99Bikes"
last_update = Sys.Date()

if (!is.null(rstudioapi::getActiveProject())) {
  .wdPath = setwd(rstudioapi::getActiveProject())
} else {
  .wdPath = dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(.wdPath)
}

path_data = "./data"
path_image = "./www"
path_utils = "./utils"

# Fonctions
# source(file.path(path_utils, ".R"), encoding = "UTF-8")
get_geocode <- function(q) {
  params =
    list(q = q, appid = "2da260c6a642ab30eabef80af647ef85", limit = 1)
  url =
    "http://api.openweathermap.org/geo/1.0/direct"
  response =
    httr::GET(url, query = params)
  
  httr::content(response)[[1]]$lat
  httr::content(response)[[1]]$lon
  
  if (response$status_code == 200) {
    df =
      bind_rows(lapply(httr::content(response), as.data.frame)) %>%
      as.data.frame(.)
    
    if (nrow(df) == 0) {
      return(list(c(lat = NA, lon = NA)))
    }
    
    return(list(c(
      lat = df$lat, lon = df$lon
    )))
    
  } else {
    return(list(c(lat = NA, lon = NA)))
  }
}

# Data
customer_address = readxl::read_xlsx(file.path(path_data, "Customer Address.xlsx"))
customer_demography = readxl::read_xlsx(file.path(path_data, "Customer Demography.xlsx"))
transaction = readxl::read_xlsx(file.path(path_data, "Transcation.xlsx"))

customer = customer_address %>%
  left_join(., customer_demography, by = c("customer_id")) %>%
  mutate(
    city = case_when(
      state %in% c("NSW", "New South Wales") ~ "Sydney",
      state %in% c("QLD") ~ "Brisbane",
      state %in% c("VIC") ~ "Melbourne",
      TRUE ~ as.character("Sydney")
    ),
    gender = case_when(
      gender %in% c("Male", "M") ~ "Homme",
      gender %in% c("Femal", "F") ~ "Femme",
      TRUE ~ as.character("Inconnu")
    )
  )

# customer = customer[1:100, ] %>%
#   rowwise() %>%
#   dplyr::mutate(geocode = get_geocode(q = paste0(city, ", ", "AU"))) %>%
#   tidyr::unnest_wider(geocode) %>%
#   as.data.frame(.)

cities = c(unique(customer$city))
genders = c(unique(customer$gender))
brands = c(unique(transaction$brand)) %>%
  .[!is.na(.)]

ui = page_navbar(
  title = app_name,
  fillable = FALSE,
  theme = bs_theme(bootswatch = "minty",
                   navbar_bg = "#ff0000",
                   base_font = font_google("Fira Sans"),
                   code_font = font_google("Fira Code"),
                   heading_font = font_google("Fredoka One")),
  sidebar = sidebar(
    title = "Filtres",
    accordion(
      accordion_panel(
        "Clients",
        selectInput(
          "cities",
          "Villes",
          choices = cities,
          selected = "",
          multiple  = TRUE
        ),
        selectInput(
          "gender",
          "Sexe",
          choices = genders,
          selected = "",
          multiple  = TRUE
        )
      ),
      accordion_panel("Achats", "Other controls go here")
    ),
    tags$img(
      src = "logo.png",
      width = "70%",
      height = "auto"
    ),
    tags$i(paste("Mis à jour le"), format(last_update, "%d %B %Y"))
  ),
  nav_spacer(),
  nav_panel(
    title = "Clients",
    icon = bs_icon("person"),
    layout_columns(
      accordion(
        open = c("Bill Length"),
        accordion_panel("Bill Length"),
        accordion_panel("About")
      ),
      card(
        card_header("name"),
        plotly::plotlyOutput("plot"),
        card_footer(
          "Source: Gorman KB, Williams TD, Fraser WR (2014).",
          popover(
            a("Learn more", href = "#"),
            markdown(
              "Originally published in: Gorman KB, Williams TD, Fraser WR (2014) Ecological Sexual Dimorphism and Environmental Variability within a Community of Antarctic Penguins (Genus Pygoscelis). PLoS ONE 9(3): e90081. [doi:10.1371/journal.pone.0090081](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0090081)"
            )
          )
        )
      ),
      card(
        card_header("Cartographie des accidents"),
        layout_sidebar(sidebar = sidebar(
          title = "Local controls", position = "right", "..."
        ))
      ),
      value_box(
        title = "Recommended Trial",
        value = 12,
        theme_color = "secondary",
        showcase = bsicons::bs_icon("handbag")
      ),
      col_widths = c(8, 4, -3, 8, 1)
    )
  ),
  nav_panel(title = "Achats", icon = bs_icon("bag-fill")),
  nav_panel(title = "Données",icon = bs_icon("database"),
            layout_columns(
              card(
              tags$img(
                src = "store.png",
                width = "100%",
                height = "auto"
                
            ), card_footer("Magasin 99Bikes de Sydney")),
            tags$p(
              "Cet ensemble de données contient les données sur les ventes et les clients de l'un des plus grands détaillants de vélos d'Australie. Ils vendent tout ce qui concerne le vélo, des accessoires aux vêtements en passant par les casques et même les vélos électriques."
            ),
            col_widths = c(3, 9))),
  nav_menu(
    title = "À propos",
    icon = bs_icon("info-circle"),
    align = "right",
    nav_item(
      tags$a("Source de données", href = "https://www.kaggle.com/datasets/tforsyth/99bikes-sales-data")
    ),
    nav_item(
      tags$a("99Bikes website", href = "https://www.99bikes.com.au/")
    )
  ),
  footer = tags$p("Développé par Mathieu Boully")
)

server <- function(input, output) {
  output$plot = plotly::renderPlotly({
    plotly::plot_ly(
      x = c(seq(1, 3)),
      y = c(seq(1, 3)),
      type = 'scatter',
      mode = 'markers'
    )
  })
}

shinyApp(ui = ui, server = server)
