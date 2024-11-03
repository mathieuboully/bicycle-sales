# rm(list = ls())

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(lubridate)
library(stringr)
library(leaflet)
library(plotly)
library(sf)
library(tidyr)

# Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# options(
#   shiny.maxRequestSize = 100 * 1024 ^ 2,
#   dplyr.summarise.inform = FALSE,
#   stringsAsFactors = FALSE
# )

# Sys.setlocale(category = "LC_ALL", locale = "French")
app_name = "99Bikes"
last_update = Sys.Date()
version = "1.0"

# if (!is.null(rstudioapi::getActiveProject())) {
#   .wdPath = setwd(rstudioapi::getActiveProject())
# } else {
#   .wdPath = dirname(rstudioapi::getSourceEditorContext()$path)
#   setwd(.wdPath)
# }

path_data = "./data"
path_image = "./www"
path_utils = "./utils"

# Fonctions
# source(file.path(path_utils, "get_geocode.R"), encoding = "UTF-8")

markers = leaflet::iconList(logo = leaflet::makeIcon(iconUrl = "./www/logo2.png", iconWidth = 20))

# Data
transcation = readxl::read_xlsx(file.path(path_data, "Transaction.xlsx"))

load(file.path(path_data, "shops.rds"))
shop = shops$shop
other_shop = shops$other_shop %>%
  mutate(lon = st_coordinates(geometry)[, 1], lat = st_coordinates(geometry)[, 2]) %>%
  unnest_wider(osm_tags, names_sep = "_")

load(file.path(path_data, "customer.rds"))

states = c(unique(customer$state))
genders = c(unique(customer$gender))
brands = c(unique(transcation$brand)) %>%
  .[!is.na(.)]

ui = page_navbar(
  title = app_name,
  fillable = FALSE,
  theme = bs_theme(
    bootswatch = "minty",
    bg = "#ffffff",
    fg = "#000000",
    primary = "#dc241e",
    secondary = "#520e0b",
    success = "#ebebeb",
    navbar_bg = "#dc241e",
    base_font = font_google("Fira Sans"),
    code_font = font_google("Fira Code"),
    heading_font = font_google("Fredoka One")
  ),
  sidebar = sidebar(
    title = "Filtres",
    accordion(
      accordion_panel(
        "Clients",
        icon = bs_icon("person"),
        selectInput(
          "states",
          "État",
          choices = states,
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
      accordion_panel(
        "Achats",
        bsicons::bs_icon("handbag"),
        dateRangeInput(
          "date",
          "Date",
          start = min(transcation$transaction_date, na.rm = T),
          end = max(transcation$transaction_date, na.rm = T),
          min = min(transcation$transaction_date, na.rm = T),
          max = max(transcation$transaction_date, na.rm = T),
          format = "dd-mm-yyyy",
          startview = "month",
          weekstart = 1,
          language = "fr",
          separator = " à ",
          width = NULL,
          autoclose = TRUE
        )
      )
    ),
    tags$img(
      src = "logo.png",
      width = "70%",
      height = "auto"
    ),
    tags$sub(paste(
      "Mis à jour le", format(last_update, "%d %B %Y")
    )),
    tags$sub(paste("Version", version))
  ),
  nav_spacer(),
  nav_panel(
    title = "Clients",
    icon = bs_icon("person"),
    layout_columns(
      card(
        full_screen = T,
        card_header(
          "Localisation des clients",
          popover(
            bsicons::bs_icon("gear"),
            title = "Paramètres de la carte",
            placement = "right",
            bslib::input_switch(id = "show_shop", label = "99Bikes"),
            bslib::input_switch(id = "show_other_shop", label = "Magasins de cycles")
          )
        ),
        leafletOutput("customer_map")
      ),
      value_box(
        title = "Nombre de clients",
        value = nrow(customer),
        theme_color = "secondary",
        showcase = bsicons::bs_icon("person-fill")
      ),
      card(
        card_header("name"),
        layout_sidebar(
          border = T,
          sidebar = sidebar(
            title = "Local controls",
            position = "right",
            open = "open",
            "oicvjofjeofjeofjo"
          )
        ),
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
      value_box(
        title = "Client total",
        value = scales::unit_format(unit = "clients")(nrow(customer)),
        theme_color = "secondary",
        showcase = bsicons::bs_icon("handbag")
      ),
      card(tableOutput("customer_resume"), card_footer(tooltip(
        bsicons::bs_icon("info-circle"),
        HTML(
          "Les adresses des clients ont été anonymisées.<br>Regroupement des clients au niveau de la capitale de l'état de rattachement."
        )
      ))),
      accordion(
        open = c("Bill Length"),
        accordion_panel("Bill Length"),
        accordion_panel("About")
      ),
      navset_card_underline(
        title = "Histograms by species",
        nav_panel("Bill Length", plotOutput("bill_length")),
        nav_panel("Bill Depth", plotOutput("bill_depth")),
        nav_panel("Body Mass", plotOutput("body_mass"))
      ),
      col_widths = c(10, 2, 12, 3, 3, 12)
    )
  ),
  nav_panel(title = "Achats", icon = bs_icon("bag-fill")),
  nav_panel(
    title = "Magasins",
    icon = bs_icon("shop"),
    layout_columns(
      card(
        tags$img(
          src = "shop.png",
          width = "100%",
          height = "auto"
          
        ),
        card_footer(tags$sub("Magasin 99Bikes de Sydney"))
      ),
      tags$p(
        "Cet ensemble de données contient les données sur les ventes et les clients de l'un des plus grands détaillants de vélos d'Australie. Ils vendent tout ce qui concerne le vélo, des accessoires aux vêtements en passant par les casques et même les vélos électriques."
      ),
      col_widths = c(12, 3, 9)
    )
  ),
  nav_menu(
    title = "À propos",
    icon = bs_icon("info-circle"),
    align = "right",
    nav_item(
      tags$a("99Bikes site web", href = "https://www.99bikes.com.au/", target = "_blank")
    ),
    nav_item(
      tags$a("GitHub", href = "https://github.com/mathieuboully/bicycle-sales", target = "_blank")
    )
  ),
  footer = tags$sub(
    "© 2024 Mathieu Boully ∙ Cette application a été crée avec",
    a("shiny", href = "https://shiny.posit.co/")
  )
)

server = function(input, output) {
  selected_groups <- reactive({
    if (is.null(input$groups_map))
      groups_map
    else
      input$groups_map
  })
  
  observe({
    if (!input$show_shop & !input$show_other_shop) {
      output$customer_map = renderLeaflet({
        coords <- data.frame(
          lat = c(-33.8688, -33.8745, -33.8745, -33.8688),  # Latitudes
          lng = c(151.2093, 151.2093, 151.2150, 151.2150)   # Longitudes
        )
        
        pal = colorNumeric(c("#ffffff", "#dc241e"), domain = as.integer(customer$DOB))
        
        map = leaflet::leaflet(options = leafletOptions(minZoom = 4, maxZoom = 17)) %>%
          addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(opacity = 0.8)) %>%
          addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>%
          setMaxBounds(
            lng1 = 113,
            lat1 = -44,
            lng2 = 154,
            lat2 = -10
          ) %>%
          addCircleMarkers(
            data = customer,
            lng = ~ lon,
            lat = ~ lat,
            color = ~ pal(as.integer(DOB)),
            radius = 3,
            opacity = 0.65
          ) %>%
          addPolygons(
            data = coords,
            lng = ~ lng,  # Longitudes
            lat = ~ lat,  # Latitudes
            color = "white",  # Couleur de la bordure
            weight = 1,  # Épaisseur de la bordure
            opacity = 1,  # Opacité de la bordure
            fillColor = "#E2E2E2",  # Couleur de remplissage
            fillOpacity = 0.2  # Opacité du remplissage
          ) %>%
          addLegend(
            data = customer,
            pal = pal,
            values = ~ as.integer(DOB),
            position = "bottomleft",
            title = "DOB",
            opacity = 0.9
          )
        
        return(map)
      })
    } else if (input$show_shop) {
      leafletProxy("customer_map") %>%
        addMarkers(
          clusterId = "shop",
          data = shop,
          lng = ~ lon,
          lat = ~ lat,
          icon = markers["logo"],
          popup = ~ paste0(
            "<strong>",
            shop_name,
            "</strong><br>",
            "Email: <a href='mailto:",
            email,
            "' target='_blank'>",
            email,
            "</a><br>",
            "Téléphone: <a href='tel:",
            phone,
            "' target='_blank'>",
            phone,
            "</a><br>",
            "<a href='",
            direction_url,
            "' target='_blank'>S'y rendre</a><br>",
            "<a href='https://www.99bikes.com.au/",
            shop_url,
            "' target='_blank'>Horaires, services, etc.</a><br>",
            "<img src='https://www.99bikes.com.au",
            image_url,
            "' width='200'>"
          )
        )
    } else if (input$show_other_shop) {
      leafletProxy("customer_map") %>%
        addCircleMarkers(
          clusterId = "other_shop",
          data = other_shop,
          lng = ~ lon,
          lat = ~ lat,
          color = "orange",
          radius = 5,
          opacity = 1,
          popup = ~ paste0("<strong>", osm_tags_name, "</strong><br>",
                           "<a href='",
                           osm_tags_website,
                           "' target='_blank'>Site web</a>")
        )
    }
  })
  
  output$plot = plotly::renderPlotly({
    plot = plotly::plot_ly(
      x = c(seq(1, 3)),
      y = c(seq(1, 3)),
      type = 'scatter',
      mode = 'markers'
    )
  })
  
  output$customer_resume = renderTable({
    customer %>%
      dplyr::group_by(city) %>%
      dplyr::summarise(n())
  }, digits = 0)
}

shinyApp(ui = ui, server = server)
