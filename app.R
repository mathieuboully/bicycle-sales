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
library(grDevices)
library(DT)
library(crosstalk)

# Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# options(
#   shiny.maxRequestSize = 100 * 1024 ^ 2,
#   dplyr.summarise.inform = FALSE,
#   stringsAsFactors = FALSE
# )

# Sys.setlocale(category = "LC_ALL", locale = "French")
app_name = "99Bikes"
last_update = file.info(file.path(getwd(), "app.R"))$atime

# if (!is.null(rstudioapi::getActiveProject())) {
#   .wdPath = setwd(rstudioapi::getActiveProject())
# } else {
#   .wdPath = dirname(rstudioapi::getSourceEditorContext()$path)
#   setwd(.wdPath)
# }

path_data = "./data"
path_image = "./www"
path_utils = "./utils"

col_pal = grDevices::colorRampPalette(c("#ebebeb", "#520e0b"))

markers = leaflet::iconList(logo = leaflet::makeIcon(iconUrl = "./www/logo2.png", iconWidth = 20))

transaction = readxl::read_xlsx(file.path(path_data, "Transaction.xlsx"))

product_line = unique(transaction$product_line)

load(file.path(path_data, "shops.rds"))

shop = shops$shop

other_shop = shops$other_shop %>%
  mutate(lon = st_coordinates(geometry)[, 1], lat = st_coordinates(geometry)[, 2]) %>%
  unnest_wider(osm_tags, names_sep = "_") %>%
  dplyr::filter(!str_detect(osm_tags_website, "99bikes"))

travel_time = 600
mode = "voiture"
isochrones = shops$isochrones %>%
  filter(value == travel_time)

other_shop_buffer = shops$other_shop_buffer

load(file.path(path_data, "customer.rds"))
wealth_segment = unique(customer$wealth_segment)
states = c(unique(customer$state))
genders = c(unique(customer$gender))
brands = c(unique(transaction$brand)) %>%
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
      open = F,
      accordion_panel(
        "Clients", icon = bsicons::bs_icon("person"),
        selectInput(
          inputId = "wealth_segment",
          label = "Segment de richesse",
          wealth_segment,
          multiple = T,
          selected = ""
        ),
        selectInput(
          inputId = "gender",
          label = "Genre",
          genders,
          multiple = T,
          selected = ""
        ),
        sliderInput(
          inputId = "past_3_years_bike_related_purchases",
          label = "Nombre de commandes",
          ticks = F,
          min = min(customer$past_3_years_bike_related_purchases),
          max = max(customer$past_3_years_bike_related_purchases),
          value = c(
            min(customer$past_3_years_bike_related_purchases),
            max(customer$past_3_years_bike_related_purchases)
          )
        )
      ),
      accordion_panel(
        "Achats", icon = bsicons::bs_icon("bag-fill"),
        selectInput(
          inputId = "product_line",
          label = "Segment produit",
          product_line,
          multiple = T,
          selected = ""
        )
      )
    ),
    tags$a(
      tags$img(
        src = "logo.png",
        width = "70%",
        height = "auto"
      ),
      href = "https://www.99bikes.com.au/",
      target = "_blank"
    ),
    tags$sub(paste(
      "Mis à jour le", format(last_update, "%d %B %Y")
    ))
  ),
  nav_spacer(),
  nav_panel(
    title = "Clients",
    icon = bs_icon("person"),
    layout_columns(
      card(
        full_screen = T,
        layout_sidebar(
          fillable = TRUE,
          border = T,
          sidebar = sidebar(
            position = "right",
            width = 500,
            open = F,
            plotly::plotlyOutput("past3_hist")
          ),
          # popover(
          #   bsicons::bs_icon("gear"),
          #   title = "Paramètres de la carte",
          #   placement = "right",
          #   bslib::input_switch(id = "show_shop", label = "99Bikes"),
          #   bslib::input_switch(id = "show_other_shop", label = "Magasins de cycles")
          # )),
          leafletOutput("customer_map")
        )
      ),
      card(
        full_screen = TRUE,
        plotly::plotlyOutput("owns_car_bar"),
        card_footer(
          "Ouverture du premier magasin à Milton en 2007.",
          popover(
            a("En savoir plus", href = "#"),
            markdown(
              "Les magasins [99Bikes](https://www.99bikes.com.au/about-us) constituent une chaîne australienne spécialisée dans la vente de vélos, accessoires et services de réparation. Depuis leur création, 99 Bikes a connu une croissance rapide et s'est imposée comme un acteur majeur du marché du vélo en Australie. L’enseigne dispose aujourd'hui de plus de 60 magasins répartis à travers le pays, couvrant notamment les grandes villes comme Sydney, Melbourne, Brisbane, et Adélaïde, mais aussi des régions moins urbaines."
            )
          )
        )
      ),
      card(full_screen = TRUE, plotly::plotlyOutput("past3_violin")),
      col_widths = c(12, 6, 6)
    )
  ),
  nav_panel(
    title = "Achats",
    icon = bs_icon("bag-fill"),
    layout_columns(
      value_box(
        title = "Vélos",
        value = scales::unit_format(unit = "références")(length(unique(
          transaction$product_id
        ))),
        theme_color = "secondary",
        showcase = bsicons::bs_icon("bicycle")
      ),
      value_box(
        title = "Chiffre d'affaire",
        value = scales::unit_format(unit = "€")(sum(transaction$list_price)),
        theme_color = "secondary",
        showcase = bsicons::bs_icon("currency-euro")
      ),
      value_box(
        title = "Ventes",
        value = length(unique(transaction$transaction_id)),
        theme_color = "secondary",
        showcase = bsicons::bs_icon("bag-fill")
      ),
      card(
        # height = 300,
        full_screen = F,
        layout_sidebar(
          fillable = TRUE,
          border = T,
          sidebar = sidebar(
            open = F,
            actionButton("btn", "A button"),
            bslib::input_task_button(
              id = "ih",
              "iui",
              icon = bs_icon("bicycle"),
              label_busy = "Processing...",
              icon_busy = rlang::missing_arg(),
              type = "primary",
              auto_reset = TRUE
            ),
            input_task_button("resample", "Resample")
          ),
          plotlyOutput("ca_per_month")
        )
      ),
      card(accordion(
        open = c("Transactions"),
        accordion_panel(title = "Transactions", plotlyOutput("transaction_scatter")),
        accordion_panel(title = "Fabricants", plotlyOutput("brand_dot")),
        accordion_panel(
          "Tableau",
          tooltip(
            bsicons::bs_icon("info-circle"),
            HTML("Tri par ordre croissant du nombre de ventes")
          ),
          dataTableOutput("brand_table")
        )
      )),
      # card(tableOutput("customer_resume"), card_footer(tooltip(
      #   bsicons::bs_icon("info-circle"),
      #   HTML(
      #     "Les adresses des clients ont été anonymisées.<br>Regroupement des clients au niveau de la capitale de l'état de rattachement."
      #   )
      # ))),
      # accordion(
      #   open = c("Bill Length"),
      #   accordion_panel("Bill Length"),
      #   accordion_panel("About")
      # ),
      # navset_card_underline(
      #   title = "Histograms by species",
      #   nav_panel("Bill Length", plotOutput("bill_length")),
      #   nav_panel("Bill Depth", plotOutput("bill_depth")),
      #   nav_panel("Body Mass", plotOutput("body_mass"))
      # ),
      col_widths = c(4, 4, 4, 12, 12)
    )
  ),
  nav_panel(
    title = "À propos",
    icon = bs_icon("info-circle"),
    fluidRow(
      column(
        width = 6,
        h2("Bienvenue sur l'application !"),
        p(
          "Cette application a été conçue pour visualiser et analyser des données sur les clients et les produits."
        ),
        br(),
        hr(),
        h3("Fonctionnalités principales"),
        tags$ul(
          tags$li("visualisation des achats liés aux vélos"),
          tags$li(
            "segmentation des clients par catégorie de richesse et secteur d'activité"
          ),
          tags$li("analyse de la fidélité des clients")
        ),
        hr(),
        h3("À propos de 99Bikes"),
        tags$p(
          tags$a(href = "https://www.99bikes.com.au/", "99Bikes", target = "_blank"),
          "est l'une des plus grandes chaînes de magasins spécialisés en vélos en Australie. Fondée en 2007, elle est devenue une référence pour les amateurs de cyclisme en proposant un large éventail de produits liés aux vélos, notamment des vélos de route, de montagne, électriques, et de loisir, ainsi qu'une large sélection d’accessoires et de pièces de rechange. La marque offre également des services de réparation et d’entretien, avec des ateliers dans la majorité de ses magasins pour répondre aux besoins techniques des cyclistes."
        )
      ),
      column(
        width = 6,
        tags$figure(
          tags$img(
            src = "shop.png",
            width = "100%",
            height = "auto"
          ),
          tags$figcaption("99Bikes de Sydney")
        ),
        hr(),
        h3("Contact"),
        p("Pour toute question ou suggestion, n'hésitez pas à me contacter :"),
        tags$ul(tags$li(
          "Email : ",
          tags$a(href = "mailto:mathieu.boully@hotmail.com", "mathieu.boully@hotmail.com")
        )),
        tags$ul(tags$li(
          tags$a(href = "https://mathieuboully-pro.netlify.app/", "Site web", target = "_blank")
        )),
        hr(),
        h3("Télécharger les données"),
        downloadButton("download_customer", tooltip(
          "Clients", HTML("Format .csv avec séparateur ';' et décimal '.'")
        )),
        br(),
        br(),
        downloadButton("download_transaction", tooltip(
          "Achats", HTML("Format .csv avec séparateur ';' et décimal '.'")
        )),
        br(),
        br(),
        downloadButton("download_shop", tooltip(
          "Magasins 99Bikes",
          HTML("Format .csv avec séparateur ';' et décimal '.'")
        )),
        br(),
        br(),
        downloadButton("download_other_shop", tooltip(
          "Magasins concurrants",
          HTML("Format .csv avec séparateur ';' et décimal '.'")
        ))
      )
    )
  ),
  # nav_menu(
  #   title = "À propos",
  #   icon = bs_icon("info-circle"),
  #   align = "right",
  #   nav_item(
  #     tags$a("99Bikes site web", href = "https://www.99bikes.com.au/", target = "_blank")
  #   ),
  #   nav_item(
  #     tags$a("GitHub", href = "https://github.com/mathieuboully/bicycle-sales", target = "_blank")
  #   )
  # ),
  footer = tags$sub(
    "© 2024",
    tags$a(href = "https://mathieuboully-pro.netlify.app/", "Mathieu Boully", target = "_blank") ,
    " ∙ Cette application a été crée avec",
    a("Shiny", href = "https://shiny.posit.co/")
  )
)


server = function(input, output, session) {
  sample <- eventReactive(input$resample, ignoreNULL = FALSE, {
    Sys.sleep(1000)
  })
  
  output$download_customer <- downloadHandler(
    filename = function() {
      paste("customer_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        customer,
        file,
        row.names = FALSE,
        sep = ";",
        dec = ".",
        fileEncoding = "UTF-8"
      )
    }
  )
  
  output$download_transaction <- downloadHandler(
    filename = function() {
      paste("transaction_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        transaction,
        file,
        row.names = FALSE,
        sep = ";",
        dec = ".",
        fileEncoding = "UTF-8"
      )
    }
  )
  
  output$download_shop <- downloadHandler(
    filename = function() {
      paste("shop_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        shop,
        file,
        row.names = FALSE,
        sep = ";",
        dec = ".",
        fileEncoding = "UTF-8"
      )
    }
  )
  
  output$download_other_shop <- downloadHandler(
    filename = function() {
      paste("other_shop_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        other_shop,
        file,
        row.names = FALSE,
        sep = ";",
        dec = ".",
        fileEncoding = "UTF-8"
      )
    }
  )
  
  selected_wealth_segment = reactive({
    if (is.null(input$wealth_segment))
      wealth_segment
    else
      input$wealth_segment
  })
  
  selected_gender = reactive({
    if (is.null(input$gender))
      genders
    else
      input$gender
  })
  
  selected_3past = reactive({
    return(
      c(
        input$past_3_years_bike_related_purchases[1],
        input$past_3_years_bike_related_purchases[2]
      )
    )
  })
  
  filtered_customer = reactive({
    customer %>%
      filter(
        wealth_segment %in% selected_wealth_segment(),
        gender %in% selected_gender(),
        between(
          past_3_years_bike_related_purchases,
          selected_3past()[1],
          selected_3past()[2]
        )
      )
  })
  
  selected_product_line = reactive({
    if (is.null(input$product_line))
      product_line
    else
      input$product_line
  })
  
  filtered_transaction = reactive({
    transaction %>%
      filter(product_line %in% selected_product_line())
  })
  
  output$customer_map = renderLeaflet({
    pal = colorNumeric(
      c("#ffffff", "#dc241e"),
      domain = as.integer(customer$past_3_years_bike_related_purchases)
    )
    
    map = leaflet::leaflet(options = leafletOptions(minZoom = 4, maxZoom = 17)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(opacity = 0.8)) %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>%
      setView(lat = -33.8688, lng = 151.2093, zoom = 10) %>%
      setMaxBounds(
        lng1 = 113,
        lat1 = -44,
        lng2 = 154,
        lat2 = -10
      ) %>%
      addPolygons(
        data = isochrones,
        weight = 1,
        color = "#ebebeb",
        fillColor = "#ebebeb",
        fillOpacity = 0.2,
        # label = ~ paste(
        #   "Temps de trajet inférieur à",
        #   floor(value / 60),
        #   "min(s)<br><sup>mode de transport :",
        #   mode,
        #   "</sup>"
        # ) 
        # %>%
        #   lapply(htmltools::HTML),
        labelOptions = labelOptions(
          noHide = F,
          direction = "top",
          opacity = 1,
          textOnly = FALSE,
          style = list(
            # "color" = col_pal(2)[2],
            # "font-size" = "12px",
            # "font-family" = "Arial",
            # "background-color" = col_pal(5)[2],
            # "border-color" = "transparent"
          )
        )
      ) %>%
      addCircleMarkers(
        data = filtered_customer(),
        lng = ~ lon,
        lat = ~ lat,
        color = ~ pal(as.numeric(
          past_3_years_bike_related_purchases
        )),
        radius = 3,
        opacity = 0.65,
        label = ~ paste(
          "Nombre de commandes : ",
          past_3_years_bike_related_purchases,
          "<br><sup>3 dernières années</sup><br>",
          "Niveau de richesse : ",
          wealth_segment,
          "<br>Véhiculé : ",
          owns_car
        ) %>%
          lapply(htmltools::HTML)
      ) %>%
      addLegend(
        data = customer,
        pal = pal,
        values = ~ as.numeric(past_3_years_bike_related_purchases),
        position = "bottomleft",
        title = HTML("Nombre de commandes<br><sup>3 dernières années</sup>"),
        opacity = 0.65
      ) %>%
      addLegend(
        labels = c(paste(
          "Magasins de vélos à proximité",
          "<br><sup>dans un rayon de",
          other_shop_buffer,
          "mètres</sup>"
        ), paste(
          "Temps de trajet inférieur à",
          floor(travel_time / 60),
          "min(s)<br><sup>mode de transport :",
          mode,
          "</sup>"
        )),
        color = c("#ffbd00", "#ffffff"),
        position = "bottomleft",
        opacity = 0.65
      ) %>%
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
      ) %>%
      leaflet::addCircleMarkers(
        data = other_shop,
        lng = ~ lon,
        lat = ~ lat,
        color = "#ffbd00",
        radius = 3,
        opacity = 1,
        popup = ~ paste0(
          "<strong>",
          osm_tags_name,
          "</strong><br>",
          "<a href='",
          osm_tags_website,
          "' target='_blank'>Site web</a>"
        )
      )
  })
  
  output$owns_car_bar = plotly::renderPlotly({
    filtered_customer() %>%
      filter(!is.na(state), !is.na(owns_car)) %>%
      dplyr::group_by(state, owns_car) %>%
      dplyr::summarise(count = n(), , .groups = 'drop') %>%
      pivot_wider(
        names_from = owns_car,
        values_from = count,
        values_fill = 0
      ) %>%
      plot_ly(
        x = ~ state,
        y = ~ Yes,
        type = "bar",
        name = "oui",
        marker = list(color = '#dc241e'),
        hovertemplate = paste(
          "%{xaxis.title.text} : %{x}<br>",
          "%{yaxis.title.text} : %{y:.0f}",
          "<extra></extra>"
        )
      ) %>%
      add_trace(
        y = ~ No,
        name = 'non',
        marker = list(color = '#520e0b')
      ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>",
            "Répartition des personnes véhiculés selon les états</b>"
          ),
          font = list(size = 15, color = "grey")
        ),
        xaxis = list(
          title = "états",
          zeroline = F,
          showgrid = F,
          showticklabels = T,
          fixedrange = T,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        yaxis = list(
          title = "nombre de clients",
          zeroline = F,
          showgrid = T,
          showticklabels = T,
          fixedrange = T,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        showlegend = T,
        legend = list(
          title = list(text = paste0("<b>", "véhiculé", "</b>")),
          orientation = "v",
          itemsizing = "constant",
          # itemwidth = ,
          font = list(color = "grey"),
          bgcolor = "#ffffff",
          bordercolor = "#ffffff",
          borderwidth = 0
        )
      )
  })
  
  output$past3_violin = renderPlotly({
    filtered_customer() %>%
      plot_ly(
        x = ~ as.factor(state),
        y = ~ past_3_years_bike_related_purchases,
        split = ~ as.factor(state),
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)
      ) %>% layout(
        title = list(
          text = paste0(
            "<b>",
            "Distribution du nombre de commandes selon les états</b>",
            "<br><sup><i>",
            "3 dernières années",
            "</i></sup>"
          ),
          font = list(size = 15, color = "grey")
        ),
        xaxis = list(title = "états"),
        yaxis = list(title = "nombre de commandes", zeroline = F),
        showlegend = T,
        legend = list(
          title = list(text = paste0("<b>", "états", "</b>")),
          orientation = "v",
          itemsizing = "constant",
          # itemwidth = ,
          font = list(color = "grey"),
          bgcolor = "#ffffff",
          bordercolor = "#ffffff",
          borderwidth = 0
        )
      )
  })
  
  output$past3_hist = renderPlotly({
    filtered_customer() %>%
      plot_ly(
        x = ~ past_3_years_bike_related_purchases,
        type = "histogram",
        marker = list(color = '#520e0b'),
        hovertemplate = paste(
          "%{xaxis.title.text} : %{x:.0f}<br>",
          "%{yaxis.title.text} : %{y:.0f}",
          "<extra></extra>"
        )
      ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>",
            "Distribution du nombre de commandes</b>",
            "<br><sup><i>",
            "3 dernières années",
            "</i></sup>"
          ),
          font = list(size = 15, color = "grey")
        ),
        xaxis = list(
          title = "nombre de commandes",
          zeroline = F,
          showgrid = F,
          showticklabels = T,
          fixedrange = T,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        yaxis = list(
          title = "total",
          zeroline = F,
          showgrid = T,
          showticklabels = T,
          fixedrange = T,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        showlegend = F
      )
  })
  
  output$ca_per_month = renderPlotly({
    filtered_transaction() %>%
      mutate(transaction_date = floor_date(transaction_date, "month")) %>%
      group_by(transaction_date) %>%
      summarise(list_price = sum(list_price)) %>%
      plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(
        x = ~ transaction_date,
        y = ~ list_price,
        name = 'list_price',
        line = list(color = '#520e0b', width = 2),
        hovertemplate = paste(
          "%{xaxis.title.text} : %{x}<br>",
          "%{yaxis.title.text} : %{y:.0f}<br>",
          "%{text}",
          "<extra></extra>"
        )
      ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>",
            "Évolution des ventes</b>",
            "<br><sup><i>",
            "chiffre d'affaires en euros",
            "</i></sup>"
          ),
          font = list(size = 15, color = "grey")
        ),
        xaxis = list(
          title = "date",
          zeroline = F,
          showgrid = T,
          showticklabels = T,
          fixedrange = T,
          color = "grey",
          tickfont = list(color = "grey"),
          rangeslider = list(visible = T),
          rangeselector = list(buttons = list(
            list(
              count = 1,
              label = "1m",
              step = "month",
              stepmode = "backward"
            ),
            list(
              count = 6,
              label = "6m",
              step = "month",
              stepmode = "backward"
            ),
            list(
              count = 1,
              label = "YTD",
              step = "year",
              stepmode = "todate"
            ),
            list(
              count = 1,
              label = "1y",
              step = "year",
              stepmode = "backward"
            ),
            list(step = "all")
          ))
        ),
        yaxis = list(
          title = "ventes [€]",
          zeroline = F,
          showgrid = F,
          showticklabels = T,
          fixedrange = T,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        showlegend = F
      )
  })
  
  output$transaction_scatter = renderPlotly({
    filtered_transaction() %>%
      group_by(customer_id) %>%
      summarise(
        n_transaction = n(),
        avg_transaction = mean(list_price, na.rm = T),
        sum_transaction = sum(list_price, na.rm = T)
      ) %>%
      dplyr::left_join(filtered_customer(), ., by = c("customer_id")) %>%
      plot_ly(
        x = ~ n_transaction,
        y = ~ avg_transaction,
        type = "scatter",
        mode = "markers",
        color = ~ state,
        text = ~ paste0(
          "véhiculé : ",
          owns_car,
          "<br>",
          "état : ",
          state,
          "<br>",
          "job : ",
          job_title
        ),
        hovertemplate = paste(
          "%{xaxis.title.text} : %{x:.0f}<br>",
          "%{yaxis.title.text} : %{y:.0f}<br>",
          "%{text}",
          "<extra></extra>"
        )
      ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>",
            "Nombre de transactions en fonction du montant moyen</b>",
            "<br><sup><i>",
            "segment basé sur les états",
            "</i></sup>"
          ),
          font = list(size = 15, color = "grey")
        ),
        xaxis = list(
          title = "nombre de transactions",
          zeroline = F,
          showgrid = F,
          showticklabels = T,
          fixedrange = T,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        yaxis = list(
          title = "montant moyen des transactions [€]",
          zeroline = F,
          showgrid = T,
          showticklabels = T,
          fixedrange = T,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        showlegend = T,
        legend = list(
          title = list(text = paste0("<b>", "états", "</b>")),
          orientation = "v",
          itemsizing = "constant",
          # itemwidth = ,
          font = list(color = "grey"),
          bgcolor = "#ffffff",
          bordercolor = "#ffffff",
          borderwidth = 0
        )
      )
  })
  
  output$brand_dot = renderPlotly({
    transaction %>%
      group_by(brand) %>%
      dplyr::summarise(sum_transaction = sum(list_price)) %>%
    plot_ly(
      x = ~ sum_transaction,
      y = ~ reorder(brand, sum_transaction),
      type = "scatter",
      mode = "markers",
      text = ~ paste0(floor(sum_transaction), ' €'),
      hoverinfo = 'text',
      marker = list(
        color = col_pal(2)[2],
        size = 20,
        line = list(color = col_pal(2)[2],
                    width = 0)
      )
    ) %>%
      layout(
        title = list(text = '<b>Total des transactions par fabricant</b>',
                     font = list(size = 15,
                                 color = "gray")),
        showlegend = FALSE,
        xaxis = list(
          title = "",
          showgrid = F,
          showticklabels = T,
          fixedrange = TRUE,
          color = "grey",
          tickfont = list(color = "grey")
          # range = ~ c(min(sum_transaction), max(sum_transaction))
        ),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          showticklabels = TRUE,
          fixedrange = TRUE,
          color = "grey",
          tickfont = list(color = "grey")
        ),
        hoverlabel = list(font = list(color = "grey"))
      )
  })
  
  output$brand_table = renderDataTable({
    DT::datatable(
      filtered_transaction() %>%
        group_by(brand) %>%
        summarise(
          n_transaction = n(),
          avg_transaction = mean(list_price, na.rm = T),
          sum_transaction = sum(list_price, na.rm = T)
        ) %>%
        arrange(n_transaction),
      rownames = FALSE,
      colnames = c(
        "Marque",
        "Nombre de transactions",
        "Moyenne des transactions",
        "Montant des transactions"
      ),
      options = list(
        searching = F,
        lengthChange = F,
        # pageLength = 5,
        # lengthMenu = c(5, 10, 15, 20),
        # scrollY = 300, scrollCollapse = TRUE
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')
      )
    ) %>%
      formatCurrency(
        c("avg_transaction", "sum_transaction"),
        currency = "€",
        digits = 0
      )
  })
}

# https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html
shinyApp(ui = ui, server = server)
