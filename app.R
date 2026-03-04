# Packages
library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(adsbr)

ui <- page_sidebar(
  tags$head(
    tags$style(HTML(".recalculating { opacity: 1.0 !important; }"))
  ),
  title = "SBJR Live",
  sidebar = sidebar(
    sliderInput(
      "refresh_rate",
      "Atualização mapa (s)",
      min = 1,
      max = 10,
      value = 5,
      step = 1
    ),
    sliderInput(
      "radius",
      "Raio (nm)",
      min = 10,
      max = 250,
      value = 30,
      step = 5
    ),
    card(
      card_header("Torre 118.4 Mhz"),
      card_body(
        tags$audio(
          controls = TRUE,
          autoplay = TRUE,
          style = "width: 100%;",
          tags$source(
            src = "http://nr9.newradio.it:9211/stream",
            type = "audio/mpeg"
          ),
          "Your browser does not support the audio element."
        )
      )
    ),
    card(
      card_header("Solo 121.6 Mhz"),
      card_body(
        tags$audio(
          controls = TRUE,
          autoplay = TRUE,
          style = "width: 100%;",
          tags$source(
            src = "http://nr9.newradio.it:9211/stream",
            type = "audio/mpeg"
          ),
          "Your browser does not support the audio element."
        )
      ),
    )
  ),
  card(
    full_screen = TRUE,
    card_header("ADS-B"),
    card_body(
      fillable = TRUE, # Allows the map to fill the card
      padding = 0, # Removes white border around the map
      leafletOutput("map", height = "100%")
    )
  )
)

server <- function(input, output, session) {
  # Map
  center_lat <- -22.9875
  center_lon <- -43.37

  poligono_presal <- data.frame(
    id = 1:11,
    lat = c(
      -23.80583,
      -24.43278,
      -25.08083,
      -26.76111,
      -28.76139,
      -27.17056,
      -25.18250,
      -24.24361,
      -23.81083,
      -23.57083,
      -23.49889
    ),
    lng = c(
      -41.89000,
      -41.61167,
      -41.52139,
      -43.76222,
      -45.38250,
      -47.95750,
      -46.76722,
      -44.55278,
      -44.03417,
      -43.13333,
      -41.99639
    )
  )

  pontos_adicionais <- data.frame(
    nome = c(
      "ABERLARDO",
      "AMÉRICAS",
      "BARRA",
      "CURICICA",
      "MARAPENDI",
      "MUNDIAL",
      "OLIMPICO",
      "PENINSULA",
      "ORLA",
      "PANELA",
      "PRAÇA",
      "PRAIA",
      "SENNA",
      "TIJUCAS",
      "TRAVÉS MORRO PANELA",
      "PONTAL"
    ),
    lat = c(
      -22.97316,
      -22.99999,
      -23.02709,
      -22.94028,
      -23.04278,
      -22.96111,
      -23.02278,
      -22.98863,
      -23.01138,
      -22.97222,
      -23.02225,
      -23.01035,
      -23.08444,
      -23.07833,
      -22.97013,
      -23.09167
    ),
    lng = c(
      -43.37127,
      -43.37310,
      -43.32878,
      -43.38722,
      -43.37611,
      -43.39167,
      -43.39444,
      -43.35054,
      -43.37391,
      -43.33694,
      -43.31663,
      -43.35196,
      -43.37639,
      -43.26750,
      -43.34932,
      -43.47500
    )
  )

  rotatedMarker <- htmlDependency(
    "Leaflet.rotatedMarker",
    "0.1.2",
    src = normalizePath("."),
    script = "www/leaflet.rotatedMarker.js"
  )

  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }

  arrow_icon <- makeIcon(
    iconUrl = "www/uparrow.png",
    iconWidth = 24,
    iconHeight = 14,
    iconAnchorX = 23,
    iconAnchorY = 12
  )

  # Initialize the static map
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "Ruas") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      setView(center_lon, center_lat, zoom = 10) |>
      addCircles(
        lng = center_lon,
        lat = center_lat,
        radius = input$radius * 1852,
        color = "#000000ff",
        weight = 2,
        fillOpacity = 0.1,
        group = "Raio de busca"
      ) |>
      addPolygons(
        data = poligono_presal,
        lng = ~lng,
        lat = ~lat,
        fillColor = "#B19CD9",
        fillOpacity = 0.4,
        color = "#6A5ACD",
        weight = 2,
        label = "Espaço Aéreo do Pré-sal",
        group = "Espaço Aéreo do Pré-sal"
      ) |>
      addLayersControl(
        baseGroups = c("Ruas", "Imagem de satélite"),
        overlayGroups = c(
          "SBRJ",
          "Pontos de navegação",
          "Aeronaves",
          "Espaço Aéreo do Pré-sal",
          "Raio de busca"
        ),
        options = layersControlOptions(collapsed = FALSE) # Deixa o controle aberto
      )
  })

  # Update map
  observe({
    # Refresh every 10000 milliseconds
    invalidateLater(input$refresh_rate * 1000, session)

    # Fetch data
    res <- fetch_point(
      provider = "adsb_lol",
      lat = center_lat,
      lon = center_lon,
      radius = input$radius
    )

    # 3. Use proxy to update ONLY the markers
    leafletProxy("map", data = res) |>
      clearMarkers() |>
      registerPlugin(rotatedMarker) |>
      addCircleMarkers(
        lng = center_lon,
        lat = center_lat,
        radius = 6,
        color = "white",
        fillColor = "#000000ff",
        fillOpacity = 1,
        weight = 2,
        label = "SBRJ",
        group = "SBRJ"
      ) |>
      addCircleMarkers(
        data = pontos_adicionais,
        lng = ~lng,
        lat = ~lat,
        radius = 4,
        color = "white",
        fillColor = "#6A5ACD",
        fillOpacity = 0.8,
        weight = 1,
        label = ~nome,
        group = "Pontos de navegação",
      ) |>
      addMarkers(
        ~lon,
        ~lat,
        label = ~ lapply(
          paste0(
            "<div style='font-family: sans-serif; min-width: 150px;'>",
            "<h5 style='margin: 0 0 5px 0; color: #2e2e2eff;'>",
            ifelse(flight == "", "N/A", flight),
            "</h5>",
            "<hr style='margin: 5px 0;'>",
            "<b>Código:</b> ",
            r,
            "<br>",
            "<b>Aeronave:</b> ",
            t,
            "<br>",
            "<b>Altitude:</b> ",
            alt_baro,
            " ft",
            "<br>",
            "<b>Velocidade (GS):</b> ",
            gs,
            " kt",
            "</div>"
          ),
          htmltools::HTML
        ),
        icon = arrow_icon,
        options = markerOptions(
          rotationAngle = ~track,
          rotationOrigin = "center center"
        ),
        group = "Aeronaves"
      )
  })
}

shinyApp(ui, server)
