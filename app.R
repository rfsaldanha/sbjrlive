# Packages
library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(httr2)
library(purrr)
library(tibble)
library(DT)
library(dplyr)

# Functions
adsb_providers <- c(
  "ADSB.lol" = "https://api.adsb.lol/v2",
  "Airplanes.live" = "https://api.airplanes.live/v2",
  "adsb.fi" = "https://opendata.adsb.fi/api/v2"
)

adsb_provider_paths <- c(
  "ADSB.lol" = "latlon",
  "Airplanes.live" = "point",
  "adsb.fi" = "latlon"
)

adsb_cache <- new.env(parent = emptyenv())
adsb_cache_ttl <- 3
center_lat <- -22.9875
center_lon <- -43.37

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

flight_columns <- list(
  hex = character(),
  flight = character(),
  r = character(),
  t = character(),
  alt_baro = character(),
  gs = numeric(),
  squawk = character(),
  category = character(),
  rssi = numeric(),
  seen = numeric(),
  seen_pos = numeric(),
  lat = numeric(),
  lon = numeric(),
  track = numeric(),
  sources = character(),
  source_count = numeric()
)

empty_flights_tibble <- function() {
  tibble::as_tibble(flight_columns)
}

empty_column <- function(template, n) {
  if (is.numeric(template)) {
    rep(NA_real_, n)
  } else {
    rep(NA_character_, n)
  }
}

normalize_flights <- function(x) {
  x <- tibble::as_tibble(x)
  missing_cols <- setdiff(names(flight_columns), names(x))

  for (col in missing_cols) {
    x[[col]] <- empty_column(flight_columns[[col]], nrow(x))
  }

  x$hex <- as.character(x$hex)
  x$flight <- as.character(x$flight)
  x$r <- as.character(x$r)
  x$t <- as.character(x$t)
  x$alt_baro <- as.character(x$alt_baro)
  x$gs <- as.numeric(x$gs)
  x$squawk <- as.character(x$squawk)
  x$category <- as.character(x$category)
  x$rssi <- as.numeric(x$rssi)
  x$seen <- as.numeric(x$seen)
  x$seen_pos <- as.numeric(x$seen_pos)
  x$lat <- as.numeric(x$lat)
  x$lon <- as.numeric(x$lon)
  x$track <- as.numeric(x$track)
  x$sources <- as.character(x$sources)
  x$source_count <- as.numeric(x$source_count)

  x
}

cache_key <- function(provider, lat, lon, radius) {
  paste(provider, lat, lon, radius, sep = "|")
}

provider_label <- function(provider) {
  provider %||% "Unknown"
}

has_value <- function(x) {
  !is.na(x) & trimws(as.character(x)) != ""
}

first_nonempty <- function(x) {
  x <- x[has_value(x)]
  if (length(x) == 0) NA_character_ else x[[1]]
}

first_number <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[[1]]
}

collapse_sources <- function(x) {
  values <- unique(unlist(strsplit(paste(na.omit(x), collapse = ", "), ",\\s*")))
  values <- sort(values[has_value(values)])
  if (length(values) == 0) NA_character_ else paste(values, collapse = ", ")
}

count_sources <- function(x) {
  values <- unique(unlist(strsplit(paste(na.omit(x), collapse = ", "), ",\\s*")))
  sum(has_value(values))
}

freshness_score <- function(seen_pos, seen) {
  dplyr::coalesce(seen_pos, seen, Inf)
}

dedupe_flights <- function(data) {
  data <- normalize_flights(data)

  if (nrow(data) == 0) {
    return(data)
  }

  data |>
    mutate(
      dedupe_key = ifelse(
        has_value(hex),
        paste0("hex:", tolower(hex)),
        paste("fallback", flight, r, t, round(lat, 3), round(lon, 3), sep = "|")
      ),
      has_position = !is.na(lat) & !is.na(lon),
      freshness = freshness_score(seen_pos, seen),
      completeness = rowSums(!is.na(pick(any_of(names(flight_columns)))))
    ) |>
    arrange(dedupe_key, desc(has_position), freshness, desc(completeness)) |>
    group_by(dedupe_key) |>
    summarise(
      across(
        c(hex, flight, r, t, alt_baro, squawk, category),
        first_nonempty
      ),
      across(
        c(gs, rssi, seen, seen_pos, lat, lon, track),
        first_number
      ),
      sources = collapse_sources(sources),
      source_count = count_sources(sources),
      .groups = "drop"
    ) |>
    select(any_of(names(flight_columns))) |>
    normalize_flights()
}

fetch_point <- function(
  lat,
  lon,
  radius,
  provider = names(adsb_providers)[[1]],
  use_cache = TRUE
) {
  base_url <- adsb_providers[[provider]]
  path_style <- adsb_provider_paths[[provider]]
  request_url <- if (identical(path_style, "point")) {
    paste0(base_url, "/point/", lat, "/", lon, "/", radius)
  } else {
    paste0(base_url, "/lat/", lat, "/lon/", lon, "/dist/", radius)
  }
  key <- cache_key(provider, lat, lon, radius)

  if (use_cache && exists(key, envir = adsb_cache, inherits = FALSE)) {
    cached <- get(key, envir = adsb_cache, inherits = FALSE)
    if (
      as.numeric(difftime(
        Sys.time(),
        cached$status$updated_at,
        units = "secs"
      )) <=
        adsb_cache_ttl
    ) {
      cached$status$from_cache <- TRUE
      return(cached)
    }
  }

  result <- tryCatch(
    {
      res <- httr2::request(base_url) |>
        httr2::req_throttle(capacity = 60, fill_time_s = 60) |>
        httr2::req_timeout(8) |>
        httr2::req_retry(max_tries = 3)

      if (identical(path_style, "point")) {
        res <- res |>
          httr2::req_url_path_append("point") |>
          httr2::req_url_path_append(lat) |>
          httr2::req_url_path_append(lon) |>
          httr2::req_url_path_append(radius)
      } else {
        res <- res |>
          httr2::req_url_path_append("lat") |>
          httr2::req_url_path_append(lat) |>
          httr2::req_url_path_append("lon") |>
          httr2::req_url_path_append(lon) |>
          httr2::req_url_path_append("dist") |>
          httr2::req_url_path_append(radius)
      }

      res <- res |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = FALSE)

      aircraft <- res$ac

      if (is.null(aircraft) || length(aircraft) == 0) {
        data <- empty_flights_tibble()
      } else {
        flat_aircraft <- purrr::map(aircraft, \(x) {
          purrr::map(x, \(y) {
            if (is.list(y)) paste(unlist(y), collapse = ", ") else y
          })
        })

        data <- flat_aircraft |>
          purrr::map(\(aircraft) {
            aircraft |>
              tibble::as_tibble() |>
              normalize_flights()
          }) |>
          purrr::list_rbind() |>
          normalize_flights()
      }

      list(
        data = data,
        status = list(
          ok = TRUE,
          message = res$msg %||% "No error",
          requested_total = res$total %||% nrow(data),
          rows = nrow(data),
          radius = radius,
          provider = provider,
          updated_at = Sys.time(),
          url = request_url,
          from_cache = FALSE
        )
      )
    },
    error = function(err) {
      list(
        data = empty_flights_tibble(),
        status = list(
          ok = FALSE,
          message = conditionMessage(err),
          requested_total = NA_integer_,
          rows = 0,
          radius = radius,
          provider = provider,
          updated_at = Sys.time(),
          url = request_url,
          from_cache = FALSE
        )
      )
    }
  )

  if (isTRUE(result$status$ok)) {
    assign(key, result, envir = adsb_cache)
  }

  result
}

fetch_provider_point <- function(lat, lon, radius, provider) {
  result <- fetch_point(
    lat = lat,
    lon = lon,
    radius = radius,
    provider = provider
  )

  if (isTRUE(result$status$ok) && nrow(result$data) > 0) {
    result$data <- result$data |>
      mutate(
        sources = provider_label(provider),
        source_count = 1
      ) |>
      normalize_flights()
  }

  result
}

fetch_pooled_point <- function(lat, lon, radius, providers = names(adsb_providers)) {
  results <- purrr::map(
    providers,
    \(provider) fetch_provider_point(lat, lon, radius, provider)
  )
  names(results) <- providers

  ok_results <- purrr::keep(results, \(x) isTRUE(x$status$ok))
  failed_results <- purrr::discard(results, \(x) isTRUE(x$status$ok))
  combined <- ok_results |>
    purrr::map("data") |>
    purrr::list_rbind()

  data <- if (is.null(combined) || nrow(combined) == 0) {
    empty_flights_tibble()
  } else {
    dedupe_flights(combined)
  }

  provider_rows <- purrr::map_int(ok_results, \(x) x$status$rows)
  failed_messages <- purrr::imap_chr(
    failed_results,
    \(x, provider) paste0(provider, ": ", x$status$message)
  )

  list(
    data = data,
    status = list(
      ok = length(ok_results) > 0,
      message = if (length(failed_messages) == 0) {
        "No error"
      } else {
        paste(failed_messages, collapse = " | ")
      },
      requested_total = sum(provider_rows),
      rows = nrow(data),
      radius = radius,
      provider = paste(providers, collapse = " + "),
      provider_count = length(providers),
      providers_ok = length(ok_results),
      providers_failed = length(failed_results),
      updated_at = Sys.time(),
      url = paste(purrr::map_chr(results, \(x) x$status$url), collapse = " | "),
      from_cache = all(purrr::map_lgl(results, \(x) isTRUE(x$status$from_cache)))
    )
  )
}

format_status <- function(status) {
  if (is.null(status)) {
    return("Aguardando primeira atualização.")
  }

  updated_at <- format(status$updated_at, "%H:%M:%S")
  cache_text <- if (isTRUE(status$from_cache)) " | cache" else ""
  if (isTRUE(status$ok)) {
    paste0(
      status$provider,
      " | ",
      status$providers_ok %||% 1,
      "/",
      status$provider_count %||% 1,
      " fonte(s) ok | ",
      status$rows,
      " aeronave(s) únicas de ",
      status$requested_total,
      " detecções em ",
      status$radius,
      " nm | Atualizado ",
      updated_at,
      cache_text
    )
  } else {
    paste0(
      status$provider,
      " indisponível | mantendo último resultado | ",
      updated_at,
      " | ",
      status$message
    )
  }
}

display_value <- function(x) {
  value <- as.character(x)
  ifelse(is.na(value) | trimws(value) == "", "N/A", value)
}

aircraft_label <- function(flight, r, t, alt_baro, gs) {
  lapply(
    paste0(
      "<div style='font-family: sans-serif; min-width: 150px;'>",
      "<h5 style='margin: 0 0 5px 0; color: #2e2e2eff;'>",
      htmltools::htmlEscape(display_value(flight)),
      "</h5>",
      "<hr style='margin: 5px 0;'>",
      "<b>Registro:</b> ",
      htmltools::htmlEscape(display_value(r)),
      "<br>",
      "<b>Tipo:</b> ",
      htmltools::htmlEscape(display_value(t)),
      "<br>",
      "<b>Altitude:</b> ",
      htmltools::htmlEscape(display_value(alt_baro)),
      " ft",
      "<br>",
      "<b>Velocidade (GS):</b> ",
      htmltools::htmlEscape(display_value(gs)),
      " kt",
      "</div>"
    ),
    htmltools::HTML
  )
}

add_aircraft_markers <- function(map, data, icon, group) {
  aircraft <- data |>
    filter(!is.na(lat), !is.na(lon))

  if (nrow(aircraft) == 0) {
    return(map)
  }

  map |>
    addMarkers(
      data = aircraft,
      lng = ~lon,
      lat = ~lat,
      label = ~ aircraft_label(flight, r, t, alt_baro, gs),
      icon = icon,
      options = markerOptions(
        rotationAngle = ~track,
        rotationOrigin = "center center"
      ),
      group = group
    )
}

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
  src = normalizePath("www"),
  script = "leaflet.rotatedMarker.js"
)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

aircraft_icon <- function(filename) {
  makeIcon(
    iconUrl = file.path("www", filename),
    iconWidth = 24,
    iconHeight = 14,
    iconAnchorX = 23,
    iconAnchorY = 12
  )
}

aircraft_icons <- list(
  A0 = aircraft_icon("a0.svg"),
  A1 = aircraft_icon("a1.svg"),
  A2 = aircraft_icon("a2.svg"),
  A3 = aircraft_icon("a3.svg"),
  A4 = aircraft_icon("a4.svg"),
  A5 = aircraft_icon("a5.svg"),
  A6 = aircraft_icon("a6.svg"),
  A7 = aircraft_icon("a7.svg")
)

other_aircraft_icon <- aircraft_icon("uparrow.png")

aircraft_groups <- c(
  paste("Aeronaves", names(aircraft_icons)),
  "Outras aeronaves"
)

ui <- page_sidebar(
  tags$head(
    tags$style(HTML(
      "
      .recalculating { opacity: 1.0 !important; }
      #api_status {
        color: #5f6368;
        font-size: 0.875rem;
        margin-bottom: 0.5rem;
      }
    "
    ))
  ),
  title = "SBJR Live",
  sidebar = sidebar(
    sliderInput(
      "refresh_rate",
      "Atualização mapa (s)",
      min = 5,
      max = 30,
      value = 10,
      step = 1
    ),
    sliderInput(
      "radius",
      "Raio (nm)",
      min = 10,
      max = 250,
      value = 50,
      step = 5
    ),
    card(
      card_header("118.4 Torre"),
      card_body(
        tags$audio(
          controls = TRUE,
          autoplay = TRUE,
          style = "width: 100%;",
          tags$source(
            src = "https://securestreams.autopo.st:2627/sbjr_tower",
            type = "audio/mpeg"
          ),
          "Your browser does not support the audio element."
        )
      )
    ),
    card(
      card_header("119.7 APP-RJ Baixo"),
      card_body(
        tags$audio(
          controls = TRUE,
          autoplay = TRUE,
          style = "width: 100%;",
          tags$source(
            src = "https://securestreams.autopo.st:2627/app_rj_baixo",
            type = "audio/mpeg"
          ),
          "Your browser does not support the audio element."
        )
      ),
    ),
    card(
      card_header("120.6 APP-RJ Leste"),
      card_body(
        tags$audio(
          controls = TRUE,
          autoplay = TRUE,
          style = "width: 100%;",
          tags$source(
            src = "https://securestreams.autopo.st:2627/app_rj_leste",
            type = "audio/mpeg"
          ),
          "Your browser does not support the audio element."
        )
      ),
    )
  ),
  page_fillable(
    layout_columns(
      card(
        full_screen = TRUE,
        card_header("ADS-B"),
        card_body(
          fillable = TRUE, # Allows the map to fill the card
          padding = 0, # Removes white border around the map
          leafletOutput("map", height = "100%")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Voos"),
        card_body(
          textOutput("api_status"),
          DTOutput(outputId = "flights_table")
        )
      ),
      col_widths = c(8, 4)
    )
  )
)

server <- function(input, output, session) {
  # Initialize the static map
  output$map <- renderLeaflet({
    leaflet() |>
      registerPlugin(rotatedMarker) |>
      addTiles(group = "Ruas") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      addProviderTiles(providers$OpenTopoMap, group = "Topografia") |>
      setView(center_lon, center_lat, zoom = 10) |>
      addCircles(
        lng = center_lon,
        lat = center_lat,
        radius = isolate(input$radius) * 1852,
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
      addCircleMarkers(
        lng = center_lon,
        lat = center_lat,
        radius = 6,
        color = "white",
        fillColor = "#000000ff",
        fillOpacity = 1,
        weight = 2,
        label = "SBJR",
        group = "SBJR"
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
      addLayersControl(
        baseGroups = c("Ruas", "Topografia", "Imagem de satélite"),
        overlayGroups = c(
          "SBJR",
          "Pontos de navegação",
          "Espaço Aéreo do Pré-sal",
          "Raio de busca",
          "Aeronaves A0",
          "Aeronaves A1",
          "Aeronaves A2",
          "Aeronaves A3",
          "Aeronaves A4",
          "Aeronaves A5",
          "Aeronaves A6",
          "Aeronaves A7",
          "Outras aeronaves"
        ),
        options = layersControlOptions(collapsed = TRUE) # Deixa o controle aberto
      )
  })

  last_successful_flights <- reactiveVal(empty_flights_tibble())
  last_fetch_status <- reactiveVal(NULL)

  # Update data
  flights_fetch <- reactive({
    invalidateLater(input$refresh_rate * 1000, session)

    result <- fetch_pooled_point(
      lat = center_lat,
      lon = center_lon,
      radius = input$radius,
      providers = names(adsb_providers)
    )

    if (isTRUE(result$status$ok)) {
      last_successful_flights(result$data)
    } else {
      result$data <- last_successful_flights()
      result$status$rows <- nrow(result$data)
    }

    last_fetch_status(result$status)
    result
  })

  flights_res <- reactive({
    flights_fetch()$data
  })

  output$api_status <- renderText({
    flights_fetch()
    format_status(last_fetch_status())
  })

  observeEvent(
    input$radius,
    {
      leafletProxy("map") |>
        clearGroup(group = "Raio de busca") |>
        addCircles(
          lng = center_lon,
          lat = center_lat,
          radius = input$radius * 1852,
          color = "#000000ff",
          weight = 2,
          fillOpacity = 0.1,
          group = "Raio de busca"
        )
    },
    ignoreInit = TRUE
  )

  # Update map
  observe({
    res <- flights_res()

    map <- Reduce(
      \(proxy, group) clearGroup(proxy, group = group),
      aircraft_groups,
      init = leafletProxy("map")
    ) |>
      registerPlugin(rotatedMarker)

    for (category in names(aircraft_icons)) {
      map <- add_aircraft_markers(
        map = map,
        data = filter(res, category == !!category),
        icon = aircraft_icons[[category]],
        group = paste("Aeronaves", category)
      )
    }

    other_aircraft <- res |>
      filter(is.na(category) | !category %in% names(aircraft_icons))

    map <- add_aircraft_markers(
      map = map,
      data = other_aircraft,
      icon = other_aircraft_icon,
      group = "Outras aeronaves"
    )

    map
  })

  output$flights_table <- renderDT(
    {
      flights_res() |>
        normalize_flights() |>
        select(
          `Hex` = hex,
          `Voo` = flight,
          `Registro` = r,
          `Tipo` = t,
          `Alt. (b)` = alt_baro,
          `Vel. (g)` = gs,
          `Squawk` = squawk,
          `Categoria` = category,
          `Fontes` = sources,
          `N fontes` = source_count,
          `ADS-B RSSI` = rssi,
          `Visto (s)` = seen,
          `Pos. vista (s)` = seen_pos,
          `Lat.` = lat,
          `Lon.` = lon
        )
    },
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      language = list(emptyTable = "Nenhuma aeronave encontrada no raio atual.")
    ),
    rownames = FALSE
  )
}

shinyApp(ui, server)
