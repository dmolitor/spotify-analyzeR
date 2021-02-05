library(dplyr)
library(httr)
library(shiny)
library(shinythemes)

source('src/lightbox.R')

ui <- fluidPage(theme = shinytheme("readable"),
                title = "Top Tracks",
                # Application title
                titlePanel(withTags(
                    div("Top Tracks",
                        div(class = 'pull-right',
                            a(href = 'https://github.com/dmolitor/spotify_analyzeR',
                              icon('github'))), hr() ))
                ),
                numericInput(inputId = "numTracks",
                             label = "Number of Tracks",
                             value = 10,
                             min = 1,
                             max = 50,
                             step = 1,
                             width = "120px"),
                selectInput(inputId = "tracks",
                            label = "Time Range",
                            choices = c("Short-term (4 weeks)" = "short_term",
                                        "Medium-term (6 months)" = "medium_term",
                                        "Long-term (Several years)" = "long_term"),
                            multiple = FALSE),
                fluidRow(
                    column(12,
                           uiOutput('topTracks'))
                    ),
                actionButton("homeButton", 
                             "Home", 
                             onclick = paste0(
                                 "location.href='",
                                 "https://dmolitor.shinyapps.io/spotify_dashboards_landing/",
                                 "';"
                             ),
                             width = "175px")
)

server <- function(input, output, session) {
    spotify_app <- reactive({
        POST(url = "https://accounts.spotify.com/api/token",
             body = list(grant_type = "authorization_code",
                         code = parseQueryString(session$clientData$url_search)$code,
                         redirect_uri = "https://dmolitor.shinyapps.io/spotify_dashboards_top_tracks/",
                         client_id = Sys.getenv("SPOTIFY_ID"),
                         client_secret = Sys.getenv("SPOTIFY_SECRET")),
             encode = "form")
    })
    
    output$topTracks <- renderUI({
        req(input$numTracks)
        req(input$tracks)
        tokens <- httr::content(spotify_app())
        access_token <- tokens$access_token
        refresh_token <- tokens$refresh_token
        
        top_tracks <- GET(url = "https://api.spotify.com/v1/me/top/tracks",
                          query = list(limit = 50,
                                       time_range = input$tracks),
                          httr::add_headers("Authorization" = paste("Bearer",
                                                                    access_token)))
        
        out <- jsonlite::fromJSON(content(top_tracks, as = "text"),
                                  flatten = TRUE)
        
        track_names <- out$items$name
        track_album_art <- lapply(out$items$album.images, function(i) {
            i$url[[1]]
        }) %>%
            unlist()
        track_album <- out$items$album.name
        track_artist <- lapply(out$items$album.artists, function(i) {
            paste(i$name, collapse = ", ")
        }) %>% unlist()

        track_info <- tibble(name = track_names,
                             src = track_album_art,
                             album = track_album,
                             artist = track_artist) %>%
            distinct(name, artist, .keep_all = TRUE)
        
        # # gallery DIV
        lightbox_gallery(track_info %>%
                             head(input$numTracks),
                         'gallery',
                         display = TRUE)

    })

}

shinyApp(ui, server)