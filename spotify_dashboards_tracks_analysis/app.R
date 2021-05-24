library(dplyr)
library(ggplot2)
library(ggridges)
library(ggrepel)
library(httr)
library(jsonlite)
library(shiny)
library(shinythemes)
library(tidyr)
library(viridis)

ui <- fluidPage(theme = shinytheme("readable"),
                title = "Audio Analysis - Top Tracks",
                # Application title
                titlePanel(withTags(
                    div("Audio Analysis - Top Tracks",
                        div(class = 'pull-right',
                            a(href = 'https://github.com/dmolitor/spotify_analyzeR',
                              icon('github'))), hr() ))
                ),
                fluidRow(
                    column(2,
                           numericInput(inputId = "numTracks",
                                        label = "Number of Tracks",
                                        value = 25,
                                        min = 1,
                                        max = 50,
                                        step = 1,
                                        width = "250px")),
                    column(4,
                           selectInput(inputId = "tracks",
                                       label = "Time Range",
                                       choices = c("Short-term (4 weeks)" = "short_term",
                                                   "Medium-term (6 months)" = "medium_term",
                                                   "Long-term (Several years)" = "long_term"),
                                       multiple = FALSE,
                                       width = "250px"))
                ),
                h2("Danceability"),
                h4("How suitable the song is for dancing on a scale of 0 - 1"),
                plotOutput("danceability", width = "100%"),
                h2("Energy"),
                h4("How fast, loud, and noisy the song is on a scale of 0 - 1"),
                plotOutput("energy", width = "100%"),
                h2("Loudness"),
                h4("Overall loudness of the song in decibels on a typical range of -60 to 0"),
                plotOutput("loudness", width = "100%"),
                h2("Speechiness"),
                h4("The presence of spoken words in a song on a scale of 0 - 1"),
                plotOutput("speechiness", width = "100%"),
                h2("Acousticness"),
                h4("How acoustic the song is on a scale of 0 - 1"),
                plotOutput("acousticness", width = "100%"),
                h2("Instrumentalness"),
                h4("How likely that a song has no vocals on a scale of 0 - 1"),
                plotOutput("instrumentalness", width = "100%"),
                h2("Valence"),
                h4("How musically happy the song is on a scale of 0 - 1"),
                plotOutput("valence", width = "100%"),
                h2("Tempo"),
                h4("How fast the song is in beats per minute"),
                plotOutput("tempo", width = "100%"),
                h2("Duration (in minutes)"),
                plotOutput("duration", width = "100%"),
                br(),
                actionButton("homeButton", 
                             "Home", 
                             onclick = paste0(
                                 "location.href='",
                                 "http://djmolitor.com/shiny/spotify_dashboards_landing/",
                                 "';"
                             ),
                             width = "175px")
)

server <- function(input, output, session) {
    
    spotify_app <- reactive({
        POST(url = "https://accounts.spotify.com/api/token",
             body = list(grant_type = "authorization_code",
                         code = parseQueryString(session$clientData$url_search)$code,
                         redirect_uri = "http://djmolitor.com/shiny/spotify_dashboards_tracks_analysis/",
                         client_id = Sys.getenv("SPOTIFY_ID"),
                         client_secret = Sys.getenv("SPOTIFY_SECRET")),
             encode = "form")
    })
    
    top_tracks_df <- reactive({
        req(input$tracks)
        req(input$numTracks)
        spotify_content <- content(spotify_app(), as = "text") %>%
            fromJSON(flatten = TRUE)
        
        access_token <- spotify_content$access_token
        
        top_tracks <- GET(url = "https://api.spotify.com/v1/me/top/tracks",
                          query = list(limit = 50,
                                       time_range = input$tracks),
                          httr::add_headers("Authorization" = paste("Bearer",
                                                                    access_token))) %>%
            content(as = "text") %>%
            fromJSON(flatten = TRUE)
        
        top_tracks_ids <- top_tracks$items$id %>%
            paste0(collapse = ",")
        
        top_tracks_artists <- lapply(top_tracks$items$album.artists, function(i){
            paste0(i$name, collapse = ", ")
        }) %>% unlist()
        
        top_tracks_id_name <- top_tracks$items %>%
            mutate(artist = top_tracks_artists) %>%
            select(id, name, artist)
        
        get_top_tracks <- GET(url = "https://api.spotify.com/v1/audio-features",
                              query = list(ids = top_tracks_ids),
                              add_headers("Authorization" = paste("Bearer",
                                                                  access_token)))
        
        top_tracks_content <- content(get_top_tracks, as = "text") %>%
            fromJSON(flatten = TRUE) %>%
            `[[`("audio_features") %>%
            left_join(top_tracks_id_name, by = "id")
        
        top_tracks_content %>%
            select(id, name, artist, where(is.numeric), -time_signature) %>%
            distinct(name, artist, .keep_all = TRUE) %>%
            mutate(duration = round(duration_ms/60000, 2))
    })
    
    output$danceability <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, danceability), 
                   y = danceability)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = danceability,
                             x = forcats::fct_reorder(name, danceability),
                             xend = forcats::fct_reorder(name, danceability)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", danceability, ")"),
                          x = forcats::fct_reorder(name, danceability), 
                          y = danceability + .1,
                          color = danceability),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(0, 1.1)) +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("danceability"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })
    
    output$energy <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, energy), 
                   y = energy)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = energy,
                             x = forcats::fct_reorder(name, energy),
                             xend = forcats::fct_reorder(name, energy)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", energy, ")"),
                          x = forcats::fct_reorder(name, energy), 
                          y = energy + .1,
                          color = energy),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(0, 1.1)) +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("energy"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })
    
    output$loudness <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, loudness), 
                   y = loudness)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = loudness,
                             x = forcats::fct_reorder(name, loudness),
                             xend = forcats::fct_reorder(name, loudness)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", loudness, ")"),
                          x = forcats::fct_reorder(name, loudness), 
                          y = loudness - 1.7,
                          color = loudness),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("loudness"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })
    
    output$speechiness <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, speechiness), 
                   y = speechiness)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = speechiness,
                             x = forcats::fct_reorder(name, speechiness),
                             xend = forcats::fct_reorder(name, speechiness)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", speechiness, ")"),
                          x = forcats::fct_reorder(name, speechiness), 
                          y = speechiness + .1,
                          color = speechiness),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(0, 1.1)) +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("speechiness"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })

    output$acousticness <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, acousticness), 
                   y = acousticness)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = acousticness,
                             x = forcats::fct_reorder(name, acousticness),
                             xend = forcats::fct_reorder(name, acousticness)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", acousticness, ")"),
                          x = forcats::fct_reorder(name, acousticness), 
                          y = acousticness + .1,
                          color = acousticness),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(0, 1.1)) +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("acousticness"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })
    
    output$instrumentalness <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, instrumentalness), 
                   y = instrumentalness)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = instrumentalness,
                             x = forcats::fct_reorder(name, instrumentalness),
                             xend = forcats::fct_reorder(name, instrumentalness)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", instrumentalness, ")"),
                          x = forcats::fct_reorder(name, instrumentalness), 
                          y = instrumentalness + .1,
                          color = instrumentalness),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(0, 1.1)) +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("instrumentalness"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })
    
    output$valence <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, valence), 
                   y = valence)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = valence,
                             x = forcats::fct_reorder(name, valence),
                             xend = forcats::fct_reorder(name, valence)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", valence, ")"),
                          x = forcats::fct_reorder(name, valence), 
                          y = valence + .1,
                          color = valence),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(0, 1.1)) +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("valence"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })
    
    output$tempo <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, tempo), 
                   y = tempo)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = tempo,
                             x = forcats::fct_reorder(name, tempo),
                             xend = forcats::fct_reorder(name, tempo)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", tempo, ")"),
                          x = forcats::fct_reorder(name, tempo), 
                          y = tempo + 20,
                          color = tempo),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(0, 220)) +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("tempo"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })
    
    output$duration <- renderPlot({
        ggplot(top_tracks_df() %>%
                   mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
                   head(input$numTracks) %>%
                   distinct(name, .keep_all = TRUE),
               aes(x = forcats::fct_reorder(name, duration), 
                   y = duration)) +
            geom_point(color = "white") +
            geom_segment(aes(y = 0, 
                             yend = duration,
                             x = forcats::fct_reorder(name, duration),
                             xend = forcats::fct_reorder(name, duration)),
                         color = "white") +
            geom_text(aes(label = paste0(artist, " (", duration, ")"),
                          x = forcats::fct_reorder(name, duration), 
                          y = duration + .55,
                          color = duration),
                      size = 3,
                      inherit.aes = FALSE) +
            coord_flip() +
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  axis.title = element_text(face = "bold", color = "white"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  title = element_text(face = "bold", color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = snakecase::to_sentence_case("duration"), x = "") +
            scale_color_viridis(option = "C", 1, begin = .2)
    })
}

shinyApp(ui, server)