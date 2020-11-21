library(shiny)
library(shinythemes)
library(magrittr)

jscode <- "Shiny.addCustomMessageHandler('urlredirect', function(message) { window.location = message;});"

scopes <- c(
    "ugc-image-upload",
    "user-read-recently-played",
    "user-read-playback-position",
    "user-top-read",
    "playlist-modify-private",
    "playlist-read-collaborative",
    "playlist-read-private",
    "playlist-modify-public" ,
    "streaming",
    "app-remote-control",
    "user-read-email",
    "user-read-private",
    "user-follow-read",
    "user-follow-modify",
    "user-library-modify",
    "user-library-read",
    "user-read-currently-playing",
    "user-read-playback-state",
    "user-modify-playback-state"
)

top_tracks_url <- "https://accounts.spotify.com/authorize" %>%
    urltools::param_set("response_type", "code") %>%
    urltools::param_set("client_id", Sys.getenv("SPOTIFY_ID")) %>%
    urltools::param_set("redirect_uri", 
                        "https%3A%2F%2Fdmolitor.shinyapps.io%2Fspotify_dashboards_top_tracks%2F") %>%
    urltools::param_set("scope", URLencode(paste0(scopes, collapse = " ")))

top_artists_url <- "https://accounts.spotify.com/authorize" %>%
    urltools::param_set("response_type", "code") %>%
    urltools::param_set("client_id", Sys.getenv("SPOTIFY_ID")) %>%
    urltools::param_set("redirect_uri", 
                        "https%3A%2F%2Fdmolitor.shinyapps.io%2Fspotify_dashboards_top_artists%2F") %>%
    urltools::param_set("scope", URLencode(paste0(scopes, collapse = " ")))

tracks_analysis_url <- "https://accounts.spotify.com/authorize" %>%
    urltools::param_set("response_type", "code") %>%
    urltools::param_set("client_id", Sys.getenv("SPOTIFY_ID")) %>%
    urltools::param_set("redirect_uri", 
                        "https%3A%2F%2Fdmolitor.shinyapps.io%2Fspotify_dashboards_tracks_analysis%2F") %>%
    urltools::param_set("scope", URLencode(paste0(scopes, collapse = " ")))

# demo_url <- "https://accounts.spotify.com/authorize" %>%
#     urltools::param_set("response_type", "code") %>%
#     urltools::param_set("client_id", "dc5afe83306449ea9af613c915b2f7fd") %>%
#     urltools::param_set("redirect_uri",
#                         "http%3A%2F%2F127.0.0.1%3A3689%2F") %>%
#     urltools::param_set("scope", URLencode(paste0(scopes, collapse = " ")))

ui <- fillPage(title = "Spotify AnalyzeR",
               padding = 10,
    tags$head(tags$script(jscode)),
    theme = shinytheme("superhero"),
    titlePanel(withTags(
        div("Spotify AnalyzeR",
            div(class = 'pull-right',
                a(href = 'https://github.com/dmolitor/spotify_analyzeR',
                  icon('github'))),
            hr()))
    ),
    fillCol(align = "center",
            h1("Personalized Spotify Insights"),
            fluidRow(),
            fluidRow(actionButton("topArtists",
                                  "Top Artists",
                                  onclick = paste0("location.href='",
                                                   top_artists_url,
                                                   "';"),
                                  width = "175px",
                                  style = "vertical-align: bottom;")),
            fluidRow(actionButton("topTracks",
                                  "Top Tracks",
                                  onclick = paste0("location.href='",
                                                   top_tracks_url,
                                                   "';"),
                                  width = "175px")),
            fluidRow(actionButton("tracksAnalysis",
                                  "Audio Analysis",
                                  onclick = paste0("location.href='",
                                                   tracks_analysis_url,
                                                   "';"),
                                  width = "175px")),
            fluidRow(),
            fluidRow(),
            fluidRow())
    # verbatimTextOutput("authToken"),
)

server <- function(input, output, session) {
    # output$authToken <- renderText({
    #     parseQueryString(session$clientData$url_search)$code
    # })
}


shinyApp(ui, server)