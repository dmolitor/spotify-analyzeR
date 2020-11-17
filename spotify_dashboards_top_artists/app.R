library(dplyr)
library(httr)
library(shiny)
library(shinythemes)

source('src/lightbox.R')

ui <- fluidPage(theme = shinytheme("superhero"),
                title = "Top Artists",
                # Application title
                titlePanel(withTags(
                    div("Top Artists",
                        div(class = 'pull-right',
                            a(href = 'https://github.com/dmolitor/spotify_analyzeR',
                              icon('github'))), hr() ))
                ),
                numericInput(inputId = "numArtists",
                             label = "Number of Artists",
                             value = 10,
                             min = 1,
                             max = 50,
                             step = 1,
                             width = "120px"),
                selectInput(inputId = "artists",
                            label = "Time Range",
                            choices = c("Short-term (4 weeks)" = "short_term",
                                        "Medium-term (6 months)" = "medium_term",
                                        "Long-term (Several years)" = "long_term"),
                            multiple = FALSE),
                fluidRow(
                    column(12, 
                           uiOutput('topArtists')
                    ))
)

server <- function(input, output, session) {
    spotify_app <- reactive({
        POST(url = "https://accounts.spotify.com/api/token",
             body = list(grant_type = "authorization_code",
                         code = parseQueryString(session$clientData$url_search)$code,
                         redirect_uri = "https://dmolitor.shinyapps.io/spotify_dashboards_top_artists/",
                         client_id = Sys.getenv("SPOTIFY_ID"),
                         client_secret = Sys.getenv("SPOTIFY_SECRET")),
             encode = "form")
    })
    
    output$topArtists <- renderUI({
        req(input$numArtists)
        req(input$artists)
        tokens <- httr::content(spotify_app())
        access_token <- tokens$access_token
        refresh_token <- tokens$refresh_token
        
        top_artists <- GET(url = "https://api.spotify.com/v1/me/top/artists",
                           query = list(limit = 50,
                                        time_range = input$artists),
                           httr::add_headers("Authorization" = paste("Bearer",
                                                                     access_token)))
        
        out <- jsonlite::fromJSON(content(top_artists, as = "text"),
                                  flatten = TRUE)$items
        
        urls <- lapply(out$images, function(i){i[1, ]}) %>% 
            bind_rows()
        artists <- out$name
        images <- tibble(src = urls$url, artist = artists)
        # gallery DIV
        lightbox_gallery(images %>% 
                             head(input$numArtists), 
                         'gallery', 
                         display = TRUE)
        
    })

}

shinyApp(ui, server)