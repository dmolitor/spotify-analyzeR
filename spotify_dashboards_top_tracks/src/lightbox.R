
lightbox_gallery <- function(df, gallery, display = 'block'){
  
  tags$div(style = sprintf('display: %s;', display),
           tagList(tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "lightbox-2.10.0/lightbox.min.css"),
                     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                   ),
                   tags$div(class = 'card-deck',
                            lapply(seq_len(nrow(df)), function(i){
                              tags$div(`data-type`="template", class = 'card',
                                       tags$a(href = df$src[i],
                                              `data-lightbox` = gallery, # this identifies gallery group
                                              `data-title` = paste0(df$name[i],
                                                                    " --- ",
                                                                    df$artist[i]),
                                              tags$img(class = 'card-img-top',
                                                       src = df$src[i]))
                                       )
                            })
                   ),
                   includeScript("www/lightbox-2.10.0/lightbox.min.js")
           ))
  
}