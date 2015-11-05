library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
     h1("Pronto Stations"),
     leafletOutput("mymap", width= "50%", height= 500),
     uiOutput("choose_columns"), 
     p()
)

server <- function(input, output, session) {
     # Check boxes
     output$choose_columns <- renderUI({
          # Get the data set with the appropriate name
          colnames <- c("Out"="check_out_count.n","In"="check_in_count.n")
          
          # Create the checkboxes and select them all by default
          radioButtons("columns", "Choose columns", 
                             choices  = colnames,
                             selected = "check_out_count.n")
     })
     output$mymap <- renderLeaflet({
        if (input$columns == "check_out_count.n") {
          color_ = "blue"
        }
         else{
           color_ = "red"
         }
          m <- leaflet(data=station_usage)
          m %>% addProviderTiles("CartoDB.Positron") %>% 
               addCircles(~long, ~lat,weight = 1, radius = ~sqrt(get(input$columns))*3, color=color_)
     })
}

shinyApp(ui, server)