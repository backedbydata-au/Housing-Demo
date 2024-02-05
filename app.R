#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

dat <- read_csv("NY-House-Dataset.csv")
dat <- dat %>% filter(PROPERTYSQFT != 2184.207862)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(HTML('
    .center-div {
      display: flex;
      justify-content: center;
      align-items: center;
    }
  ')),
  fluidRow(class="center-div",
    column(width = 10,
  HTML("<div style='justify-content:space-between; align-items: center; display: flex;'>
          <div><h1 style='margin-left: 1em;'>New York Property Data</h1></div>
          <div><img style='margin-right: 1em;' height=120em src='logo.png'></div>
       </div>"),

              
  
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "type",
          label = "Property Type",
          choices = c("House" = "House for sale",
                      "Co-op" = "Co-op for sale",
                      "Townhouse" = "Townhouse for sale",
                      "Condo" = "Condo for sale"),
          selected = "House for sale"
        ),
        
        sliderInput("max_price", "Price",
                    min=0, 
                    max=60,
                    value=c(0,20),
                    step=0.1,
                    round=1,
                    pre="$",
                    post="M"),
        
        sliderInput("max_sqft", "Area (SqFt)",
                    min=0, 
                    max=40000,
                    value=10000),
        
        
      ),
      mainPanel(
        plotlyOutput(outputId = "scatterplot", height="600px"),
        
      ),
    ),
    )),
  tags$footer(align = "center", style = "
position:absolute;
bottom:0;
width:100%;
height:50px; /* Height of the footer */
padding: 10px;
z-index: 1000;",
              "Data is from", HTML("<a href=https://www.kaggle.com/datasets/nelgiriyewithana/new-york-housing-market>kaggle</a>"))


)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    showModal(modalDialog(
      title="Welcome!",
      "Welcome to this demo made by Backed By Data! This is an example of what Backed By Data can develop for your business with any of your data. Please interact with the widgets and graph!",
      easyClose = TRUE
    ))
  
  output$scatterplot <- renderPlotly({
    p <- dat %>% filter(TYPE == input$type) %>%
      ggplot(aes(x=PROPERTYSQFT, y=PRICE, text=ADDRESS, colour=SUBLOCALITY)) +
      geom_point(alpha=0.7) +
      coord_cartesian(ylim = c(input$max_price[1]*1000000, input$max_price[2]*1000000),
                      xlim = c(0, input$max_sqft)) +
      scale_y_continuous(labels = scales::dollar_format(scale=1/1000000, suffix="M"))+
      ylab("Price")+
      xlab("Area (SqFt)")+
      labs(colour="")
    
    ggplotly(p)
  })
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
