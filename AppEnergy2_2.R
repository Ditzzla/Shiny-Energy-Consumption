#### _______________________________________ ####
# This is the second Energy Consumption Shiny App!!!!!!!
#### _______________________________________ ####

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)
library(shinythemes)
library(ggthemes)
library(rsconnect)



#changing the theme ----
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(15, 0, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 15, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "#303030"),  
      legend.key = element_rect(color = "white",  fill = "#303030"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "#303030", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "grey35"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_blank(),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "#303030", fill = "#303030"),  
      plot.title = element_text(size = base_size*1.2, color = "white",  margin = margin(0, 0, 20, 0)),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}


ui <- fluidPage(theme = shinytheme("darkly"),
  
  headerPanel("Private Household Energy Consumption - Shiny App"),
  
  sidebarPanel(
    
    
    radioButtons(inputId = "timeFrame", label = h3("Select Timesteps"), 
                 choices = list(Year = "year",
                                Season = "season",
                                Month ="month",
                                Week = "week"),
                 selected = NULL, inline = FALSE, width = "100%"),
    
    
    sliderInput("range", label = h3("Select Period"), min = 2007,
                max = 2010, value = c(2007, 2010)),
    
    
    checkboxGroupInput("EnergySource", label = h3("Select a Energy source"), 
                       choices = list(Global_active_power = "Global_active_power",
                                      Sub_metering_3 = "Sub_metering_3",
                                      Sub_metering_2 ="Sub_metering_2",
                                      Sub_metering_1 = "Sub_metering_1",
                                      Missing_energy = "Missing_energy"),
                       selected = "Global_active_power")
    

    ),
  
  mainPanel(
    plotOutput("plot1"),
        verbatimTextOutput("stats")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Creating an interactive object reading from input$num
  energy <- reactive(data <-  read.csv(paste0("energy_shiny_grouped_", input$timeFrame, ".csv")))
  

  
  output$plot1 <- renderPlot(
    {
      starting_date <- as.POSIXct(paste(input$range[1],"01","01", sep = "-"))
      end_date <- as.POSIXct(paste(input$range[2]+1,"01","01", sep = "-"))
      
      myplot <- ggplot() + ggtitle("Energy consumption plot") + ylab("Energy in [Wh]") + xlab("Year")  +
        scale_color_manual(
          name = "Legend",
          values = c(
            "Global_active_power" = "#232bff",
            "Sub_metering_1" = "#05ba1a",
            "Sub_metering_2" = "#ff291e",
            "Sub_metering_3" = "#5bffdb",
            "Missing_energy" = "#fcbb2e"
          )) + theme_black() + xlim(starting_date, end_date)
      
        if("Global_active_power" %in% input$EnergySource){
          myplot <- myplot + geom_line(data = energy(), aes(x = as.POSIXct(Date), y = Global_active_power, colour = "Global_active_power"))
        }
        
        if("Sub_metering_3" %in% input$EnergySource){
          myplot <- myplot + geom_line(data = energy(), aes(x = as.POSIXct(Date), y = Sub_metering_3, colour = "Sub_metering_3"))
        }
      
        if("Sub_metering_2" %in% input$EnergySource){
          myplot <- myplot + geom_line(data = energy(), aes(x = as.POSIXct(Date), y = Sub_metering_2, colour = "Sub_metering_2"))
        }
        
        if("Sub_metering_1" %in% input$EnergySource){
          myplot <- myplot + geom_line(data = energy(), aes(x = as.POSIXct(Date), y = Sub_metering_1, colour = "Sub_metering_1"))
        }
      
        if("Missing_energy" %in% input$EnergySource){
          myplot <- myplot + geom_line(data = energy(), aes(x = as.POSIXct(Date), y = Missing_energy, colour = "Missing_energy"))
        }
      
        return(myplot)
    })
  
  # output$stats <- renderText(
  #     print(input$EnergySource)
  # )
  
}

# Run the application 
shinyApp(ui = ui, server = server)



