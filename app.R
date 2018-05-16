library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(plyr)
library(data.table)
library(devtools)


if (!exists("UA")) UA <- readRDS("customs_16_17_iron_ore_quality.RDS")
if (!exists("IMF")) IMF <- readRDS("IMF_MI_iron_ore_fines_62Fe_CFR_Tiajin.RDS")


IMF <- na.omit(IMF)
UA <- data.table(na.omit(UA))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Iron ore prices"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("period",
                     "Dates:",
                     start = as.Date("2016-07-01"), end = as.Date("2017-12-01"),
                     min = as.Date("2000-01-01"),
                     max = as.Date("2017-12-31")),
      
      selectInput("color_grouping", "Color by:", 
                  choices = c("Exporter","INCOTERMS","Importer1","Importer2", "Declarant", 
                              "intermediary_country", "destination_country","Delivery_point"),
                  multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      selectInput("shape_grouping", "Shape by (for scatterplot only):", 
                  choices = c("Exporter","INCOTERMS", "Importer1","Importer2", "Declarant", 
                              "intermediary_country", "destination_country", "Delivery_point"),
                  selected = "INCOTERMS", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      selectInput("measure", "Measured in (for structure only):", 
                  choices = c("Weight_THND_TON", "Volume_MLN_USD"),
                  selected = "Weight_THND_TON", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
      
    ),
     
    
    mainPanel(
      tabsetPanel(type="tabs",
        tabPanel("General scatterplot", plotlyOutput("generalPlot")), 
        tabPanel("Quality gradient", plotlyOutput("qualityPlot")),
        tabPanel("Structure barplot", plotlyOutput("structurePlot")),
        tabPanel("KZRK", plotlyOutput("kzrkPlot",  height = "800px")),
        tabPanel("EVRAZ", plotlyOutput("evrazPlot",  height = "800px"))
      )
    )
  )
)

# Define server logic required to draw a plot

server <- function(input, output) {
  
  output$generalPlot <- renderPlotly({ 
    
  ggplotly(  ggplot()+
      geom_line(data=IMF, aes(x=date, y=price, col="IMF world price"))+
      geom_point(data=UA, aes(x=date, y=calc_price, 
                              col=get(input$color_grouping),
                              shape=get(input$shape_grouping),
                              size=Brutto, alpha=0.5) )+  
      scale_x_date(limits=input$period)+
      scale_y_continuous(name = "Price, USD/t", limits=c(0,100))+
      scale_shape_discrete(name = input$shape_grouping )+
      scale_color_discrete(name = input$color_grouping )+
      scale_alpha(name = "")+
      theme(legend.position = "bottom")  
  )
    
  })
    
    output$qualityPlot <- renderPlotly({ 
      ggplot()+
    geom_line(data=IMF, aes(x=date, y=price))+   
    geom_point(data=UA, aes(x=date, y=calc_price, 
                            col=ore_quality,
                            shape=get(input$shape_grouping),
                            size=Brutto,  alpha=0.5) )+ 
    scale_x_date(limits=input$period)+
    scale_y_continuous(name = "Price, USD/t", limits=c(0,100))+
    scale_colour_gradient(low = "green", high = "red", name ="Ore quality")+
    scale_size_continuous(name = "")+
    scale_shape_discrete(name = input$shape_grouping)+
    scale_alpha(name = "")+
    theme(legend.position = "bottom") 
    })
    
    output$structurePlot <- renderPlotly({  
      
     ggplotly( ggplot(data = UA[date %inrange% input$period, 
          c("Weight_THND_TON", "Volume_MLN_USD") := list( sum(Brutto)/1e6, sum(invoice_value_USD)/1e6),
          by = get(input$color_grouping)],
             aes(x=get(input$color_grouping), y=get(input$measure), fill=get(input$color_grouping)))+
        geom_bar(stat="identity", show.legend = FALSE)+
        scale_x_discrete(name=input$color_grouping)+
        scale_y_continuous(name=input$measure)+
        theme(axis.text.x = element_text(angle=90),
        axis.text.y = element_text(angle=45))
     )
      
    })
    
    output$kzrkPlot <- renderPlotly({ 
      ggplot()+
        geom_line(data=IMF, aes(x=date, y=price))+
        geom_point(data=UA %>% filter(Exporter=='KZRK'), aes(x=date, y=calc_price, 
                                        col=get(input$color_grouping),
                                        shape=get(input$shape_grouping),
                                        alpha=.3) )+  
        geom_smooth(data=UA %>% filter(Exporter=='KZRK'), aes(x=date, y=calc_price))+
        scale_x_date(limits=input$period)+
        scale_y_continuous(name = "Price, USD/t", limits=c(20,90))+
        scale_shape_discrete(name = "")+
        scale_color_discrete(name = "")+
        scale_alpha(name = "")+
        guides(fill=guide_legend(title=""))+
        theme(legend.position = "bottom")+
        facet_grid(Importer1~.)
    })
    
    output$evrazPlot <- renderPlotly({ 
      ggplot()+
        geom_line(data=IMF, aes(x=date, y=price))+
        geom_point(data=UA %>% filter(Exporter=='EVRAZ'), aes(x=date, y=calc_price, 
                                                            col=get(input$color_grouping),
                                                            shape=get(input$shape_grouping),
                                                            alpha=.3) )+  
        geom_smooth(data=UA %>% filter(Exporter=='EVRAZ'), aes(x=date, y=calc_price))+
        scale_x_date(limits=input$period)+
        scale_y_continuous(name = "Price, USD/t", limits=c(20,90))+
        scale_shape_discrete(name = "")+
        scale_color_discrete(name = "")+
        scale_alpha(name = "")+
        guides(fill=guide_legend(title=""))+
        theme(legend.position = "bottom")+
        facet_grid(Importer1~.)
    })
    
  }
  
 

# Run the application 
shinyApp(ui = ui, server = server)

