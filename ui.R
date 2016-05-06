library(shiny)

shinyUI(fluidPage(  
  title = "BI Planning",  
  
  #   fluidRow(
  #     # column(6,div(style = "height:800px;background-color: yellow;"),plotOutput('distPlot'))
  #     column(6,style = "height:800px;background-color: yellow;",plotOutput('distPlot'))
  #     ),
  
  fluidRow( 
    tabsetPanel(
      tabPanel("Gantt Chart", plotOutput('distPlot', height=700)),  
      tabPanel("Tables", tableOutput("personweek")),
      tabPanel("Summary", tableOutput("summary")),
      tabPanel("Allocation", tableOutput("allocation"))
    ))
  ,
  hr(),  
  fluidRow(
    column(6, uiOutput("sliders"))
    #,
    #     column(2,
    #            
    #            sliderInput("SteveH","Steve:",min = 1,max = 32,value = 16, width=150),      
    #            sliderInput("UlrikH","Ulrik:",min = 0,max = 37,value = 0, width=150)
    #     ),
    #     column(2,
    #            sliderInput("IvoH","Ivo:",min = 1,max = 34,value = 28, width=150),
    #            sliderInput("VedranH","Vedran:",min = 0,max = 40,value = 0, width=150)
    #   ), column(2,sliderInput("BasH","Bas:",min = 1,max = 34,value = 28, width=150))
  )
))




# shinyUI(fluidPage(  
#   splitLayout(
#     style = "height:1080px;width:1600px;background-color: yellow;",
#     cellArgs = list(style = "padding: 6px"),
#     plotOutput('distPlot'),
#     uiOutput("sliders")
#   )
#   )
# )


