

shinyUI(fluidPage(
  
  titlePanel("Machine Learning ROCKS"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(9, fileInput('img_0', 'choose a picture')), 
        column(3, br(), actionButton('img_0_btn', 'reset'))
      ), 
      br(), 
      br(), 
      fluidRow(
        column(9, fileInput('img_1', 'choose a picture')), 
        column(3, br(), actionButton('img_1_btn', 'reset'))
      ), 
      fluidRow(
        column(9, fileInput('img_2', 'choose a picture')), 
        column(3, br(), actionButton('img_2_btn', 'reset'))
      ), 
      fluidRow(
        column(9, fileInput('img_3', 'choose a picture')), 
        column(3, br(), actionButton('img_3_btn', 'reset'))
      ), 
      
      tags$script(
        'Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
        var id = "#" + x + "_progress";
        var idBar = id + " .bar";
        $(id).css("visibility", "hidden");
        $(idBar).css("width", "0%");
        });'
      )
    ),
    
    mainPanel(
      
      tags$head(tags$style(type = 'text/css', 
                           "#img_0_output img {width:20vw !important;}")),
      tags$head(tags$style(type = 'text/css', 
                           "#img_1_output img {width:20vw !important;}")),
      tags$head(tags$style(type = 'text/css', 
                           "#img_2_output img {width:20vw !important;}")),
      tags$head(tags$style(type = 'text/css', 
                           "#img_3_output img {width:20vw !important;}")),
      
      fluidRow(
        column(6, imageOutput('img_0_output')), 
        column(6, actionButton('emotion_btn', 'get emotion'), tableOutput('img_0_emotion'))
      ),
      
      fluidRow(
        column(
          4, 
          verbatimTextOutput('confidence_1'), 
          imageOutput('img_1_output')
        ), 
        column(
          4, 
          verbatimTextOutput('confidence_2'),
          imageOutput('img_2_output')
        ), 
        column(
          4, 
          verbatimTextOutput('confidence_3'), 
          imageOutput('img_3_output')
        )
      )
    )
  )
))
