
library(shiny)
shinyServer(function(input, output, session) {
  
  observe({
    input$img_0_btn
    session$sendCustomMessage(type = 'resetFileInputHandler', 'img_0')
  })
  
  observe({
    input$img_1_btn
    session$sendCustomMessage(type = 'resetFileInputHandler', 'img_1')
  })
  
  observe({
    input$img_2_btn
    session$sendCustomMessage(type = 'resetFileInputHandler', 'img_2')
  })
  
  observe({
    input$img_3_btn
    session$sendCustomMessage(type = 'resetFileInputHandler', 'img_3')
  })
  
  # ========== render image
  renderImageFunc <- function(img) {
    renderImage({
      if (is.null(input[[img]])) {
        return(NULL)
      }
      list(src = input[[img]]$datapath)
    }, deleteFile = FALSE
    )
  }
  
  observe({
    if (!is.null(input$img_0)) {
      output$img_0_output <- renderImageFunc('img_0')
      output$emotion_btn_ui <- renderUI({
        actionButton('emotion_btn', 'get emotion')
      })
    }
    
    if (!is.null(input$img_1)) {
      output$img_1_output <- renderImageFunc('img_1')
    }
    
    if (!is.null(input$img_2)) {
      output$img_2_output <- renderImageFunc('img_2')
    }
    
    if (!is.null(input$img_3)) {
      output$img_3_output <- renderImageFunc('img_3')
    }
  })
  
  # ========== render emotion table
  
  observeEvent(input$emotion_btn, {
    img_0_emotion <- reactive(
      if (!is.null(isolate(input$img_0))) {
        withProgress(message = 'fetching emotion...', value = 0, {
          setProgress(0.5)
          return(getEmotionResponse(isolate(input$img_0$datapath), emotion_key))
          setProgress(1)
        })
      }
    )
    
    output$img_0_emotion <- renderTable(
      expr = img_0_emotion(),
      striped = TRUE,
      hover = TRUE,
      align = 'c'
    )
  })
  
  # ========== render confidence
  getFaceIdReactive <- function(img, face_key) {
    reactive(
      if (!is.null(input[[img]])) {
        return(getFaceId(input[[img]]$datapath, face_key))
      }
    )
  }
  img_0_faceId <- getFaceIdReactive('img_0', face_key)
  
  renderPrintConfidence <- function(j, face_key) {
    renderPrint({
      tryCatch({
        if (!is.null(img_0_faceId()) & !is.null(input[[j]])) {
          pb <- Progress$new()
          on.exit(pb$close())
          pb$set(message = 'fetching faceId...', value = 0.33)
          
          img_j_faceId <- getFaceId(input[[j]]$datapath, face_key)
          
          pb$set(message = 'caculating similarity...', value = 0.67)
          
          getSim(img_0_faceId(), img_j_faceId, face_key) %>%
            extract2(1) %>%
            extract2('confidence') %>%
            return()
        }
      }, error = function(e) {
        e$message
      })
    })
  }
  
  output$confidence_1 <- renderPrintConfidence('img_1', face_key)
  output$confidence_2 <- renderPrintConfidence('img_2', face_key)
  output$confidence_3 <- renderPrintConfidence('img_3', face_key)
  
})
