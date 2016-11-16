library(httr)
library(magrittr)

emotion_key <- jsonlite::fromJSON('project_oxford/config.json')$emotion_key
face_key <- jsonlite::fromJSON('project_oxford/config.json')$face_key

renderImageFunc <- function(img) {
  renderImage({
      if (is.null(input[[img]])) {
        return(NULL)
      }
      list(src = input[[img]]$datapath)
  }, deleteFile = FALSE
  )
}

getEmotionResponse <- function(img.path, emotion_key){
  
  result <- content(
    POST(
      url = "https://api.projectoxford.ai/emotion/v1.0/recognize", 
      content_type('application/octet-stream'), 
      add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotion_key)),
      body = upload_file(img.path),
      encode = 'multipart'
    )
  ) %>% 
    extract2(1) %>% 
    extract2('scores')
  result <- data.frame(emotion = names(result), 
                       rating = unlist(result, use.names = FALSE), 
                       stringsAsFactors = FALSE)
  result$rating <- round(result$rating * 100 / sum(result$rating), 2)
  
  return(result)
}

getSim <- function(faceId, faceIds, face_key) {
  content(
    POST(
      url = 'https://api.projectoxford.ai/face/v1.0/findsimilars', 
      content_type('application/json'), 
      add_headers(.headers = c('Ocp-Apim-Subscription-Key' = face_key)), 
      body = list(faceId = faceId,
                  faceIds = I(faceIds),
                  mode = 'matchFace'), 
      encode = 'json'
    )
  )
}

getFaceId <- function(img.path, face_key) {
  # cat(sprintf("\n image: %s \n", img.path))
  content(
    POST(
      url = 'https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true', 
      content_type('application/octet-stream'), 
      add_headers(.headers = c('Ocp-Apim-Subscription-Key' = face_key)), 
      body = upload_file(img.path), 
      encode = 'multipart'
    )
  ) %>% 
    extract2(1) %>% 
    extract2('faceId')
}

getImgFaceIds <- function(path, face_key) {
  img_files <- dir(path, '*.jpg|png') %>% 
    file.path(path, .)
  cat(sprintf("get %d image files: \n\n", length(img_files)), 
      paste(img_files, '\n'), 
      "\n ============== \n")
  
  result <- pbapply::pblapply(img_files, getFaceId, face_key = face_key)
  img_id <- rep(img_files, times = sapply(result, length))
  result <- cbind(
    img_id, 
    faceId = unlist(result, use.names = FALSE)
  )
  
  return(result)
}