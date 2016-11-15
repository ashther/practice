library(httr)
library(purrr)
library(magrittr)

getEmotionResponse <- function(img.path, key){
  
  emotionURL <- "https://api.projectoxford.ai/emotion/v1.0/recognize"
  
  mybody <- upload_file(img.path)
  
  emotionResponse <- POST(
    url = emotionURL, 
    content_type('application/octet-stream'), 
    add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
    body = mybody,
    encode = 'multipart'
  )
}

face_key <- '3b891b40472240b89f1cf6202cf0cbbb'

getSim <- function(faceId, faceIds, face_key) {
  result <- content(
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
  do.call(rbind, result)
}

getFaceId <- function(img.path, face_key) {
  # cat(sprintf("\n image: %s \n", img.path))
  result <- content(
    POST(
      url = 'https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true', 
      content_type('application/octet-stream'), 
      add_headers(.headers = c('Ocp-Apim-Subscription-Key' = face_key)), 
      body = upload_file(img.path), 
      encode = 'multipart'
    )
  )
  map_chr(result, 'faceId')
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