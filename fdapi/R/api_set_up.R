# These scripts will be responsible for setting up a working directory to save
# client id and secret code files, as well as create directories for saving data in later functions



setUpApi <- function(){

  # findpath to the home enviornment - should likely be in documents on PC
  path_to_save_to = as.list(Sys.getenv())$HOME
  master_dir_name = "/Vald API"
  oauth           = "/Authentication Information"
  teamInfo        =  "/Team Info"
  athleteInfo     = "/Athlete Info"
  athleteTest     = "/Athlete Test"
  summaryTest     ="/Test Summary"
  testTrial       ="/Test Trials"


  #create Vald API folder
  if(!dir.exists(paste(path_to_save_to,master_dir_name, sep = ""))){
    dir.create(path = paste(path_to_save_to,"/Vald API", sep = ""))
  }

  #create oauth folder
  if(!dir.exists(paste(path_to_save_to      #home path
                       ,master_dir_name     #Vald Api Folder
                       ,oauth               #Authorization Folder
                       ,sep = ""))){
    dir.create(paste(path_to_save_to      #home path
                     ,master_dir_name     #Vald Api Folder
                     ,oauth               #Authorization Folder
                     ,sep = ""))

  }


  #create teaminfo folder
  if(!dir.exists(paste(path_to_save_to      #home path
                       ,master_dir_name     #Vald Api Folder
                       ,teamInfo               #Authorization Folder
                       ,sep = ""))){
    dir.create(paste(path_to_save_to      #home path
                     ,master_dir_name     #Vald Api Folder
                     ,teamInfo               #Authorization Folder
                     ,sep = ""))

  }


  #create athlete info folder
  if(!dir.exists(paste(path_to_save_to      #home path
                       ,master_dir_name     #Vald Api Folder
                       ,athleteInfo               #athleteInfo Folder
                       ,sep = ""))){
    dir.create(paste(path_to_save_to      #home path
                     ,master_dir_name     #Vald Api Folder
                     ,athleteInfo               #athleteInfo Folder
                     ,sep = ""))

  }

  #create athleteTest info folder
  if(!dir.exists(paste(path_to_save_to      #home path
                       ,master_dir_name     #Vald Api Folder
                       ,athleteTest               #athleteTest Folder
                       ,sep = ""))){
    dir.create(paste(path_to_save_to      #home path
                     ,master_dir_name     #Vald Api Folder
                     ,athleteTest               #athleteTest Folder
                     ,sep = ""))

  }



  #create summaryTest info folder
  if(!dir.exists(paste(path_to_save_to      #home path
                       ,master_dir_name     #Vald Api Folder
                       ,summaryTest               #summaryTest Folder
                       ,sep = ""))){
    dir.create(paste(path_to_save_to      #home path
                     ,master_dir_name     #Vald Api Folder
                     ,summaryTest               #summaryTest Folder
                     ,sep = ""))

  }


  #create testTrial info folder
  if(!dir.exists(paste(path_to_save_to      #home path
                       ,master_dir_name     #Vald Api Folder
                       ,testTrial               #testTrial Folder
                       ,sep = ""))){
    dir.create(paste(path_to_save_to      #home path
                     ,master_dir_name     #Vald Api Folder
                     ,testTrial               #testTrial Folder
                     ,sep = ""))

  }


  # create csv for file paths
  write.csv(x = data.frame( path_to_save_to = as.list(Sys.getenv())$HOME
                            ,master_dir_name = "/Vald API"
                            ,oauth           = "/Authentication Information"
                            ,teamInfo        =  "/Team Info"
                            ,athleteInfo     = "/Athlete Info"
                            ,athleteTest     = "/Athlete Test"
                            ,summaryTest     ="/Test Summary"
                            ,testTrial       ="/Test Trials")
            ,file = paste(path_to_save_to
                          ,master_dir_name
                          ,oauth
                          ,'filePaths'
                          ,sep = ''))

  #input client id
  clientId = trimws(
    svDialogs::dlgInput(message = "Enter a Type you client id here:"
                        ,default = "Client Id Received from Vald Support")$res
  )

  #input secret code
  secretCode = trimws(
    svDialogs::dlgInput(message = "Enter a Type you secret code here:"
                        ,default = "Secret Code Received from Vald Support")$res
  )


  # encode secret code to oauthInfo
  oauthInfo = jsonlite::base64_enc(paste(clientId, secretCode, sep = ":"))

  # write encoded oauth to file in Authorization Folder - will be used to refresh token if needed
  write(x = oauthInfo
        ,paste(path_to_save_to
               ,master_dir_name
               ,oauth
               ,'/secretfiles.txt',
               sep = ""))

  # writes file for encoded token
  secret = read.csv(paste(path_to_save_to
                          ,master_dir_name
                          ,oauth
                          ,'/secretfiles.txt',
                          sep = "")
                    ,header = FALSE)[1,1]

  # request a token from vald
  req <- httr::POST("https://security.valdperformance.com/connect/token",
                    httr::add_headers(
                      "Authorization" = paste("Basic", gsub("\n", "", secret)),
                      "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                    body = "grant_type=client_credentials")

  httr::stop_for_status(req, "authenticate")

  token <- paste("Bearer", httr::content(req)$access_token)

  # writes token to authorizaiton file
  write.csv(token
            ,file = paste(path_to_save_to
                          ,master_dir_name
                          ,oauth
                          ,'/token.txt'
                          ,sep = "")
            ,row.names = FALSE)
}


read.token = function(){
  path_to_save_to = as.list(Sys.getenv())$HOME
  master_dir_name = "/Vald API"
  oauth           = "/Authentication Information"
  teamInfo        =  "/Team Info"
  athleteInfo     = "/Athlete Info"
  athleteTest     = "/Athlete Test"
  summaryTest     ="/Test Summary"
  testTrial       ="/Test Trials"

  secret = data.table::fread(paste(path_to_save_to
                                   ,master_dir_name
                                   ,oauth
                                   ,'/secretfiles.txt',
                                   sep = "")
                             ,header = FALSE)[1,1]

  # request a token from vald
  req <- httr::POST("https://security.valdperformance.com/connect/token",
                    httr::add_headers(
                      "Authorization" = paste("Basic", gsub("\n", "", secret)),
                      "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                    body = "grant_type=client_credentials")

  httr::stop_for_status(req, "authenticate")

  token <- paste("Bearer", httr::content(req)$access_token)

  # writes token to authorizaiton file
  write.csv(token
            ,file = paste(path_to_save_to
                          ,master_dir_name
                          ,oauth
                          ,'/token.txt'
                          ,sep = "")
            ,row.names = FALSE)


  read.csv(file = paste(path_to_save_to
                        ,master_dir_name
                        ,oauth
                        ,'/token.txt'
                        ,sep = ""))

    }



