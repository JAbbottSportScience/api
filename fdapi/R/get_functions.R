

#Retrieves team information
get.team.info.save <- function(){

  path_to_save_to = as.list(Sys.getenv())$HOME
  master_dir_name = "/Vald API"
  oauth           = "/Authentication Information"
  teamInfo        =  "/Team Info"
  athleteInfo     = "/Athlete Info"
  athleteTest     = "/Athlete Test"
  summaryTest     ="/Test Summary"
  testTrial       ="/Test Trials"
  baseurl         ="https://fdapi.valdperformance.com/v2019q3/"


  teams_url = "https://fdapi.valdperformance.com/v2019q3/teams/"

  team_info = jsonlite::fromJSON(txt = httr:: content(httr::GET(url = teams_url
                                                                , httr::add_headers("Authorization" = read.token()[1,1]))
                                                      , as = 'text')
                                 ,simplifyDataFrame = TRUE)

  team_info$loaded_date <- Sys.time()
  write.csv(team_info, file = paste(path_to_save_to
                          ,master_dir_name
                          ,teamInfo
                          ,'/team information.csv'
                          ,sep = '')
                          ,row.names = FALSE)

  print(paste("You have connected to the"
              ,team_info$name
              ,'Vald API'
              ,"and have saved team information to"
              ,paste("               .../"
                     ,master_dir_name
                     ,teamInfo
                     ,'team information.csv'
                     ,sep = '')))
  }


#Retrieves Athlete information
get.athlete.info.save <- function(){

  path_to_save_to = as.list(Sys.getenv())$HOME
  master_dir_name = "/Vald API"
  oauth           = "/Authentication Information"
  teamInfo        =  "/Team Info"
  athleteInfo     = "/Athlete Info"
  athleteTest     = "/Athlete Test"
  summaryTest     ="/Test Summary"
  testTrial       ="/Test Trials"
  baseurl         ="https://fdapi.valdperformance.com"

  athleteUrl = paste(baseurl, read.csv(paste(path_to_save_to
                                             ,master_dir_name
                                             ,teamInfo
                                             ,'/team information.csv'
                                             ,sep = ''))[1,6]
                     ,sep = '')

  athlete_info = tidyr::unnest(jsonlite::fromJSON(txt = httr:: content(httr::GET(url = athleteUrl
                           ,httr::add_headers("Authorization" = read.token()[1,1]))
                           ,as = 'text')
                           ,simplifyDataFrame = TRUE)
                           ,cols = c(attributes, links))

  athlete_info$loaded_date = Sys.time()


  data.table::fwrite(x = athlete_info
            , file =  paste(path_to_save_to
                                  ,master_dir_name
                                  ,athleteInfo
                                  ,'/athlete information.csv'
                                  ,sep = '')
            ,row.names = FALSE
            ,sep = ",")


  print(paste('Information loaded and saved for'
        ,length(unique(athlete_info$id))
        ,'athlete(s).'
        ,sep = " "))


}





#Retrieves Athelte Test Info
get.test.trial.info.save<- function(){

  start = Sys.time()


  path_to_save_to = as.list(Sys.getenv())$HOME
  master_dir_name = "/Vald API"
  oauth           = "/Authentication Information"
  teamInfo        =  "/Team Info"
  athleteInfo     = "/Athlete Info"
  athleteTest     = "/Athlete Test"
  summaryTest     ="/Test Summary"
  testTrial       ="/Test Trials"
  baseurl         ="https://fdapi.valdperformance.com/v2019q3/teams/da8f0cb2-2138-49ac-8dcd-ed38f0424648/tests/1"




if(file.exists(paste(path_to_save_to
                     ,master_dir_name
                     ,oauth
                     ,"/last_upload_date"
                     ,sep = ''))){

  date_from =   data.table::fread(paste(path_to_save_to
                                        ,master_dir_name
                                        ,oauth
                                        ,"/last_upload_date"
                                        ,sep = ''))[[1]][1]

  baseurl = paste(baseurl,"?modifiedFrom=",date_from, sep="")
}



  test_pages =  jsonlite::fromJSON(txt = httr:: content(
    httr::GET(url = baseurl
              ,httr::add_headers("Authorization" = read.token()[1,1]))
              ,as = 'text')
              ,simplifyDataFrame = TRUE)$totalPages


  test_info = vector(mode='list', length = test_pages)

for(i in 1:test_pages){

url = ifelse(exists('date_from'),
             paste("https://fdapi.valdperformance.com/v2019q3/teams/da8f0cb2-2138-49ac-8dcd-ed38f0424648/tests/"
                   ,i
                   ,"?modifiedFrom="
                   ,date_from, sep=""),
             paste("https://fdapi.valdperformance.com/v2019q3/teams/da8f0cb2-2138-49ac-8dcd-ed38f0424648/tests/"
                   ,i
                   ,sep="")
)

  test_info[[i]] = tidyr::unnest(jsonlite::fromJSON(txt = httr:: content(
    httr::GET(url = url
              ,httr::add_headers("Authorization" = read.token()[1,1]))
    ,as = 'text')
    ,simplifyDataFrame = TRUE)$items[,c("id","teamId","athleteId","hubAthleteId","recordingId","recordedUTC",
                                        "recordedTimezone", "analysedUTC","lastModifiedUTC" ,"testType","weight","links"  )]
    ,cols = 'links')

  print(paste('Finished page',i, 'of', test_pages, sep = " "))

}

  test_info_bound = dplyr::bind_rows(test_info)
  test_info_bound$recordedUTC = lubridate::as_date(test_info_bound$recordedUTC)

  months = levels(interaction(lubridate::month(strtrim(test_info_bound$recordedUTC,10))
                       ,lubridate::year(strtrim(test_info_bound$recordedUTC,10))))

  for(month in months){
    if(file.exists(paste(path_to_save_to
                             ,master_dir_name
                             ,athleteTest
                             ,"/"
                             ,month
                             ,".csv"
                             ,sep=''))){
             temp = data.table::fread(paste(path_to_save_to
                                ,master_dir_name
                                ,athleteTest
                                ,"/"
                                ,month
                                ,".csv"
                                ,sep = ''))
             temp$recordedUTC = as.Date(temp$recordedUTC)
             temp$analysedUTC = as.Date(temp$analysedUTC)
             temp$lastModifiedUTC = as.Date(temp$lastModifiedUTC)
             temp2 = test_info_bound[interaction(lubridate::month(strtrim(test_info_bound$recordedUTC,10))
                                   ,lubridate::year(strtrim(test_info_bound$recordedUTC,10))) %in% month,]

             temp2$recordedUTC = as.Date(temp2$recordedUTC)
             temp2$analysedUTC = as.Date(temp2$analysedUTC)
             temp2$lastModifiedUTC = as.Date(temp2$lastModifiedUTC)

             temp_join = dplyr::full_join(temp,temp2)

             data.table::fwrite(temp_join
                                ,paste(path_to_save_to
                                       ,master_dir_name
                                       ,athleteTest
                                       ,"/"
                                       ,month
                                       ,".csv"
                                       ,sep = "")) }else{
        temp2 = test_info_bound[interaction(lubridate::month(strtrim(test_info_bound$recordedUTC,10))
                                                                       ,lubridate::year(strtrim(test_info_bound$recordedUTC,10))) == month,]

           data.table::fwrite(temp2
                              ,paste(path_to_save_to
                                     ,master_dir_name
                                     ,athleteTest
                                     ,"/"
                                     ,month
                                     ,".csv"
                                     ,sep = ""))
                                       }



  }


  last_upload = data.frame(max(lubridate::as_date(strtrim(test_info_bound$lastModifiedUTC,10))))
  data.table::fwrite(x = last_upload
                    ,file = paste(path_to_save_to
                           ,master_dir_name
                           ,oauth
                           ,"/last_upload_date"
                           ,sep = ''))



  #get test ids for trial retrieval
  test_ids = dplyr::bind_rows(test_info)$id
  #allocate a list for writing trial results to in for loop
  trial_information = vector(mode = 'list', length = length(test_ids))
  # name the components of the list as trial ids
  names(trial_information) = test_ids

  # get trial ids
  for(id in test_ids){

    trials_to_go = ifelse(exists('trials_to_go')
                          ,trials_to_go - 1
                          ,length(test_ids) - 1)


    url = paste("https://fdapi.valdperformance.com/v2019q3/teams/da8f0cb2-2138-49ac-8dcd-ed38f0424648/tests"
                ,"/"
                ,id
                ,"/trials"
                ,sep = "")


    trial_information[[id]] = invisible(tidyr::unnest(
      tidyr::unnest(jsonlite::fromJSON(txt = httr:: content(httr::GET(url = url
                                                                      ,httr::add_headers("Authorization" = read.token()[1,1]))
                                                            ,as = 'text')
                                       ,simplifyDataFrame = TRUE)
                    ,cols = results
                    ,names_repair = "unique")
      ,cols = definition
      ,names_repair = 'unique'))

    colnames(trial_information[[id]]) = c('id'
                                          ,'athleteId'
                                          ,'hubAthleteId'
                                          ,'recordedUTC'
                                          ,'recordedOffset'
                                          ,'recordedTimezone'
                                          ,'startTime'
                                          ,'endTime'
                                          ,'resultId'
                                          ,'value'
                                          ,'time'
                                          ,'limb.1'
                                          ,'repeat'
                                          ,'result_id'
                                          ,'result'
                                          ,'description'
                                          ,'result_name'
                                          ,'unit'
                                          ,'repeatable'
                                          ,'asymmmetry'
                                          ,'lastModifiedUTC'
                                          ,'limb2')

    print(paste(trials_to_go, 'more trials to download.'))

  }

  trial_information_bound = as.data.frame(dplyr::bind_rows(trial_information))


  days = levels(interaction(lubridate::year(strtrim(trial_information_bound$recordedUTC,10))
                            ,lubridate::month(strtrim(trial_information_bound$recordedUTC,10))
                            ,lubridate::day(strtrim(trial_information_bound$recordedUTC,10))))

  for(day in days){
    if(file.exists(paste(path_to_save_to
                         ,master_dir_name
                         ,testTrial
                         ,"/"
                         ,day
                         ,".csv"
                         ,sep=''))){
      temp = data.table::fread(paste(path_to_save_to
                                     ,master_dir_name
                                     ,testTrial
                                     ,"/"
                                     ,day
                                     ,".csv"
                                     ,sep=''))
      temp$recordedUTC = as.Date(temp$recordedUTC)
      temp$lastModifiedUTC = as.Date(temp$lastModifiedUTC)
      temp2 = trial_information_bound[interaction(lubridate::year(strtrim(trial_information_bound$recordedUTC,10))
                                                  ,lubridate::month(strtrim(trial_information_bound$recordedUTC,10))
                                                  ,lubridate::day(strtrim(trial_information_bound$recordedUTC,10))) == day,]

      temp2$recordedUTC = as.Date(temp2$recordedUTC)
      temp2$lastModifiedUTC = as.Date(temp2$lastModifiedUTC)

      temp_join = dplyr::full_join(temp,temp2)

      data.table::fwrite(temp_join
                         ,paste(path_to_save_to
                                ,master_dir_name
                                ,testTrial
                                ,"/"
                                ,day
                                ,".csv"
                                ,sep='')) }else{
                                  temp2 = trial_information_bound[interaction(lubridate::year(strtrim(trial_information_bound$recordedUTC,10))
                                                                              ,lubridate::month(strtrim(trial_information_bound$recordedUTC,10))
                                                                              ,lubridate::day(strtrim(trial_information_bound$recordedUTC,10))) == day,]

                                  data.table::fwrite(temp2
                                                     ,paste(path_to_save_to
                                                            ,master_dir_name
                                                            ,testTrial
                                                            ,"/"
                                                            ,day
                                                            ,".csv"
                                                            ,sep=''))
                                }
  }
  end = Sys.time()
  print(end-start)
}
