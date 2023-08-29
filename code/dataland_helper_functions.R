# Polls -------------------------------------------------------------------

#' Scrape polls
#'
#' @param url: URL to website with polling data. 
#' @param log_file: Txt file where error messages are logged. 
#'
#' @return Data frame with cleaned polls
#'
#' @examples
polls_get <- function(url = "https://cdn-dev.economistdatateam.com/jobs/pds/code-test/index.html", 
                      log_file = "error_log.txt") {
  
  tryCatch({
    
    # get raw html
    response <- read_html(url)
    
    # extract polls and convert to data.frame
    polls <- response %>% 
      html_table() %>% 
      as.data.frame()
    
    # extract column names
    col_names <- response %>% 
      html_nodes(xpath = "/html/body/main/table/thead") %>% 
      html_text() %>% 
      str_remove_all(pattern="\n") %>% 
      str_squish() %>% 
      str_split_1("\\s")
    colnames(polls) <- tolower(col_names)
    
    # clean polls
    polls_cleaned <- polls %>% 
      mutate(overseas = if_else(str_detect(sample, "[*]"),1,0),        # add dummy whether overseas territories are included
             alternate = if_else(str_detect(chettam, "[**]"), 1, 0),   # add dummy whether included in alternate question 
             date = base::as.Date(date, "%m/%d/%y"),                   # convert date format
             sample = sample %>%                                       # convert sample size to numeric
               str_remove_all("\\D+") %>% 
               as.numeric()) %>% 
      rename("n" = sample) %>% 
      mutate_at(vars(-c("date", "pollster", "n", "overseas", "alternate")),   # convert poll support shares to numeric 
                function(x) as.numeric(str_remove_all(x, "%"))/100)  %>% 
      suppressWarnings()
    
    # return polls
    return(polls_cleaned)
    
  }, 
  error = function(e) {
    cat(Sys.time(), 
        paste0("An error occured during scraping or clening polling data: ", e,
               "\n"),  
        file = log_file, 
        append = TRUE)
    message(paste0("Error: Something went wrong during scraping or processing polling data, check ", 
                   log_file, "."))
  })
}


#' Write/update polls
#'
#' @param file Name of csv file containing polls. 
#' @param path Path, where csv file will be/is stored.
#' @param log_file Txt file where error messages are logged. 
#'
#' @return Writes/updates csv file to/in defined path.
#'
#' @examples
polls_update <- function(file = "polls.csv", path = "data/", log_file = "error_log.txt"){

  tryCatch({
    
    file_path <- paste0(path, file)
    
    if(file.exists(file_path)){ # if file with polls already exists compare & update 
      
      old_polls <- read.csv(file_path) # if poll file exists save previous version
      polls <- polls_get()
      
      if(ncol(old_polls) != ncol(polls)| nrow(old_polls) != nrow(polls)){
        print("Polls will be updated.")
        
        # rename existing polls & keep a back-up copy (old_polls.csv)
        file.rename(from = file_path, to = paste0(path, "old_", file))
        
        # safe new polls
        write.csv(polls, file_path, row.names = F)
        
      } else {
        print("Polls are up to date.")
      }
      
    } else { # if file with polls does not exist yet write new
      polls <- polls_get()
      write.csv(polls, file_path, row.names = F)
      print("Polling data file has been generated.")
    }
  }, 
  error = function(e) {
    cat(Sys.time(), 
        paste0("An error occured during updating or generating polling data: ", 
               e, "\n"),  
        file = log_file, 
        append = TRUE)
    message(paste0("Error: Something went wrong during updating or generating polling data, check ", 
                   log_file, "."))
  })
}


# Trends ------------------------------------------------------------

#' Candidate specific poll trend
#'
#' @param polls Data frame with polls and date. 
#' @param candidate Character string containing column name where polls for one specific candidate are stored. 
#' @param dates Vector containing date sequence for which trend is estimated. 
#'
#' @return Vector with candidate trend.
#'
#' @examples
candidate_avg <- function(polls, candidate, dates){
  
  tryCatch({
    
    # get poll average for date for selected candidate
    candidate_avg <- sapply(dates, function(x) polls %>% 
                              select(tidyselect::all_of(candidate), date) %>% 
                              filter(date %in% seq((x-7), x, by = 1)) %>% 
                              select(tidyselect::all_of(candidate))) %>% 
      sapply(mean, na.rm = T) %>% 
      unname() 
    
    return(candidate_avg)
    
  }, 
  error = function(e) {
    cat(Sys.time(), 
        paste0("An error occured while generating trend for ", candidate, ": ", 
               e, "\n"),  
        file = log_file, 
        append = TRUE)
    message(paste0("Error: Something went wrong while generating the trend for ", 
    candidate, "check ", log_file, "."))
  }
  )
}

#' Poll trends for all candidates
#'
#' @param polls_path Character string containing path where csv file with polls is stored. 
#' @param alternate Logical, should alternate questions with subset of candidates be used. Default is FALSE.
#' @param overseas Logical, should polls excluding overseas territories be included. Default is TRUE.
#' @param log_file Character string containing name of txt file where error messages are logged.  
#'
#' @return Data frame with date column and on column for each candidate.
#'
#' @examples
poll_avg <- function(polls_path = "data/polls.csv", alternate = F, overseas = T,
                     log_file = "error_log.txt"){
  
  tryCatch({

    # read polls
    polls <- read.csv(polls_path)
    
    # ensure date format
    polls <- polls %>% 
      mutate(date = as.Date(date, "%Y-%m-%d"))
    
    # exclude polls excluding overseas territories 
    if(overseas == F){
      polls <- polls %>% 
        filter(overseas == 0)
    }    
    
    # order by date
    polls <- polls[order(polls$date),]
    
    # include only one version of alternate question
    if(alternate == F){ # include alternate polls with all candidates
      
      # remove alternate version with subsample of candidates
      polls <- polls %>% 
        filter(alternate == 0)
    } else { # include alternate question with subsample of candidates
      
      # order polls such that alternate version with subsample of candidates comes first
      polls <- polls[order(polls$date, polls$pollster, -polls$alternate),] 
      
      # remove duplicated
      polls <- polls[-which(duplicated(polls[c("date", "pollster", "n")])),]
    }
    
    # get dates
    dates <- seq(from = as.Date("10/11/23", "%m/%d/%y"), to = as.Date(max(polls$date)), by = 1) 
    
    # get candidate names
    candidates <- colnames(polls)[-which(colnames(polls) %in% c("date", "pollster", "n", "overseas", "alternate"))]
    
    # get trend for each candidate
    trends <- sapply(candidates, function(x) 
      candidate_avg(polls = polls, candidate = x, dates = dates)) %>% 
      as.data.frame()
    
    # fill missing values with previous mean, if there is no previous mean, with subsequent
    trends <- trends %>% fill(all_of(candidates), .direction = "downup")
    
    if(all(is.na(trends))){
      cat(Sys.time(), "Error: The trends file is empty\n", 
          file = log_file, 
          append = TRUE)
      stop("The trends file is empty")
      
    }
    
    # add date
    trends$date <- dates
    
    return(trends)
  }, 
  error = function(e) {
    cat(Sys.time(), paste0("An error occured while generating trends:", e, 
                           "\n"), 
        file = log_file, append = TRUE)
    message(paste0("Error: Something went wrong while generating trends, check ", 
                   log_file, "."))
  })
  
  
}


#' Save/update trends
#'
#' @param file Character string containing name of csv file with trend data.
#' @param path Character string containing path where trend csv file will be/is stored.
#'
#' @examples
trends_update <- function(file = "trends.csv", path = "data/"){

  tryCatch({
    
    file_path <- paste0(path, file)
    
    if(file.exists(file_path)){ # if file with polls already exists compare & update 
      
      old_trends <- read.csv(file_path)
      trends <- poll_avg()
      
      if(ncol(old_trends) != ncol(trends)| nrow(old_trends) != nrow(trends)){
        print("Trends will be updated.")
        
        # rename existing polls & keep a back-up copy (old_polls.csv)
        file.rename(from = file_path, to = paste0(path, "old_", file))
        
        # safe new polls
        write.csv(trends, file_path, row.names = F)
        
      } else {
        print("Trends are up to date.")
      }
      
    } else { # if file with trends does not exist yet write 
      trends <- poll_avg()
      write.csv(trends, file_path, row.names = F)
      print("Trends data file has been generated.")
    } 
  }, 
  
  error = function(e) {
    cat(Sys.time(), paste0("An error occured while updating/writing trends:", e, 
                           "\n"), 
        file = log_file, append = TRUE)
    message(paste0("Error: Something went wrong while updating/writing trends, check ", 
                   log_file, "."))
  })
}





