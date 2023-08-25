


# Get polls ---------------------------------------------------------------

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
  
  # get raw html
  response <- tryCatch({
    read_html(url)
  }, 
  error = function(e) {
    cat(Sys.time(), "Error: Website is not accessible or does not exist", file = log_file, append = TRUE)
    stopifnot("Error: Website is not accessible or does not exist")
  })
  
  # extract polls and convert to data.frame
  polls <- response %>% 
    html_table() %>% 
    as.data.frame()
  
  # check whether there are polls
  tryCatch({
    if(!is.data.frame(polls) ||nrow(polls) == 0 || ncol(polls) == 0){
      stop("There are no polls.")
    }
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
  })
  
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
           date = base::as.Date(date, "%m/%d/%y"),                         # convert date format
           sample = sample %>%                                       # convert sample size to numeric
             str_remove_all("[,]|[*]") %>% 
             as.numeric()) %>% 
    mutate_at(vars(-c("date", "pollster", "sample", "overseas", "alternate")),   # convert poll support shares to numeric 
              function(x) as.numeric(str_remove_all(x, "%"))/100)  %>% 
    suppressWarnings()
  
  # return polls
  return(polls_cleaned)
  
}



# Compare polls -----------------------------------------------------------

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

  file_path <- paste0(path, file)
  
  if(file.exists(file_path)){ # if file with polls already exists compare & update 
    
    old_polls <- read.csv(file_path)
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
  }
}


# Trend ------------------------------------------------------------

#' Candidate specific poll trend
#'
#' @param polls Data frame with polls and date 
#' @param candidate Column name where polls for one specific candidate are stored 
#' @param dates Date sequence for which trend is estimated 
#'
#' @return Vector with candiadte trend
#'
#' @examples
candidate_avg <- function(polls, candidate, dates){
  
  # get poll average for date for selected candidate
  candidate_avg <- sapply(dates, function(x) polls %>% 
                            select(tidyselect::all_of(candidate), date) %>% 
                            filter(date %in% seq((x-7), x, by = 1)) %>% 
                            select(tidyselect::all_of(candidate))) %>% 
    sapply(mean, na.rm = T) %>% 
    unname() 
  
  return(candidate_avg)
  
}

#' Poll trend for all candiadtes
#'
#' @param polls_path Path where csv file with polls is stored. 
#' @param alternate Should alternate questions with subset of candidates be used. Default is FALSE.
#' @param log_file Txt file where error messages are logged.  
#'
#' @return Data frame with date column and on column for each candiadte
#' @export
#'
#' @examples
poll_avg <- function(polls_path = "data/polls.csv", alternate = F, log_file = "error_log.txt"){
  
  # read polls
  tryCatch({
    polls <- read.csv(polls_path)
    
    # ensure date format
    polls <- polls %>% 
      mutate(date = as.Date(date, "%Y-%m-%d"))
    
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
      polls <- polls[-which(duplicated(polls[c("date", "pollster", "sample")])),]
    }
    
    # get dates
    dates <- seq(from = as.Date("10/11/23", "%m/%d/%y"), to = as.Date(max(polls$date)), by = 1) 
    
    # get candidate names
    candidates <- colnames(polls)[-which(colnames(polls) %in% c("date", "pollster", "sample", "overseas", "alternate"))]
    
    # get trend for each candidate
    trend <- sapply(candidates, function(x) 
      candidate_avg(polls = polls, candidate = x, dates = dates)) %>% 
      as.data.frame()
    
    # fill missing values with previous mean, if there is no previous mean, with subsequent
    trend <- trend %>% fill(all_of(candidates), .direction = "downup")
    
    if(all(is.na(trend))){#
      cat(Sys.time(), "Error: The trend file is empty\n", 
          file = log_file, 
          append = TRUE)
      stop("The trend file is empty")
      
    }
    
    # add date
    trend$date <- dates
    
    return(trend)
  }, 
  error = function(e) {
    cat(Sys.time(), paste0("Error: There is no file called ", polls_path, "\n"), file = log_file, append = TRUE)
    stop(conditionMessage(e))
  })
  
}

trend_update <- function(file = "trend.csv", path = "data/"){
  
  file_path <- paste0(path, file)
  
  if(file.exists(file_path)){ # if file with polls already exists compare & update 
    
    old_trend <- read.csv(file_path)
    trend <- poll_avg()
    
    if(ncol(old_trend) != ncol(trend)| nrow(old_trend) != nrow(trend)){
      print("Trend will be updated.")
      
      # rename existing polls & keep a back-up copy (old_polls.csv)
      file.rename(from = file_path, to = paste0(path, "old_", file))
      
      # safe new polls
      write.csv(trend, file_path, row.names = F)
      
    } else {
      print("Trend is up to date.")
    }
    
  } else { # if file with trend does not exist yet write 
    trend <- poll_avg()
    write.csv(trend, file_path, row.names = F)
  }
}





