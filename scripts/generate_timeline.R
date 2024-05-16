library(odbc)
library(dplyr)
library(lubridate)
library(vistime)

#======================
#   INPUT VARIABLES
#======================
selected_start_date <- "2024-05-15"
selected_stop_date  <- "2024-05-16"
destination_path    <- "C:\\job-execution-gantt-plots"
str_to_match        <- NA  # Add your matching regex string here
str_to_unmatch      <- NA  # Add your unmatching regex string here
#======================

getJobGanttByDate <- function(selected_start_date_str, selected_stop_date_str, jobs_info_df, str_to_match, str_to_unmatch) {
  
  if (length(selected_start_date_str) <= 10)
    selected_date_start = ymd_hms(paste(selected_start_date_str, "00:00:00"))
  
  if (length(selected_stop_date_str) <= 10)
    selected_date_end   = ymd_hms(paste(selected_stop_date_str, "23:59:59"))
  
  dt_selected <- c(selected_date_start, selected_date_end)
  
  if ((!is.na(str_to_match) && str_to_match != "") & (is.na(str_to_unmatch) || str_to_unmatch == "")) {
    jobs_info_df <- jobs_info_df %>% filter(grepl(str_to_match, job_name, ignore.case = TRUE))
  } else if ((is.na(str_to_match) || str_to_match == "") & (!is.na(str_to_unmatch) && str_to_unmatch != "")) {
    jobs_info_df <- jobs_info_df %>% filter(!grepl(str_to_unmatch, job_name, ignore.case = TRUE))
  } else if ((!is.na(str_to_match) && str_to_match != "") & (!is.na(str_to_unmatch) && str_to_unmatch != "")) {
    jobs_info_df <- jobs_info_df %>% filter(grepl(str_to_match, job_name, ignore.case = TRUE) & !grepl(str_to_unmatch, job_name, ignore.case = TRUE))
  }
  
  jobs_to_show_df <- jobs_info_df %>% 
    mutate(
      color = ifelse(job_status == "Succeeded", "#008b00",
                     ifelse(job_status == "Failed", "#8b0000", "#cdcd00")
      ),
      job_start_str = as.character(job_start_datetime),
      job_end_str   = as.character(job_stop_datetime)
    ) %>% 
    mutate(
      tooltip = sprintf("<b>%s</b>\n<i>From:</i> %s  -  <i>To:</i> %s\n<i>Duration:</i> %02d:%02d:%02d\n\n<i>Message:</i>\n%s",
                        job_name, job_start_datetime, job_stop_datetime,
                        job_duration_hours, job_duration_minutes, job_duration_seconds,
                        gsub("[.][ ]+", ".\n", jobs_info_df$job_message))
    ) %>%
    rowwise() %>%
    mutate(
      overlap_sec = DescTools::Overlap(dt_selected, c(job_start_datetime, job_stop_datetime))
    ) %>%
    filter(overlap_sec > 0) %>% 
    select(
      event = job_name,
      start = job_start_datetime,
      end   = job_stop_datetime,
      group = job_status,
      color = color,
      tooltip = tooltip,
      overlap_sec
    )
  
  fig <- vistime(
    data        = jobs_to_show_df,
    optimize_y  = TRUE,
    show_labels = FALSE,
    title       = paste("Agent Jobs running from day", selected_start_date_str, "to", selected_stop_date_str)
  )
  
  return(fig)
  
}

conn <- DBI::dbConnect(
  odbc::odbc(),
  server = "localhost", 
  database = "master",
  trusted_connection = "yes",
  driver = "{ODBC Driver 17 for SQL Server}"
)

jobs_df <- DBI::dbGetQuery(conn,
                           "SELECT
 job_name,
 job_start_datetime,
 job_stop_datetime,
 job_duration_in_seconds,
 job_duration_hours,
 job_duration_minutes,
 job_duration_seconds,
 job_status,
 job_message
FROM master.dbo.dba_GetJobsHistory2
ORDER BY job_start_datetime DESC")

DBI::dbDisconnect(conn)

fig <- getJobGanttByDate(selected_start_date_str = selected_start_date,
                         selected_stop_date_str = selected_stop_date,
                         jobs_info_df = jobs_df,
                         str_to_match = str_to_match,
                         str_to_unmatch = str_to_unmatch)

output_msg <- ""

if (dir.exists(file.path(destination_path))) {
  
  selected_start_date_str <- gsub("\\s", "_", gsub("-", "", gsub(":", "", selected_start_date)))
  selected_stop_date_str <- gsub("\\s", "_", gsub("-", "", gsub(":", "", selected_stop_date)))
  
  file_name <- paste0("job-execution-gantt-", selected_start_date_str, "_", selected_stop_date_str, ".html")
  
  setwd(destination_path)
  htmlwidgets::saveWidget(fig, file = file.path(destination_path, file_name),
                          title = paste0("SQL Jobs Gantt from ", selected_start_date, " to ", selected_stop_date), selfcontained = TRUE)
  
  output_msg <- paste0("Jobs Gantt plot named '", file_name, "' saved into ", destination_path, " folder")
} else {
  
  output_msg <- paste0("Destination folder doesn't exist: ", destination_path)
  
}

output_msg
