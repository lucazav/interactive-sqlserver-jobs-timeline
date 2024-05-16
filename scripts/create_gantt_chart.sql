USE [master]
GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[sp_GenerateJobExecutionGanttPlot]
 @StartDate DATE = NULL,
 @EndDate DATE = NULL,
 @RegexStringToMatch NVARCHAR(100) = NULL,
 @RegexStringToUnmatch NVARCHAR(100) = NULL,
 @OutputPath VARCHAR(500) = 'C:/job-execution-gantt-plots'
AS
BEGIN
 SET NOCOUNT ON;

 DECLARE
  @CalculatedStartDate DATE,
  @CalculatedEndDate DATE;

 IF @StartDate IS NULL AND @EndDate IS NULL
 BEGIN
  SET @CalculatedStartDate = DATEADD(DAY, -2, GETDATE());
  SET @CalculatedEndDate = DATEADD(DAY, 1, GETDATE());
 END
 ELSE IF @StartDate IS NOT NULL AND @EndDate IS NULL
 BEGIN
  SET @CalculatedStartDate = @StartDate;
  SET @CalculatedEndDate = DATEADD(DAY, 3, @StartDate);
 END
 ELSE IF @StartDate IS NULL AND @EndDate IS NOT NULL
 BEGIN
  SET @CalculatedStartDate = DATEADD(DAY, -3, @EndDate);
  SET @CalculatedEndDate = @EndDate;
 END
 ELSE
 BEGIN
  SET @CalculatedStartDate = @StartDate;
  SET @CalculatedEndDate = @EndDate;
 END

 RAISERROR ('Getting jobs...', 0, 1) WITH NOWAIT;

 DROP TABLE IF EXISTS #JobDetails;
 
 SELECT
    job_name
  , job_start_datetime
  , job_stop_datetime
  , job_duration_in_seconds
  , job_duration_hours
  , job_duration_minutes
  , job_duration_seconds
  , job_status
  , job_message
 INTO #JobDetails
 FROM master.dbo.dba_GetJobsHistory
 ORDER BY
  job_start_datetime DESC

 RAISERROR ('Query done.', 0, 1) WITH NOWAIT;

 
 RAISERROR ('Executing the script...', 0, 1) WITH NOWAIT;

 DECLARE @rScriptPart1 NVARCHAR(MAX) = N'

 library(dplyr)
 library(lubridate)
 library(vistime)

 getJobGanttByDate <- function(selected_start_date_str, selected_stop_date_str, jobs_info_df, str_to_match, str_to_unmatch) {
  
   selected_date_start = ymd_hms(paste(selected_start_date_str, "00:00:00"))
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
    color = ifelse(job_status == "Succeded", "#008b00",
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
    overlap_sec = DescTools::Overlap( dt_selected,
           c(job_start_datetime, job_stop_datetime) )
  ) %>%
  filter( overlap_sec > 0 ) %>% 
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
 '

 DECLARE @rScriptPart2 NVARCHAR(MAX) = CONCAT(N'

 selected_start_date <- "', CONVERT(CHAR(10), @CalculatedStartDate, 23), N'"
 selected_stop_date  <- "', CONVERT(CHAR(10), @CalculatedEndDate, 23), N'"
 destination_path    <- "', REPLACE(@OutputPath, '\', '\\'), N'"
 str_to_match        <- ', IIF(@RegexStringToMatch IS NULL, N'NA', N'"' + @RegexStringToMatch + N'"'), N'
 str_to_unmatch      <- ', IIF(@RegexStringToUnmatch IS NULL, N'NA',  N'"' + @RegexStringToUnmatch + N'"'), N'
 
 ')

 DECLARE @rScriptPart3 NVARCHAR(MAX) = N'
 fig <- getJobGanttByDate(selected_start_date_str = selected_start_date,
                             selected_stop_date_str = selected_stop_date,
                             jobs_info_df = jobs_df,
                             str_to_match = str_to_match,
                             str_to_unmatch = str_to_unmatch)

 output_msg <- ""

 if ( dir.exists(file.path(destination_path)) ) {
  
      selected_start_date_str <- gsub("\\s", "_", gsub("-", "", gsub(":", "", selected_start_date)))
      selected_stop_date_str <- gsub("\\s", "_", gsub("-", "", gsub(":", "", selected_stop_date)))
      
      file_name <- paste0("job-execution-gantt-", selected_start_date_str, "_", selected_stop_date_str, ".html")
   
   setwd(destination_path)
   htmlwidgets::saveWidget(fig, file = file.path(destination_path, file_name),
                              title = paste0("SQL Jobs Gantt from ", selected_start_date, " to ", selected_stop_date), selfcontained = TRUE)
  
   output_msg <- paste0("Jobs gantt plot named ''", file_name, "'' saved into ", destination_path, " folder")

 } else {
  
   output_msg <- paste0("Destination folder doesn''t exist: ", destination_path)

 }

 OutputDataSet <- data.frame(output_msg = output_msg)
 '

 DECLARE @rScript NVARCHAR(MAX) = CONCAT(@rScriptPart1, @rScriptPart2, @rScriptPart3);

 
 EXEC sp_execute_external_script
    @language = N'fraud_R'
  , @script = @rScript
  , @input_data_1 = N'SELECT * FROM #JobDetails;'
  , @input_data_1_name = N'jobs_df'
 WITH RESULT SETS
 ((
  output_msg VARCHAR(MAX)
 ));

 RAISERROR ('Script executed.', 0, 1) WITH NOWAIT;

END
GO
