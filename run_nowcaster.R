#' @title run_nowcasting
#'
#' @description Wrapper function to use the nowcaster package
#'
#' @param dataset dataset to be formatted as data by week
#' @param trim.data How much to trim of the data?
#' @param bins_age Bins of age to cu the data, parsing from nowcasting_inla. Optional
#' @param date_onset Column of dates of onset of the events, normally date of onset of first symptoms of cases
#' @param date_report Column of dates of report of the event, normally date of digitation of the notification of cases
#' @param age_col Age column to be where to  cut the data into age classes. Optional
#' @param K How many weeks to forecast ahead?
#' [Default] K is 0, no forecasting ahead
#'
#' @return Data in weeks format, with the maximum dates for the last week used
#' @export
#'
#' @examples Assumes epidemiological weeks from Sunday to Saturday

run_nowcaster <- function(dataset_path,
                           last_epiweek,
                           date_onset='date_event',
                           date_report='date_upload',
                           trim.data,
                           age_strata=F,
                           bins_age,
                           age_col,
                           K=0,
                           silent=F,
                           ...){
  
  get_D_ata<-function(p){
    if(str_detect(p,".xlsx$")){
      data <- data.frame(read_xlsx(p,sheet=1),stringsAsFactors =F)
    }
    else if(str_detect(p,".xls$")){
      data <- data.frame(read_xls(p,sheet=1),stringsAsFactors =F)
    } 
    else if(str_detect(p,".csv$")){
      data <- fread(p,header =T,stringsAsFactors =F, data.table=F, check.names = T)
    } else{
      data <- fread('./Demo_Data/Demo_line_data.csv',header =T,stringsAsFactors =F, data.table=F, check.names = T)
    }
    data
  }
  
  last_epiweek <- input$lastepiweek_nowcast
  last_epi.year <- as.integer(str_split(last_epiweek, '-')[[1]][1])
  last_epi.week <- as.integer(str_split(last_epiweek, '-')[[1]][2])
  
  # Filter case notifications to insertions up to the week to be nowcasted
  df <- get_D_ata(inFile_c$datapath) %>%
    mutate(epi.year = epiyear(date_upload),
           epi.week = epiweek(date_upload),
           epi.yearweek = paste0(epi.year, '-', epi.week),
           date_event = as.Date(date_event),
           date_upload = as.Date(date_upload)) %>%
    filter(epi.year <= last_epi.year |
             epi.week <= last_epi.week)
  
    if ('district' %in% names(df)){
      df.nowcast <- df %>% 
        group_by(district) %>%
        group_modify(~ nowcasting_inla(.x,
                                       date_onset=date_onset,
                                       date_report=date_report,
                                       data.by.week=T)$total)
    } else {
      df.nowcast <- nowcasting_inla(dataset=df,
                                    date_onset=date_onset,
                                    date_report=date_report,
                                    data.by.week=T)$total
    }
    
  return(list(line_data=df, nowcast=df.nowcast))
}