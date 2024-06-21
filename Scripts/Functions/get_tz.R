get_tz <- function(dates = dates, loc = loc){
  # need to change the dates from UTC
  if(sign(loc[1]) == -1){
    gmtzone <- "+"
  }else{
    gmtzone <- ""
  }
  tz <- paste0("Etc/GMT", gmtzone, round(loc[1]/15*-1, 0))
  attr(dates, "tzone") <- tz
  return(dates)
}