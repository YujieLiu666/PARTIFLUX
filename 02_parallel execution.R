run_one_site <- function(site_name, file_name, lat, lon,
                         data_dir, work_dir) {
  
  library(data.table)
  library(bigleaf)
  library(REddyProc)
  library(tidyverse)
  library(lutz)
  
  tryCatch({
    
    # ------------------------------
    # timezone
    tz_name <- tz_lookup_coords(lat = lat, lon = lon)
    
    TimeZoneHour <- as.numeric(format(
      as.POSIXct(Sys.time(), tz = tz_name),
      "%z"
    )) / 100
    
    # ------------------------------
    # load data
    setwd(data_dir)
    df <- fread(file_name)
    df[df == -9999] <- NA
    
    EddyData <- data.frame(
      TIMESTAMP_START = df$TIMESTAMP_START,
      NEE = df$NEE_CUT_REF,
      Rg = ifelse(df$PPFD_IN < 0, 0, PPFD.to.Rg(df$PPFD_IN)),
      PPFD = df$PPFD_IN,
      Tair = df$TA_F,
      RH = ifelse(df$RH > 100, 100, df$RH),
      VPD = df$VPD_F,
      Ustar = df$USTAR,
      GPP_DT_CUT_REF = df$GPP_DT_CUT_REF,
      GPP_NT_CUT_REF = df$GPP_NT_CUT_REF,
      RECO_DT_CUT_REF = df$RECO_DT_CUT_REF,
      RECO_NT_CUT_REF = df$RECO_NT_CUT_REF
    )
    
    # ------------------------------
    # time vars
    EddyData <- EddyData %>%
      mutate(
        TIMESTAMP = substr(TIMESTAMP_START, 1, 12),
        Year = substr(TIMESTAMP_START, 1, 4),
        Month = substr(TIMESTAMP_START, 5, 6),
        Day = substr(TIMESTAMP_START, 7, 8),
        Hour = as.numeric(substr(TIMESTAMP_START, 9, 10)) + 0.5,
        Date = as.Date(paste(Year, Month, Day, sep = "-")),
        DoY = yday(Date)
      )
    
    EddyData$Year <- as.numeric(EddyData$Year)
    
    # ------------------------------
    # REddyProc
    EddyDataWithPosix <- fConvertTimeToPosix(
      EddyData, 'YDH',
      Year = 'Year', Day = 'DoY', Hour = 'Hour'
    )
    
    EProc <- sEddyProc$new(
      site_name, EddyDataWithPosix,
      c('NEE','Rg','Tair','VPD','Ustar')
    )
    
    EProc$sMDSGapFill('Tair', FillAll = FALSE, minNWarnRunLength = NA)
    EProc$sMDSGapFill('VPD', FillAll = FALSE, minNWarnRunLength = NA)
    EProc$sMDSGapFill('NEE')
    EProc$sMDSGapFill('Rg')
    
    EProc$sSetLocationInfo(
      LatDeg = lat, LongDeg = lon,
      TimeZoneHour = TimeZoneHour
    )
    
    # ------------------------------
    # partitioning
    EProc$sMRFluxPartition()
    EddyData$GPP_NT <- EProc$sTEMP$GPP_f
    
    EProc$sGLFluxPartition()
    EddyData$GPP_DT <- EProc$sTEMP$GPP_DT
    
    EProc$sTKFluxPartition()
    EddyData$GPP_MDT <- EProc$sTEMP$GPP_DT
    
    # ------------------------------
    # save
    setwd(work_dir)
    fwrite(EddyData, paste0(site_name, ".csv"))
    
    return(site_name)
    
  }, error = function(e) {
    message("Failed: ", site_name)
    return(NULL)
  })
}

# Example application ------------------------------------------------------------
library(future)
library(future.apply)
library(data.table)

site_info <- fread("PARTIFLUX_NEE_task")  # the spreadsheet on Google drive
# site_info <- site_info[!is.na(file_name) & file_name != ""] # The metadata for lon and lat is to be updated
plan(multisession, workers = 6)  # adjust cores

results <- future_lapply(
  1:nrow(site_info),
  function(i) {
    run_one_site(
      site_name = site_info$site_name[i],
      file_name = site_info$file_name[i],
      lat = site_info$lat[i],
      lon = site_info$lon[i],
      data_dir = "your_data_dir", 
      work_dir = "your_output_dir"
    )
  }
)

# another option: linux/macOS only 
# library(parallel)
# 
# results <- mclapply(
#   1:nrow(site_info),
#   function(i) {
#     run_one_site(
#       site_info$site_name[i],
#       site_info$file_name[i],
#       site_info$lat[i],
#       site_info$lon[i],
#       data_dir,
#       work_dir
#     )
#   },
#   mc.cores = 6
# )
