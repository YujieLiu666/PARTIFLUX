run_one_site <- function(site_name, file_name, lat, lon,
                         data_dir, work_dir) {
  
  library(data.table)
  library(bigleaf)
  library(ggplot2)
  library(REddyProc)
  library(tidyverse)
  library(lutz)
  library(ggpubr)
  
  tryCatch({
    
    cat("Running:", site_name, "\n")
    
    # ------------------------------
    # Time zone
    tz_name = tz_lookup_coords(lat = lat, lon = lon)
    
    TimeZoneHour <- as.numeric(
      format(
        as.POSIXct(Sys.time(), tz = tz_name),
        "%z"
      )
    ) / 100
    
    # ------------------------------
    # Load data
    df = fread(file.path(data_dir, file_name))
    df[df == -9999] <- NA
    
    EddyData = data.frame(
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
    # Time variables
    recreate_time_vars <- function(df) {
      df %>%
        mutate(
          TIMESTAMP = substr(TIMESTAMP_START, 1, 12),
          Year = substr(TIMESTAMP_START, 1, 4),
          Month = substr(TIMESTAMP_START, 5, 6),
          Day = substr(TIMESTAMP_START, 7, 8),
          Hour = as.numeric(substr(TIMESTAMP_START, 9, 10)) + 0.5,
          Date = as.Date(paste(Year, Month, Day, sep = "-")),
          DoY = yday(Date)
        )
    }
    
    EddyData = recreate_time_vars(EddyData)
    EddyData = EddyData[EddyData$Year == 2018, ]  # keep your test filter
    
    # ------------------------------
    # Initialize REddyProc
    EddyData$Year <- as.numeric(EddyData$Year)
    EddyData$Hour <- as.numeric(EddyData$Hour)
    EddyData$DoY  <- as.numeric(EddyData$DoY)
    
    EddyDataWithPosix <- fConvertTimeToPosix(
      EddyData, 'YDH',
      Year = 'Year', Day = 'DoY', Hour = 'Hour'
    )
    
    EProc <- sEddyProc$new(
      site_name,
      EddyDataWithPosix,
      c('NEE','Rg','Tair','VPD','Ustar')
    )
    
    # ------------------------------
    # Gap filling
    EProc$sMDSGapFill('Tair', FillAll = FALSE, minNWarnRunLength = NA)
    EProc$sMDSGapFill('VPD', FillAll = FALSE, minNWarnRunLength = NA)
    EProc$sMDSGapFill('NEE')
    EProc$sMDSGapFill('Rg')
    
    cat("MDS gapfilled meteorological variables!\n")
    
    EProc$sSetLocationInfo(
      LatDeg = lat,
      LongDeg = lon,
      TimeZoneHour = TimeZoneHour
    )
    
    # Save base object
    saveRDS(EProc, file.path(work_dir, paste0(site_name, ".rds")))
    
    # ------------------------------
    # Partitioning methods
    
    ## ---- NT ----
    cat("start of NT...\n")
    EProc = readRDS(file.path(work_dir, paste0(site_name, ".rds")))
    EProc$sSetLocationInfo(LatDeg = lat, LongDeg = lon, TimeZoneHour = TimeZoneHour)
    EProc$sMRFluxPartition()
    saveRDS(EProc, file.path(work_dir, paste0(site_name, "_NT.rds")))
    
    EddyData$RECO_NT = EProc$sTEMP$Reco
    EddyData$GPP_NT  = EProc$sTEMP$GPP_f
    
    ## ---- DT ----
    cat("start of DT...\n")
    EProc = readRDS(file.path(work_dir, paste0(site_name, ".rds")))
    EProc$sSetLocationInfo(LatDeg = lat, LongDeg = lon, TimeZoneHour = TimeZoneHour)
    EProc$sGLFluxPartition()
    saveRDS(EProc, file.path(work_dir, paste0(site_name, "_DT.rds")))
    
    EddyData$GPP_DT       = EProc$sTEMP$GPP_DT
    EddyData$GPP_DT_SD    = EProc$sTEMP$GPP_DT_SD
    EddyData$RECO_DT      = EProc$sTEMP$Reco_DT
    EddyData$RECO_DT_SD   = EProc$sTEMP$Reco_DT_SD
    
    ## ---- MDT ----
    cat("start of MDT...\n")
    EProc = readRDS(file.path(work_dir, paste0(site_name, ".rds")))
    EProc$sSetLocationInfo(LatDeg = lat, LongDeg = lon, TimeZoneHour = TimeZoneHour)
    EProc$sTKFluxPartition()
    saveRDS(EProc, file.path(work_dir, paste0(site_name, "_MDT.rds")))
    
    EddyData$GPP_MDT      = EProc$sTEMP$GPP_DT
    EddyData$GPP_MDT_SD   = EProc$sTEMP$GPP_DT_SD
    EddyData$RECO_MDT     = EProc$sTEMP$Reco_DT
    EddyData$RECO_MDT_SD  = EProc$sTEMP$Reco_DT_SD
    
    # ------------------------------
    # Save final data
    fwrite(EddyData, file.path(work_dir, paste0(site_name, ".csv")))
    
    cat("Finished:", site_name, "\n")
    
    return(site_name)
    
  }, error = function(e) {
    message("Failed: ", site_name)
    message(e)
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


# Below are just notes for my self ------------------------------------------------------------
# Another option: linux/macOS only 
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

# For HPC user
# library(data.table)
# 
# # read site table
# site_info <- fread("your_site_table.csv")
# 
# # clean rows
# site_info <- site_info[
#   !is.na(file_name) & file_name != "" &
#     !is.na(lat) & !is.na(lon)
# ]
# 
# # ------------------------------
# # get job index from SLURM
# args <- commandArgs(trailingOnly = TRUE)
# i <- as.numeric(args[1])
# 
# cat("Processing row:", i, "\n")
# 
# # ------------------------------
# # source your function
# source("run_one_site.R")
# 
# # ------------------------------
# # run ONE site
# run_one_site(
#   site_name = site_info$site_name[i],
#   file_name = site_info$file_name[i],
#   lat = site_info$lat[i],
#   lon = site_info$lon[i],
#   data_dir = "your_data_dir",
#   work_dir = "your_output_dir"
# )

#!/bin/bash
#SBATCH --job-name=flux_partition
#SBATCH --output=logs/out_%A_%a.txt
#SBATCH --error=logs/err_%A_%a.txt
#SBATCH --array=1-50
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=12:00:00
# 
# module load R   # or your HPC's R module
# 
# Rscript run_site.R $SLURM_ARRAY_TASK_ID

