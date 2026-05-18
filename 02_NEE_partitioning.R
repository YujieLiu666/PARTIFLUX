# packages --------------------------------------------
{
  # Fast data import and efficient table operations
  library(data.table)
  # Micrometeorological and ecosystem flux calculations
  # (e.g., VPD, ET, conductance, energy balance)
  library(bigleaf)
  # Data visualization and plotting
  library(ggplot2)
  # Eddy covariance flux processing and gap-filling tools
  library(REddyProc)
  # Collection of packages for data manipulation, wrangling, and visualization
  # Includes dplyr, tidyr, readr, tibble, stringr, etc.
  library(tidyverse)
  # Lookup geographic time zones from latitude/longitude coordinates
  library(lutz)
  library(ggpubr)
  
  # Working directory
  work_dir = "F:/23 PARTIFLUX/Output"
  data_dir = ("C:/Users/yl763/OneDrive - Northern Arizona University/Shared_PARTIFLUX/HH_FLUXNET2025")
  
  # Site-specific information
  site_name = "AR-CCg" 
  file_name = "AMF_AR-CCg_FLUXNET_FLUXMET_HH_2018-2024_v1.3_r1.csv"
  lat = -35.9244 ; lon = -61.1855
  tz_name = tz_lookup_coords(lat = lat, lon = lon)
  
  # current UTC offset in hours
  TimeZoneHour <- as.numeric(
    format(
      as.POSIXct(Sys.time(), tz = tz_name),
      "%z"
    )
  ) / 100
  
  # @Mo, I understand by specifying the site information above, there is no need to 
  # change anything in the codes below. Please double check.
}

# load data --------------------------------------------
{
  setwd(data_dir)
  df = fread(file_name) # change to your own
  df[df == -9999] <- NA 
  
  # down-select variables
  EddyData = data.frame(
    TIMESTAMP_START = df$TIMESTAMP_START,
    NEE = df$NEE_CUT_REF,
    Rg = ifelse(df$PPFD_IN < 0, 0, PPFD.to.Rg(df$PPFD_IN)),
    PPFD = df$PPFD_IN,
    Tair = df$TA_F,
    RH = ifelse(df$RH > 100, 100, df$RH),
    VPD = df$VPD_F,
    Ustar = df$USTAR,
    GPP_DT_CUT_REF = df$GPP_DT_CUT_REF, # please check if we should use another variable 
    GPP_NT_CUT_REF = df$GPP_NT_CUT_REF, # please check if we should use another variable 
    RECO_DT_CUT_REF = df$RECO_DT_CUT_REF, # please check if we should use another variable 
    RECO_NT_CUT_REF = df$RECO_NT_CUT_REF # please check if we should use another variable 
    
  )
  
  # adding time variables
  recreate_time_vars <- function(df) {
    df %>%
      mutate(
        TIMESTAMP = substr(TIMESTAMP_START, 1, 12),
        Year = substr(TIMESTAMP_START, 1, 4),
        Month = substr(TIMESTAMP_START, 5, 6),
        Day = substr(TIMESTAMP_START, 7, 8),
        Hour = as.numeric(substr(TIMESTAMP_START, 9, 10)) + c(0.5, 1),
        Date = as.Date(paste(Year, Month, Day, sep = "-")),
        DoY = yday(Date)
      )
  }
  EddyData = recreate_time_vars(EddyData)
}

# Initialize EProc --------------------------------------------
{
  setwd(work_dir)
  EddyData$Year <- as.numeric(EddyData$Year)
  EddyData$Hour <- as.numeric(EddyData$Hour)
  EddyData$DoY <- as.numeric(EddyData$DoY)
  EddyDataWithPosix <- fConvertTimeToPosix(EddyData, 'YDH', Year = 'Year', Day = 'DoY', Hour  = 'Hour') 
  EProc <- sEddyProc$new(site_name, EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))
  
  # FLUXNET data is almost gap-free, but you will still gapfill the meteorological variables to get the quality flags 
  EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)
  EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)     
  EProc$sMDSGapFill('NEE')
  EProc$sMDSGapFill('Rg') # required by daytime partitioning 
  print("MDS gapfilled meterological variables!")
  
  # Change to your site-specific information carefully here!!!
  ## TimeZoneHour: time zone offset from UTC (Coordinated Universal Time), without daytime saving. 
  ## (You can ask ChatGPT for the TimeZoneHour giving coordinates for a study site)
  EProc$sSetLocationInfo(LatDeg = lat, LongDeg = lon, TimeZoneHour = TimeZoneHour) 
  # Save Eproc object
  saveRDS(EProc, paste0(site_name, ".rds"))
}

# Different partitioning methods --------------------------------------------
{
  # Nighttime-based partitioning Reichstein et al. (2005)
  print("start of NT: night partitioning ... ")
  EProc = readRDS(paste0(site_name, ".rds"))
  EProc$sSetLocationInfo(LatDeg = lat, LongDeg = lon, TimeZoneHour = TimeZoneHour) 
  EProc$sMRFluxPartition() 
  saveRDS(EProc, paste0(site_name, "_NT.rds"))
  
  ## save parameters and variables back to dataframe
  # EddyData$PotRad_NT = EProc$sTEMP$PotRad
  # EddyData$FP_NEEnight_NT = EProc$sTEMP$FP_NEEnight
  # EddyData$FP_Temp_NT = EProc$sTEMP$FP_Temp
  # EddyData$E_0_NT = EProc$sTEMP$E_0
  # EddyData$R_ref_NT = EProc$sTEMP$R_ref
  EddyData$RECO_NT = EProc$sTEMP$Reco
  EddyData$GPP_NT = EProc$sTEMP$GPP_f
  # EddyData$GPP_NT_fqc = EProc$sTEMP$GPP_fqc
  
  # Daytime-based partitioning Lasslop et al. (2010)
  print("start of DT: daytime partitioning ... ")
  EProc = readRDS(paste0(site_name, ".rds"))
  EProc$sSetLocationInfo(LatDeg = lat, LongDeg = lon, TimeZoneHour = TimeZoneHour) 
  EProc$sGLFluxPartition() 
  saveRDS(EProc, paste0(site_name, "_DT.rds"))
  ## save parameters and variables back to dataframe
  EddyData$GPP_DT = EProc$sTEMP$GPP_DT
  EddyData$GPP_NT_SD = EProc$sTEMP$GPP_DT_SD
  EddyData$RECO_DT =  EProc$sTEMP$Reco_DT
  EddyData$RECO_DT_SD = EProc$sTEMP$Reco_DT_SD
  
  # Modified daytime partitioning Keenan et al. (2019)
  print("start of MDT: modified daytime partitioning ... ")
  EProc = readRDS(paste0(site_name, ".rds"))
  EProc$sSetLocationInfo(LatDeg = lat, LongDeg = lon, TimeZoneHour = TimeZoneHour) 
  EProc$sTKFluxPartition() 
  saveRDS(EProc, paste0(site_name, "_MDT.rds"))
  ## save parameters and variables back to dataframe
  EddyData$GPP_MDT = EProc$sTEMP$GPP_DT
  EddyData$GPP_MDT_SD = EProc$sTEMP$GPP_DT_SD
  EddyData$Reco_MDT_SD = EProc$sTEMP$Reco_DT_SD
  EddyData$RECO_MDT = EProc$sTEMP$Reco_DT
  
  # Save the dataframe 
  fwrite(EddyData, paste0(site_name, ".csv"))
} 
 


# Load output and plot  --------------------------------------------
{
  df =  fread(paste0(site_name, ".csv")); names(df)
  
  ## Plot GPP ------------------------
  ## Plot DT vs MDT 
  ggplot(df, aes(x = GPP_DT_CUT_REF, y = GPP_MDT)) +
    geom_point(alpha = 0.6, size = 2, col = "grey") +
    geom_smooth(method = "lm", se = FALSE) +
    stat_regline_equation(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
      label.x.npc = "left",
      label.y.npc = "top",
      size = 4
    ) +
    facet_wrap(~ Year, scales = "fixed") +
    labs(
      x = expression(paste("GPP_DT_CUT_REF (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      y = expression(paste("GPP_MDT (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      title = "Comparison of GPP from DT and MDT partitioning"
    ) +
    theme_bw()
  
  
  ## Plot NT vs MDT 
  ggplot(df, aes(x = GPP_NT_CUT_REF, y = GPP_MDT)) +
    geom_point(alpha = 0.6, size = 2, col = "grey") +
    geom_smooth(method = "lm", se = FALSE) +
    stat_regline_equation(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
      label.x.npc = "left",
      label.y.npc = "top",
      size = 4
    ) +
    facet_wrap(~ Year, scales = "fixed") +
    labs(
      x = expression(paste("GPP_NT_CUT_REF (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      y = expression(paste("GPP_MDT (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      title = "Comparison of GPP from NT and MDT partitioning"
    ) +
    theme_bw()
  
 
  
  ## Plot DT, NT, MDT by Date 
  ggplot(df, aes(x = Date)) +
    geom_point(aes(y = GPP_DT_CUT_REF, color = "DT"), alpha = 0.6) +
    geom_point(aes(y = GPP_NT_CUT_REF, color = "NT"), alpha = 0.6) +
    geom_point(aes(y = GPP_MDT, color = "MDT"), alpha = 0.6) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    labs(
      x = "Year",
      y = expression(paste("GPP (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      color = "Partitioning",
      title = "Time series of GPP from different partitioning methods"
    ) +
    theme_bw() + theme(legend.position = "top")
  
  
  ## Plot Reco ------------------------
  ## Plot DT vs MDT 
  p1 = ggplot(df, aes(x = RECO_DT_CUT_REF, y = RECO_MDT)) +
    geom_point(alpha = 0.6, size = 2, col = "grey") +
    geom_smooth(method = "lm", se = FALSE) +
    stat_regline_equation(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
      label.x.npc = "left",
      label.y.npc = "top",
      size = 4
    ) +
    facet_wrap(~ Year, scales = "fixed") +
    labs(
      x = expression(paste("Reco_DT_CUT_REF (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      y = expression(paste("Reco_MDT (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      title = "Comparison of Reco from DT and MDT partitioning"
    ) +
    theme_bw() + xlim(0,35) + ylim(0,35)
  
  
  ## Plot NT vs MDT 
  p2 = ggplot(df, aes(x = GPP_NT_CUT_REF, y = GPP_MDT)) +
    geom_point(alpha = 0.6, size = 2, col = "grey") +
    geom_smooth(method = "lm", se = FALSE) +
    stat_regline_equation(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
      label.x.npc = "left",
      label.y.npc = "top",
      size = 4
    ) +
    facet_wrap(~ Year, scales = "fixed") +
    labs(
      x = expression(paste("GPP_NT_CUT_REF (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      y = expression(paste("GPP_MDT (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      title = "Comparison of GPP from NT and MDT partitioning"
    ) +
    theme_bw() +
    xlim(-30, 60) + ylim(-30, 60)
  
  
  ## Plot DT, NT, MDT by Date 
  p3= ggplot(df, aes(x = Date)) +
    geom_point(aes(y = GPP_DT_CUT_REF, color = "DT"), alpha = 0.6) +
    geom_point(aes(y = GPP_NT_CUT_REF, color = "NT"), alpha = 0.6) +
    geom_point(aes(y = GPP_MDT, color = "MDT"), alpha = 0.6) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    labs(
      x = "Year",
      y = expression(paste("GPP (", mu, "mol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
      color = "Partitioning",
      title = "Time series of GPP from different partitioning methods"
    ) +
    theme_bw() + theme(legend.position = "top")
  
  combined_plot <- ggarrange(
    
    # top row: p1 and p2
    ggarrange(
      p1, p2, ncol = 2,nrow = 1
    ),
    
    # bottom row: p3
    p3, ncol = 1, nrow = 2,
    
    # relative heights
    heights = c(1, 1)
  )
  
  ggsave(
    filename = paste0( site_name, "_figures.pdf"),
    plot = combined_plot,
    width = 15,
    height = 12
  )

}

