# packages --------------------------------------------
{
  library(data.table)
  library(bigleaf)
  library(ggplot2)
  library(REddyProc)
  library(tidyverse)
}

# load data --------------------------------------------
{
  # Change to your site name and input data
  site_name = "US-NR1" # change to your own
  # work_dir = "/scratch/yl763/PARTIFLUX"; setwd(work_dir) # change to your own
  work_dir = "F:/23 PARTIFLUX/NEE_MDT/"; setwd(work_dir) # change to your own
  df = fread("FLX_US-NR1_FLUXNET2015_FULLSET_HH_1998-2014_1-4.csv") # change to your own
  df[df == -9999] <- NA 
  
  # down-select variables
  EddyData = data.frame(
    TIMESTAMP_START = df$TIMESTAMP_START,
    NEE = df$NEE_VUT_REF,
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
  # EddyData = EddyData[EddyData$Year > 2009, ] # subset data to test
}

# Initialize EProc --------------------------------------------
{
  EddyData$Year <- as.numeric(EddyData$Year)
  EddyData$Hour <- as.numeric(EddyData$Hour)
  EddyData$DoY <- as.numeric(EddyData$DoY)
  EddyDataWithPosix <- fConvertTimeToPosix(EddyData, 'YDH', Year = 'Year', Day = 'DoY', Hour  = 'Hour') 
  EProc <- sEddyProc$new(site_name, EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))
}
# Partitioning NEE into Reco and GPP --------------------------------------------
{
  # FLUXNET data is almost gap-free, but you will still gapfill the meteorological variables to get the quality flags 
  EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)
  EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)     
  EProc$sMDSGapFill('NEE')
  EProc$sMDSGapFill('Rg') # required by daytime partitioning 
  print("MDS gapfilled meterological variables!")
  
  # Change to your site-specific information carefully here!!!
  ## TimeZoneHour: time zone offset from UTC (Coordinated Universal Time), without daytime saving. 
  ## (You can ask ChatGPT for the TimeZoneHour giving coordinates for a study site)
  EProc$sSetLocationInfo(LatDeg = 40.0329, LongDeg =-105.5464, TimeZoneHour = -7) 
  # Save Eproc object
  saveRDS(EProc, paste0(site_name, "_MDT.rds"))
  
  # Modified daytime partitioning 
  print("start of daytime partitioning ... ")
  # Modified daytime-based Flux partitioning after Keenan et al. (2019)
  EProc$sTKFluxPartition() 
  print("end of MDT partitioning... ")
  
  # Save the output variables to original dataframe
  EddyData$GPP_MDT = EProc$sTEMP$GPP_DT # MDT stands for modified daytime
  EddyData$RECO_MDT = EProc$sTEMP$Reco_DT
  
  # Save the dataframe 
  fwrite(EddyData, paste0(site_name, "_MDT.csv"))
  
  # Save Eproc object
  saveRDS(EProc, paste0(site_name, "_MDT.rds"))
}

# Load output and plot  --------------------------------------------
# (please think about if you want to make the figures in different ways) ... 
{
  df =  fread(paste0(site_name, "_MDT.csv")); names(df)
  # Eproc = readRDS(paste0(site_name, "_MDT.rds"))
  
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
  ggsave(paste0("Figure_",site_name, "_GPP_DT_vs_MDT.png"), width = 8, height = 6)
  
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
  
  ggsave(paste0("Figure_", site_name, "_GPP_NT_vs_MDT.png"), width = 8, height = 6)
  
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
  
 
  ggsave(paste0("Figure_", site_name, "_GPP_time_series.png"), width = 8, height = 4)
  
  ## Plot Reco ------------------------
  ## Plot DT vs MDT 
  ggplot(df, aes(x = RECO_DT_CUT_REF, y = RECO_MDT)) +
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
      title = "Comparison of GPP from DT and MDT partitioning"
    ) +
    theme_bw()
  namenameggsave(paste0("Figure_",site_name, "_Reco_DT_vs_MDT.png"), width = 8, height = 6)
  
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
  
  ggsave(paste0("Figure_", site_name, "_GPP_NT_vs_MDT.png"), width = 8, height = 6)
  
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
  
  
  ggsave(paste0("Figure_", site_name, "_GPP_time_series.png"), width = 8, height = 4)
  

  
}
