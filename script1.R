## ==========================================================
## Trevor-style standard REddyProc partitioning for US-NR1
##  - Daytime: Lasslop (GL) partitioning
##  - Night-time: Reichstein (MR) partitioning
##  - No light inhibition (single Rref)
## ==========================================================

## --- Packages ------------------------------------------------
library(sirad)
library(chron)
library(REddyProc)
library(plyr)
library(mlegp)

## --- User settings -------------------------------------------
site_name   <- "US-NR1"  # Niwot Ridge
work_dir    <- "F:/23 PARTIFLUX/NEE_MDT/"  # <<< change if needed
setwd(work_dir)

## FLUXNET2015 FULLSET file for US-NR1
data_file   <- "FLX_US-NR1_FLUXNET2015_FULLSET_HH_1998-2014_1-4.csv"  # <<< check name

## Directory where you stored Trevor's modified REddyProc R files
## e.g., clone his repo and point to the /resources/REddyProc/R folder
resource_dir <- "F:/23 PARTIFLUX/R/"  # <<< change to your local path

## --- Source Trevor's modified Lasslop routine ----------------
## This is the same file he uses in A1_ScriptPart_main_All_dev2args.txt
source(file.path(resource_dir, "PartitioningLasslop10.R"))

## --- Load data -----------------------------------------------
tmp <- read.table(data_file, header = TRUE, sep = ",")

## Replace typical FLUXNET missing values
tmp[tmp <= -9990] <- NA  # conservative; adjust if you prefer

## --- Extract needed FLUXNET variables ------------------------
TIMESTAMP_START             <- tmp$TIMESTAMP_START
TIMESTAMP_END               <- tmp$TIMESTAMP_END
VPD_F                       <- tmp$VPD_F
VPD_F_QC                    <- tmp$VPD_F_QC
TA_F                        <- tmp$TA_F
TA_F_QC                     <- tmp$TA_F_QC
TS_F                        <- tmp$TS_F_MDS_1
TS_F_QC                     <- tmp$TS_F_MDS_1_QC
NIGHT                       <- tmp$NIGHT
SW_IN_F                     <- tmp$SW_IN_F
SW_IN_F_QC                  <- tmp$SW_IN_F_QC
SW_IN_F_MDS                 <- tmp$SW_IN_F_MDS
SW_IN_F_MDS_QC              <- tmp$SW_IN_F_MDS_QC
NEE_VUT_USTAR50             <- tmp$NEE_VUT_USTAR50
NEE_VUT_USTAR50_QC          <- tmp$NEE_VUT_USTAR50_QC
NEE_VUT_USTAR50_RANDUNC     <- tmp$NEE_VUT_USTAR50_RANDUNC
SW_IN_POT                   <- tmp$SW_IN_POT

## --- Fill missing NEE uncertainty (RANDUNC) ------------------
## Same logic as Trevor's original code: separate fits for NEE >= 0 and NEE < 0

## For flux >= 0
tmpx <- NEE_VUT_USTAR50
tmpy <- NEE_VUT_USTAR50_RANDUNC
tmpy[tmpy <= -2000] <- NA   # remove -9999 etc.
indX <- tmpx >= 0
myline.fit <- lm(tmpy[indX] ~ tmpx[indX])
a <- myline.fit$coefficients[1]
b <- myline.fit$coefficients[2]
cat("Uncertainty parameters (NEE >= 0):", a, b, "\n")
NEE_VUT_USTAR50_RANDUNC[NEE_VUT_USTAR50 >= 0] <-
  a + NEE_VUT_USTAR50[NEE_VUT_USTAR50 >= 0] * b

## For flux < 0
tmpx <- NEE_VUT_USTAR50
tmpy <- tmp$NEE_VUT_USTAR50_RANDUNC
tmpy[tmpy <= -2000] <- NA
indX <- tmpx < 0
myline.fit <- lm(tmpy[indX] ~ tmpx[indX])
a <- myline.fit$coefficients[1]
b <- myline.fit$coefficients[2]
cat("Uncertainty parameters (NEE < 0):", a, b, "\n")
NEE_VUT_USTAR50_RANDUNC[NEE_VUT_USTAR50 < 0] <-
  a - abs(NEE_VUT_USTAR50[NEE_VUT_USTAR50 < 0]) * b

## --- Create DAY variable -------------------------------------
DAY <- 1 - NIGHT

## --- Build time variables from TIMESTAMP_START ---------------
timeStampChar <- as.character(TIMESTAMP_START)

Data.F <- data.frame(
  year  = as.integer(substr(timeStampChar, 1, 4)),
  month = as.integer(substr(timeStampChar, 5, 6)),
  day   = as.integer(substr(timeStampChar, 7, 8)),
  hour  = as.integer(substr(timeStampChar, 9,10)) +
    as.integer(substr(timeStampChar,11,12)) / 60
)

## Remove rows with invalid years (if any)
Data.F <- Data.F[!is.na(Data.F$year), ]

## ==========================================================
## Trevor-style standard REddyProc partitioning for US-NR1
##  - Daytime: Lasslop (GL) partitioning
##  - Night-time: Reichstein (MR) partitioning
##  - No light inhibition (single Rref)
## ==========================================================

## --- Packages ------------------------------------------------
library(sirad)
library(chron)
library(REddyProc)
library(plyr)
library(mlegp)

## --- User settings -------------------------------------------
site_name   <- "US-NR1"  # Niwot Ridge
work_dir    <- "F:/23 PARTIFLUX/NEE_MDT/"  # <<< change if needed
setwd(work_dir)

## FLUXNET2015 FULLSET file for US-NR1
data_file   <- "FLX_US-NR1_FLUXNET2015_FULLSET_HH_1998-2014_1-4.csv"  # <<< check name

## Directory where you stored Trevor's modified REddyProc R files
## e.g., clone his repo and point to the /resources/REddyProc/R folder
resource_dir <- "F:/23 PARTIFLUX/R"  # <<< change to your local path

## --- Source Trevor's modified Lasslop routine ----------------
## This is the same file he uses in A1_ScriptPart_main_All_dev2args.txt
source(file.path(resource_dir, "PartitioningLasslop10.R"))

## --- Load data -----------------------------------------------
tmp <- read.table(data_file, header = TRUE, sep = ",")

## Replace typical FLUXNET missing values
tmp[tmp <= -9990] <- NA  # conservative; adjust if you prefer

## --- Extract needed FLUXNET variables ------------------------
TIMESTAMP_START             <- tmp$TIMESTAMP_START
TIMESTAMP_END               <- tmp$TIMESTAMP_END
VPD_F                       <- tmp$VPD_F
VPD_F_QC                    <- tmp$VPD_F_QC
TA_F                        <- tmp$TA_F
TA_F_QC                     <- tmp$TA_F_QC
TS_F                        <- tmp$TS_F_MDS_1
TS_F_QC                     <- tmp$TS_F_MDS_1_QC
NIGHT                       <- tmp$NIGHT
SW_IN_F                     <- tmp$SW_IN_F
SW_IN_F_QC                  <- tmp$SW_IN_F_QC
SW_IN_F_MDS                 <- tmp$SW_IN_F_MDS
SW_IN_F_MDS_QC              <- tmp$SW_IN_F_MDS_QC
NEE_VUT_USTAR50             <- tmp$NEE_VUT_USTAR50
NEE_VUT_USTAR50_QC          <- tmp$NEE_VUT_USTAR50_QC
NEE_VUT_USTAR50_RANDUNC     <- tmp$NEE_VUT_USTAR50_RANDUNC
SW_IN_POT                   <- tmp$SW_IN_POT

## --- Fill missing NEE uncertainty (RANDUNC) ------------------
## Same logic as Trevor's original code: separate fits for NEE >= 0 and NEE < 0

## For flux >= 0
tmpx <- NEE_VUT_USTAR50
tmpy <- NEE_VUT_USTAR50_RANDUNC
tmpy[tmpy <= -2000] <- NA   # remove -9999 etc.
indX <- tmpx >= 0
myline.fit <- lm(tmpy[indX] ~ tmpx[indX])
a <- myline.fit$coefficients[1]
b <- myline.fit$coefficients[2]
cat("Uncertainty parameters (NEE >= 0):", a, b, "\n")
NEE_VUT_USTAR50_RANDUNC[NEE_VUT_USTAR50 >= 0] <-
  a + NEE_VUT_USTAR50[NEE_VUT_USTAR50 >= 0] * b

## For flux < 0
tmpx <- NEE_VUT_USTAR50
tmpy <- tmp$NEE_VUT_USTAR50_RANDUNC
tmpy[tmpy <= -2000] <- NA
indX <- tmpx < 0
myline.fit <- lm(tmpy[indX] ~ tmpx[indX])
a <- myline.fit$coefficients[1]
b <- myline.fit$coefficients[2]
cat("Uncertainty parameters (NEE < 0):", a, b, "\n")
NEE_VUT_USTAR50_RANDUNC[NEE_VUT_USTAR50 < 0] <-
  a - abs(NEE_VUT_USTAR50[NEE_VUT_USTAR50 < 0]) * b

## --- Create DAY variable -------------------------------------
DAY <- 1 - NIGHT

## --- Build time variables from TIMESTAMP_START ---------------
timeStampChar <- as.character(TIMESTAMP_START)

Data.F <- data.frame(
  year  = as.integer(substr(timeStampChar, 1, 4)),
  month = as.integer(substr(timeStampChar, 5, 6)),
  day   = as.integer(substr(timeStampChar, 7, 8)),
  hour  = as.integer(substr(timeStampChar, 9,10)) +
    as.integer(substr(timeStampChar,11,12)) / 60
)

## Remove rows with invalid years (if any)
Data.F <- Data.F[!is.na(Data.F$year), ]

## Julian day within each year
Data.F$julian <-
  julian(Data.F$month, Data.F$day, Data.F$year) -
  julian(1, 1, unique(na.omit(Data.F$year))[1])

## --- Append flux and met data -------------------------------
Data.F <- cbind(
  Data.F,
  data.frame(
    NEE      = NEE_VUT_USTAR50,
    NEE_QC   = NEE_VUT_USTAR50_QC,
    NEE_SE   = NEE_VUT_USTAR50_RANDUNC,
    SW_IN_F  = SW_IN_F,
    SW_IN_F_QC = SW_IN_F_QC,
    SW_IN_POT  = SW_IN_POT,
    TA_F     = TA_F,
    TA_F_QC  = TA_F_QC,
    VPD_F    = VPD_F,
    VPD_F_QC = VPD_F_QC,
    NIGHT    = NIGHT,
    DAY      = DAY
  )
)

## --- Convert to POSIX time for REddyProc ---------------------
dfall_posix <- fConvertTimeToPosix(
  Data.F,
  'YMDH',
  Year.s  = 'year',
  Month.s = 'month',
  Day.s   = 'day',
  Hour.s  = 'hour'
)

hourz        <- unique(dfall_posix$hour)
nRecInDay.i  <- length(hourz)

## --- Create output directory --------------------------------
out_dir <- file.path(work_dir, "data_REddyProcOutput")
if (!dir.exists(out_dir)) dir.create(out_dir)

## ============================================================
## 1. Night-time partitioning (MR) – standard (no inhibition)
## ============================================================
cat("##################################################\n")
cat("Running night-time partitioning for", site_name, "\n")

EddyProc.C <- sEddyProc(
  site_name,
  dfall_posix,
  c('NEE','NEE_QC','SW_IN_F','TA_F','TA_F_QC','VPD_F'),
  ColPOSIXTime.s = 'DateTime',
  DTS.n          = nRecInDay.i
)

## Location: US-NR1 (Niwot Ridge)
EddyProc.C$sSetLocationInfo(
  LatDeg = 40.0329, LongDeg =-105.5464, TimeZoneHour = -7
)

EddyProc.C$sMRFluxPartition(
  FluxVar.s      = 'NEE',
  QFFluxVar.s    = 'NEE_QC',
  QFFluxValue.n  = 0,
  TempVar.s      = 'TA_F',
  QFTempVar.s    = 'TA_F_QC',
  QFTempValue.n  = 0,
  RadVar.s       = 'SW_IN_F'
)

FilledEddyDataNT.F <- EddyProc.C$sExportResults()

## ============================================================
## 2. Day-time partitioning (GL) – standard (no inhibition)
## ============================================================
cat("##################################################\n")
cat("Running daytime partitioning for", site_name, "\n")

yearsUnique <- unique(Data.F$year)
df.REddy    <- NULL

for (ii in yearsUnique) {
  cat("  Year:", ii, "\n")
  indX        <- Data.F$year == ii
  Data.FcYear <- Data.F[indX, ]
  tmpPart     <- NULL
  
  try(
    tmpPart <- partitionNEEGL(
      Data.FcYear,
      NEEVar.s      = "NEE",
      QFNEEVar.s    = "NEE_QC",
      QFNEEValue.n  = 0,
      NEESdVar.s    = "NEE_SE",
      TempVar.s     = "TA_F",
      QFTempVar.s   = "TA_F_QC",
      VPDVar.s      = "VPD_F",
      QFVPDVar.s    = "VPD_F_QC",
      RadVar.s      = "SW_IN_F",
      PotRadVar.s   = "DAY",
      Suffix.s      = "",
      nRecInDay     = nRecInDay.i,
      controlGLPart = partGLControl(
        nBootUncertainty                = 10L,
        isAssociateParmsToMeanOfValids  = FALSE,
        isLasslopPriorsApplied          = TRUE,
        isBoundLowerNEEUncertainty      = FALSE,
        smoothTempSensEstimateAcrossTime = TRUE
      )
    ),
    silent = TRUE
  )
  
  ## If GL fitting failed, create an empty structure so rbind works
  if (is.null(tmpPart)) {
    RecoDTVar.s    <- "Reco_DT"
    GPPDTVar.s     <- "GPP_DT"
    RecoDTSdVar.s  <- paste0(RecoDTVar.s, "_SD")
    GPPDTSdVar.s   <- paste0(GPPDTVar.s, "_SD")
    
    emptyTmp <- data.frame(
      FP_VARnight   = rep(NA_real_, nrow(Data.FcYear)),
      FP_VARday     = NA_real_,
      NEW_FP_Temp   = NA_real_,
      NEW_FP_VPD    = NA_real_,
      FP_R_refNight = NA_real_,
      FP_R_ref      = NA_real_,
      FP_E0         = NA_real_,
      FP_alpha      = NA_real_,
      FP_beta       = NA_real_,
      FP_k          = NA_real_,
      FP_qc         = NA_integer_,
      FP_dRecPar    = NA_integer_,
      Reco_DT       = NA_real_,
      GPP_DT        = NA_real_,
      Reco_DT_SD    = NA_real_,
      GPP_DT_SD     = NA_real_
    )
    attr(emptyTmp$FP_VARnight, 'varnames') <- 'NEE.NEE_QC_0_night'
    attr(emptyTmp$FP_VARday,   'varnames') <- 'NEE.NEE_QC_0_day'
    attr(emptyTmp[[RecoDTVar.s]],   'varnames') <- RecoDTVar.s
    attr(emptyTmp[[GPPDTVar.s]],    'varnames') <- GPPDTVar.s
    attr(emptyTmp[[RecoDTSdVar.s]], 'varnames') <- RecoDTSdVar.s
    attr(emptyTmp[[GPPDTSdVar.s]],  'varnames') <- GPPDTSdVar.s
    
    tmpPart <- emptyTmp
  }
  
  if (is.null(df.REddy)) {
    df.REddy <- tmpPart
  } else {
    df.REddy <- rbind(df.REddy, tmpPart)
  }
}

## --- Replace NA with -9999 and write outputs -----------------
Data.F[is.na(Data.F)]             <- -9999
FilledEddyDataNT.F[is.na(FilledEddyDataNT.F)] <- -9999
df.REddy[is.na(df.REddy)]         <- -9999

## Night-time partitioning output
write.table(
  FilledEddyDataNT.F,
  file      = file.path(out_dir, paste0(site_name, "REddyProc_NT_VUT_USTAR50.csv")),
  append    = FALSE,
  sep       = ",",
  row.names = FALSE
)

## Daytime GPP/Reco
GPP_DT  <- cbind(Data.F$year, df.REddy$GPP_DT)
Reco_DT <- cbind(df.REddy$Reco_DT)

write.table(
  GPP_DT,
  file      = file.path(out_dir, paste0(site_name, "_GPP_DT_VUT_USTAR50.csv")),
  append    = FALSE,
  sep       = ",",
  row.names = FALSE
)

write.table(
  Reco_DT,
  file      = file.path(out_dir, paste0(site_name, "_Reco_DT_VUT_USTAR50.csv")),
  append    = FALSE,
  sep       = ",",
  row.names = FALSE
)

## Parameters from GL fits
parameters <- cbind(
  Data.F$year, Data.F$month, Data.F$julian, Data.F$hour, Data.F$DAY,
  df.REddy$FP_R_refNight, df.REddy$FP_R_ref, df.REddy$FP_E0,
  df.REddy$FP_alpha, df.REddy$FP_beta, df.REddy$FP_k, df.REddy$FP_qc
)
colnames(parameters) <- c(
  "Year", "Month", "Day", "Hour", "DAYNIGHT",
  "R_night", "R_ref", "E0", "alpha", "beta", "k", "qc"
)

write.table(
  parameters,
  file      = file.path(out_dir, paste0(site_name, "REddyProc_parameters.csv")),
  append    = FALSE,
  sep       = ",",
  col.names = NA
)

cat("Finished standard Trevor-style partitioning for", site_name, "\n")


