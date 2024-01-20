# Collate annotations for DCLDE 2026 data

rm(list =ls())
library(lubridate)
library(dplyr)


# Data to pretty
# 
# 1) Ocean Networks Canada 
# 2) Viers
# 3) DFO Pilkington
# 4) DFO Yerk
# 5) SMRU

############################################################################
# Final output column names

colOut = c('Soundfile','Dep','LowFreqHz','HighFreqHz','FileEndSec', 'UTC',
           'FileBeginSec','ClassSpecies','KW','KW_certain','Ecotype', 'Provider')

ClassSpeciesList = c('KW', 'HW', 'AB', 'UndBio')

# For the class species, options are: KW, HW, AB, and UndBio
# Killer whale, humpback whale, abiotic, and unknown Bio which includes 
# Other dolphin acoustically active species ranging from fin whales, to 
# white beaked dolphins to seagulls

###############################################################################
# 1) ONC- data from globus


# Jasper, April, and Jenn have all annotated thse files. There is some overla
JasperAnno = read.csv('E:\\DCLDE2026/ONC/Meta/BarkleyCanyonAnnotations_Public_Final.csv')
AprJenAnno = read.csv('E:\\DCLDE2026/ONC/Meta/jen_onc_barkley-canyon_annot.csv')


# Add bool for KWs
JasperAnno <- JasperAnno %>%
  mutate(KW = as.numeric(grepl("Oo", Species)),
         KW_certain = as.numeric(grepl("Oo", Species)))



# Add Ecotype 
JasperAnno$Ecotype[JasperAnno$Ecotype == 'SRKW'] ='SRKW'
JasperAnno$Ecotype[JasperAnno$Comments == 'BKW'] ='BKW'
JasperAnno$Ecotype[JasperAnno$Comments == 'OKW'] ='OKW'

colnames(JasperAnno)[c(2,3,4,5,6)]<-c('FileBeginSec','FileEndSec', 
                                        'HighFreqHz', 'LowFreqHz','ClassSpecies')
JasperAnno$Dep='BarkLeyCanyon'

# Get time
JasperAnno <- JasperAnno %>%
  mutate(filename = as.character(Soundfile),  # Make sure the column is treated as character
         UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z).*", "\\1", filename), 
                          format = "%Y%m%dT%H%M%S.%OSZ"))
JasperAnno$FileBeginSec= as.numeric(JasperAnno$FileBeginSec)
JasperAnno$UTC = JasperAnno$UTC+ seconds(as.numeric(JasperAnno$FileBeginSec))

JasperAnno$ClassSpecies[JasperAnno$ClassSpecies=='Oo']= 'KW'
JasperAnno$ClassSpecies[JasperAnno$ClassSpecies=='Mn']= 'HW'
JasperAnno$ClassSpecies[!JasperAnno$ClassSpecies %in% ClassSpeciesList] = 'UndBio'
JasperAnno$Provider= 'ONC'


# Add bool for KWs
AprJenAnno <- AprJenAnno %>%
  mutate(KW =         as.numeric(grepl("KW", sound_id_species)),
         KW_certain = !as.numeric(grepl("\\?",  sound_id_species)),
         UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z).*", "\\1", filename), 
                          format = "%Y%m%dT%H%M%S.%OSZ"))


# Add Ecotype 
AprJenAnno$Ecotype = NA
AprJenAnno$Ecotype[AprJenAnno$Comments == 'KWSR'] ='SRKW'
AprJenAnno$Ecotype[AprJenAnno$Comments == 'KWT'] ='BKW'
AprJenAnno$Ecotype[AprJenAnno$Comments == 'KWT?'] ='BKW'


colnames(AprJenAnno)[c(1,2,3,4,5, 6)]<-c('LowFreqHz','HighFreqHz','FileEndSec',
                                        'FileBeginSec','Soundfile','ClassSpecies')

AprJenAnno$UTC = AprJenAnno$UTC+ seconds(as.numeric(AprJenAnno$FileBeginSec))

AprJenAnno$Dep='BarkLeyCanyon'
# Set humpbacks

# Undetermined biotic sounds
AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 
                          c("KW?/PWSD?",  "Echolocation",
                            "Echolocation?", "KW/PWSD?","PWSD?",
                            "PWSD/KW?", "PWSD", "PWSD?", "KW/PWSD?",
                            "Odontocete", "Odontocete?")] = 'UndBio'

# Probably abitoic sounds
AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 
                          c("Unknown", "Vessel Noise","Vessel noise",
                            "Unknown ", "Vessel Noise?", "unknown",
                            "Mooring", "Vessel noise?", "Uknown", "")]= 'Ab'
AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 'KW?'] = 'KW'
AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 'HW?'] = 'HW'


AprJenAnno$ClassSpecies[AprJenAnno$Comments == 'HW'] ='HW'
AprJenAnno$Provider = 'ONC_HALLO' # I think HALLO paid for JASCO's annotations


ONC_anno = rbind(AprJenAnno[, c(colOut)], JasperAnno[,  c(colOut)])
rm(list= c('JasperAnno', 'AprJenAnno'))


############################################################################
# DFO Pilkington
############################################################################

# No seconds in UTC
# 
# PilkAnno1 = read.csv('D:\\DCLDE 2024/DFO_Pilkington/annotations/annot_H50bjRcb_SM_det.csv')
# PilkAnno2 = read.csv('D:\\DCLDE 2024/DFO_Pilkington/annotations/annot_KkHK0R2F_SM_det.csv')

PilkAnno1 = read.csv('E:\\DCLDE2026/DFO_Pilkington/annotations/annot_H50bjRcb_SM_det.csv')
PilkAnno2 = read.csv('E:\\DCLDE2026/DFO_Pilkington/annotations/annot_KkHK0R2F_SM_det.csv')


PilkAnno1$Dep='DFO_01'
PilkAnno2$Dep='DFO_02'

PilkAnno = rbind(PilkAnno1, PilkAnno2)

table(PilkAnno$sound_id_species)

# Add bool for KWs
PilkAnno <- PilkAnno %>%
  mutate(KW = as.numeric(grepl("KW", sound_id_species)),
         KW_certain = as.numeric(grepl("KW?", sound_id_species)))

# Add Ecotype- note uncertain ecotypes getting their own guess
PilkAnno$Ecotype = as.factor(PilkAnno$kw_ecotype)
levels(PilkAnno$Ecotype)<-c(NA, 'NRKW', 'NRKW', 'OKW', 'OKW', 'SRKW', 'SRKW',
                            'BKW', 'BKW', NA)


PilkAnno$Soundfile = PilkAnno$filename
PilkAnno$FileBeginSec = PilkAnno$start
PilkAnno$FileEndSec = PilkAnno$end
PilkAnno$LowFreqHz = PilkAnno$freq_min
PilkAnno$HighFreqHz = PilkAnno$freq_max
PilkAnno$UTC = as.POSIXct(PilkAnno$utc, format="%Y-%m-%d %H:%M:%OS", tz = 'UTC')+
  seconds(as.numeric(PilkAnno$utc_ms)/1000)

PilkAnno$Provider = 'DFO_Pilkington'
PilkAnno$ClassSpecies = PilkAnno$sound_id_species

# Clean up the abiotic counds
PilkAnno$ClassSpecies[
  PilkAnno$ClassSpecies %in% c('Vessel Noise', 'Unknown', '', 'Mooring Noise', 
                               'Chain?', 'ADCP', 'Anchor Noise', 'Clang',
                               'Vessel Noise?', 'Chain','No sound data',
                               "Blast/Breach", "Fishing Gear", "Breach",
                               'Rubbing')] = 'AB'

PilkAnno$ClassSpecies[!PilkAnno$ClassSpecies %in% ClassSpeciesList] = 'UndBio'
PilkAnno$ClassSpecies[!is.na(PilkAnno$Ecotype)] = 'KW'

DFO_Pilk = PilkAnno[, c(colOut)]

rm(list= c('PilkAnno1', 'PilkAnno2', 'PilkAnno'))


  


##############################################################################
# JASCO- Malahat

# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'E:/DCLDE2026/JASCO/annotations/', 
                        pattern = 'annot_Malahat', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
JASCO_malahat <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  if (nrow(data) > 0) {
    data$Dep <- basename(file)  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

JASCO_malahat <- JASCO_malahat %>%
  mutate(
    KW = as.numeric(grepl("KW", sound_id_species)),
    KW_certain = !as.numeric(grepl("\\?", sound_id_species)),
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S")
  )

JASCO_malahat$UTC = JASCO_malahat$UTC+ 
  seconds(as.numeric(JASCO_malahat$start))


# Add Ecotype 
JASCO_malahat$Ecotype = NA
JASCO_malahat$Ecotype[JASCO_malahat$kw_ecotype == 'SRKW'] ='SRKW'
JASCO_malahat$Ecotype[JASCO_malahat$kw_ecotype == 'SRKW?'] ='BKW'
JASCO_malahat$Ecotype[JASCO_malahat$kw_ecotype == 'KWT?'] ='BKW'
JASCO_malahat$Ecotype[JASCO_malahat$kw_ecotype == 'TKW?'] ='BKW'



colnames(JASCO_malahat)[c(5,6,3,4,1,8)]<-c('LowFreqHz','HighFreqHz','FileEndSec',
                                         'FileBeginSec','Soundfile','ClassSpecies')

JASCO_malahat$ClassSpecies[JASCO_malahat$ClassSpecies == 'KW?'] ='KW'
JASCO_malahat$ClassSpecies[JASCO_malahat$ClassSpecies %in% 
                             c("VESSEL", "VESSEL/HW?",  "VESSEL CHAIN", 
                               "CHAIN","UNK", "HW/KW?", "HW?")]= 'Ab'

JASCO_malahat$ClassSpecies[JASCO_malahat$ClassSpecies %in% 
                             c("HW/KW", "SEAGULL?")]= 'UndBio'

JASCO_malahat$Provider = 'JASCO_Malahat'


JASCO_malahat = JASCO_malahat[,c(colOut)]



############################################################################
# DFO Yerk
############################################################################

# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'E:\\DCLDE2026\\DFO_Yerk\\Audio_and_labels\\Annotations', 
                        pattern = '*csv', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
DFO_Yerk <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  if (nrow(data) > 0) {
    data$Dep <- basename(file)  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

# Fucking PAMGuard
DFO_Yerk = DFO_Yerk[DFO_Yerk$duration>0,]
DFO_Yerk = DFO_Yerk[!duplicated(DFO_Yerk),]

# Standardize formatting
DFO_Yerk$Soundfile = DFO_Yerk$soundfile
DFO_Yerk$LowFreqHz = DFO_Yerk$lf
DFO_Yerk$HighFreqHz = DFO_Yerk$hf
DFO_Yerk$UTC = as.POSIXct( DFO_Yerk$date_time_utc[1],  
                           format="%Y-%m-%d %H:%M:%OS", tz = 'UTC')

DFO_Yerk$FileBeginSec = DFO_Yerk$elapsed_time_seconds
DFO_Yerk$FileEndSec = DFO_Yerk$FileBeginSec+DFO_Yerk$duration/1000

DFO_Yerk$Ecotype = as.factor(DFO_Yerk$species)
levels(DFO_Yerk$Ecotype)<-c(NA, 'NRKW', 'SRKW', 'BKW', NA, NA)

DFO_Yerk$ClassSpecies = as.factor(DFO_Yerk$species)
levels(DFO_Yerk$ClassSpecies)<-c('HW', 'KW', 'KW', 'KW', 'UndBio', 'AB')

DFO_Yerk$KW_certain = ifelse(DFO_Yerk$ClassSpecies== 'KW', 1,0)
DFO_Yerk$Provider = 'DFO_Yerk'

# Apparently no uncertain calls
DFO_Yerk$KW = DFO_Yerk$KW_certain

DFO_Yerk = DFO_Yerk[, colOut]




###############################################################################
# Viers Data

Viersanno = read.csv('E:\\DCLDE2026\\OrcaSound\\Meta/ModifiedAnnotations.csv')
Viersanno$start_time_s = as.numeric(Viersanno$start_time_s) 

Viersanno$DateTime <- with(Viersanno, paste(date, pst_or_master_tape_identifier))

# Exclude round 1
Viersanno = subset(Viersanno, dataset != 'podcast_round1')

Viersanno$UTC <- 
  with(Viersanno, as.POSIXct(DateTime, tz = "America/Los_Angeles", 
                             format = "%m/%d/%Y %H:%M:%S"))

# Problem children
problemData = unique(Viersanno$dataset[is.na(Viersanno$UTC)])
problemIdx = which(is.na(Viersanno$UTC))
  
Viersanno$UTC[problemIdx] <- 
  as.POSIXct(Viersanno$DateTime[problemIdx],
             tz = "America/Los_Angeles", 
             format = "%Y_%m_%d %H:%M:%S")


Viersanno$ClassSpecies = Viersanno$Species
Viersanno$ClassSpecies[Viersanno$ClassSpecies == 'Oo'] = 'KW'
Viersanno$ClassSpecies[Viersanno$ClassSpecies == 'Noise'] = 'Ab'

Viersanno$Provider = 'OrcaSound'
Viersanno$KW = ifelse(Viersanno$ClassSpecies== 'KW',1,0)
Viersanno$KW_certain = ifelse(Viersanno$ClassSpecies== 'KW',1,0)
Viersanno$Dep = Viersanno$location
Viersanno$Soundfile = Viersanno$wav_filename
Viersanno$LowFreqHz =NaN
Viersanno$HighFreqHz =NaN

Viersanno$FileBeginSec = Viersanno$start_time_s
Viersanno$FileEndSec = Viersanno$start_time_s+Viersanno$duration_s

Viersanno = Viersanno[,c(colOut)]

rm(list =c('problemData', 'problemIdx'))

###########################################################################
# SIMRES
##########################################################################



# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'E:\\DCLDE2026\\SIMRES/annotations/', 
                        pattern = '*txt', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
SIMRES <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.table(file, header = TRUE, sep = '\t')
  if (nrow(data) > 0) {
    data$Dep <- basename(file)  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

SIMRES$ClassSpecies = as.factor(SIMRES$Sound.ID.Species)
levels(SIMRES$ClassSpecies)<-c(NA, 'UndBio', 'KW', 'KW')
SIMRES$KW_certain =  as.numeric(grepl("KW", SIMRES$Sound.ID.Species))

#check SIMRES files in UTC
SIMRES$UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z)_.*", "\\1", 
                            SIMRES$Begin.File[1]),  
                        format = "%Y%m%dT%H%M%S.%OSZ",
                        tz = 'UTC')+seconds(SIMRES$File.Offset..s.)

SIMRES$Ecotype = as.factor(SIMRES$KW.Ecotype)
levels(SIMRES$Ecotype)<- c(NA, 'SRKW', 'SRKW')
SIMRES$KW = ifelse(SIMRES$ClassSpecies == 'KW', 1,0)

SIMRES$Soundfile = SIMRES$Begin.File
SIMRES$Dep = 'Tekteksen'
SIMRES$LowFreqHz = SIMRES$Low.Freq..Hz.
SIMRES$HighFreqHz = SIMRES$Low.Freq..Hz.
SIMRES$FileBeginSec = SIMRES$File.Offset..s.
SIMRES$FileEndSec = SIMRES$File.Offset..s.+SIMRES$Delta.Time..s.
SIMRES$Provider = 'SIMRES'

SIMRES= SIMRES[, colOut]

#############################################################################

allAnno = rbind(DFO_Pilk, ONC_anno, JASCO_malahat, Viersanno, DFO_Yerk, SIMRES)

# overall annotations
table(allAnno$ClassSpecies)

# Pull out the killer whale data
KW_data = subset(allAnno, KW ==1)

table(KW_data$Ecotype)


############################################################################
# Ensure all files are present with their respective annoations


# DFO Pilkington
# Summary infomation
audioDir = 'E:/DCLDE2026/DFO_Pilkington/'
wav_files <- list.files(path = audioDir, pattern = "\\.flac$", recursive = TRUE,
                        full.names = FALSE)
num_wav_files <- length(wav_files)
print(num_wav_files)

DFO_anno$Soundfile[1] %in% wav_files



