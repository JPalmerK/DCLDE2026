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
           'FileBeginSec','ClassSpecies','KW','KW_certain','Ecotype', 'Provider',
           'AnnotationLevel', 'FilePath')

ClassSpeciesList = c('KW', 'HW', 'AB', 'UndBio')
AnnotationLevelList = c('File', 'Detection', 'Call')
# For the class species, options are: KW, HW, AB, and UndBio
# Killer whale, humpback whale, abiotic, and unknown Bio which includes 
# Other dolphin acoustically active species ranging from fin whales, to 
# white beaked dolphins to seagulls

###############################################################################
# 1) ONC- data from globus


# Jasper, April, and Jenn have all annotated thse files. There is some overla
JasperAnno = read.csv('E:\\DCLDE2026/ONC/Annotations/BarkleyCanyonAnnotations_Public_Final.csv')
AprJenAnno = read.csv('E:\\DCLDE2026/ONC/Annotations/jen_onc_barkley-canyon_annot.csv')


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
JasperAnno$AnnotationLevel = 'Call'

# Add the filepath and check that files are ok
# Dayfolder
dayFolderPath = 'E:\\DCLDE2026\\ONC\\Audio\\BarkleyCanyon'

JasperAnno$FilePath = file.path(dayFolderPath, format(JasperAnno$UTC, "%Y%m%d"),
                                JasperAnno$Soundfile)

JasperAnno$FileOk  = file.exists(JasperAnno$FilePath) 



# Add bool for KWs
AprJenAnno <- AprJenAnno %>%
  mutate(KW =         as.numeric(grepl("KW", sound_id_species)),
         KW_certain = !as.numeric(grepl("\\?",  sound_id_species)),
         UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z).*", "\\1", filename), 
                          format = "%Y%m%dT%H%M%S.%OSZ"))


# Add Ecotype 
AprJenAnno$Ecotype = NA
AprJenAnno$Ecotype[AprJenAnno$kw_ecotype == 'KWSR'] ='SRKW'
AprJenAnno$Ecotype[AprJenAnno$kw_ecotype == 'KWT'] ='BKW'
AprJenAnno$Ecotype[AprJenAnno$kw_ecotype == 'KWT?'] ='BKW'


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
                            "Mooring", "Vessel noise?", "Uknown", "")]= 'AB'
AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 'KW?'] = 'KW'
AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 'HW?'] = 'HW'


AprJenAnno$ClassSpecies[AprJenAnno$Comments == 'HW'] ='HW'
AprJenAnno$Provider = 'ONC_HALLO' # I think HALLO paid for JASCO's annotations

AprJenAnno$AnnotationLevel = 'Call'


dayFolderPath = 'E:\\DCLDE2026\\ONC\\Audio\\BarkleyCanyon'

AprJenAnno$FilePath = file.path(dayFolderPath, format(AprJenAnno$UTC, "%Y%m%d"),
                                AprJenAnno$Soundfile)

AprJenAnno$FileOk  = file.exists(AprJenAnno$FilePath) 

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


PilkAnno1$Dep='WVanIsl'
PilkAnno2$Dep='NorthBc'


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

PilkAnno$AnnotationLevel = 'Call'

PilkAnno$dur = PilkAnno$FileEndSec  - PilkAnno$FileBeginSec
PilkAnno$end_time = PilkAnno$UTC+ seconds(PilkAnno$dur)

# Sort and then identify overlaps
PilkAnno <- PilkAnno %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))





dayFolderPath_WV = 'E:\\DCLDE2026\\DFO_Pilkington\\Audio\\DFOCRP_H50bjRcb-WCV1'
dayFolderPath_NBC = 'E:\\DCLDE2026\\DFO_Pilkington\\Audio\\DFOCRP_KkHK0R2F-NML1'

PilkAnno$FilePath = 'blarg'

PilkAnno1$Dep='WVanIsl'
PilkAnno2$Dep='NorthBc'


WVIidx = which(PilkAnno$Dep == 'WVanIsl')
NBCidx = which(PilkAnno$Dep != 'WVanIsl')

PilkAnno$FilePath[WVIidx] = 
  file.path(dayFolderPath_WV, format(PilkAnno$UTC[WVIidx], "%Y%m%d"),
            PilkAnno$Soundfile[WVIidx])
PilkAnno$FilePath[NBCidx] = 
  file.path(dayFolderPath_NBC, format(PilkAnno$UTC[NBCidx], "%Y%m%d"),
            PilkAnno$Soundfile[NBCidx])

PilkAnno$FileOk  = file.exists(PilkAnno$FilePath) 


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
    data$Dep <- as.factor(basename(file))  # Add filename as a new column
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



colnames(JASCO_malahat)[c(5,6,3,4,1,8)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                           'FileEndSec', 'Soundfile','ClassSpecies')

JASCO_malahat$ClassSpecies[JASCO_malahat$ClassSpecies == 'KW?'] ='KW'
JASCO_malahat$ClassSpecies[JASCO_malahat$ClassSpecies %in% 
                             c("HW/KW?", "HW?")]= 'HW'

JASCO_malahat$ClassSpecies[JASCO_malahat$ClassSpecies %in% 
                             c("VESSEL", "VESSEL/HW?",  "VESSEL CHAIN", 
                               "CHAIN","UNK")]= 'AB'

JASCO_malahat$ClassSpecies[JASCO_malahat$ClassSpecies %in% 
                             c("HW/KW", "SEAGULL?")]= 'UndBio'


JASCO_malahat$AnnotationLevel = 'Call'


JASCO_malahat$end_time = JASCO_malahat$UTC+ seconds(JASCO_malahat$dur)

# Sort and then identify overlaps
JASCO_malahat <- JASCO_malahat %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))



JASCO_malahat$Provider = 'JASCO_Malahat'
levels(JASCO_malahat$Dep)<-c('STN3', 'STN4', 'STN5', 'STN6')

# Filepaths
dayFolderPath = 'E:\\DCLDE2026\\JASCO\\Audio'
JASCO_malahat$FilePath = 
  file.path(dayFolderPath, JASCO_malahat$Dep, 
            format(JASCO_malahat$UTC, "%Y%m%d"),
            JASCO_malahat$Soundfile)

JASCO_malahat$FileOk  = file.exists(JASCO_malahat$FilePath) 


JASCO_malahat = JASCO_malahat[,c(colOut)]



############################################################################
# DFO Yerk
############################################################################

# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'E:\\DCLDE2026\\DFO_Yerk\\Annotations', 
                        pattern = '*csv', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
DFO_Yerk <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  if (nrow(data) > 0) {
    data$Dep <- as.factor(basename(file))  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

levels(DFO_Yerk$Dep)<-c('CarmanahPt', 'StrGeoN1', 'StrGeoN2','StrGeoS1',
                        'StrGeoS2','SwanChan')

# Fucking PAMGuard
DFO_Yerk = DFO_Yerk[DFO_Yerk$duration>0,]
DFO_Yerk = DFO_Yerk[!duplicated(DFO_Yerk),]

# Standardize formatting
DFO_Yerk$Soundfile = DFO_Yerk$soundfile
DFO_Yerk$LowFreqHz = DFO_Yerk$lf
DFO_Yerk$HighFreqHz = DFO_Yerk$hf
DFO_Yerk$UTC = as.POSIXct( DFO_Yerk$date_time_utc,  
                           format="%Y-%m-%d %H:%M:%OS", tz = 'UTC')

DFO_Yerk$FileBeginSec = DFO_Yerk$elapsed_time_seconds
DFO_Yerk$FileEndSec = DFO_Yerk$FileBeginSec+DFO_Yerk$duration/1000

DFO_Yerk$Ecotype = as.factor(DFO_Yerk$species)
levels(DFO_Yerk$Ecotype)<-c(NA, 'NRKW', 'SRKW', 'BKW', NA, NA)

DFO_Yerk$ClassSpecies = as.factor(DFO_Yerk$species)
levels(DFO_Yerk$ClassSpecies)<-c('HW', 'KW', 'KW', 'KW', 'UndBio', 'AB')

DFO_Yerk$KW_certain = ifelse(DFO_Yerk$ClassSpecies== 'KW', 1,0)
DFO_Yerk$Provider = 'DFO_Yerk'


DFO_Yerk$AnnotationLevel = 'Detection'


# Apparently no uncertain calls
DFO_Yerk$KW = DFO_Yerk$KW_certain


DFO_Yerk$dur = DFO_Yerk$FileEndSec  - DFO_Yerk$FileBeginSec
DFO_Yerk$end_time = DFO_Yerk$UTC+ seconds(DFO_Yerk$dur)

# Sort and then identify overlaps
DFO_Yerk <- DFO_Yerk %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))

# Add a new column for deployment folder
DFO_Yerk$DepFolder = DFO_Yerk$Dep
levels(DFO_Yerk$DepFolder)<-c('CMN_2022-03-08_20220629_ST_utc',
                              'SOGN_20210905_20211129_AMAR_utc',
                              'SOGN_20210905_20211129_AMAR_utc',
                              'SOGS_20210904_20211118_AMAR_utc',
                              'SOGS_20210904_20211118_AMAR_utc',
                              'SWAN_20211113_20220110_AMAR_utc')

# Filepaths
dayFolderPath = 'E:\\DCLDE2026\\DFO_Yerk\\Audio'
DFO_Yerk$FilePath = 
  file.path(dayFolderPath, DFO_Yerk$DepFolder, 
            format(DFO_Yerk$UTC, "%Y%m%d"),
            DFO_Yerk$Soundfile)

DFO_Yerk$FileOk  = file.exists(DFO_Yerk$FilePath) 


DFO_Yerk = DFO_Yerk[, colOut]




###############################################################################
# Viers Data

Viersanno = read.csv('E:\\DCLDE2026\\OrcaSound\\Annotations/ModifiedAnnotations.csv')
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


Viersanno$AnnotationLevel = ifelse(Viersanno$ClassSpecies == 'KW',
                                   'Detection', 'File')

# Filepaths
dayFolderPath = 'E:\\DCLDE2026\\OrcaSound\\Audio'
Viersanno$FilePath = 
  file.path(dayFolderPath,Viersanno$Soundfile)

Viersanno$FileOk  = file.exists(Viersanno$FilePath) 

Viersanno = Viersanno[,c(colOut)]

rm(list =c('problemData', 'problemIdx'))

###########################################################################
# SIMRES
##########################################################################



# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'E:\\DCLDE2026\\SIMRES/annotations/datman/selection_tables/SIMRES2022/', 
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

# Clean out blank rows
SIMRES = SIMRES[!is.na(SIMRES$Selection),]

SIMRES$ClassSpecies = as.factor(SIMRES$Sound.ID.Species)
SIMRES$KW_certain =  as.numeric(grepl("KW", SIMRES$Sound.ID.Species))
levels(SIMRES$ClassSpecies)<-c(NA, 'UndBio','KW',  'KW', 'UndBio')

#check SIMRES files in UTC
SIMRES$UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z)_.*", "\\1", 
                            SIMRES$Begin.File[1]),  
                        format = "%Y%m%dT%H%M%S.%OSZ",
                        tz = 'UTC')+seconds(SIMRES$File.Offset..s.)

SIMRES$Ecotype = as.factor(SIMRES$KW.Ecotype)
levels(SIMRES$Ecotype)<- c(NA, NA, 'SRKW', 'SRKW')
SIMRES$KW = ifelse(SIMRES$ClassSpecies == 'KW', 1,0)

SIMRES$Soundfile = SIMRES$Begin.File
SIMRES$Dep = 'Tekteksen'
SIMRES$LowFreqHz = SIMRES$Low.Freq..Hz.
SIMRES$HighFreqHz = SIMRES$Low.Freq..Hz.
SIMRES$FileBeginSec = SIMRES$File.Offset..s.
SIMRES$FileEndSec = SIMRES$File.Offset..s.+SIMRES$Delta.Time..s.
SIMRES$Provider = 'SIMRES'
SIMRES$AnnotationLevel = 'Call'


# Filepaths- wackadoodle for SIMRES
dayFolderPath = 'E:\\DCLDE2026\\SIMRES\\Audio\\'

# 1. List all files in the target directory
allAudio <- list.files(path = dayFolderPath, pattern = "\\.flac$", 
                       full.names = TRUE, recursive = TRUE)

# 2. Check if each file exists and get the full path
SIMRES$FilePath <- sapply(SIMRES$Soundfile, function(filename) {
  # Find the full path of the file (if it exists)
  full_path <- allAudio[grep(paste0("/", filename, "$"), allAudio)]
  if (length(full_path) > 0) return(full_path[1]) else return(NA)
})



SIMRES$FileOk  = file.exists(SIMRES$FilePath) 



SIMRES= SIMRES[, colOut]

#############################################################################

allAnno = rbind(DFO_Pilk, ONC_anno, JASCO_malahat, Viersanno, DFO_Yerk, SIMRES)
allAnno$Duration = 0
allAnno$Duration[allAnno$Provider != 'OrcaSound']=
allAnno$FileEndSec[allAnno$Provider != 'OrcaSound'] - allAnno$FileBeginSec[allAnno$Provider != 'OrcaSound']
  
allAnno = rbind( ONC_anno, DFO_Pilk,JASCO_malahat, DFO_Yerk, SIMRES)


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



