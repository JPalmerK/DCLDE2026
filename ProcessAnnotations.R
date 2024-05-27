# Collate annotations for DCLDE 2026 data

rm(list =ls())
library(lubridate)
library(dplyr)


# Data to pretty
# 
# 1) Ocean Networks Canada 
# 2) Viers
# 3) DFO Pilkington
# 4) DFO Yurk
# 5) SMRU
# 6) VPFA
# 7) Scripps

############################################################################
# Final output column names

colOut = c('Soundfile','Dep','LowFreqHz','HighFreqHz','FileEndSec', 'UTC',
           'FileBeginSec','ClassSpecies','KW','KW_certain','Ecotype', 'Provider',
           'AnnotationLevel', 'FilePath', 'FileOk')

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
#AprJenAnno = read.csv('E:\\DCLDE2026/ONC/Annotations/jen_onc_barkley-canyon_annot.csv')


JASPERsflist = read.csv('C:\\Users/kaitlin.palmer/Downloads/DownloadListpy (2).csv',
                        header = FALSE)

JasperAnno$FilesInList = JasperAnno$Soundfile %in% JASPERsflist$V1
colnames(JasperAnno)[8]<-'origEcotype'


# Add bool for KWs
JasperAnno <- JasperAnno %>%
  mutate(KW = as.numeric(grepl("Oo", Species)))

JasperAnno$KW_certain =NA 

JasperAnno$KW_certain[JasperAnno$KW ==1 & 
                        grepl("\\|", JasperAnno$KW) == FALSE]= 1
JasperAnno$KW_certain[JasperAnno$KW ==1 & 
                        grepl("\\|", JasperAnno$Species) == TRUE] = 0

# Add Ecotype 
JasperAnno$Ecotype<-JasperAnno$origEcotype
JasperAnno$Ecotype[JasperAnno$Ecotype == 'SRKW'] ='SRKW'
JasperAnno$Ecotype[JasperAnno$Comments == 'BKW'] ='BKW'
JasperAnno$Ecotype[JasperAnno$Comments == 'OKW'] ='OKW'

colnames(JasperAnno)[c(2,3,4,5)]<-c('FileBeginSec','FileEndSec', 
                                        'HighFreqHz', 'LowFreqHz')
JasperAnno$ClassSpecies = JasperAnno$Species


JasperAnno$Dep='BarkLeyCanyon'

# Get time
JasperAnno <- JasperAnno %>%
  mutate(filename = as.character(Soundfile),  # Make sure the column is treated as character
         UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z).*", "\\1", filename),
                          format = "%Y%m%dT%H%M%S.%OSZ",  tz = 'UTC'))

JasperAnno$FileBeginSec= as.numeric(JasperAnno$FileBeginSec)
JasperAnno$UTC = JasperAnno$UTC+ seconds(as.numeric(JasperAnno$FileBeginSec))

JasperAnno$ClassSpecies[JasperAnno$ClassSpecies=='Oo']= 'KW'
JasperAnno$ClassSpecies[JasperAnno$ClassSpecies=='Mn']= 'HW'
JasperAnno$ClassSpecies[!JasperAnno$ClassSpecies %in% ClassSpeciesList] = 'UndBio'
JasperAnno$Provider= 'ONC'
JasperAnno$AnnotationLevel = 'Call'

# # Add bool for KWs
# AprJenAnno <- AprJenAnno %>%
#   mutate(KW =         as.numeric(grepl("KW", sound_id_species)),
#          UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z).*", "\\1", filename), 
#                           format = "%Y%m%dT%H%M%S.%OSZ", tz='UTC'))
# 
# AprJenAnno$KW_certain = NA
# AprJenAnno$KW_certain[AprJenAnno$KW==1]=1
# AprJenAnno$KW_certain[AprJenAnno$KW==1 &
#                         as.numeric(grepl("\\?", AprJenAnno$sound_id_species))]=0
# 
# AprJenAnno$KW_certain = as.numeric(AprJenAnno$KW_certain)
# AprJenAnno$KW_certain[AprJenAnno$KW==0]<-NA
# 
# 
# # Add Ecotype 
# AprJenAnno$Ecotype = NA
# AprJenAnno$Ecotype[AprJenAnno$kw_ecotype == 'KWSR'] ='SRKW'
# AprJenAnno$Ecotype[AprJenAnno$kw_ecotype == 'KWT'] ='BKW'
# AprJenAnno$Ecotype[AprJenAnno$kw_ecotype == 'KWT?'] ='BKW'
# 
# 
# colnames(AprJenAnno)[c(1,2,3,4,5)]<-c('LowFreqHz','HighFreqHz','FileEndSec',
#                                         'FileBeginSec','Soundfile')
# 
# AprJenAnno$ClassSpecies = AprJenAnno$sound_id_species
# 
# 
# AprJenAnno$UTC = AprJenAnno$UTC+ seconds(as.numeric(AprJenAnno$FileBeginSec))
# 
# AprJenAnno$Dep='BarkLeyCanyon'
# # Set humpbacks
# 
# # Undetermined biotic sounds
# AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 
#                           c("KW?/PWSD?",  "Echolocation",
#                             "Echolocation?", "KW/PWSD?","PWSD?",
#                             "PWSD/KW?", "PWSD", "PWSD?", "KW/PWSD?",
#                             "Odontocete", "Odontocete?")] = 'UndBio'
# 
# # Probably abitoic sounds
# AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 
#                           c("Unknown", "Vessel Noise","Vessel noise",
#                             "Unknown ", "Vessel Noise?", "unknown",
#                             "Mooring", "Vessel noise?", "Uknown", "")]= 'AB'
# AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 'KW?'] = 'KW'
# AprJenAnno$ClassSpecies[AprJenAnno$ClassSpecies %in% 'HW?'] = 'HW'
# 
# 
# AprJenAnno$ClassSpecies[AprJenAnno$Comments == 'HW'] ='HW'
# 
# # Index of ucnertain killer whale calls  IN Apr Jenn
# idxUncertainKWcalls = which(AprJenAnno$ClassSpecies== 'KW' & 
#                               AprJenAnno$confidence %in% c('Medium', 'm', 'l',
#                                                            'Low', 'M', 'L'))
# AprJenAnno$KW_certain[idxUncertainKWcalls]<-FALSE
# AprJenAnno$KW_certain[AprJenAnno$ClassSpecies != 'KW']= FALSE
# 
# AprJenAnno$Provider = 'ONC_HALLO' # I think HALLO paid for JASCO's annotations
# 
# AprJenAnno$AnnotationLevel = 'Call'
# 
#  
# AprJenAnno$Annotater = 'AprJen'
# JasperAnno$Annotater = 'Jasper'


ONC_anno = rbind(AprJenAnno[, c(colOut[1:12], 'Annotater')],
                 JasperAnno[,  c(colOut[1:12], 'Annotater')])

ONC_anno = rbind(JasperAnno[,  c(colOut[1:12], 'Annotater')])


ONC_anno$dur = as.numeric(ONC_anno$FileEndSec)  - 
  as.numeric(ONC_anno$FileBeginSec)
ONC_anno$end_time = ONC_anno$UTC+ seconds(ONC_anno$dur)

# Sort and then identify overlaps
ONC_anno <- ONC_anno %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))



#rm(list= c('JasperAnno', 'AprJenAnno'))

# some of the dates are fucked up
badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20130615T062356.061Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20130615T062356', 
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.061)+ONC_anno$FileBeginSec[badidx]

badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140402T032959.061Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140402T032959', format="%Y%m%dT%H%M%S", tz="UTC")+seconds(0.061)

# Was this date hand entered? Appears to be 60 rather than 06
badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140701T605216.144Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140701T065216',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.144)+ONC_anno$FileBeginSec[badidx]

badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140702T054216.391Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140702T054216',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.391)+ONC_anno$FileBeginSec[badidx]

badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140804T1345424.361Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140804T1345424',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.361)+ONC_anno$FileBeginSec[badidx]


badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140903T231409.061Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140903T231409',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.061)+ONC_anno$FileBeginSec[badidx]


badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20141004T102246.170Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20141004T102246',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.170)+ONC_anno$FileBeginSec[badidx]

badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20141103T174455.370.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20141103T174455',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.370)+ONC_anno$FileBeginSec[badidx]


badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140104T140150.099Z-HPF.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140104T140150',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.099)+ONC_anno$FileBeginSec[badidx]



badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140202T131456.257Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140104T140150',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.099)+ONC_anno$FileBeginSec[badidx]



badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20141004T102246.170Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20141004T102246',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.170)+ONC_anno$FileBeginSec[badidx]


dayFolderPath = 'E:\\DCLDE2026\\ONC\\Audio\\BarkleyCanyon'
ONC_anno$FilePath = file.path(dayFolderPath,
                              format(ONC_anno$UTC-seconds(ONC_anno$FileBeginSec), "%Y%m%d"),
                              ONC_anno$Soundfile)
ONC_anno$AnnotationLevel = 'call'

ONC_anno$FileOk  = file.exists(ONC_anno$FilePath)



ONC_anno$FileUTC =ONC_anno$UTC- seconds(as.numeric(ONC_anno$FileBeginSec))




# Create a list of the 'not ok files'
missingData = ONC_anno[ONC_anno$FileOk== FALSE,]

ONC_anno = ONC_anno[!ONC_anno$Soundfile %in% missingData$Soundfile,]

# 
# # Ensure all missing files in that list
# missingData$FilesInONClist = missingData$Soundfile %in% JASPERsflist$V1
# 
# 
# 
# # All of these files are undtermined biological sounds so we are just going to
# # cull them
# 
# 
# 
# # Now make a new copy of the missing files for the python download
# needandcanget = unique(missingData$Soundfile[missingData$FilesInONClist == TRUE])
# needandcantget = unique(missingData$Soundfile[missingData$FilesInONClist == FALSE])
# 
# JASPERsflist$ToDownload = JASPERsflist$V1 %in% needandcanget
# JASPERsflist$MIA = JASPERsflist$V1 %in% needandcantget
# 
# ListSub = JASPERsflist[JASPERsflist$ToDownload== TRUE,]
# ListOutstanding = JASPERsflist[JASPERsflist$MIA== TRUE,]
# 
# write.csv(ListSub[,c(1:2)], 
#           'C:\\Users/kaitlin.palmer/Downloads/DownloadListpyMod.csv',
#             row.names = FALSE)


# 
# # figure out the still missing files..
# aa = missingData[missingData$FilesInONClist==FALSE,]
# . 
# aa = data.frame(MissingFiles = unique(missingData$Soundfile[missingData$FilesInONClist==FALSE]))
# 
# aa <- aa %>%
#   mutate(filename = as.character(MissingFiles ), 
#          UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z).*", "\\1", filename),
#                           format = "%Y%m%dT%H%M%S.%OSZ",  tz = 'UTC'))
# 
# 

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
  mutate(KW = as.numeric(grepl("KW", sound_id_species)))

PilkAnno$KW_certain= NA
PilkAnno$KW_certain[PilkAnno$KW==1] =1
PilkAnno$KW_certain[PilkAnno$KW==1 & 
                      grepl("//?", PilkAnno$sound_id_species)]<-0
                                                


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

PilkAnno$Provider = 'DFO_CRP'
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

PilkAnno$KW_certain[PilkAnno$ClassSpecies!= 'KW']=NA

DFO_Pilk = PilkAnno[, c(colOut)]



PilkAnno$dur = PilkAnno$FileEndSec  - PilkAnno$FileBeginSec

# Sort and then identify overlaps
PilkAnno <- PilkAnno %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))

rm(list= c('PilkAnno1', 'PilkAnno2', 'PilkAnno'))

# 
# # List which files are not in annotations list
# audio.files = data.frame(
#   filename = list.files('E:\\DCLDE2026\\DFO_Pilkington\\Audio\\',
#            pattern ='.flac', recursive = TRUE, include.dirs = TRUE))
# audio.files$Soundfile =basename(audio.files$filename)
# audio.files$fullfile = paste0('E:\\DCLDE2026\\DFO_Pilkington\\Audio\\', audio.files$Soundfile )
# PilkAnno = merge(audio.files, DFO_Pilk, by ='Soundfile', all.x = TRUE)
# 
# PilkAnno$keep = PilkAnno$Soundfile %in% DFO_Pilk$Soundfile
# 
# # keep the file after all the start files, just incase
# idxExtraKeep = which(PilkAnno$keep ==TRUE)
# 
# PilkAnno$keep[idxExtraKeep+1] = TRUE
# 
# # Remove files without annotations
# filesRm = PilkAnno[PilkAnno$keep == FALSE,]
# file.remove(filesRm$fullfile)
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
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )

JASCO_malahat$KW_certain= NA
JASCO_malahat$KW_certain[JASCO_malahat$KW==1] =1
JASCO_malahat$KW_certain[JASCO_malahat$KW==1 & 
                      grepl("//?", JASCO_malahat$sound_id_species)]<-0

JASCO_malahat$UTC = JASCO_malahat$UTC+ 
  seconds(as.numeric(JASCO_malahat$start))


# Add Ecotype 
JASCO_malahat$Ecotype = NA
JASCO_malahat$Ecotype[JASCO_malahat$kw_ecotype == 'SRKW'] ='SRKW'
JASCO_malahat$Ecotype[JASCO_malahat$kw_ecotype == 'SRKW?'] ='BKW'
JASCO_malahat$Ecotype[JASCO_malahat$kw_ecotype == 'KWT?'] ='BKW'
JASCO_malahat$Ecotype[JASCO_malahat$kw_ecotype == 'TKW?'] ='BKW'

JASCO_malahat$KW_certain[JASCO_malahat$ClassSpecies!= 'KW']=0



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

JASCO_malahat$dur = JASCO_malahat$FileEndSec-JASCO_malahat$FileBeginSec

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
  file.path(dayFolderPath, JASCO_malahat$Dep,JASCO_malahat$path)

JASCO_malahat$FileOk  = file.exists(JASCO_malahat$FilePath) 


JASCO_malahat = JASCO_malahat[,c(colOut)]


# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('E:\\DCLDE2026\\JASCO/Audio/',
           pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

JASCO = merge(audio.files, JASCO_malahat, by ='Soundfile', all.x = TRUE)

JASCO$keep = JASCO$Soundfile %in% JASCO_malahat$Soundfile

# keep the file after all the start files, just incase
idxExtraKeep = which(JASCO$keep ==TRUE)

JASCO$keep[idxExtraKeep+1] = TRUE

# Remove files without annotations
filesRm = PilkAnno[PilkAnno$keep == FALSE,]
file.remove(filesRm$filename)

############################################################################
# DFO Yurk
############################################################################

# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'E:\\DCLDE2026\\DFO_Yurk\\Annotations', 
                        pattern = '*csv', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
DFO_Yurk <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  if (nrow(data) > 0) {
    data$Dep <- as.factor(basename(file))  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

levels(DFO_Yurk$Dep)<-c('CarmanahPt', 'StrGeoN1', 'StrGeoN2','StrGeoS1',
                        'StrGeoS2','SwanChan')

# Fucking PAMGuard
DFO_Yurk = DFO_Yurk[DFO_Yurk$duration>0,]
DFO_Yurk = DFO_Yurk[!duplicated(DFO_Yurk),]

# Standardize formatting
DFO_Yurk$Soundfile = DFO_Yurk$soundfile
DFO_Yurk$LowFreqHz = DFO_Yurk$lf
DFO_Yurk$HighFreqHz = DFO_Yurk$hf
DFO_Yurk$UTC = as.POSIXct( DFO_Yurk$date_time_utc,  
                           format="%Y-%m-%d %H:%M:%OS", tz = 'UTC')

DFO_Yurk$FileBeginSec = DFO_Yurk$elapsed_time_seconds
DFO_Yurk$FileEndSec = DFO_Yurk$FileBeginSec+DFO_Yurk$duration/1000

DFO_Yurk$Ecotype = as.factor(DFO_Yurk$species)
levels(DFO_Yurk$Ecotype)<-c(NA, 'NRKW', 'SRKW', 'BKW', NA, NA)

DFO_Yurk$ClassSpecies = as.factor(DFO_Yurk$species)
levels(DFO_Yurk$ClassSpecies)<-c('HW', 'KW', 'KW', 'KW', 'UndBio', 'AB')

DFO_Yurk$KW_certain = ifelse(DFO_Yurk$ClassSpecies== 'KW', 1,0)
DFO_Yurk$KW_certain[DFO_Yurk$ClassSpecies != 'KW'] = NA  
DFO_Yurk$Provider = 'DFO_WDA'


DFO_Yurk$AnnotationLevel = 'Detection'


# Apparently no uncertain calls
DFO_Yurk$KW = DFO_Yurk$KW_certain


DFO_Yurk$dur = DFO_Yurk$FileEndSec  - DFO_Yurk$FileBeginSec
DFO_Yurk$end_time = DFO_Yurk$UTC+ seconds(DFO_Yurk$dur)

# Sort and then identify overlaps
DFO_Yurk <- DFO_Yurk %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))

# Add a new column for deployment folder
DFO_Yurk$DepFolder = DFO_Yurk$Dep
levels(DFO_Yurk$DepFolder)<-c('CMN_2022-03-08_20220629_ST_utc',
                              'SOGN_20210905_20211129_AMAR_utc',
                              'SOGN_20210905_20211129_AMAR_utc',
                              'SOGS_20210904_20211118_AMAR_utc',
                              'SOGS_20210904_20211118_AMAR_utc',
                              'SWAN_20211113_20220110_AMAR_utc')

# Filepaths
dayFolderPath = 'E:\\DCLDE2026\\DFO_Yurk\\Audio'
DFO_Yurk$FilePath = 
  file.path(dayFolderPath, DFO_Yurk$DepFolder, 
            format(DFO_Yurk$UTC, "%Y%m%d"),
            DFO_Yurk$Soundfile)

DFO_Yurk$FileOk  = file.exists(DFO_Yurk$FilePath) 


DFO_Yurk = DFO_Yurk[, colOut]




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

# Problem children- different bloody start times
problemData = unique(Viersanno$dataset[is.na(Viersanno$UTC)])
problemIdx = which(is.na(Viersanno$UTC))
  
Viersanno$UTC[problemIdx] <- 
  as.POSIXct(Viersanno$DateTime[problemIdx],
             tz = "America/Los_Angeles", 
             format = "%Y_%m_%d %H:%M:%S")


Viersanno$ClassSpecies = Viersanno$Species
Viersanno$ClassSpecies[Viersanno$ClassSpecies == 'Oo'] = 'KW'
Viersanno$ClassSpecies[Viersanno$ClassSpecies == 'Noise'] = 'AB'

Viersanno$Provider = 'OrcaSound'
Viersanno$KW = ifelse(Viersanno$ClassSpecies== 'KW',1,0)
Viersanno$KW_certain = ifelse(Viersanno$ClassSpecies== 'KW',1,NA)
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

Viersanno[Viersanno$AnnotationLevel=='File', 
          c('FileBeginSec', 'FileEndSec',
            'UTC', 'LowFreqHz', 'HighFreqHz')]= NA

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
levels(SIMRES$ClassSpecies)<-c('AB', 'UndBio','KW',  'KW', 'KW')


#check SIMRES files in UTC
SIMRES$UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z)_.*", "\\1", 
                            SIMRES$Begin.File),  
                        format = "%Y%m%dT%H%M%S.%OSZ",
                        tz = 'UTC')+seconds(SIMRES$File.Offset..s.)

SIMRES$Ecotype = as.factor(SIMRES$KW.Ecotype)
levels(SIMRES$Ecotype)<- c(NA, NA, 'SRKW', 'SRKW')

SIMRES$KW =as.numeric(grepl("KW", SIMRES$Sound.ID.Species))

SIMRES$KW_certain = NA
SIMRES$KW_certain[SIMRES$KW ==1] =1
SIMRES$KW_certain[SIMRES$Confidence %in% c('low')]=0

SIMRES$KW_certain= NA
SIMRES$KW_certain[SIMRES$KW==1] =1
SIMRES$KW_certain[SIMRES$KW==1 & 
                    grepl("\\?", SIMRES$Sound.ID.Species)]<-0



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


############################################################################
# VFPA - JASCO Strait of Georgia (Roberts Bank in Globus)
############################################################################

# Strait fo Georgia
VPFA_SoG<- read.csv('D:\\VFPA/Annotations/annot_RB_man_det.csv')

VPFA_SoG <- VPFA_SoG %>%
  mutate(
    KW = as.numeric(grepl("KW", sound_id_species)),
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}.\\d{3}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )


VPFA_SoG$KW_certain= NA
VPFA_SoG$KW_certain[VPFA_SoG$KW==1] =1
VPFA_SoG$KW_certain[VPFA_SoG$KW==1 & 
                           grepl("\\?", VPFA_SoG$sound_id_species)]<-0

VPFA_SoG$UTC = VPFA_SoG$UTC+ 
  seconds(as.numeric(VPFA_SoG$start))

# Add Ecotype 
VPFA_SoG$Ecotype = NA
VPFA_SoG$Ecotype[VPFA_SoG$kw_ecotype == 'SRKW'] ='SRKW'
VPFA_SoG$Ecotype[VPFA_SoG$kw_ecotype == 'SRKW?'] ='SRKW'
VPFA_SoG$Ecotype[VPFA_SoG$kw_ecotype == 'KWT?'] ='BKW'
VPFA_SoG$Ecotype[VPFA_SoG$kw_ecotype == 'TKW?'] ='BKW'


colnames(VPFA_SoG)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                           'FileEndSec', 'Soundfile')

VPFA_SoG$ClassSpecies<- VPFA_SoG$sound_id_species


VPFA_SoG$ClassSpecies[VPFA_SoG$ClassSpecies == 'KW?'] ='KW'
VPFA_SoG$ClassSpecies[VPFA_SoG$ClassSpecies %in% 
                             c("HW/KW?", "HW?")]= 'HW'

VPFA_SoG$ClassSpecies[VPFA_SoG$ClassSpecies %in% 
                             c("Vessel Noise", "Vessel Noise?",  "Noise", 
                               "Sonar","UN")]= 'AB'

VPFA_SoG$ClassSpecies[VPFA_SoG$ClassSpecies %in% 
                             c("KW/PWSD?", "PWSD","FS")]= 'UndBio'

VPFA_SoG$AnnotationLevel = 'Call'

VPFA_SoG$dur = VPFA_SoG$FileEndSec-VPFA_SoG$FileBeginSec

VPFA_SoG$end_time = VPFA_SoG$UTC+ seconds(VPFA_SoG$dur)

VPFA_SoG$Dep='StraitofGeorgia'
VPFA_SoG$Provider = 'JASCO_VFPA'

# Sort and then identify overlaps
VPFA_SoG <- VPFA_SoG %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))


# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('D:\\VFPA/StraitofGeorgia_Globus-RobertsBank/',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Make sure all audio files are present for all annotations
if (all(VPFA_SoG$Soundfile %in% audio.files$Soundfile)){
  print('All data present for annogations')
}else{
  print('Missing data')
  VPFA_SoG$Soundfile[which(!VPFA_SoG$Soundfile %in% audio.files$Soundfile)]
}

VPFA_SoG$FilePath = paste0('VFPA/StraitofGeorgia_Globus-RobertsBank/',VPFA_SoG$Soundfile)

# 
# 
# JASCO = merge(audio.files, VPFA_SoG, by ='Soundfile', all.x = TRUE, all.y = FALSE)
# JASCO$keep = JASCO$Soundfile %in% VPFA_SoG$Soundfile
# 
# # keep the file after all the start files, just incase
# idxKeep = which(JASCO$keep)
# idxKeepall = unique(c(idxKeep-1, idxKeep, idxKeep+1))
# JASCO$keep[idxKeepall] = TRUE
# 
# # 
# # # Remove files without annotations
# # filesRm = JASCO$[JASCO$keep == FALSE,]
# # file.remove(JASCO$filesRm)
# # 
# # # Remove files without annotations
# # filesRm = PilkAnno[PilkAnno$keep == FALSE,]
# # file.remove(filesRm$filename)

############################################################################
# VFPA - JASCO Boundary Pass
############################################################################


# Boundary Pass
VPFA_BoundaryPass<- read.csv('D:\\VFPA/Annotations/annot_BP_man_det.csv')
VPFA_BoundaryPass <- VPFA_BoundaryPass %>%
  mutate(
    KW = as.numeric(grepl("KW", sound_id_species)),
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )

VPFA_BoundaryPass$KW_certain= NA
VPFA_BoundaryPass$KW_certain[VPFA_BoundaryPass$KW==1] =1

# Remove 'duplicate' and 'repeat' annotations
VPFA_BoundaryPass= VPFA_BoundaryPass[
  !VPFA_BoundaryPass$sound_id_species %in% c('Repeat', 'Duplicate'),]

UncertainKWidx = which(VPFA_BoundaryPass$KW==1 & 
                         grepl("\\?", VPFA_BoundaryPass$sound_id_species))
VPFA_BoundaryPass$KW_certain[UncertainKWidx]=0

# Get time in UTC
VPFA_BoundaryPass$UTC = VPFA_BoundaryPass$UTC+ 
  seconds(as.numeric(VPFA_BoundaryPass$start))

# Add Ecotype 
VPFA_BoundaryPass$Ecotype = NA
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'SRKW'] ='SRKW'
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'SRKW?'] ='SRKW'
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'KWT?'] ='BKW'
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'TKW?'] ='BKW'
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'TKW'] ='BKW'



colnames(VPFA_BoundaryPass)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                    'FileEndSec', 'Soundfile')

VPFA_BoundaryPass$ClassSpecies<- VPFA_BoundaryPass$sound_id_species


VPFA_BoundaryPass$ClassSpecies[VPFA_BoundaryPass$ClassSpecies == 'KW?'] ='KW'
VPFA_BoundaryPass$ClassSpecies[VPFA_BoundaryPass$ClassSpecies %in% 
                        c("HW/KW?",  "KW/HW?", "HW?")]= 'HW'

VPFA_BoundaryPass$ClassSpecies[VPFA_BoundaryPass$ClassSpecies %in% 
                        c("Vessel Noise", "Vessel Noise?",  "Noise", 
                          "Sonar","UN", "NN", "BACKGROUND", 'UNK')]= 'AB'

VPFA_BoundaryPass$ClassSpecies[VPFA_BoundaryPass$ClassSpecies %in% 
                        c("KW/PWSD?", "PWSD","FS",  "PWSD?")]= 'UndBio'

VPFA_BoundaryPass$AnnotationLevel = 'Call'
VPFA_BoundaryPass$dur = VPFA_BoundaryPass$FileEndSec-VPFA_BoundaryPass$FileBeginSec
VPFA_BoundaryPass$end_time = VPFA_BoundaryPass$UTC+ seconds(VPFA_BoundaryPass$dur)
VPFA_BoundaryPass$Dep='BoundaryPass'
VPFA_BoundaryPass$Provider = 'JASCO_VFPA'



# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('D:\\VFPA/BoundaryPass/',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Make sure all audio files are present for all annotations
if (all(VPFA_BoundaryPass$Soundfile %in% audio.files$Soundfile)){
  print('All data present for annogations')
}else{
  print('Missing data')
  VPFA_BoundaryPass$Soundfile[which(!VPFA_BoundaryPass$Soundfile %in% audio.files$Soundfile)]
}

VPFA_BoundaryPass$FilePath = paste0('VFPA/BoundaryPass/',VPFA_BoundaryPass$Soundfile)

############################################################################
# VFPA - JASCO Haro Strait North
############################################################################


# Haro Strait North
VPFA_HaroNB<- read.csv('D:\\VFPA/Annotations/annot_VFPA-HaroStrait-NB_SM_coarse.csv')
VPFA_HaroNB <- VPFA_HaroNB %>%
  mutate(
    KW = as.numeric(grepl("KW", sound_id_species)),
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )

VPFA_HaroNB$KW_certain= NA
VPFA_HaroNB$KW_certain[VPFA_HaroNB$KW==1] =1

UncertainKWidx = which(VPFA_HaroNB$KW==1 & 
                         grepl("\\?", VPFA_HaroNB$sound_id_species))
VPFA_HaroNB$KW_certain[UncertainKWidx]=0

# Get time in UTC
VPFA_HaroNB$UTC = VPFA_HaroNB$UTC+ 
  seconds(as.numeric(VPFA_HaroNB$start))

# Add Ecotype 
VPFA_HaroNB$Ecotype = NA
VPFA_HaroNB$Ecotype[VPFA_HaroNB$kw_ecotype == 'SRKW'] ='SRKW'
VPFA_HaroNB$Ecotype[VPFA_HaroNB$kw_ecotype == 'SRKW?'] ='SRKW'
VPFA_HaroNB$Ecotype[VPFA_HaroNB$kw_ecotype == 'KWT?'] ='BKW'
VPFA_HaroNB$Ecotype[VPFA_HaroNB$kw_ecotype == 'TKW?'] ='BKW'



colnames(VPFA_HaroNB)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                             'FileEndSec', 'Soundfile')

VPFA_HaroNB$ClassSpecies<- VPFA_HaroNB$sound_id_species


VPFA_HaroNB$ClassSpecies[VPFA_HaroNB$ClassSpecies == 'KW?'] ='KW'
VPFA_HaroNB$ClassSpecies[VPFA_HaroNB$ClassSpecies %in% 
                                 c("HW/KW?", "HW?")]= 'HW'

VPFA_HaroNB$ClassSpecies[VPFA_HaroNB$ClassSpecies %in% 
                                 c("Vessel Noise", "Vessel Noise?",  "Noise", 
                                   "Sonar","UN", "BELL","VESSEL", "UNK")]= 'AB'

VPFA_HaroNB$ClassSpecies[VPFA_HaroNB$ClassSpecies %in% 
                                 c("KW/PWSD?", "PWSD","FS")]= 'UndBio'

VPFA_HaroNB$AnnotationLevel = 'Call'
VPFA_HaroNB$dur = VPFA_HaroNB$FileEndSec-VPFA_HaroNB$FileBeginSec
VPFA_HaroNB$end_time = VPFA_HaroNB$UTC+ seconds(VPFA_HaroNB$dur)
VPFA_HaroNB$Dep='HaroStraitNorth'
VPFA_HaroNB$Provider = 'JASCO_VFPA'



# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('D:\\VFPA/VFPA-HaroStrait-NB/',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Make sure all audio files are present for all annotations
if (all(VPFA_HaroNB$Soundfile %in% audio.files$Soundfile)){
  print('All data present for annogations')
  VPFA_HaroNB$FileOk=1
}else{
  print('Missing data')
  VPFA_HaroNB$Soundfile[which(!VPFA_HaroNB$Soundfile %in% audio.files$Soundfile)]
}

VPFA_HaroNB$FilePath = paste0('VFPA/VFPA-HaroStrait-NB/',VPFA_HaroNB$Soundfile)

############################################################################
# VFPA - JASCO Haro Strait South
############################################################################

# Haro Strait South
VPFA_HaroSB<- read.csv('D:\\VFPA/Annotations/annot_VFPA-HaroStrait-SB_SM_coarse.csv')
VPFA_HaroSB <- VPFA_HaroSB %>%
  mutate(
    KW = as.numeric(grepl("KW", sound_id_species)),
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )

VPFA_HaroSB$KW_certain= NA
VPFA_HaroSB$KW_certain[VPFA_HaroSB$KW==1] =1

UncertainKWidx = which(VPFA_HaroSB$KW==1 & 
                         grepl("\\?", VPFA_HaroSB$sound_id_species))
VPFA_HaroSB$KW_certain[UncertainKWidx]=0

# Get time in UTC
VPFA_HaroSB$UTC = VPFA_HaroSB$UTC+ 
  seconds(as.numeric(VPFA_HaroSB$start))

# Add Ecotype 
VPFA_HaroSB$Ecotype = NA
VPFA_HaroSB$Ecotype[VPFA_HaroSB$kw_ecotype == 'SRKW'] ='SRKW'
VPFA_HaroSB$Ecotype[VPFA_HaroSB$kw_ecotype == 'SRKW?'] ='SRKW'
VPFA_HaroSB$Ecotype[VPFA_HaroSB$kw_ecotype == 'KWT?'] ='BKW'
VPFA_HaroSB$Ecotype[VPFA_HaroSB$kw_ecotype == 'TKW?'] ='BKW'


colnames(VPFA_HaroSB)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                        'FileEndSec', 'Soundfile')

VPFA_HaroSB$ClassSpecies<- VPFA_HaroSB$sound_id_species


VPFA_HaroSB$ClassSpecies[VPFA_HaroSB$ClassSpecies == 'KW?'] ='KW'
VPFA_HaroSB$ClassSpecies[VPFA_HaroSB$ClassSpecies %in% 
                            c("HW/KW?", "HW?")]= 'HW'

VPFA_HaroSB$ClassSpecies[VPFA_HaroSB$ClassSpecies %in% 
                            c("Vessel Noise", "Vessel Noise?",  "Noise", 
                              "Sonar","UN", "BELL", "VESSEL", 'UNK')]= 'AB'

VPFA_HaroSB$ClassSpecies[VPFA_HaroSB$ClassSpecies %in% 
                            c("KW/PWSD?", "PWSD","FS")]= 'UndBio'

VPFA_HaroSB$AnnotationLevel = 'Call'
VPFA_HaroSB$dur = VPFA_HaroSB$FileEndSec-VPFA_HaroSB$FileBeginSec
VPFA_HaroSB$end_time = VPFA_HaroSB$UTC+ seconds(VPFA_HaroSB$dur)
VPFA_HaroSB$Dep='HaroStraitSouth'
VPFA_HaroSB$Provider = 'JASCO_VFPA'



# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('D:\\VFPA/VFPA-HaroStrait-SB/',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Make sure all audio files are present for all annotations
if (all(VPFA_HaroSB$Soundfile %in% audio.files$Soundfile)){
  print('All data present for annogations')
}else{
  print('Missing data')
  VPFA_HaroSB$Soundfile[which(!VPFA_HaroSB$Soundfile %in% audio.files$Soundfile)]
}


###########################################################################
# SCRIPPS
############################################################################


# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'D:\\Scripps\\Annoations', 
                        pattern = '*txt', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
scripps <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.table(file, header = TRUE, sep = '\t')
  if (nrow(data) > 0) {
    data$Dep <- as.factor(basename(file))  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))



# Cape Elizabeth and Quinault Canyon
levels(scripps$Dep)<-c("Cpe_Elz", "Quin_Can")

# Set the initial ecotypes then match the format
scripps$Ecotype <- as.factor(gsub(".*_(.*)\\.wav", "\\1", scripps$Begin.File))
levels(scripps$Ecotype)<-c('BKW', 'BKW', 'BKW', 'BKW', 'OKW', 'SRKW',
                           'SRKW','BKW', 'BKW')
scripps$Ecotype[scripps$ClassSpecies != ""]<- NaN

scripps$ClassSpecies= as.factor(scripps$ClassSpecies)
levels(scripps$ClassSpecies)<-c('KW', 'AB', 'AB', 'HW', 'KW')

scripps$KW = ifelse(scripps$ClassSpecies=='KW',1,0)
scripps$KW_certain = ifelse(scripps$ClassSpecies=='KW',1,NA)

scripps$Soundfile = scripps$Begin.File
scripps$LowFreqHz = scripps$Low.Freq..Hz.
scripps$HighFreqHz = scripps$Low.Freq..Hz.
scripps$FileBeginSec = scripps$Beg.File.Samp..samples./200000
scripps$FileEndSec = scripps$FileBeginSec+(scripps$End.Time..s.- scripps$Begin.Time..s)
scripps$Provider = 'SIO'
scripps$AnnotationLevel = 'Call'
scripps$FilePath = scripps$Begin.Path


#check SIMRES files in UTC
scripps$UTC = as.POSIXct(sub(".*_(\\d{6}_\\d{6})_.*", "\\1",
                             scripps$Begin.File),  
                        format = "%y%m%d_%H%M%S",
                        tz = 'UTC')
scripps$FileOk=1


xx
#############################################################################

allAnno = rbind(DFO_Pilk, ONC_anno[, colOut], Viersanno, DFO_Yurk, SIMRES)

# overall annotations
table(allAnno$ClassSpecies)

# Pull out the killer whale data
KW_data = subset(allAnno, KW ==1)

table(KW_data$Ecotype)


############################################################################
# Histogram of months
library(ggplot2)

ggplot(data = allAnno[!is.na(allAnno$ClassSpecies),], 
       aes(x= month(UTC)))+
  geom_histogram(aes(fill  = as.factor(ClassSpecies)), bins = 12)+
  facet_wrap(~Provider)+
  scale_fill_discrete(name = "KW Annotations")+
  theme_bw()+
  xlab('Month')

ggplot(data = subset(KW_data, Ecotype %in% c('NRKW', 'SRKW', 'OKW', 'BKW'))
                       , aes(x= month(UTC)))+
  geom_histogram(aes(fill  = as.factor(Ecotype)), bins = 12)+
  facet_wrap(~Provider)+
  scale_fill_discrete(name = "KW Annotations")+
  theme_bw()+
  xlab('Month')

###########################################################################
# Check each dataset to see which audio files have kw calls
##########################################################################


# Filepaths- wackadoodle for SIMRES -OK!
dayFolderPath = 'E:\\DCLDE2026\\SIMRES\\Audio\\'
SimresAudio <- list.files(path = dayFolderPath, pattern = "\\.flac$", 
                       full.names = FALSE, recursive = TRUE)
all(basename(SimresAudio) %in% SIMRES$Soundfile)

# Podcast Data- More
dayFolderPath = 'E:\\DCLDE2026\\OrcaSound\\Audio'
ViersannoAudio = list.files(path = dayFolderPath, pattern = "\\.wav$", 
                                full.names = FALSE, recursive = TRUE)
which(!basename(ViersannoAudio) %in% Viersanno$Soundfile)


# Yurk - full deployment, too many files
dayFolderPath = 'E:\\DCLDE2026\\DFO_Yurk\\Audio'
YurkAudio = list.files(path = dayFolderPath, pattern = "\\.wav$", 
                            full.names = FALSE, recursive = TRUE)
all(basename(YurkAudio) %in% DFO_Yerk$Soundfile)



# JASCO Malahat
file_list <- list.files(path = 'E:/DCLDE2026/JASCO/Audio/', 
                        pattern = '\\.wav$', recursive = TRUE)

length(which(basename(file_list) %in% JASCO_malahat$Soundfile))



