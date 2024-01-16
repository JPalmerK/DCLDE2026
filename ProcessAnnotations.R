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
AprJenAnno$Ecotype = NaN
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
#MOVE TO E DRIVE, MORE SPACE!!*
PilkAnno1 = read.csv('D:\\DCLDE 2024/DFO_Pilkington/annotations/annot_H50bjRcb_SM_det.csv')
PilkAnno2 = read.csv('D:\\DCLDE 2024/DFO_Pilkington/annotations/annot_KkHK0R2F_SM_det.csv')
PilkAnno1$Dep='DFO_01'
PilkAnno2$Dep='DFO_02'

PilkAnno = rbind(PilkAnno1, PilkAnno1)

table(PilkAnno$sound_id_species)

# Add bool for KWs
PilkAnno <- PilkAnno %>%
  mutate(KW = as.numeric(grepl("KW", sound_id_species)),
         KW_certain = as.numeric(grepl("KW?", sound_id_species)))

# Add Ecotype 
PilkAnno$Ecotype = NaN
PilkAnno$Ecotype[PilkAnno$kw_ecotype == 'SRKW'] ='SRKW'
PilkAnno$Ecotype[PilkAnno$kw_ecotype == 'TKW'] ='BKW'
PilkAnno$Ecotype[PilkAnno$kw_ecotype == 'OKW'] ='OKW'
PilkAnno$Ecotype[PilkAnno$kw_ecotype == 'NRKW'] ='NRKW'


colnames(PilkAnno)[c(1,3,4,5,6, 8,13)]<-c('Soundfile','FileBeginSec','FileEndSec',
                                    'LowFreqHz','HighFreqHz','ClassSpecies', 'UTC')

PilkAnno$Provider = 'DFO'
DFO_anno = PilkAnno[, c(colOut)]

# Clean up the abiotic counds
DFO_anno$ClassSpecies[
  DFO_anno$ClassSpecies %in% c('Vessel Noise', 'Unknown', '', 'Mooring Noise', 
                               'Chain?', 'ADCP', 'Anchor Noise', 'Clang',
                               'Vessel Noise?', 'Chain','No sound data',
                               "Blast/Breach", "Fishing Gear", "Breach",
                               'Rubbing')] = 'AB'

DFO_anno$ClassSpecies[!DFO_anno$ClassSpecies %in% ClassSpeciesList] = 'UndBio'
rm(list= c('PilkAnno1', 'PilkAnno2', 'PilkAnno'))


##############################################################################
# JASCO- Malahat

# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'D:/DCLDE 2024/JASCO/annotations/', 
                        pattern = 'annot_Malahat', full.names = TRUE)

# Read and concatenate the CSV files
JASCO_malahat <- do.call(rbind, lapply(file_list, read.csv))

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
JASCO_malahat$Ecotype = NaN
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

#############################################################################
 
allAnno = rbind(DFO_anno, ONC_anno, JASCO_malahat)


# Pull out the killer whale data
KW_data = subset(allAnno, KW ==1)

table(KW_data$Ecotype)




###############################################################################
# Viers Data

Viersanno = read.csv('E:\\DCLDE2026\\OrcaSound\\Meta/ModifiedAnnotations.csv')
Viersanno$start_time_s = as.numeric(Viersanno$start_time_s) 

# Exclude round 1
Viersanno = subset(Viersanno, dataset != 'podcast_round1')

# Cut times wtih start == 0
Viersanno= Viersanno[Viersanno$start_time_s>0,]


pcaast.01 = subset(Viersanno, dataset =='podcast_round1')
pcaast.02 = subset(Viersanno, dataset =='podcast_round2')
pcaast.03 = subset(Viersanno, dataset =='podcast_round3')
pcaast.05 = subset(Viersanno, dataset =='podcast_round5')
pcaast.06 = subset(Viersanno, dataset =='podcast_round6')
pcaast.07 = subset(Viersanno, dataset =='podcast_round7')
pcaast.09 = subset(Viersanno, dataset =='podcast_round9')
pcaast.10 = subset(Viersanno, dataset =='podcast_round10')
pcaast.11 = subset(Viersanno, dataset =='podcast_round11')
pcaast.12 = subset(Viersanno, dataset =='podcast_round12')

