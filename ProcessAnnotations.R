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
           'FileBeginSec','ClassSpecies','KW','KW_certain','Ecotype')

###############################################################################
# 1) ONC- data from globus


# Jasper, April, and Jenn have all annotated thse files. There is some overla
JasperAnno = read.csv('D:\\DCLDE 2024\\ONC/annotations/datman/ONC_JasperKanes/BarkleyCanyonAnnotations_forpublication1.csv')
AprJenAnno = read.csv('D:\\DCLDE 2024/ONC/annotations/datman/HALLO_JenApril/jen_onc_barkley-canyon_annot.csv')


# Add bool for KWs
JasperAnno <- JasperAnno %>%
  mutate(KW = as.numeric(grepl("Oo", Species)),
         KW_certain = as.numeric(grepl("Oo", Species)))



# Add Ecotype 
JasperAnno$Ecotype = NaN
JasperAnno$Ecotype[JasperAnno$Comments == 'SRKW'] ='SRKW'
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




ONC_anno = rbind(JasperAnno[, c(colOut)], JasperAnno[,  c(colOut)])

############################################################################
# DFO Pilkington
############################################################################

# No seconds in UTC
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



colOut = c('Soundfile','Dep','LowFreqHz','HighFreqHz','FileEndSec', 'UTC',
           'FileBeginSec','ClassSpecies','KW','KW_certain','Ecotype')

colnames(PilkAnno)[c(1,3,4,5,6, 8,13)]<-c('Soundfile','FileBeginSec','FileEndSec',
                                    'LowFreqHz','HighFreqHz','ClassSpecies', 'UTC')

DFO_anno = PilkAnno[, c(colOut)]

rm(list= c('PilkAnno1', 'PilkAnno2', 'PilkAnno'))

###############################################################################


allAnno = rbind(DFO_anno, ONC_anno)

