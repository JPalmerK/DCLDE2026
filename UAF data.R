# Reorganize UAF data


#############################################################################
# University of Alaska this section moves the files into deployments
#############################################################################
library(dplyr)
library(lubridate)

source('C:/Users/kaity/Documents/GitHub/DCLDE2026/TestFx.R')

# Downloaded Audio locationf
root_dir ='E:\\DCLDE\\UAF\\BIN\\Myers_DCLDE_2026_killer_whale_data'

# New Audio Location (where to move the organized files too)
new_root ='E:\\DCLDE\\UAF\\Audio'

# Annotations location (already moved all the annotations here)
annot_root =  'E:\\DCLDE\\UAF\\Annotations\\'

# Load the deployment info
UAF_depInfo = read.csv('E:\\DCLDE\\UAF\\BIN\\Myers_DCLDE_2026_killer_whale_data\\Myers_DCLDE_2026_files.csv')


# Downloaded Annotations (already organized)
UAF_file_list <- list.files(path = annot_root,
                        pattern = '.txt', full.names = TRUE,
                        recursive = TRUE)



# Read and concatenate the selection tables files with filename as a 
# separate column (if non-empty)
UAF <- do.call(rbind, lapply(UAF_file_list, function(file) {
  data <- read.table(file, header = TRUE, sep = '\t')
  audioFile<- basename(file)
  Hyd <- strsplit(audioFile, "\\.")[[1]][[1]]
  
  if (nrow(data) > 0) {
    data$Dep <- as.factor(strsplit(dirname(file),'/')[[1]][4])  # Add filename as a new column
    data$Hyd <- as.factor(Hyd)
    parts <- strsplit(audioFile, "\\.")[[1]][1:2]
    filename <- paste(parts[1], parts[2], sep = ".")
    filename <- paste0(filename, ".wav")
    
    data$AudioFile =filename
    data$FolderName = dirname(file)
      return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

# remove 'table' from the field recordings 
UAF$AudioFile <- gsub("\\.Table1", "", UAF$AudioFile)
# and "table1"... shoot me in the face please
UAF$AudioFile <- gsub("\\.Table", "", UAF$AudioFile)

# Function to find full path for a single filename
find_file_path <- function(filename, root_dir) {
  pattern <- filename
  file_path <- list.files(path = root_dir, pattern = pattern, 
                          recursive = TRUE, full.names = TRUE)
  if (length(file_path) > 0) {
    return(file_path)
  } else {
    return(NA)  # Return NA if file not found
  }
}


# Apply the function to each filename in data$filename
UAF$audio_path <- sapply(UAF$AudioFile, find_file_path,
                         root_dir = 'E:\\DCLDE\\UAF\\BIN')





# Merge the files
aa = merge(UAF, UAF_depInfo, by= 'AudioFile', all.x = TRUE)
aa$new_loc = file.path('E:\\DCLDE\\UAF\\Audio', aa$Location, aa$AudioFile)
aa$new_path = file.path('E:\\DCLDE\\UAF\\Audio', aa$Location)

bb= aa[!duplicated(aa$new_loc),]
bb = bb[bb$Location != 'Field',]

# # copy the files into deployments based on hydrophones
# for(ii in 1:nrow(bb)){
#   new_location = bb$new_loc[ii]
#   
#   # check that it needs to be moved
#   if(!is.na(bb$audio_path[ii])){
#     if (!file.exists(new_location)) {
#       # Create the directory if it doesn't exist
#       dir.create(new_location, recursive = TRUE)  
#     }
#     # Copy the file to the new location
#     file.copy(bb$audio_path[ii], new_location, overwrite = TRUE) 
#   }
# }

# copy the files into deployments based on hydrophones
for(ii in 1:nrow(bb)){

  
  file.copy(bb$audio_path[ii],  bb$new_loc[ii], overwrite = TRUE)
  print(ii)
}



# List the files in the old and new directories, make sure everything is fine
new_files <- list.files(path = 'E:\\DCLDE\\UAF\\Audio', pattern = pattern, 
                        recursive = TRUE, full.names = TRUE)







# Ok now create a new path based on the hydrophone location
newRoot = 'E:\\DCLDE\\UAF\\Audio'
UAF$NewLoc = file.path(newRoot, UAF$Hyd)

# copy the files into deployments based on hydrophones
for(ii in 1:nrow(UAF)){
  new_location = UAF$NewLoc[ii]
  
  # check that it needs to be moved
  if(!is.na(UAF$audio_path[ii])){
    if (!file.exists(new_location)) {
      # Create the directory if it doesn't exist
      dir.create(new_location, recursive = TRUE)  
    }
     # Copy the file to the new location
    file.copy(UAF$audio_path[ii], new_location, overwrite = TRUE) 
  }
}


###############################################################################
# Now the files are reorganized, this section adds the required headings as 
# per June 5th email
###############################################################################
# Downloaded Audio locationf
root_dir ='E:\\DCLDE\\UAF\\BIN\\Myers_DCLDE_2026_killer_whale_data'

# New Audio Location (where to move the organized files too)
new_root ='E:\\DCLDE\\UAF\\Audio'

# Annotations location (already moved all the annotations here)
annot_root =  'E:\\DCLDE\\UAF\\Annotations\\'

# Get a list of annotation files
file_list <- list.files(path = annot_root,
                        pattern = '.txt', full.names = TRUE,
                        recursive = TRUE)



# Read and concatenate the selection tables  with filename as a separate column (if non-empty)
UAF <- do.call(rbind, lapply(file_list, function(file) {
  
  # read the selection table
  data <- read.table(file, header = TRUE, sep = '\t')
  
  # get the audio file name
  audioFile<- basename(file)
  
  # Hydrophone id
  Hyd <- strsplit(audioFile, "\\.")[[1]][[1]]
  
  if (nrow(data) > 0) {
    
    # Add filename as a new column and get deployment from the filename
    data$Dep <- as.factor(strsplit(dirname(file),'/')[[1]][4])  
    data$Hyd <- as.factor(Hyd)
    parts <- strsplit(audioFile, "\\.")[[1]][1:2]
    filename <- paste(parts[1], parts[2], sep = ".")
    filename <- paste0(filename, ".wav")
    
    #AudioFile is one of the required column headings
    data$AudioFile =filename
    
    # For ecotype later on
    data$FolderName = dirname(file)
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

# remove 'table' from the field recordings
UAF$AudioFile <- gsub("\\.Table", "", UAF$AudioFile)




# Function to find full path for a single filename
find_file_path <- function(filename, new_root) {
  pattern <- filename
  file_path <- list.files(path = new_root, pattern = pattern, 
                          recursive = TRUE, full.names = TRUE)
  if (length(file_path) > 0) {
    return(file_path)
  } else {
    return(NA)  # Return NA if file not found
  }
}


# Apply the function to each filename in data$filename
UAF$audio_path <- sapply(UAF$AudioFile, find_file_path, new_root = new_root)


# Check that all files are found
UAF$FileOk  = file.exists(UAF$audio_path) 


# Make sure all audio files are present for all annotations
if (all(UAF$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}


# Missing data, not on the google drive so remove
UAF = subset(UAF, AudioFile != '20200627_03871.wav')

# Now we have the actual filename and path as well as the deployment and 
# setup for the ecotype data



##########################################################################
# Create the required headings

# Origional Ecotype was not provided and instead based off filename. 
# KW and KW_certain are easy


# This is the actual filename, we had to faff around a bit to get it here
UAF$filename<-UAF$AudioFile

# To get the timestamps we need to merge the annotations with a separate file
# list that has the times
fileTimestamps = read.csv('E:\\DCLDE\\UAF\\Meta\\Myers_DCLDE_2026_files.csv')


UAF = merge(UAF, fileTimestamps, by.x = 'AudioFile',
            by.y='FileName', all.x=TRUE)



# UTC is the UTC plus the file offset
UAF$UTC<- as.POSIXct(UAF$UTC)+seconds(UAF$Begin.Time..s.)

UAF$Soundfile<- UAF$AudioFile
UAF$AnnotationLevel<-'Call'


ClassSpeciesList = c('KW', 'HW', 'AB', 'UndBio')
AnnotationLevelList = c('File', 'Detection', 'Call')
EcotypeList = c('SRKW', 'BKW', 'OKW', 'NRKW')

# Ok Field recordings don't have an ecotype or population with them...Based
# on the filename Resident and Southern Alaska resident
UAF$Ecotype[is.na(UAF$Ecotype)]<- 'Resident'
UAF$finalizedEcotype =as.factor(UAF$Population)

levels(UAF$finalizedEcotype)<-c('BKW', 'BKW', 'OKW', 'SAR')
UAF$Ecotype = UAF$finalizedEcotype


############################################################################
# Finish up required headings
###########################################################################
colOut = c('Soundfile','Dep','LowFreqHz','HighFreqHz','FileEndSec', 'UTC',
           'FileBeginSec','ClassSpecies','KW','KW_certain','Ecotype', 'Provider',
           'AnnotationLevel', 'FilePath', 'FileOk')


UAF$HydId<-UAF$Hyd
UAF$HydId<-as.factor(UAF$HydId)
levels(UAF$HydId)[9:59]<-'Field'


UAF$Hyd<- UAF$Location
UAF$KW = 1 # only KW annotated
UAF$ClassSpecies= 'KW' 
UAF$KW_certain = 1 # Only annotated calls that were certainly KW

UAF$Soundfile<- UAF$AudioFile
UAF$FileEndSec = UAF$Begin.Time..s.
UAF$FileEndSec = UAF$End.Time..s.
UAF$Provider <- 'UAF'
UAF$AnnotationLevel<- 'Call'
UAF$FilePath = UAF$audio_path
UAF$LowFreqHz = UAF$Low.Freq..Hz.
UAF$HighFreqHz = UAF$High.Freq..Hz.
UAF$FileBeginSec = UAF$Begin.Time..s.



runTests(UAF, EcotypeList, ClassSpeciesList)


UAF_anno= UAF[,colOut]

