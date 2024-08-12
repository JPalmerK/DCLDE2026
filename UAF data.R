# Reorganize UAF data


#############################################################################
# University of Alaska this section moves the files into deployments
#############################################################################
library(dplyr)
library(lubridate)
# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'E:/DCLDE/UAF/Annotations/',
                        pattern = '.txt', full.names = TRUE,
                        recursive = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
UAF <- do.call(rbind, lapply(file_list, function(file) {
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

root_dir ='E:\\UAF\\Myers_DCLDE_2026_killer_whale_data'


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
UAF$audio_path <- sapply(UAF$AudioFile, find_file_path, root_dir = root_dir)

# Ok now create a new path based on the hydrophone location
UAF$NewLoc = file.path(root_dir, UAF$Hyd)

# copy the files
for(ii in 1:nrow(UAF)){
  new_location = UAF$NewLoc[ii]
  # check that it needs to be moed
  if(!is.na(UAF$audio_path[ii])){
    if (!file.exists(new_location)) {
      dir.create(new_location, recursive = TRUE)  # Create the directory if it doesn't exist
    }
    file.copy(UAF$audio_path[ii], new_location, overwrite = TRUE)  # Copy the file to the new location
  }
}

###############################################################################
# Now the files are reorganized, this section adds the required headings as 
# per June 5th email
###############################################################################


# Get a list of annotation files
file_list <- list.files(path = 'E:/DCLDE/UAF/Annotations/',
                        pattern = '.txt', full.names = TRUE,
                        recursive = TRUE)



# Read and concatenate the selection tables  with filename as a separate column (if non-empty)
UAF <- do.call(rbind, lapply(file_list, function(file) {
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
UAF$AudioFile <- gsub("\\.Table", "", UAF$AudioFile)


#Correct the deployment
UAF$Dep<-UAF$Hyd
levels(UAF$Dep)[9:59]<-'Field' # do this first otherwise it changes the length
levels(UAF$Dep)[1:8]<-c('RB','MS', 'KB', 'KB', 'MS', 'HE','KB', 'MS')


root_dir ='E:\\DCLDE\\UAF\\Audio'

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
UAF$audio_path <- sapply(UAF$AudioFile, find_file_path, root_dir = root_dir)


###########################################################################
# QA QC#
###########################################################################
# Check that regions aren't duplicated within a hydrophone
hydList = unique(UAF$Hyd)[1:8]
regList = unique(UAF$Dep)
for(ii in 1:length(hydList)){
  regions = unique(UAF$Dep[UAF$Hyd== hydList[ii]])
  print(regions)
}

for(ii in 1:length(regList)){
  hyds = unique(UAF$Hyd[UAF$Dep== regList[ii]])
  print(paste(regList[ii],hyds))
}
##########################################################################

# Ecotype was based off filename
UAF$EcotypeOrig = as.factor(UAF$FolderName)

levels(UAF$EcotypeOrig)<-c('AT1', 'GAT', 'SAR', 'Offshore')
UAF$Ecotype<-UAF$EcotypeOrig
UAF$KW = 1
UAF$ClassSpecies= 'KW'
UAF$KW_certain = 1


# Make consistant with the rest of the labels- add SAR to manuscript levels
levels(UAF$Ecotype)<-c('AT1','GAT','SAR', 'OKW')
UAF$filename<-UAF$AudioFile

# Merge the files
UAF = merge(UAF, fileList[, c('FileName', 'UTC')], by.x = 'AudioFile',
            by.y='FileName', all.x=TRUE)



#Timestampes are different between field and soundtraps
UAF_ST = subset(UAF, Dep!='Field')
UAF_field = subset(UAF, Dep =='Field')


# Link the file with the list of files for proper UTC calculations
fileList = read.csv('E:\\DCLDE\\UAF\\Myers_DCLDE_2026_killer_whale_data/Myers_DCLDE_2026_files.csv')
fileList$UTC = as.POSIXct(fileList$UTC)




UAF_ST <- UAF_ST %>%
  mutate(
    # Extracting date from filename
    date_str = sub(".*\\.(\\d{12})\\.wav", "\\1", filename),
    
    # Convert extracted date string to POSIXct format
    UTC = as.POSIXct(date_str, format = "%y%m%d%H%M%S", tz = "UTC")+seconds(UAF_ST$Begin.Time..s.)
  )

# Field recordings are different

UAF_ST <- UAF_ST %>%
  mutate(
    # Extracting date from filename
    
    # Convert extracted date string to POSIXct format
    UTC = as.POSIXct(date_str, format = "%y%m%d%H%M%S", tz = "UTC")+seconds(UAF_ST$Begin.Time..s.)
  )



# Ok now get the file locations

root_dir ='E:\\UAF\\Myers_DCLDE_2026_killer_whale_data/Audio/'



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
UAF$audio_path <- sapply(UAF$AudioFile, find_file_path, root_dir = root_dir)

# Report ecotype
UAF$Ecotype = as.factor(UAF$kw_ecotype)






# Cape Elizabeth and Quinault Canyon
levels(UAF$Dep)<-c("AT1", "GAT", 'Offshore', 'SAR')

#Create the orgional ecotypes
UAF$OrigEcotype<-UAF$Dep





# Cape Elizabeth and Quinault Canyon
levels(UAF$Dep)<-c("AT1", "GAT", 'Offshore', 'SAR')

# Set the initial ecotypes then match the format
levels()

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
scripps$FileOk=TRUE


scripps = scripps[, colOut]
runTests(scripps, EcotypeList, ClassSpeciesList)