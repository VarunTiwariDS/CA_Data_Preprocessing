#Check working directory and List Files in the working directory
getwd()
list.files()

#Creating a function for read all the different month's NI Crime csv files in the directory
load_all_files <- function(path) {
    files <- dir(path, pattern = '\\.csv', full.names = TRUE, recursive = TRUE)
    tables <- lapply(files, read.csv)
    do.call(rbind, tables)
}

#1) Amalgamate all of the crime data from each csv file into one dataset. Save this dataset into a csv file called AllNICrimeData.
#Calling the function to combine all the files from extarcted rar folder into a new dataframe
AllNICrimeData <- load_all_files("./NICrimeData")

#read the top 6 rows of the object AllNICrimeData using head function
head(AllNICrimeData)

#check the structure of AllNICrimeData
str(AllNICrimeData)

#write the dataframe to an output csv file and analyze the data
write.csv(AllNICrimeData, file = "AllNICrimeData.csv", row.names = FALSE)

#2) Modify the crime data in the newly created csv file (stored in AllNICrimeData) and remove the attributes:CrimeID,Reported by,Falls within,LSOA code,LSOA name, Last.outcome.category, Context.
AllNICrimeData_subset <- subset(AllNICrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name, Last.outcome.category, Context))
#Check structure of newly created dataframe with subset columns
str(AllNICrimeData_subset)

#3)Modify the AllNICrimeData dataset so that the Location attribute contains only the street name. For example, “On or near Westrock Square ” becomes “Westrock Square ”.
AllNICrimeData_subset$Location <- gsub("On or near", '', AllNICrimeData_subset$Location, ignore.case = FALSE)
AllNICrimeData_subset$Location <- trimws(AllNICrimeData_subset$Location, which = "both")
AllNICrimeData_subset$Location <- sub("^$", "No Location", AllNICrimeData_subset$Location)

#read the top 6 rows of the object AllNICrimeData_subset using head function
head(AllNICrimeData_subset)

#4) Modify each crime type attribute to contain summary information for crime per location. Delete duplicate Location records. Save the dataset as AllNICrimeDataSummary.
#Installing dplyr package for summarizing NI Crime Data set on location and reshaping it in form of pivot.
#install.packages("dplyr")
library(dplyr)
AllNICrimeData_Summary <- group_by(AllNICrimeData_subset, Location)

#using reshape package for using dcast function to reshape/pivot the data
require(reshape2)
AllNICrimeData_Summary_Pivot <- dcast(AllNICrimeData_Summary, Location ~ Crime.type)
head(AllNICrimeData_Summary_Pivot)


###Cleaning and Manipulating the Post Code Data from NIPostcodes File
#5) Loading the NIPostcodes file to a dataframe object and naming its columns
NIPostcodes <- read.csv("NIPostcodes.csv", header = FALSE)
head(NIPostcodes)
colnames(NIPostcodes) <- c('Organisation_Name', 'Sub-building_Name', 'Building_Name', 'Number', 'Primary_Thorfare', 'Alt_Thorfare',
                           'Secondary_Thorfare', 'Locality', 'Townland', 'Town', 'County', 'Postcode', 'x-coordinates',
                           'y-coordinates', 'Primary_Key_(identifier)')
#Moving the Primary Key identifier from last column to first column.
NIPostcodes <- NIPostcodes[, c(15, 1:14)]
head(NIPostcodes)
str(NIPostcodes)

#Replacing missing values from the data as some identifier.
NIPostcodes[, 2:13] <- lapply(NIPostcodes[, 2:13], as.character)
NIPostcodes[NIPostcodes == ""] <- 'No Value'

#Categorizing the County column based on the year they were created.
NIPostcodes$County <- as.factor(NIPostcodes$County)
NIPostcodes$County_Created <- ifelse(NIPostcodes$County == "ARMAGH", 'Before 1600s', ifelse(NIPostcodes$County == "TYRONE",
                                     'Before 1600s', ifelse(NIPostcodes$County == "FERMANAGH", 'Before 1600s', 'After 1600s')))

head(NIPostcodes)

####Merging both NIPostCode dataset and NICrime Data set to produce final output. We are only merging few columns from NIPostCodes

AllNICrimeData_Summary_Pivot$Location <- toupper(AllNICrimeData_Summary_Pivot$Location)
#Primary_Thorfare
FinalNICrimeData <- merge(AllNICrimeData_Summary_Pivot, NIPostcodes[, c("Primary_Thorfare", "Town", "County", "Postcode")],
                          by.x = 'Location', by.y = 'Primary_Thorfare')
#keeping unique data from  final FinalNICrimeData
FinalNICrimeData <- unique(FinalNICrimeData)

head(FinalNICrimeData)
str(FinalNICrimeData)

#6)Rearranging the columns and writing the final output in the csv file FinalNICrimeData
write.csv(FinalNICrimeData[, c(1, 16, 17, 18, 2:15)], file = "FinalNICrimeData.csv", row.names = FALSE)
