
# Formatting the raw root necrosis data from PlantCV ----------------------

#Merging of raw image datasets from plantCV 

#Load libraries needed
library(tidyverse)
library(dplyr)
library(sommer)
library(lme4)
library(lmerTest)
library(lmtest)
library(tidyr)
library(readr)
library(ggpubr)
library(minque)




# Namulonge 2019/2020 -----------------------------------------------------


Namulonge_2020_formatted <- read.csv("Namulonge_2020_formatted.csv")
dim(Namulonge_2020_formatted ) #19532    29
library(data.table)




Namulonge_2020_for1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', Namulonge_2020_formatted$Image.ID), ' ')))

#Naming the columns 
names(Namulonge_2020_for1)[1] <- "ID"
names(Namulonge_2020_for1)[2] <- "Other"
dim(Namulonge_2020_for1) #19532     2

Names_ids <- Namulonge_2020_formatted[ , c(1,2)]   

data_table_1 = data.table(Namulonge_2020_for1)
data_table_2 = data.table(Names_ids)


DATA2 <-cbind(data_table_1, data_table_2)



Namulonge_2020_for1.1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', DATA2$ID), ' ')))

#Naming the columns 
names(Namulonge_2020_for1.1)[1] <- "Plot_id"
names(Namulonge_2020_for1.1)[2] <- "Plant_Num"
dim(Namulonge_2020_for1.1) #19532     2

Names_ids.1 <- Namulonge_2020_formatted[ , c(1,2)]   

data_table_1.1 = data.table(Namulonge_2020_for1.1)
data_table_2.1 = data.table(Names_ids.1)


DATA2.1 <-cbind(data_table_1.1, data_table_2.1)

Namulonge_2020_for2 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Namulonge_2020_for1$Other), ' ')))
names(Namulonge_2020_for2)[1] <- "Root_number"
names(Namulonge_2020_for2)[2] <- "Other1"
data_table_3 = data.table(Namulonge_2020_for2)
DATA3 <-cbind(DATA2, data_table_3)

Namulonge_2020_for3 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Namulonge_2020_for2$Other1), ' ')))
names(Namulonge_2020_for3)[1] <- "Image_number"
names(Namulonge_2020_for3)[2] <- "Other2"
data_table_4 = data.table(Namulonge_2020_for3)
DATA4 <-cbind(DATA3, data_table_4)
DATA5 <-cbind(DATA4, DATA2.1)
write_csv(DATA5, file = "Namulonge_2020_map.csv")
names(DATA5)[11] <- "Xs"
names(DATA5)[12] <- "dropped"
DATA5 <- subset(DATA5, select = -c(X,Xs,dropped))

Root_necrosis_Namulonge_2020 <- merge(Namulonge_2020_formatted,DATA5, by="Image.ID")
dim(Root_necrosis_Namulonge_2020) #19532    37

Root_necrosis_Namulonge_2020



####add year location in the file

length_of_column <- length(Root_necrosis_Namulonge_2020$Image.ID)

Root_necrosis_Namulonge_2020$Location <- rep('Namulonge', length_out = length_of_column)

Root_necrosis_Namulonge_2020$Year <- rep('2019', length_out = length_of_column)






# Namulonge 2020/2021 -----------------------------------------------------




Namulonge_2021_formatted <- read.csv("Data/Namulonge_2021_formatted.csv")
dim(Namulonge_2021_formatted) #15240    29
Namulonge_2021_for1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', Namulonge_2021_formatted$Image.ID), ' ')))

#Naming the columns 
names(Namulonge_2021_for1)[1] <- "ID"
names(Namulonge_2021_for1)[2] <- "Other"
dim(Namulonge_2021_for1) #15240     2


Names_ids_2021 <- Namulonge_2021_formatted[ , c(1,2)]   

data_table_1_2021 = data.table(Namulonge_2021_for1)
data_table_2_2021 = data.table(Names_ids_2021)



DATA2_2021 <-cbind(data_table_1_2021, data_table_2_2021)


Namulonge_2021_for1.1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', DATA2_2021$ID), ' ')))

#Naming the columns 
names(Namulonge_2021_for1.1)[1] <- "Plot_id"
names(Namulonge_2021_for1.1)[2] <- "Plant_Num"
dim(Namulonge_2021_for1.1) #15240     2

Names_ids.1 <- Namulonge_2021_formatted[ , c(1,2)]   

data_table_1.1 = data.table(Namulonge_2021_for1.1)
data_table_2.1 = data.table(Names_ids.1)


DATA2.11 <-cbind(data_table_1.1, data_table_2.1)






Namulonge_2021_for2 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Namulonge_2021_for1$Other), ' ')))
names(Namulonge_2021_for2)[1] <- "Root_number"
names(Namulonge_2021_for2)[2] <- "Other1"
data_table_3_2021 = data.table(Namulonge_2021_for2)
DATA3_2021 <-cbind(DATA2_2021, data_table_3_2021)

Namulonge_2021_for3 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Namulonge_2021_for2$Other1), ' ')))
names(Namulonge_2021_for3)[1] <- "Image_number"
names(Namulonge_2021_for3)[2] <- "Other2"
data_table_4_2021 = data.table(Namulonge_2021_for3)
DATA4_2021 <-cbind(DATA3_2021, data_table_4_2021)

DATA5_2021 <-cbind(DATA4_2021, DATA2.11)
dim(DATA5_2021) # 15240    12
write_csv(DATA5_2021, file = "Namulonge_2021_map.csv")

names(DATA5_2021)[11] <- "Xs"
names(DATA5_2021)[12] <- "dropped"
DATA5_2021 <- subset(DATA5_2021, select = -c(X,Xs,dropped))


Root_necrosis_Namulonge_2021 <- merge(Namulonge_2021_formatted,DATA5_2021, by="Image.ID")
dim(Root_necrosis_Namulonge_2021) #15240    37





####add year location in the file

length_of_column <- length(Root_necrosis_Namulonge_2021$Image.ID)

Root_necrosis_Namulonge_2021$Location <- rep('Namulonge', length_out = length_of_column)

Root_necrosis_Namulonge_2021$Year <- rep('2020', length_out = length_of_column)





# extracting the namulonge code for 2019 to get the gerplasm name that will be used in creating the codes for 2020
Namulonge_2019 <- read.csv("Namulonge_2019.csv")
dim(Namulonge_2019) #3592   33


Name_codes2019 <- Namulonge_2019 %>% dplyr::select(germplasmName , observationUnitName)

names(Root_necrosis_Namulonge_2021)[names(Root_necrosis_Namulonge_2021) == "ID"] <- "observationUnitName"


#FINDING UNIQUE VALUES 




Root_necrosis_Namulonge_2021.1 <-merge(Root_necrosis_Namulonge_2021, Name_codes2019, by="observationUnitName", all.x  = TRUE)

Root_necrosis_Namulonge_2021.2 <- subset (Root_necrosis_Namulonge_2021.1, select = -observationUnitName)

dim(Root_necrosis_Namulonge_2021.2) #15240    39



####add year location in the file

length_of_column <- length(Root_necrosis_Namulonge_2021.2$Image.ID)

Root_necrosis_Namulonge_2021.2$Location <- rep('Namulonge', length_out = length_of_column)

Root_necrosis_Namulonge_2021.2$Year <- rep('2020', length_out = length_of_column)






# Serere 2019/2020 --------------------------------------------------------




#serere
Serere_2020_formatted <- read.csv("Serere_2020_formatted.csv")
dim(Serere_2020_formatted)#9949   29


Serere_2020_for1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', Serere_2020_formatted$Image.ID), ' ')))

#Naming the columns 
names(Serere_2020_for1)[1] <- "ID"
names(Serere_2020_for1)[2] <- "Other"
dim(Serere_2020_for1) #9949    2

Names_ids_serere <- Serere_2020_formatted[ , c(1,2)]   

data_table_1_serere = data.table(Serere_2020_for1)
data_table_2_serere = data.table(Names_ids_serere)



DATA2_serere <-cbind(data_table_1_serere, data_table_2_serere)




Serere_2020_for1.1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', DATA2_serere$ID), ' ')))

#Naming the columns 
names(Serere_2020_for1.1)[1] <- "Plot_id"
names(Serere_2020_for1.1)[2] <- "Plant_Num"
dim(Serere_2020_for1.1) #9949    2

Names_ids_serere.1 <- Serere_2020_formatted[ , c(1,2)]   

data_table_1.1 = data.table(Serere_2020_for1.1)
data_table_2.1 = data.table(Names_ids_serere.1)


DATA2.11 <-cbind(data_table_1.1, data_table_2.1)

dim(DATA2.11)#9949    4









Serere_2020_for2 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2020_for1$Other), ' ')))
names(Serere_2020_for2)[1] <- "Root_number"
names(Serere_2020_for2)[2] <- "Other1"
data_table_3_serere = data.table(Serere_2020_for2)
DATA3_serere <-cbind(DATA2_serere, data_table_3_serere)

Serere_2020_for3 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2020_for2$Other1), ' ')))
names(Serere_2020_for3)[1] <- "Image_number"
names(Serere_2020_for3)[2] <- "Other2"
data_table_4_serere = data.table(Serere_2020_for3)


DATA4_serere <-cbind(DATA3_serere, data_table_4_serere)

DATA5_serere <-cbind(DATA4_serere, DATA2.11)




write_csv(DATA5_serere, file = "Serere_2020_map.csv")


names(DATA5_serere)[11] <- "Xs"
names(DATA5_serere)[12] <- "dropped"
DATA5_serere <- subset(DATA5_serere, select = -c(X,Xs,dropped))



Root_necrosis_Serere_2020 <- merge(Serere_2020_formatted,DATA5_serere, by="Image.ID")

dim(Root_necrosis_Serere_2020) #9949   37

####add year location in the file

length_of_column <- length(Root_necrosis_Serere_2020 $Image.ID)


Root_necrosis_Serere_2020 $Location <- rep('Serere', length_out = length_of_column)
Root_necrosis_Serere_2020$Year <- rep('2019', length_out = length_of_column)


























# Serere 2020/ 2021--------------------------------------------------------


Serere_2021_formatted <- read.csv("Serere_2021_formatted.csv")
dim(Serere_2021_formatted) #5131   29


Serere_2021_2_formatted <- read.csv("Serere_2021_2_formatted.csv")
dim(Serere_2021_2_formatted)#1522   29




Serere_2021_2_for1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', Serere_2021_2_formatted$Image.ID), ' ')))

#Naming the columns 
names(Serere_2021_2_for1)[1] <- "ID"
names(Serere_2021_2_for1)[2] <- "Other"
dim(Serere_2021_2_for1) #1522    2

Names_ids <- Serere_2021_2_formatted[,c(1,2)]   

data_table_1 = data.table(Serere_2021_2_for1)
data_table_2 = data.table(Names_ids)





DATA2 <-cbind(data_table_1, data_table_2)



Serere_2021_2_for1.1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', DATA2$ID), ' ')))

df1_separated <- Serere_2021_2_for1.1%>%
  separate(X2, into = c("Plot_id", "Plant_Num"), sep = "_")

dim(df1_separated)

#Naming the columns 
#names(Serere_2021_2_for1.1)[1] <- "Plot_id"
#names(Serere_2021_2_for1.1)[2] <- "Plant_Num"
#dim(Serere_2021_2_for1.1) #1522    2

#Names_ids.1 <- Serere_2021_2_formatted[,c(1,2)]   

#data_table_1.1 = data.table(Serere_2021_2_for1.1)
#data_table_2.1 = data.table(Names_ids.1)


#DATA2.1 <-cbind(data_table_1.1, data_table_2.1)

Serere_2021_2_for2 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2021_2_for1$Other), ' ')))
names(Serere_2021_2_for2)[1] <- "Root_number"
names(Serere_2021_2_for2)[2] <- "Other1"
data_table_3 = data.table(Serere_2021_2_for2)
DATA3 <-cbind(DATA2, data_table_3)

Serere_2021_2_for3 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2021_2_for2$Other1), ' ')))
names(Serere_2021_2_for3)[1] <- "Image_number"
names(Serere_2021_2_for3)[2] <- "Other2"
data_table_4 = data.table(Serere_2021_2_for3)
DATA4 <-cbind(DATA3, data_table_4)

DATA5 <-cbind(DATA4, df1_separated)

dim(DATA5)
write_csv(DATA4, file = "Serere_2021_2_map.csv")


dataserere_2021_necrosis <-merge(Serere_2021_2_formatted, DATA5, by="Image.ID")
dim(dataserere_2021_necrosis) #1522   39

dataserere_2021_necrosis <- subset(dataserere_2021_necrosis, select = -X.y)

dataserere_2021_necrosis <- dataserere_2021_necrosis %>% rename(X = X.x)
dim(dataserere_2021_necrosis) #1522   35


##############################################location year effects 

length_of_column <- length(dataserere_2021_necrosis$Image.ID)


dataserere_2021_necrosis$Location <- rep('Serere', length_out = length_of_column)
dataserere_2021_necrosis$Year <- rep('2020', length_out = length_of_column)

dim(dataserere_2021_necrosis) #1522   40






#second file 
#this is file observation ids were use 
dim(Serere_2021_formatted) #5131   29

Serere_2021_for1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2021_formatted$Image.ID), ' ')))

#Naming the columns 
names(Serere_2021_for1)[1] <- "ID"
names(Serere_2021_for1)[2] <- "Other"
dim(Serere_2021_for1) #5131    2

Names_ids <- Serere_2021_formatted[,c(1,2)]   

data_table_1 = data.table(Serere_2021_for1)
data_table_2 = data.table(Names_ids)





DATA2_2021_serere <-cbind(data_table_1, data_table_2)




Serere_2021_for2 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2021_for1$Other), ' ')))
names(Serere_2021_for2)[1] <- "Root_number"
names(Serere_2021_for2)[2] <- "Other1"
data_table_3 = data.table(Serere_2021_for2)
DATA3 <-cbind(DATA2_2021_serere, data_table_3)


Serere_2021_for3 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2021_2_for2$Other1), ' ')))
names(Serere_2021_for3)[1] <- "Image_number"
names(Serere_2021_for3)[2] <- "Other2"
data_table_4 = data.table(Serere_2021_for3)

DATA4 <-cbind(DATA3, data_table_4)
dim(DATA4) #5131    8

write_csv(DATA4, file = "Serere_2021_version2_map.csv")






#getting the plot id 
library(dplyr)
Serere_2020 <- read.csv("Serere_2020.csv")
dim(Serere_2020)#3018   33

Namings_id <- Serere_2020 %>% dplyr::select(observationUnitDbId , observationUnitName)

names(DATA4)[1] <- "observationUnitDbId"

DATA4$observationUnitDbId <- as.numeric(DATA4$observationUnitDbId )
dim(DATA4) #5131    8

Dataset_serere1 <- merge(DATA4,Namings_id, by= "observationUnitDbId", all.x = TRUE)
dim(Dataset_serere1) # 5131    9

Serere_necrosis_2021_part1 <- merge(Dataset_serere1,Serere_2021_formatted,by="Image.ID")
dim(Serere_necrosis_2021_part1) #5131   37




######BECAUSE THERE WERE NO PLOTS ATTACHED USE THE OBSERVATION UNIT NAME TO EXTRACT THE PLOT AND PLANT NUMBER 

Serere_2021_part_1.2 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+_[^_]+_[^_]+_[^_]+_[^_]+)_(.*)$', '\\1 \\2', Serere_necrosis_2021_part1$observationUnitName), ' ')))


df_separated <- Serere_2021_part_1.2 %>%
  separate(X2, into = c("Plot_id", "Plant_Num"), sep = "_")



#result <- data.frame(First_Column = split_string[1], Second_Column = split_string[2])


#split_string <- strsplit(Serere_2021_part_1.1$X2, "_")[[1]]
#Serere_2021_part_1.1$Plot_id <- split_string[1]
#Serere_2021_part_1.1$Plant_Num <- split_string[2]


DATA6_serere <- subset(df_separated, select = - X1)

DATA6_serere$observationUnitName <-Serere_necrosis_2021_part1$observationUnitName



Serere_2021_2 <-bind_cols(Serere_necrosis_2021_part1, DATA6_serere)

dim(Serere_2021_2)

length_of_column <- length(Serere_2021_2$Image.ID)

Serere_2021_2 $Location <- rep('Serere', length_out = length_of_column)

Serere_2021_2 $Year <- rep('2020', length_out = length_of_column)


##################################################################merge final dataset 

dim(Root_necrosis_Namulonge_2020) #19532    39
dim(Root_necrosis_Namulonge_2021.2) #15240    39
dim(Root_necrosis_Serere_2020) #9949   39
dim(dataserere_2021_necrosis) #1522   40
dim(Serere_2021_2) #5131   42




Root1 <- merge(Root_necrosis_Namulonge_2020,Root_necrosis_Namulonge_2021.2, all = TRUE)
dim(Root1)


Root2 <- merge(Root1,Root_necrosis_Serere_2020, all = TRUE)
dim(Root2) #44721



Root3 <- merge(Root2,dataserere_2021_necrosis, all = TRUE)
dim(Root3) #46243    41



Root4 <- merge(Root3,Serere_2021_2, all = TRUE)
dim(Root4) # 51374    46



dim(Namulonge_2020_formatted) #19532    29
dim(Namulonge_2021_formatted) #15240    29
dim(Serere_2020_formatted) #9949   29
dim(Serere_2021_formatted) #5131   29
dim(Serere_2021_2_formatted )#1522   29


#total  <- 51374


# Merging CBSD severity data from the 1-5 scoring method ------------------




######## Datasets 
Namulonge2020 <- read.csv("7795_layout.csv")
dim(Namulonge2020) #402  26

serere2020 <- read.csv("7746_layout.csv")

dim(serere2020) #398  28

serere2019 <- read.csv("7071_layout.csv")
dim(serere2019) # 440  27 

Namulonge2019<- read.csv("6492_layout.csv")
dim(Namulonge2019) #486  27





#######LOAD THE 2020 NAMULONGE


Namulonge_data2020 <- select(Namulonge2020, accession_name, plot_name, block_number, plot_number)

dim(Namulonge_data2020)





# Subset rows based on a condition
subset_df <-CBSD_root_necrosis %>% filter(Location== "Namulonge" , Year=="2020")

dim(subset_df) #15240    16

dim(Namulonge_data2020) #402   4

subset_df1<- subset(subset_df, select = -c(Plot_id,ID ))
#Plot_id
rowname_counts <- table(subset_df1$germplasmName)




# Assuming your dataset is a data frame named 'df' and you want to count occurrences of specific row names
#row_names_to_count <- c("UG110017", "Mkumba", "TMEB204")  # Replace these with the row names you want to count

# Count occurrences of specified row names
#row_name_counts <- table(rownames(subset_df$germplasmName) %in% row_names_to_count)

# Print the counts
#print(row_name_counts)

#ID 
Namulonge_data2020<- Namulonge_data2020 %>%
  rename(germplasmName = accession_name)


Namulonge_data2020_merge <- inner_join(subset_df1,Namulonge_data2020, by= "germplasmName" )
dim(Namulonge_data2020_merge)


rowname_counts <- table(Namulonge_data2020_merge$germplasmName)


rowname_counts1 <- table(subset_df$germplasmName)


# Find row indices of rows with row name "A"
rows_to_remove <- which(Namulonge_data2020_merge$germplasmName == "UG110017")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_remove) >= 15694) {
  rows_to_remove <- rows_to_remove[1:14573]
}

# Remove the specified rows
Namulonge_data2020_merge <- Namulonge_data2020_merge[-rows_to_remove, , drop = FALSE]



# Find row indices of rows with row name "A"
rows_to_remove1 <- which(Namulonge_data2020_merge$germplasmName == "TMEB204")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_remove1) >= 6468) {
  rows_to_remove1 <- rows_to_remove1[1:6006]
}

# Remove the specified rows
Namulonge_data2020_merge1 <- Namulonge_data2020_merge[-rows_to_remove1, , drop = FALSE]
dim(Namulonge_data2020_merge1)

dim(Namulonge_data2020_merge)


# Find row indices of rows with row name "A"
rows_to_remove2 <- which(Namulonge_data2020_merge$germplasmName == "Mkumba")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_remove2) >= 9002) {
  rows_to_remove2 <- rows_to_remove2[1:8359]
}

# Remove the specified rows
Namulonge_data2020_merge2 <- Namulonge_data2020_merge1[-rows_to_remove2, , drop = FALSE]
dim(Namulonge_data2020_merge2 ) # 15906    17



rowname_counts
Namulonge_data2020_merge


################to solve those that are different we will merge 

View(Namulonge_data2020_merge2)

Namulonge_data2020_merge2<- Namulonge_data2020_merge2 %>%
  rename(Plot_id = plot_name)


dim(Namulonge_data2020_merge2)

unique_values <- unique(Namulonge_data2020_merge2$Root_number) ####right
unique_values

unique_values1 <- unique(Namulonge_data2020_merge2$Image_number) ####right
unique_values1

#####RANDOMLY REMOVE # Number of rows to remove
rows_to_remove <- 666

# Generate random indices to remove
indices_to_remove <- sample(1:nrow(Namulonge_data2020_merge2), rows_to_remove)

# Remove the randomly selected rows
subset_data <- Namulonge_data2020_merge2[-indices_to_remove, ]

# Print the dimensions of the subsetted data to confirm 600 rows are removed
print(dim(subset_data))


dim(subset_df1)

Final_Namulonge_2020 <- subset_data


dim(Final_Namulonge_2020)


#########Namulonge2019
Namulonge_2019_subset <-CBSD_root_necrosis %>% filter(Location== "Namulonge" , Year=="2019")

dim(Namulonge_2019_subset) #19532    16

dim(Namulonge2019) #486  27


Namulonge_data2019 <- select(Namulonge2019, accession_name, plot_name, block_number, plot_number)

dim(Namulonge_data2019)

Namulonge_data2019 <- Namulonge_data2019 %>%
  rename(germplasmName = accession_name, Plot_id = plot_name)




Namulonge_data2019_merge <- inner_join(Namulonge_2019_subset,Namulonge_data2019, by= "Plot_id" )
dim(Namulonge_data2019_merge)

Namulonge_data2019_merge <- subset(Namulonge_data2019_merge, select = -c(germplasmName.x))

Namulonge_data2019_merge <- Namulonge_data2019_merge %>%
  rename(germplasmName = germplasmName.y)


dim(Namulonge_data2019_merge)#19532    18




#####serere


#2019
Serere_2019_subset <-CBSD_root_necrosis %>% filter(Location== "Serere" , Year=="2019")

dim(Serere_2019_subset) #9949   16

dim(serere2019) #440  27


Serere_data2019 <- select(serere2019, accession_name, plot_name, block_number, plot_number)

dim(Serere_data2019)

Serere_data2019<- Serere_data2019 %>%
  rename(germplasmName = accession_name, Plot_id = plot_name)




Serere_data2019_merge <- inner_join(Serere_2019_subset,Serere_data2019 , by= "Plot_id" )
dim(Serere_data2019_merge)





#2020
Serere_2020_subset <-CBSD_root_necrosis %>% filter(Location== "Serere" , Year=="2020")

dim(Serere_2020_subset) #6653   16
dim(serere2020) #398  28


Serere_data2020 <- select(serere2020, accession_name, plot_name, block_number, plot_number)

dim(Serere_data2020)

Serere_data2020<- Serere_data2020 %>%
  rename(germplasmName = accession_name, Plot_id = plot_name)



########################################


# Serere_2021_data --------------------------------------------------------


Serere_2021_formatted <- read.csv("Serere_2021_formatted.csv")
dim(Serere_2021_formatted) #5131   29


#second file 
#this is file observation ids were use 
dim(Serere_2021_formatted) #5131   29

Serere_2021_for1 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2021_formatted$Image.ID), ' ')))

#Naming the columns 
names(Serere_2021_for1)[1] <- "ID"
names(Serere_2021_for1)[2] <- "Other"
dim(Serere_2021_for1) #5131    2

Names_ids <- Serere_2021_formatted[,c(1,2)]   
library(data.table)
data_table_1 = data.table(Serere_2021_for1)
data_table_2 = data.table(Names_ids)


DATA2_2021_serere <-cbind(data_table_1, data_table_2)


Serere_2021_for2 <- data.frame(do.call("rbind", strsplit(sub('(^[^_]+)_(.*)$', '\\1 \\2', Serere_2021_for1$Other), ' ')))
names(Serere_2021_for2)[1] <- "Root_number"
names(Serere_2021_for2)[2] <- "Other1"
data_table_3 = data.table(Serere_2021_for2)
DATA3 <-cbind(DATA2_2021_serere, data_table_3)


#getting the plot id 
library(dplyr)
Serere_2020 <- read.csv("Serere_2020.csv")
dim(Serere_2020)#3018   33

Namings_id <- Serere_2020 %>% dplyr::select(observationUnitDbId , observationUnitName)

names(DATA3)[1] <- "observationUnitDbId"

DATA3$ID <- as.numeric(DATA3$ID )
dim(DATA3) #5131    8


Namings_id<- Namings_id %>%
  rename( ID = observationUnitDbId )

Dataset_serere1 <- merge(DATA3,Namings_id, by= "ID", all.x = TRUE)
dim(Dataset_serere1) # 5131    9

replacement_names <-Dataset_serere1$observationUnitName

Serere_2020_subset
Serere_2020_subset


# Replace NA values in column1 with replacement_names
Serere_2020_subset$ID <- ifelse(is.na(Serere_2020_subset$ID), replacement_names, Serere_2020_subset$ID)



# Print the updated data frame
print(Serere_2020_subset)





###################################################################

# Separate the column variable
Serere_2020_subset$Plot_id <- sub("_\\d+$", "", Serere_2020_subset$ID)
Serere_2020_subset$number <- sub(".*_(\\d+)$", "\\1", Serere_2020_subset$ID)

Serere_data2020_merge <- merge(Serere_2020_subset,Serere_data2020 , by= "Plot_id" )
dim(Serere_data2020_merge)

Serere_data2020_merge <- subset(Serere_data2020_merge, select = -c(germplasmName.x))

Serere_data2020_merge <- Serere_data2020_merge %>%
  rename(germplasmName = germplasmName.y)



rowname_counts3 <- table(Serere_data2020_merge$germplasmName)


dim(Final_Namulonge_2020)
dim(Namulonge_data2019_merge)
dim(Serere_data2019_merge)
dim(Serere_data2020_merge)



Root1 <- merge(Final_Namulonge_2020,Namulonge_data2019_merge, all = TRUE)
dim(Root1) #34772    20


Root2 <- merge(Serere_data2019_merge,Serere_data2020_merge, all = TRUE)
dim(Root2) 


Root3<- merge(Root1 ,Root2, all = TRUE)
dim(Root3) 




########use plot data for unique id 



Namu_data2020 <- select(Namulonge2020, accession_name,cassava.brown.streak.disease.leaf.severity.3.month.evaluation.CO_334.0000204, cassava.brown.streak.disease.leaf.severity.6.month.evaluation.CO_334.0000205, cassava.brown.streak.disease.root.severity.12.month.evaluation.CO_334.0000201)

dim(Namu_data2020)


library(dplyr)

# Rename the columns
Namu_data2020 <- Namulonge2020 %>%
  select(
    germplasmName = accession_name,
    CBSDs3 = cassava.brown.streak.disease.leaf.severity.3.month.evaluation.CO_334.0000204,
    CBSDs6 = cassava.brown.streak.disease.leaf.severity.6.month.evaluation.CO_334.0000205,
    CBSDs12 = cassava.brown.streak.disease.root.severity.12.month.evaluation.CO_334.0000201
  )


Datas_nam2020 <- merge(Final_Namulonge_2020,Namu_data2020 , by= "germplasmName", all.x = TRUE )

dim(Datas_nam2020)


rowname_counts <- table(Final_Namulonge_2020$germplasmName)


rowname_counts1 <- table(Datas_nam2020 $germplasmName)


# Find row indices of rows with row name "A"
rows_to_remove <- which(Datas_nam2020 $germplasmName == "UG110017")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_remove) >= 15204) {
  rows_to_remove <- rows_to_remove[1:14118]
}

# Remove the specified rows
Datas_nam2020  <- Datas_nam2020 [-rows_to_remove, , drop = FALSE]
dim(Datas_nam2020)






# Find row indices of rows with row name "A"
rows_to_remove1 <- which(Datas_nam2020 $germplasmName == "TMEB204")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_remove1) >= 3164) {
  rows_to_remove1 <- rows_to_remove1[1:2938]
}

# Remove the specified rows
Datas_nam2020_1 <- Datas_nam2020 [-rows_to_remove1, , drop = FALSE]
dim(Datas_nam2020_1)

dim(Datas_nam2020 )



# Find row indices of rows with row name "A"
rows_to_remove2 <- which(Datas_nam2020$germplasmName == "Mkumba")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_remove2) >= 84308) {
  rows_to_remove2 <- rows_to_remove2[1:78286]
}

# Remove the specified rows
Datas_nam2020_2 <- Datas_nam2020_1[-rows_to_remove2, , drop = FALSE]
dim(Datas_nam2020_2 ) # 15906    17


######### Serere 2020


Seeredata2020 <- serere2020 %>%
  select(
    germplasmName = accession_name,
    CBSDs3 = cassava.brown.streak.disease.leaf.severity.3.month.evaluation.CO_334.0000204,
    CBSDs6 = cassava.brown.streak.disease.leaf.severity.6.month.evaluation.CO_334.0000205,
    CBSDs12 = cassava.brown.streak.disease.root.severity.12.month.evaluation.CO_334.0000201
  )


Datas_sere2020 <- merge(Serere_data2020_merge, Seeredata2020 , by= "germplasmName",all.x = TRUE )

dim(Datas_sere2020)


rowname_counts <- table(Serere_data2020_merge$germplasmName)


rowname_counts1 <- table(Datas_sere2020$germplasmName)





# Find row indices of rows with row name "A"
rows_to_removesere <- which(Datas_sere2020 $germplasmName == "UG110017")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removesere) >= 1040) {
  rows_to_removesere <- rows_to_removesere[1:960]
}

# Remove the specified rows
Datas_sere2020  <-Datas_sere2020 [-rows_to_removesere, , drop = FALSE]
dim(Datas_sere2020)






# Find row indices of rows with row name "A"
rows_to_removesere1 <- which(Datas_sere2020 $germplasmName == "TMEB204")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removesere1) >=52) {
  rows_to_removesere1 <- rows_to_removesere1[1:48]
}

# Remove the specified rows
Datas_sere2020_1 <- Datas_sere2020 [-rows_to_removesere1, , drop = FALSE]
dim(Datas_sere2020_1)





# Find row indices of rows with row name "A"
rows_to_removesere2 <- which(Datas_sere2020$germplasmName == "Mkumba")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removesere2) >= 572) {
  rows_to_removesere2<- rows_to_removesere2[1:528]
}

# Remove the specified rows
Datas_sere2020_2 <- Datas_sere2020[-rows_to_removesere2,, drop = FALSE]
dim(Datas_sere2020_2 ) # 15906    17


#########Serere 2019

Seeredata2019 <- serere2019 %>%
  select(
    germplasmName.y = accession_name,
    CBSDs3 = cassava.brown.streak.disease.leaf.severity.3.month.evaluation.CO_334.0000204,
    CBSDs6 = cassava.brown.streak.disease.leaf.severity.6.month.evaluation.CO_334.0000205,
    CBSDs12 = cassava.brown.streak.disease.root.severity.12.month.evaluation.CO_334.0000201
  )


Datas_sere2019 <- merge(Serere_data2019_merge, Seeredata2019 , by= "germplasmName.y",all.x = TRUE )

dim(Datas_sere2019)


rowname_counts <- table(Serere_data2019_merge$germplasmName.y)


rowname_counts1 <- table(Datas_sere2019$germplasmName.y)


# Find row indices of rows with row name "A"
rows_to_removesere2019 <- which(Datas_sere2019 $germplasmName.y == "UG110017")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removesere2019 ) >= 17160) {
  rows_to_removesere2019  <- rows_to_removesere2019 [1:16380]
}

# Remove the specified rows
Datas_sere2019  <-Datas_sere2019 [-rows_to_remove, , drop = FALSE]
dim(Datas_sere2019)


# Find row indices of rows with row name "A"
rows_to_removesere20191 <- which(Datas_sere2019 $germplasmName.y == "TMEB204")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removesere20191) >=5830 ) {
  rows_to_removesere20191 <- rows_to_removesere20191[1:5565]
}

# Remove the specified rows
Datas_sere2019_1 <- Datas_sere2019 [-rows_to_removesere20191, , drop = FALSE]
dim(Datas_sere2019_1)





# Find row indices of rows with row name "A"
rows_to_removesere20192 <- which(Datas_sere2019$germplasmName.y == "Mkumba")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removesere20192) >= 10692) {
  rows_to_removesere20192 <- rows_to_removesere20192[1:10206]
}

# Remove the specified rows
Datas_sere2019_2 <- Datas_sere2019[-rows_to_removesere20192,, drop = FALSE]
dim(Datas_sere2019_2 ) # 



#####Namulonge 2019


Namulonge2019DATA <- Namulonge2019 %>%
  select(
    germplasmName = accession_name,
    CBSDs3 = cassava.brown.streak.disease.leaf.severity.3.month.evaluation.CO_334.0000204,
    CBSDs6 = cassava.brown.streak.disease.leaf.severity.6.month.evaluation.CO_334.0000205,
    CBSDs12 = cassava.brown.streak.disease.root.severity.12.month.evaluation.CO_334.0000201
  )


Datas_NAM2019 <- merge(Namulonge_data2019_merge, Namulonge2019DATA , by= "germplasmName",all.x = TRUE )

dim(Datas_NAM2019)


rowname_counts <- table(Namulonge_data2019_merge$germplasmName)


rowname_counts1 <- table(Datas_NAM2019$germplasmName)




# Find row indices of rows with row name "A"
rows_to_removenam2019 <- which(Datas_NAM2019$germplasmName == "UG110017")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removenam2019) >= 24552) {
  rows_to_removenam2019 <- rows_to_removenam2019[1:23188]
}

# Remove the specified rows
Data_nam2019  <-Datas_NAM2019 [-rows_to_removenam2019, , drop = FALSE]
dim(Data_nam2019)



# Find row indices of rows with row name "A"
rows_to_removenam20191 <- which(Datas_NAM2019 $germplasmName == "TMEB204")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removenam20191) >=10782) {
  rows_to_removenam20191 <- rows_to_removenam20191[1:10183]
}

# Remove the specified rows
Data_nam2019_1 <-Datas_NAM2019 [rows_to_removenam20191, , drop = FALSE]
dim(Data_nam2019_1)




# Find row indices of rows with row name "A"
rows_to_removenam20192 <- which(Datas_NAM2019$germplasmName == "Mkumba")

# If there are more than 500 rows with row name "A", remove 500 of them
if(length(rows_to_removenam20192) >= 15570) {
  rows_to_removenam20192 <- rows_to_removenam20192[1:14705]
}

# Remove the specified rows
Data_nam2019_2 <- Datas_NAM2019[-rows_to_removenam20192,, drop = FALSE]
dim(Data_nam2019_2 ) # 15906    17







###### Merging the datasets 


Namulonge2020 <- Datas_nam2020_2[, c( 1,8,18,24,26,28,29,30,32,34,36,37,38,46,47,48,49,50,51)]

Namulonge2020 <- na.omit(Namulonge2020)




Serer2020 <- Datas_sere2020_2[, c( 1,2,9,19,25,27, 29,30,31,33,35,37,38,39,49,50,51,52,53)]

Serer2020 <- na.omit(Serer2020)



Serer2019 <- Datas_sere2019_2 [, c( 1,8,18,24,26,28,29,30,32,34,36, 38,39,49,50,51,52,53)]

Serer2019 <- na.omit(Serer2019)
# Change the name of a column using the names() function
names(Serer2019)[names(Serer2019) == "germplasmName.y"] <- "germplasmName"




Namulonge2019 <- Data_nam2019_2[, c( 1,8,18,24,26,28,29,30,32,34,36,37,38,39,48,49,50,51,52)]

Namulonge2019 <- na.omit(Namulonge2019)


######merging files 

#version1 <-merge(Namulonge2019 , Namulonge2020, by = "germplasmName", "solidity_necrosis", "convex_hull_vertices_necrosis", "ellipse_angle_necrosis", "ellipse_eccentricity_necrosis",
#   "percent_necrosis", "necrotic_area_fraction", "necrotic_width_fraction", "Root_number", "Image_number", "Plot_id", "Plant_Num", "Location", "Year", "block_number",
#  "plot_number", "CBSDs3", "CBSDs6", "CBSDs12" , all  = TRUE)

version1 <- merge(
  Namulonge2019,
  Namulonge2020,
  by = c(
    "germplasmName", 
    "solidity_necrosis", 
    "convex_hull_vertices_necrosis", 
    "ellipse_angle_necrosis", 
    "ellipse_eccentricity_necrosis",
    "percent_necrosis", 
    "necrotic_area_fraction", 
    "necrotic_width_fraction", 
    "Root_number", 
    "Image_number", 
    "Plot_id", 
    "Plant_Num", 
    "Location", 
    "Year", 
    "block_number",
    "plot_number", 
    "CBSDs3", 
    "CBSDs6", 
    "CBSDs12"
  ),
  all = TRUE
)





version2 <- merge(
  version1,
  Serer2020,
  by = c(
    "germplasmName", 
    "solidity_necrosis", 
    "convex_hull_vertices_necrosis", 
    "ellipse_angle_necrosis", 
    "ellipse_eccentricity_necrosis",
    "percent_necrosis", 
    "necrotic_area_fraction", 
    "necrotic_width_fraction", 
    "Root_number", 
    "Image_number", 
    "Plot_id", 
    "Plant_Num", 
    "Location", 
    "Year", 
    "block_number",
    "plot_number", 
    "CBSDs3", 
    "CBSDs6", 
    "CBSDs12"
  ),
  all = TRUE
)





version3 <- merge(
  version2,
  Serer2019,
  by = c(
    "germplasmName", 
    "solidity_necrosis", 
    "convex_hull_vertices_necrosis", 
    "ellipse_angle_necrosis", 
    "ellipse_eccentricity_necrosis",
    "percent_necrosis", 
    "necrotic_area_fraction", 
    "necrotic_width_fraction", 
    "Root_number", 
    "Image_number", 
    "Plot_id", 
    #"Plant_Num",
    "Location", 
    "Year", 
    "block_number",
    "plot_number", 
    "CBSDs3", 
    "CBSDs6", 
    "CBSDs12"
  ),
  all = TRUE
)

write.csv(version3, file = "Rootnecrosis_novermber2023final.csv")

#############################end 

##########histograms of the un-transformed transformed data 
hist(version3$solidity_necrosis)
hist(version3$convex_hull_vertices_necrosis)
hist(version3$ellipse_angle_necrosis)
hist(version3$ellipse_eccentricity_necrosis)
hist(version3$percent_necrosis)
hist(version3$necrotic_area_fraction)
hist(version3$necrotic_width_fraction)
hist(version3$CBSDs3)
hist(version3$CBSDs6)
hist(version3$CBSDs12)



# Log-transform the "values" column and create a new column "log_values"
#Solidity of necrosis , ellipse_eccentricity_necrosis and ellipse_angle_necrosis were not log transformed 
version3$convex_hull_vertices_necrosis<- log(version3$convex_hull_vertices_necrosis)
version3$percent_necrosis<- log(version3$percent_necrosis)
version3$necrotic_area_fraction<- log(version3$necrotic_area_fraction)
version3$necrotic_width_fraction<- log(version3$necrotic_width_fraction)
version3$CBSDs3<- log(version3$CBSDs3)
version3$CBSDs6 <- log(version3$CBSDs6)
version3$CBSDs12<- log(version3$CBSDs12)




#######ploting the histograms of the log transformed data 

hist(version3$convex_hull_vertices_necrosis)


hist(version3$percent_necrosis)
hist(version3$necrotic_area_fraction)
hist(version3$necrotic_width_fraction)
hist(version3$CBSDs3)
hist(version3$CBSDs6)
hist(version3$CBSDs12)



# Create a new column "Entry_type" based on "germplasm" using dplyr
data_frame1 <- version3 %>%
  mutate(Entry_type = ifelse(germplasmName %in% c("Mkumba", "TMEB204", "UG110017"), "check", "test"))


#Rename multiple columns
data_frame2 <- data_frame1 %>%
  rename(
    SN = solidity_necrosis,
    CHVN =convex_hull_vertices_necrosis,
    EAN = ellipse_angle_necrosis, 
    EEN = ellipse_eccentricity_necrosis,
    NECRO = percent_necrosis, 
    NAF = necrotic_area_fraction, 
    NWF = necrotic_width_fraction
    
  )



write.csv(data_frame2, file = "Full_rootnecrosis_data2023.csv")





#phenotypic correlation
#####phenotypic correlations 

library(psych)



subset_data3 <- data_frame2[, c(3,4,5,6,7,8,9,17,18,19)]


# Create scatter plot matrix with histograms and correlation values
pairs.panels(subset_data3,
             method = "pearson", # Correlation method (you can change it to "spearman" for Spearman correlation)
             hist.col = "#75AADB", # Histogram color
             density = TRUE, # Show density plots on histograms
             ellipses = TRUE, # Show correlation ellipses
             stars = TRUE, # Show significance stars for correlations
             pch = 16, # Point type for scatter plots
             bg = "#FFA07A", # Point color
             # main = "Phenotypic Correlations", # Main title
             cex.cor = 1.2, # Font size of correlation values
             tl.cex = 0.6, # Font size of variable names
             col = "#2E86C1" # Scatter plot color
)







# Bi-plot analysis --------------------------------------------------------

####adding the enviromental variable by pasting location and year effects to do the biplot analyisis

data_frame2$ENV <- paste( data_frame2$Location,  data_frame2$Year, sep = "_")



Data <-data_frame2 %>% select(germplasmName, ENV)




Data6 <- cbind(Data,subset_data3)




#Ranking enviroments 
Model1 <- gtb(Data6 , germplasmName, resp =  c( SN,CHVN,EAN,EEN,NECRO,NAF,NWF,CBSDs3,CBSDs6,CBSDs12))
summary(Model1)
plot(Model1, 
     type = 6,
     col.gen = "blue",
     col.env = "red",
     size.text.gen = 4)






####genotype by trait  rankings
plot(Model1, sel_env = "ENV")



####relationships among traits 
plot(Model1, 
     type = 10,
     col.gen = "blue",
     col.env = "red",
     size.text.gen = 4)



plot(Model1, 
     type = 3,
     col.gen = "blue",
     col.env = "red",
     size.text.gen = 4)



plot(Model1, 
     type = 2,
     second= "PC3")




ge_details(Data6 ,
           env = ENV,
           gen = germplasmName,
           resp = everything())



GGEMODEL11 <- gge(Data6,ENV, germplasmName,resp =  c(SN,CHVN,EEN, EAN,NECRO,NAF,NWF,CBSDs3,CBSDs6,CBSDs12))
plot(GGEMODEL11)


library(ggpubr)

Plot1 <- plot(Model1, 
              type = 1,
              col.gen = "blue",
              col.env = "red",
              size.text.gen = 4)



Plot2 <-plot(GGEMODEL11)




combined_plot <- ggarrange(Plot1,
                           Plot2,
                           nrow = 1,
                           ncol =2)






##############################this was run on the server 
rm(list = ls())

############################# Explore R Script ##############

parallel::detectCores() # get number of cores

n.cores <- parallel::detectCores() - 28 # select # required
n.cores 

# create cluster object
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = my.cluster) # register cluster
foreach::getDoParRegistered() # check if running
foreach::getDoParWorkers() # verify worker

#run code or scrip

###############################
#automatic install of packages if they are not installed already
list.of.packages <- c(
  "foreach",
  "doParallel",
  "ranger",
  "palmerpenguins",
  "tidyverse",
  "kableExtra",
  "data.table",
  "dplyr"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}

#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i, 
      character.only = TRUE
    )
  )
}



#accounting for the augmented design

#closeAllConnections()
data <- within(data_frame2, {
  new <- ifelse(data_frame2$Entry_type!="check",0,1)
});


#checks are labeled 1 while test clones are zero
data1 <- within(data, {
  entryc <- ifelse(data$new > 1, 999,data$germplasmName)})


#####################create unique variable for plot, plant and root number 
data1$Plot_number <- paste( data1$Location, data1$Year,data1$plot_number, sep = "_")


data1$plantnumber <- paste(data1$plotNumber,data1$plant_number, sep = "_")
data1$rootnumber <- paste(data1$plantnumber,data1$Root_number, sep = "_")

#Factors are location, year, block plot, plant number, root number, image postion
data1$Location <- as.factor(data1$Location)
data1$Year <- as.factor(data1$Year)
data1$block_number <- as.factor(data1$block_number)
data1$Image_number <- as.factor(data1$Image_number)
#random variables 
data1$Plot_number <- as.factor(data1$Plot_number)
data1$plantnumber <- as.factor(data1$plantnumber)
data1$rootnumber <- as.factor(data1$rootnumber)

# list of traits
train_names <- c( "SN","CBSDs12", "CHVN",  "EAN", "EEN", "NECRO", "NAF", "NWF", "CBSDs3", "CBSDs6")


foreach (i = train_names) %dopar% {
  
  # load libraries
  library(nlme)
  library(dplyr)
  library(lme4)
  
  # I think you should still put in the fixed effect for image_number
  Model1 <- lmer(get(i) ~ Location + Year + Entry_type +  Image_number + 
                   (1|germplasmName:new) + (1|Plot_number) + 
                   (1|plantnumber) + (1|rootnumber) +  (1|block_number), data=data1)
  
  saveRDS(Model1, paste0(i,"_Model1.rds"))
  
}

parallel::stopCluster(cl = my.cluster) # stop cluster when done






#################################################Broadsense heritability calculation 



##plot harmonic mean
unique_varieties <- unique(data1$germplasmName)


# Function to figure out how many plots each variety has been evaluated in
plotsPerVariety <- function(varName){
  dataVar <- data1 %>% dplyr::filter(germplasmName == varName)
  unique_plots <- unique(dataVar$Plot_number)
  return(length(unique_plots))
}
plotsPerVar <- sapply(unique_varieties, plotsPerVariety)




# Calculate the harmonic mean of plotsPerVar for var of plots
plantsPerVariety <- function(varName){
  dataVar <- data1 %>% dplyr::filter(germplasmName == varName)
  unique_plants <- unique(dataVar$plantnumber)
  return(length(unique_plants))
}

plantsPerVar <- sapply(unique_varieties, plantsPerVariety)


# Calculate the harmonic mean of plantsPerVar for var of plants within plots
rootsPerVariety <- function(varName){
  dataVar <- data1 %>% dplyr::filter(germplasmName == varName)
  unique_roots <- unique(dataVar$rootnumber)
  return(length(unique_roots))
}
rootsPerVar <- sapply(unique_varieties, rootsPerVariety)

# Calculate the harmonic mean of rootsPerVar for var of roots within plants

imagesPerVar <- table(data1$germplasmName)

# Calculate the harmonic mean of imagesPerVar for residual

# Calculate the harmonic mean of imagesPerVar for residual
imagesPerVar <- table(data1$germplasmName)


##plot harmonic mean 

plot_harmonic_mean <- 1 / mean(1 / plotsPerVar)




##plantt harmonic mean 

plant_harmonic_mean <- 1 / mean(1 / plantsPerVar)





##Root harmonic mean 

Root_harmonic_mean <- 1 / mean(1 /rootsPerVar)





##image harmonic mean  / residual variance 

image_harmonic_mean <- 1/ mean(1/imagesPerVar)


#####mixed models from the server 

#SN
one <- readRDS("SN_Model1.rds")
summary(one)


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense <- 0.0028357 / (0.0028357 + 0.0215549/plot_harmonic_mean + 0.000e+00/plant_harmonic_mean + 0.0001274/Root_harmonic_mean +  0.0664743/image_harmonic_mean )
Broad_sense #0.163621


#NWF
two <- readRDS("NWF_Model1.rds")
summary(two)


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense2 <-3.801e-02/ (3.801e-02  + 1.986e-01/plot_harmonic_mean + 0.006726 /plant_harmonic_mean + 2.222e-03/Root_harmonic_mean + 8.257e-01/image_harmonic_mean )
Broad_sense2 # 0.2138588




#NECRO
three <- readRDS("NECRO_Model1.rds")
summary(three)


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense3 <- 0.154405 / (0.154405   + 0.314171/plot_harmonic_mean +  0.000000 /plant_harmonic_mean + 0.004424/Root_harmonic_mean + 1.144918/image_harmonic_mean )
Broad_sense3 # 0.4200823



#NAF
four <- readRDS("NAF_Model1.rds")
summary(four)


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense4 <- 0.155980  / (0.155980   + 0.833246 /plot_harmonic_mean +  0.000000 /plant_harmonic_mean + 0.007412/Root_harmonic_mean +  2.521895 /image_harmonic_mean )
Broad_sense4 #0.2176867




#EEN
five <- readRDS("EEN_Model1.rds")
summary(five)


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense5 <- 8.049e-04/ (8.049e-04+ 3.019e-03/plot_harmonic_mean + 0.000e+00 /plant_harmonic_mean + 1.423e-05/Root_harmonic_mean +  2.507e-02 /image_harmonic_mean )
Broad_sense5 # 0.2728182





#EAN
six <- readRDS("EAN_Model1.rds")
summary(six )


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense6 <- 5.436e+01 / (5.436e+01 + 2.082e+02/plot_harmonic_mean + 3.021e-07 /plant_harmonic_mean + 4.494e+00/Root_harmonic_mean + 2.708e+03/image_harmonic_mean )
Broad_sense6 # 0.258459






#CHVN
seven <- readRDS("CHVN_Model1.rds")
summary(seven )


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense7 <- 0.0018996/ (0.0018996 + 0.0170078 /plot_harmonic_mean + 0.0000000 /plant_harmonic_mean + 0.0001667/Root_harmonic_mean + 0.0581177/image_harmonic_mean )
Broad_sense7 # 0.1418272


#CBSDs6
Eight <- readRDS("CBSDs6_Model1.rds")
summary(Eight )


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense8 <- 0.084967/ (0.084967+  0.045808 /plot_harmonic_mean + 0.000000 /plant_harmonic_mean + 0.000000 /Root_harmonic_mean +  0.021120 /image_harmonic_mean )
Broad_sense8 # 0.7399655




#CBSDs3
nine <- readRDS("CBSDs3_Model1.rds")
summary(nine  )


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense9 <- 0.0566751/ (0.0566751+  0.0327534 /plot_harmonic_mean + 0.000000 /plant_harmonic_mean + 0.000000 /Root_harmonic_mean +  0.0130909/image_harmonic_mean )
Broad_sense9 #0.7265139



#CBSDs3
ten <- readRDS("CBSDs12_Model1.rds")
summary(ten)


#Broad_sense <- geno_variance / (geno_variance + plot_variance + plant_variance/harmonicmean_plants + rootnumber_variance/harmonicmean_rootnumber  + cross_section_variance/harmonicmean_crossec  )

Broad_sense10 <-  0.116472 /(0.116472 +  0.072300 /plot_harmonic_mean + 0.000000 /plant_harmonic_mean + 0.000000 /Root_harmonic_mean +  0.047307/image_harmonic_mean )
Broad_sense10 # 0.71149










#CBSDs3


blups9 <- ranef(nine)$`germplasmName:new`

dim(blups9) 


write.csv(blups9, file = "CBSDs3_Blups.csv")






#CBSDs12


blups10 <- ranef(ten)$`germplasmName:new`

dim(blups10) 


write.csv(blups10, file = "CBSDs12_Blups.csv")






######reload the BLUPS 
SN_Blups <- read.csv("SN_Blups.csv")


new_names <- c("germplasmName", "SN")
colnames(SN_Blups) <- new_names



NWF_Blups <- read.csv("NWF_Blups.csv")


new_names <- c("germplasmName", "NWF")
colnames(NWF_Blups) <- new_names



NECRO_Blups <- read.csv("NECRO_Blups.csv")
new_names <- c("germplasmName", "NECRO")
colnames(NECRO_Blups) <- new_names



NAF_Blups <- read.csv("NAF_Blups.csv")
new_names <- c("germplasmName", "NAF")
colnames(NAF_Blups) <- new_names


EEN_Blups <- read.csv("EEN_Blups.csv")
new_names <- c("germplasmName", "EEN")
colnames(EEN_Blups) <- new_names


EAN_Blups <- read.csv("EAN_Blups.csv")
new_names <- c("germplasmName", "EAN")
colnames(EAN_Blups) <- new_names


CHVN_Blups <- read.csv("CHVN_Blups.csv")
new_names <- c("germplasmName", "CHVN")
colnames(CHVN_Blups) <- new_names


CBSDs3_Blups <- read.csv("CBSDs3_Blups.csv")
new_names <- c("germplasmName", "CBSDs3")
colnames(CBSDs3_Blups) <- new_names



CBSDs6_Blups <- read.csv("CBSDs6_Blups.csv")
new_names <- c("germplasmName", "CBSDs6")
colnames(CBSDs6_Blups) <- new_names


########dergressed BLUPS 


BLUP <- ranef(one , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(one),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_SN.csv")






BLUP <- ranef(two , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(two),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_NWF.csv")






BLUP <- ranef(three , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(three),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_NECRO.csv")






BLUP <- ranef(four , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(four),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_NAF.csv")






BLUP <- ranef(five , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(four),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_EEN.csv")




BLUP <- ranef(six , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(six),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_EAN.csv")







BLUP <- ranef(seven , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(seven),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_CHVN.csv")




BLUP <- ranef(Eight , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(Eight ),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_CBSDs6.csv")


BLUP <- ranef(nine , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(nine),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[2,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_CBSDs3.csv")



BLUP <- ranef(ten , condVar=TRUE)$`germplasmName:new`

PEV <- c(attr(BLUP, "postVar"))
VARCOMPS <- as.data.frame(VarCorr(ten),comp="Variance") 

CLONE <- VARCOMPS[2,5]

ResidVar <- (VARCOMPS[5,5])
#deregressing OF blupS
out <- BLUP/(1-(PEV/CLONE))

write.csv(out, file = "Deregressed_Blups_CBSDs12.csv")





SN_Blups <- read.csv("Deregressed_Blups_SN.csv")


new_names <- c("germplasmName", "SN")
colnames(SN_Blups) <- new_names



NWF_Blups <- read.csv("Deregressed_Blups_NWF.csv")


new_names <- c("germplasmName", "NWF")
colnames(NWF_Blups) <- new_names



NECRO_Blups <- read.csv("Deregressed_Blups_NECRO.csv")
new_names <- c("germplasmName", "NECRO")
colnames(NECRO_Blups) <- new_names



NAF_Blups <- read.csv("Deregressed_Blups_NAF.csv")
new_names <- c("germplasmName", "NAF")
colnames(NAF_Blups) <- new_names


EEN_Blups <- read.csv("Deregressed_Blups_EEN.csv")
new_names <- c("germplasmName", "EEN")
colnames(EEN_Blups) <- new_names


EAN_Blups <- read.csv("Deregressed_Blups_EAN.csv")
new_names <- c("germplasmName", "EAN")
colnames(EAN_Blups) <- new_names


CHVN_Blups <- read.csv("Deregressed_Blups_CHVN.csv")
new_names <- c("germplasmName", "CHVN")
colnames(CHVN_Blups) <- new_names


CBSDs3_Blups <- read.csv("Deregressed_Blups_CBSDs3.csv")
new_names <- c("germplasmName", "CBSDs3")
colnames(CBSDs3_Blups) <- new_names



CBSDs6_Blups <- read.csv("Deregressed_Blups_CBSDs6.csv")
new_names <- c("germplasmName", "CBSDs6")
colnames(CBSDs6_Blups) <- new_names



CBSDs12_Blups <- read.csv("Deregressed_Blups_CBSDs12.csv")
new_names <- c("germplasmName", "CBSDs12")
colnames(CBSDs12_Blups) <- new_names





New_data1 <- merge(SN_Blups, CHVN_Blups, by= "germplasmName")


New_data2 <- merge(New_data1,EAN_Blups , by= "germplasmName", all.x = TRUE)



New_data3 <- merge(New_data2, EEN_Blups, by= "germplasmName" , all.x = TRUE)


New_data4 <- merge(New_data3, NECRO_Blups, by= "germplasmName" , all.x = TRUE)


New_data5 <- merge(New_data4, NAF_Blups, by= "germplasmName", all.x = TRUE)



New_data6 <- merge(New_data5, NWF_Blups , by= "germplasmName", all.x = TRUE)



New_data7 <- merge(New_data6, CBSDs3_Blups , by= "germplasmName", all.x = TRUE)

New_data8 <- merge(New_data7, CBSDs6_Blups , by= "germplasmName", all.x = TRUE)

New_data9 <- merge(New_data8, CBSDs12_Blups , by= "germplasmName", all.x = TRUE)

# Sample data frame with a column containing the pattern
data <- data.frame(column_with_pattern = New_data9$germplasmName)

# Extracting the desired pattern
data$extracted_names <- sub(":0", "", data$column_with_pattern)

# Print the result
print(data)

nEW_MERGE <- merge(New_data9,data, by.x ="germplasmName", by.y="column_with_pattern")



selected_data <-nEW_MERGE[, -1]


selected_data <- selected_data %>%
  rename(germplasmName = extracted_names)

##########seedling code that was used in genotyping to match the phenotype data with the genotype data 
common_code <- read.csv("common_code.csv")

common_code<- common_code %>%
  rename(germplasmName = accession_name)


degressed_BLUPs <- merge(selected_data ,common_code, by= 'germplasmName', all.x = TRUE)
write.csv(degressed_BLUPs, file = "degressed_BLUPs.csv")



# Gemma code  -------------------------------------------------------------


#*** means my personal information on the server
scp ***@login.sgn.cornell.edu:/home/***/*_Nov2023.* .



###################### March 2023 #############################################################
##---- Run GEMMA for Image phenotypes --------##

# keep only samples with phenotype and genotype #
## GEMMA_Rootnecrosis_pheno.csv to GEMMA_Rootnecrosis_pheno_sub298.csv #

# map image phenotype to the .fam pheno input file from previous GEMMA analysis .fam file #

combinedC2_final_PLINK_Mar2023.fam

# copy phenotype file to the server
scp ~/Desktop/Leah_image_analysis/GEMMA_Mar2023/combinedC2_final_PLINK_Mar2023.fam.txt   ***@login.sgn.cornell.edu:/home/ln242/ 
  
  scp  ln242@solanine:/home/ln242/combinedC2_final_PLINK_Mar2023.fam.txt .

mv combinedC2_final_PLINK_Mar2023.fam.txt combinedC2_final_2_PLINK.fam


## SWITCH directory to "/home/ln242/gemma2/genetic-data-analysis-2" to carry out GEMMA analysis ###

cd /home/ln242/gemma2/genetic-data-analysis-2


## run GEMMA
#generate centered relationship matrix#
./gemma -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -gk 1 -o combinedC2_final_2_PLINK_image_Mar2023

####
Reading Files ... 
## number of total individuals = 3194
## number of analyzed individuals = 320
## number of covariates = 1
## number of phenotypes = 1
## number of total SNPs = 51860
## number of analyzed SNPs = 30750
Calculating Relatedness Matrix ... 
Reading SNPs  ==================================================100.00%
####

#run GEMMA
# ./gemma -bfile ../../GEMMA-0.98.4/combinedC2_final_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -o combinedC2_final_PLINK_GEMMA #




univariate GWAS

###UNIVARIATE GWAS



#for CHVN	

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 3 -o combinedC2_final_2_PLINK_GEMMA_mar23_CHVN


#for EAN

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 4 -o combinedC2_final_2_PLINK_GEMMA_mar23_EAN	

#

#for EEN

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 5 -o combinedC2_final_2_PLINK_GEMMA_mar23_EEN	



#for NAF

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 10 -o combinedC2_final_2_PLINK_GEMMA_mar23_NAF	




#for NECRO

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 11 -o combinedC2_final_2_PLINK_GEMMA_mar23_NECRO	


#for NWF

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 12 -o combinedC2_final_2_PLINK_GEMMA_mar23_NWF	









#for SN	

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 14 -o combinedC2_final_2_PLINK_GEMMA_mar23_SN







#for CBSDs3	

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 16 -o combinedC2_final_2_PLINK_GEMMA_mar23_CBSDs3




#for CBSDs6

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 17 -o combinedC2_final_2_PLINK_GEMMA_mar23_CBSDs6	




#for CBSDs12

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_Mar2023.cXX.txt  -lmm 2 -n 18 -o combinedC2_final_2_PLINK_GEMMA_mar23_CBSDs12






Multivariate gwas

#---- for multi-traits ----#

# CHVN_NWF_NAF

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_May2023.cXX.txt  -lmm 2 -n 1 2 14 9 3 7 5 11 -o combinedC2_final_2_PLINK_GEMMA_CHVN_NWF_NAF_may2023



  
  # EAN_NECRO
  
  ./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_May2023.cXX.txt  -lmm 2 -n 14 3 7 12 -o combinedC2_final_2_PLINK_GEMMA_EAN_NECRO_may2023


#

# NWF_NAF

./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_May2023.cXX.txt  -lmm 2 -n 5 13 -o combinedC2_final_2_PLINK_GEMMA_NWF_NAF_may2023


# NECRO_CBSDs3_CBSDs6
./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_May2023.cXX.txt  -lmm 2 -n 15 8 6 4 11 -o combinedC2_final_2_PLINK_GEMMA_NECRO_CBSDs3_CBSDs6_may2023




# NECRO_CBSDs12
./gemma-0.98.4-linux-static-AMD64 -bfile ../../GEMMA-0.98.4/combinedC2_final_2_PLINK -k output/combinedC2_final_2_PLINK_image_May2023.cXX.txt  -lmm 2 -n 6 4 11 -o combinedC2_final_2_PLINK_GEMMA_NECRO_CBSDs12_may2023












# Plotting Gemma out puts  ------------------------------------------------


gwscan1 <- read.table("CBSDs3.assoc.txt", header = TRUE)

gwscan2 <- read.table("CBSDs6.assoc.txt", header = TRUE)

gwscan3 <- read.table("CBSDs12.assoc.txt", header = TRUE)


gwscan4 <- read.table("CHVN.assoc.txt", header = TRUE)

gwscan5 <- read.table("EEN.assoc.txt", header = TRUE)


gwscan6 <- read.table("EAN.assoc.txt", header = TRUE)


gwscan7 <- read.table("NAF.assoc.txt", header = TRUE)
gwscan8 <- read.table("NWF.assoc.txt", header = TRUE)

gwscan9 <- read.table("NECRO.assoc.txt", header = TRUE)

gwscan10<- read.table("SN.assoc.txt", header = TRUE)



# AIM: Multiple testing to identify significant SNPs  ---------------------



#Previous calculation of MEFF effective number of markers 
Meff = 6477
p_threshold = (1 - (1 - 0.05))^1/6477
p_threshold #7.719623e-06





colR <- c("blue4", "orange3","blue4", "orange3","blue4", "orange3","blue4", "orange3","blue4", "orange3","blue4", "orange3","blue4", "orange3","blue4", "orange3","blue4", "orange3")





df1 <- gwscan1[,c(1,3,10,2)]
colnames(df1) <- c("CHR","BP","P","SNP")
df1$BP <- as.numeric(df1$BP)
df1$CHR <- as.numeric(df1$CHR)

B= ggplot(data = df1,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 




df2 <- gwscan2[,c(1,3,10,2)]
colnames(df2) <- c("CHR","BP","P","SNP")
df2$BP <- as.numeric(df2$BP)
df2$CHR <- as.numeric(df2$CHR)

C= ggplot(data = df2,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 



df3 <- gwscan3[,c(1,3,10,2)]
colnames(df3) <- c("CHR","BP","P","SNP")
df3$BP <- as.numeric(df3$BP)
df3$CHR <- as.numeric(df3$CHR)

D= ggplot(data = df3,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 



df4 <- gwscan4[,c(1,3,10,2)]
colnames(df4) <- c("CHR","BP","P","SNP")
df4$BP <- as.numeric(df4$BP)
df4$CHR <- as.numeric(df4$CHR)

E= ggplot(data = df4,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 


df5 <- gwscan5[,c(1,3,10,2)]
colnames(df5) <- c("CHR","BP","P","SNP")
df5$BP <- as.numeric(df5$BP)
df5$CHR <- as.numeric(df5$CHR)

F= ggplot(data = df5,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 



df6 <- gwscan6[,c(1,3,10,2)]
colnames(df6) <- c("CHR","BP","P","SNP")
df6$BP <- as.numeric(df6$BP)
df6$CHR <- as.numeric(df6$CHR)

G= ggplot(data = df6,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 



df7 <- gwscan7[,c(1,3,10,2)]
colnames(df7) <- c("CHR","BP","P","SNP")
df7$BP <- as.numeric(df7$BP)
df7$CHR <- as.numeric(df7$CHR)

H= ggplot(data = df7,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 


df8 <- gwscan8[,c(1,3,10,2)]
colnames(df8) <- c("CHR","BP","P","SNP")
df8$BP <- as.numeric(df8$BP)
df8$CHR <- as.numeric(df8$CHR)

I= ggplot(data = df8,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 



df9 <- gwscan9[,c(1,3,10,2)]
colnames(df9) <- c("CHR","BP","P","SNP")
df9$BP <- as.numeric(df9$BP)
df9$CHR <- as.numeric(df9$CHR)

J= ggplot(data = df9,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 




df10 <- gwscan10[,c(1,3,10,2)]
colnames(df10) <- c("CHR","BP","P","SNP")
df10$BP <- as.numeric(df10$BP)
df10$CHR <- as.numeric(df10$CHR)

Q= ggplot(data = df10,
          aes(x = as.factor(CHR),
              y = -log10(P),color=as.factor(CHR))) + geom_hline(yintercept = -log10(p_threshold), color = "red") +
  geom_jitter() + 
  labs(x = "Chromosomes")+
  theme_classic() + theme(legend.position = 'none') + scale_color_manual(values=colR) 





# significant associations ------------------------------------------------

figure <- ggarrange(  F,J , 
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)
figure

figure2 <- ggarrange(I, Q, E, G, H,
                     labels = c("A", "B", "C", "D", "E"),
                     ncol = 1, nrow = 5)

figure2



figure3 <- ggarrange( B, C, D ,
                      labels = c("A", "B",'C'),
                      ncol = 1, nrow = 3)
figure3




# QQPLOTS -----------------------------------------------------------------

gg_qqplot <- function(P, ci = 0.95) {
  n  <- length(P)
  df <- data.frame(
    observed = -log10(sort(P)),
    expected = -log10(ppoints(n)),
    clower   = -log10(qbeta(p = (1 - ci) / 2, shape1 = 1:n, shape2 = n:1)),
    cupper   = -log10(qbeta(p = (1 + ci) / 2, shape1 = 1:n, shape2 = n:1))
  )
  log10Pe <- expression(paste("Expected -log"[10], plain(P)))
  log10Po <- expression(paste("Observed -log"[10], plain(P)))
  ggplot(df) +
    geom_point(aes(expected, observed), shape = 1, size = 3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.5) +
    xlab(log10Pe) +
    ylab(log10Po)
}


QQ_2 <-gg_qqplot(df1$P)
QQ_3 <-gg_qqplot(df2$P)
QQ_4 <-gg_qqplot(df3$P)


QQ_5 <-gg_qqplot(df4$P)
QQ_6 <-gg_qqplot(df5$P)
QQ_7 <-gg_qqplot(df6$P)

QQ_8 <-gg_qqplot(df7$P)
QQ_9 <-gg_qqplot(df8$P)
QQ_10 <-gg_qqplot(df9$P)
QQ_11 <-gg_qqplot(df10$P)



figure4 <- ggarrange( QQ_2, QQ_3, QQ_4, 
                      labels = c("A", "B", "C"),
                      ncol = 3, nrow = 1)
figure4



figure5 <- ggarrange( QQ_7, QQ_10,  
                      labels = c("A", "B"),
                      ncol = 2, nrow = 1)
figure5



figure6 <- ggarrange( QQ_5, QQ_6, QQ_8, QQ_9, QQ_11,
                      labels = c( "A", "B", "C", "D","E"),
                      ncol = 5, nrow = 1)
figure6









# Significant associations ------------------------------------------------

Threshold_Meff= -log10(p_threshold)
Threshold_Meff #5.112404


#CBSDS12
CBSDs12 <-transform(df3,P = -log10(P))
CBSDs12_signf <-  subset(CBSDs12, P >=Threshold_Meff)

dim(CBSDs12_signf) #1  4

write.csv(CBSDs12_signf, file = "CBSDs12_signf_signf.csv")


#EEN


EEN <-transform(df5,P = -log10(P))
EEN_signf <-  subset(EEN, P >=Threshold_Meff)

dim(EEN_signf) #2 4

write.csv(EEN_signf, file = "EEN_signf.csv")

#NECRO


NECRO <-transform(df9,P = -log10(P))
NECRO_signf <-  subset(NECRO, P >=Threshold_Meff)

dim(NECRO_signf) # 6 4

write.csv(NECRO_signf, file = "NECRO_signf.csv")







########variance explained by SNPs 


library(tidyr)
library(rrBLUP)
library(magrittr)

library(EMMREML)
#####marker data 

Marker_data <- readRDS("New_marker_data_51862_SNPs.rds")

SNP <- Marker_data$MARKER
####tracking filtering
rownames(Marker_data) <- SNP
#removing the first column 
markerdataNoSNP <- Marker_data[,-1]
#transposing and changing the code to 1,0,-1
Markerdata1= t(markerdataNoSNP -1)




# Data filtering  ---------------------------------------------------------

#### filtering for missing individuals and missing markers 
Data_filtered <- function(Markerdata1, NIM, NMG){
  
  missing_indids <-apply(Markerdata1,1, function(x){
    return(length(which(is.na(x)))/ncol(Markerdata1))
  })
  
  missing_genos <- apply(Markerdata1,2,function(x) {
    return(length(which(is.na(x)))/nrow(Markerdata1))
  })
  
  filtering <- Markerdata1[which( missing_indids< NIM),which(missing_genos < NMG)]
  
  return(filtering)
}


#remove individuals with >10% data and makers with >5% missing data
filtered <-  Data_filtered (Markerdata1,0.10,0.05)


###from the filtering no genotype is filtered


# Filtering for minor allele frequency ------------------------------------


#Calculating allele freq and filtering for minor allele frequency using the filtered marker data 

MAf_filtered <-filtered + 1

alleleFreq <- colMeans(MAf_filtered )/2




MAF <- apply(MAf_filtered, 2, function(x) sum(x) / (length(x) * 2))

#filtering for MAF

Genotypes_filtered <- filtered[,which(MAF > 0.01 & MAF < 0.99)]







#####narrowsense heritability 




commonInd <- intersect(rownames(Genotypes_filtered),degressed_BLUPs$observationUnitName) ####commonindividuals 
length(commonInd) 


C2_Geno_1_subset <- Genotypes_filtered[(rownames(Genotypes_filtered)%in%commonInd),] 

Phenotypes_subset <- degressed_BLUPs [degressed_BLUPs$observationUnitName %in% commonInd,]
dim(Phenotypes_subset) 






CHVN <- Phenotypes_subset[,c(4,13)]

# remove NAs
CHVN_na <- na.omit(CHVN)




EAN <- Phenotypes_subset[,c(5,13)]

EAN_na <- na.omit(EAN)


EEN <- Phenotypes_subset[,c(6,13)]

# remove NAs
EEN_na <- na.omit(EEN)


NAF<- Phenotypes_subset[,c(8,13)]

# remove NAs
NAF_na <- na.omit(NAF)


NECRO<- Phenotypes_subset[,c(7,13)]

# remove NAs
NECRO_na <- na.omit(NECRO)


NWF<- Phenotypes_subset[,c(9,13)]

# remove NAs
NWF_na <- na.omit(NWF)







SN<- Phenotypes_subset[,c(3,13)]

# remove NAs
SN_na <- na.omit(SN)





CBSDs3<- Phenotypes_subset[,c(10,13)]

# remove NAs
CBSDs3_na <- na.omit(CBSDs3)



CBSDs6<- Phenotypes_subset[,c(11,13)]

# remove NAs
CBSDs6_na <- na.omit(CBSDs6)


CBSDs12<- Phenotypes_subset[,c(12,13)]

# remove NAs
CBSDs12_na <- na.omit(CBSDs12)







#making dosage files for each trait 


dosageC2_123format_CHVN <- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%CHVN_na$observationUnitName,]



dosageC2_123format_EAN <- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%EAN_na$observationUnitName,]




dosageC2_123format_EEN <- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%EEN_na$observationUnitName,]



dosageC2_123format_NAF<- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%NAF_na$observationUnitName,]



dosageC2_123format_NECRO<- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%NECRO_na$observationUnitName,]




dosageC2_123format_NWF<- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%NWF_na$observationUnitName,]






dosageC2_123format_SN<- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%SN_na$observationUnitName,]





dosageC2_123format_CBSDs3<- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%CBSDs3_na$observationUnitName,]


dosageC2_123format_CBSDs6<- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%CBSDs6_na$observationUnitName,]


dosageC2_123format_CBSDs12<- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%CBSDs12_na$observationUnitName,]



###################Narrowsense heritability 

####Relation matrix 

allK3<-A.mat(dosageC2_123format_CHVN)



Z3<-model.matrix(~-1+ CHVN_na$observationUnitName)

########Narrow sense heritability 
model_h3<-emmreml(y=CHVN_na$CHVN, X=matrix(1,nrow=nrow(CHVN_na),ncol=1), Z=Z3, K=allK3)

model_h3$Vu/(model_h3$Vu+model_h3$Ve) # 0.2263138


######

allK4 <-A.mat(dosageC2_123format_EAN)



Z4<-model.matrix(~-1+ EAN_na$observationUnitName)


model_h4<-emmreml(y=EAN_na$EAN, X=matrix(1,nrow=nrow(EAN_na),ncol=1), Z=Z4, K=allK4)

model_h4$Vu/(model_h4$Vu+model_h4$Ve) # 0.110258



#################
allK5 <-A.mat(dosageC2_123format_EEN)



Z5<-model.matrix(~-1+ EEN_na$observationUnitName)

model_h5<-emmreml(y=EEN_na$EEN, X=matrix(1,nrow=nrow(EEN_na),ncol=1), Z=Z5, K=allK5)

model_h5$Vu/(model_h5$Vu+model_h5$Ve) # 0.09290265




####################
allK10 <-A.mat(dosageC2_123format_NAF)



Z10<-model.matrix(~-1+ NAF_na$observationUnitName)


model_h10<-emmreml(y=NAF_na$NAF, X=matrix(1,nrow=nrow(NAF_na),ncol=1), Z=Z10, K=allK10)

model_h10$Vu/(model_h10$Vu+model_h10$Ve) # 0.1681511



########################
allK11 <-A.mat(dosageC2_123format_NECRO)



Z11<-model.matrix(~-1+ NECRO_na$observationUnitName)


model_h11<-emmreml(y=NECRO_na$NECRO, X=matrix(1,nrow=nrow(NECRO_na),ncol=1), Z=Z11, K=allK11)

model_h11$Vu/(model_h11$Vu+model_h11$Ve) #0.1526021


#########################

allK12 <-A.mat(dosageC2_123format_NWF)



Z12<-model.matrix(~-1+ NWF_na$observationUnitName)


model_h12<-emmreml(y=NWF_na$NWF, X=matrix(1,nrow=nrow(NWF_na),ncol=1), Z=Z12, K=allK12)

model_h12$Vu/(model_h12$Vu+model_h12$Ve) #  0.128448

###########################################


allK14 <-A.mat(dosageC2_123format_SN)



Z14<-model.matrix(~-1+ SN_na$observationUnitName)


model_h14<-emmreml(y=SN_na$SN, X=matrix(1,nrow=nrow(SN_na),ncol=1), Z=Z14, K=allK14)

model_h14$Vu/(model_h14$Vu+model_h14$Ve) #0.03366398


###########################################################

allK16 <-A.mat(dosageC2_123format_CBSDs3)



Z16<-model.matrix(~-1+ CBSDs3_na$observationUnitName)

model_h16<-emmreml(y=CBSDs3_na$CBSDs3, X=matrix(1,nrow=nrow(CBSDs3_na),ncol=1), Z=Z16, K=allK16)

model_h16$Vu/(model_h16$Vu+model_h16$Ve) #0.3382605




allK17 <-A.mat(dosageC2_123format_CBSDs6)



Z17<-model.matrix(~-1+ CBSDs6_na$observationUnitName)


model_h17<-emmreml(y=CBSDs6_na$CBSDs6, X=matrix(1,nrow=nrow(CBSDs6_na),ncol=1), Z=Z17, K=allK17)

model_h17$Vu/(model_h17$Vu+model_h17$Ve) #0.4178154

###########

allK18 <-A.mat(dosageC2_123format_CBSDs12)



Z18<-model.matrix(~-1+ CBSDs12_na$observationUnitName)


model_h18<-emmreml(y=CBSDs12_na$CBSDs12, X=matrix(1,nrow=nrow(CBSDs12_na),ncol=1), Z=Z18, K=allK18)

model_h18$Vu/(model_h18$Vu+model_h18$Ve) # 0.4979522






##########################variance explained by significant snps 


######average of BLUPs  of CBSD SEVERITY TRAITS 
# Create a new column with the average of three columns
degressed_BLUPs$average_column <- rowMeans(degressed_BLUPs[, c("CBSDs3", "CBSDs6", "CBSDs12")])

# View the updated dataset
head(degressed_BLUPs)

Phenotypes_subset <- degressed_BLUPs [degressed_BLUPs$observationUnitName %in% commonInd,]


average_column<- Phenotypes_subset[,c(14,13)]

# remove NAs
average_column_na <- na.omit(average_column)


dosageC2_123format_average_column <- C2_Geno_1_subset[rownames(C2_Geno_1_subset)%in%average_column_na$observationUnitName,]


head(dosageC2_123format_average_column[1:5,1:5])


####Relation matrix 

average_colum_K<-A.mat(dosageC2_123format_average_column)



average_column_Z<-model.matrix(~-1+ average_column_na$observationUnitName)

########Narrow sense heritability 
model_h3_average_colum<-emmreml(y=average_column_na$average_column, X=matrix(1,nrow=nrow(average_column_na),ncol=1), Z=average_column_Z, K=average_colum_K)

model_h3_average_colum$Vu/(model_h3_average_colum$Vu+model_h3_average_colum$Ve) # 0.4800517



######variance explained by SNPs 
AllSnps <- c("S7_718516", "S11_31660720","S1_28931143", "S1_28972056" ,"S1_28977687" ,"S1_29058977", "S1_29063012" ,"S1_29103386","S1_18560046")

signfSNPs <- dosageC2_123format_average_column [,colnames(dosageC2_123format_average_column ) %in% AllSnps ]
table(unlist(signfSNPs ))

#####relationshhip matrix of individuals with the significant snps
signfSNPsK46<-A.mat(signfSNPs)


minusscores_signfSNPs46<-dosageC2_123format_average_column[,!(colnames(dosageC2_123format_average_column)%in% AllSnps)]


table(unlist(minusscores_signfSNPs46))

minusscores_signfSNPsK46<-A.mat(minusscores_signfSNPs46)


average_column_na$observationUnitName<-factor(as.character(average_column_na$observationUnitName), levels=rownames(minusscores_signfSNPsK46))


Z46<-model.matrix(~-1+average_column_na$observationUnitName)


modelsignfloci46<-emmremlMultiKernel(y=average_column_na$average_column, X=matrix(1,nrow=nrow(average_column_na),ncol=1), Zlist=list(Z46,Z46), Klist=list(minusscores_signfSNPsK46, signfSNPsK46))


varus<-modelsignfloci46$Vu*modelsignfloci46$weights
varus[2]

varus[1]

varus[1]/(varus[1]+varus[2]+modelsignfloci46$Ve) # others =  9.999e-05
varus[2]/(varus[1]+varus[2]+modelsignfloci46$Ve) # signf regions =0 # all genotypes are the same (homozygote regions and do not express )






######Significant snps 
ALL_EEN <-EEN_signf$SNP


signfSNPs_scores46 <- dosageC2_123format_EEN[,colnames(dosageC2_123format_EEN) %in%ALL_EEN]
table(unlist(signfSNPs_scores46 ))

#####relationshhip matrix of individuals with the significant snps
signfSNPsK46<-A.mat(signfSNPs_scores46)


minusscores_signfSNPs46<-dosageC2_123format_EEN[,!(colnames(dosageC2_123format_EEN)%in% ALL_EEN)]


table(unlist(minusscores_signfSNPs46))

minusscores_signfSNPsK46<-A.mat(minusscores_signfSNPs46)


EEN_na$observationUnitName<-factor(as.character(EEN_na$observationUnitName), levels=rownames(minusscores_signfSNPsK46))


Z46<-model.matrix(~-1+EEN_na$observationUnitName)


modelsignfloci46<-emmremlMultiKernel(y=EEN_na$EEN, X=matrix(1,nrow=nrow(EEN_na),ncol=1), Zlist=list(Z46,Z46), Klist=list(minusscores_signfSNPsK46, signfSNPsK46))


varus<-modelsignfloci46$Vu*modelsignfloci46$weights
varus[2]

varus[1]

varus[1]/(varus[1]+varus[2]+modelsignfloci46$Ve) # others =  9.999e-05
varus[2]/(varus[1]+varus[2]+modelsignfloci46$Ve) # signf regions =0




#trait 2 

ALL_NECRO <-NECRO_signf$SNP





signfSNPs_scores50 <- dosageC2_123format_NECRO[,colnames(dosageC2_123format_NECRO)%in%ALL_NECRO]


table(unlist(signfSNPs_scores50))


signfSNPsK50<-A.mat(signfSNPs_scores50)

minusscores_signfSNPs50<-dosageC2_123format_NECRO[,!(colnames(dosageC2_123format_NECRO)%in%ALL_NECRO)]

minusscores_signfSNPsK50<-A.mat(minusscores_signfSNPs50)


NECRO_na$observationUnitName<-factor(as.character(NECRO_na$observationUnitName), levels=rownames(minusscores_signfSNPsK50))


Z50<-model.matrix(~-1+NECRO_na$observationUnitName)


modelsignfloci50<-emmremlMultiKernel(y=NECRO_na$NECRO, X=matrix(1,nrow=nrow(NECRO_na),ncol=1), Zlist=list(Z50,Z50), Klist=list(minusscores_signfSNPsK50, signfSNPsK50))


varus<-modelsignfloci50$Vu*modelsignfloci50$weights
varus[2]

varus[1]

varus[1]/(varus[1]+varus[2]+modelsignfloci50$Ve) # others =  9.999e-05
varus[2]/(varus[1]+varus[2]+modelsignfloci50$Ve) # signf regions =0




# CBSDs12 -----------------------------------------------------------------

CBSDs12_ALL_SNPS <-CBSDs12_signf$SNP







#combined traits 


signfSNPs_scores12.1 <- dosageC2_123format_CBSDs12[,colnames(dosageC2_123format_CBSDs12) %in%CBSDs12_ALL_SNPS]
length(signfSNPs_scores12.1)#  1192



signfSNPsK12.1<-A.mat(signfSNPs_scores12.1)
dim(signfSNPsK12.1)#298 298

minusscores_signfSNPs12.1<-dosageC2_123format_CBSDs12[,!(colnames(dosageC2_123format_CBSDs12)%in%CBSDs12_ALL_SNPS)]
dim(minusscores_signfSNPs12.1) #298 48599
minusscores_signfSNPsK12.1<-A.mat(minusscores_signfSNPs12.1)
dim(minusscores_signfSNPsK12.1) # 298 298
CBSDs12_na$observationUnitName<-factor(as.character(CBSDs12_na$observationUnitName), levels=rownames(minusscores_signfSNPsK12.1))


Z12.1<-model.matrix(~-1+CBSDs12_na$observationUnitName)


modelsignfloci12.1<-emmremlMultiKernel(y=CBSDs12_na$CBSDs12, X=matrix(1,nrow=nrow(CBSDs12_na),ncol=1), Zlist=list(Z12.1,Z12.1), Klist=list(minusscores_signfSNPsK12.1, signfSNPsK12.1))


varus<-modelsignfloci12.1$Vu*modelsignfloci12.1$weights
varus[2]#8.1201219e-24

varus[1]#2.997977e-24

varus[1]/(varus[1]+varus[2]+modelsignfloci12.1$Ve) # others =  0.014129731
varus[2]/(varus[1]+varus[2]+modelsignfloci12.1$Ve) # signf regions =0.0012794257











#####################################################################################################


#cbsd12
#"S1_18560046"

position <- 18560046  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)

#1 mb above and below 18560046 ( 17560046  -  19560046 )




#"S1_28931143
position <- 128931143  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)



######"S1_28972056"
position <- 28972056  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)


#"S1_28977687"

position <- 28977687  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)


#"S1_29058977

position <- 29058977  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)




#"S1_29063012"

position <- 29063012  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)




#"S1_29103386"

position <- 29103386  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)


#"S7_718516" 
position <- 718516  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)






# "S11_31660720"

position <- 31660720  # Your original position

# Calculate positions above and below
position_above <- position + 1000000  # 1 megabase above
position_below <- position - 1000000  # 1 megabase below

# Create strings with "1 mb" added
position_string <- paste("1 mb above and below", position, "(", position_below, " - ", position_above, ")", sep = " ")

# Print the result
cat(position_string)




