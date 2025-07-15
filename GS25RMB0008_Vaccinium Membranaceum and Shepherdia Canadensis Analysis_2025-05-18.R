# BC Ministry Contract No: GS25RMB0008 ----
# Ministry Representative: Dr. Garth Mowat
# Contractor Information: Evelyn Hamilton / Subcontractor: Dr. Julia Chandler
# Decision tree analysis: code completion date: January 8, 2025
# Extension Note: code completion date: February 28, 2025
# Research Report: code completion date: April 25, 2025

# Install required packages ----
install.packages("Rtools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("moments")

citation("rpart.plot")

# Import the 5 datasets ----

# define the path for the folder containing all datasets
folder.path <- "C:/berries/berry_data/"

# list the files of interest (.csv) in the desired folder
file.list <- list.files(path = folder.path, pattern = "\\.csv$", full.names = TRUE)

# import the files from the desired folder
elements <- lapply(file.list, read.csv)

# create a data frame for each dataset (i.e. element) in the list of imported files
names(elements) <- paste('df', seq_along(file.list), sep = '.')
df.1 <- data.frame(elements$df.1[, c(2, 3, 5, 6, 9, 10, 12, 13, 24, 25, 34)]) 
df.2 <- data.frame(elements$df.2[, c(2, 3, 5, 6, 9, 10, 12, 13, 32, 34, 43)]) 
df.3 <- data.frame(elements$df.3[, c(1, 3, 7, 8, 9, 11, 10, 24, 20, 20, 22)])
df.4 <- data.frame(elements$df.4[, c(3, 6, 8, 9, 35, 15, 13, 31, 24, 24, 26)])
df.5 <- data.frame(elements$df.5[, c(2, 3, 5, 6, 10, 11, 13, 14, 35, 37, 46)])

# change the variable names for the 5 datasets
vars <- c("Site", "Year", "Latitude", "Longitude", "Logging", "Canopy Cover",
          "Aspect", "Slope", "Species", "Species Cover", "Productivity")

names(df.1) <- vars ; names(df.2) <- vars ; names(df.3) <- vars ;
names(df.4) <- vars ; names(df.5) <- vars

# update the 'Year' variable
df.1$Year <- 2016 ; df.2$Year <- 2017 ; df.5$Year <- 2020

# update the 'Species' variable
df.3$Species <- "Black Huckleberry (Vaccinium membranaceum)"
df.4$Species <- "Black Huckleberry (Vaccinium membranaceum)"


# Add elevation points ----

library(elevatr)

# create the function to apply (e.g., retrieving the elevation points for each df)
elevation.function <- function(df) {
  intersect.loc <- data.frame(
    x = df$Longitude,
    y = df$Latitude)
  
  df$elevation <- get_elev_point(
    intersect.loc, prj = "EPSG:4326", src = c("epqs","aws"))
}

# create the list of data frames
df.list <- list(df.1, df.2, df.3, df.4, df.5)

# apply the function to each data frame in the list
elev.result <- lapply(df.list, elevation.function)

# names and create a data frame for each element of the elevation list
names(elev.result) <- paste('elev', seq_along(file.list), sep = '.')
list2env(elev.result,envir=.GlobalEnv)

dat.1 <- data.frame(df.1, elev.1$elevation)
dat.2 <- data.frame(df.2, elev.2$elevation)
dat.3 <- data.frame(df.3, elev.3$elevation)
dat.4 <- data.frame(df.4, elev.4$elevation)
dat.5 <- data.frame(df.5, elev.5$elevation)

names(dat.1)[12] <- "Elevation" ; names(dat.2)[12] <- "Elevation" ; names(dat.3)[12] <- "Elevation" ;
names(dat.4)[12] <- "Elevation" ; names(dat.5)[12] <- "Elevation"


# Add climateBC data   ----

# Format for input to climateBC
# 2016 data
cli.in.2016 <- data.frame(dat.1[, c(1:4,12)])
names(cli.in.2016) <- c("ID1", "ID2", "lat", "long", "el")

# 2017 data
dat.2.3 <- rbind(dat.2, dat.3)
cli.in.2017 <- data.frame(dat.2.3[, c(1:4,12)])
names(cli.in.2017) <- c("ID1", "ID2", "lat", "long", "el")

# 2018 data
cli.in.2018 <- data.frame(dat.4[, c(1:4,12)])
names(cli.in.2018) <- c("ID1", "ID2", "lat", "long", "el")

# 2019 data
cli.in.2020 <- data.frame(dat.5[, c(1:4,12)])
names(cli.in.2020) <- c("ID1", "ID2", "lat", "long", "el")

# export data frames to add climate data to each
write.csv(cli.in.2016, file = "C:/berries/climateBC/input_2016.csv", row.names=FALSE)
write.csv(cli.in.2017, file = "C:/berries/climateBC/input_2017.csv", row.names=FALSE)
write.csv(cli.in.2018, file = "C:/berries/climateBC/input_2018.csv", row.names=FALSE)
write.csv(cli.in.2020, file = "C:/berries/climateBC/input_2020.csv", row.names=FALSE)



# IMPORT the 12 climate datasets

# define the path for the folder containing all datasets
folder.path.climate <- "C:/berries/climateBC/"

# list the files of interest (.csv) in the desired folder
file.list.climate <- list.files(path = folder.path.climate, pattern = "\\.csv$", full.names = TRUE)

# import the files from the desired folder
elements.climate <- lapply(file.list.climate, read.csv)

#name each element by its file in the climateBC folder (keep only the last part)
names(elements.climate) <- paste(as.list(substr(file.list.climate, 26, 41)))

# extract each element into a data frame
list2env(elements.climate,envir=.GlobalEnv)

# update the column names to reflect past years
# 2016
colnames(t_2016_Year_2015)[6:270] <- paste(colnames(t_2016_Year_2015)[6:270], "_1", sep ="")
colnames(t_2016_Year_2014)[6:270] <- paste(colnames(t_2016_Year_2014)[6:270], "_2", sep ="")

# 2017
colnames(t_2017_Year_2016)[6:270] <- paste(colnames(t_2017_Year_2016)[6:270], "_1", sep ="")
colnames(t_2017_Year_2015)[6:270] <- paste(colnames(t_2017_Year_2015)[6:270], "_2", sep ="")

# 2018
colnames(t_2018_Year_2017)[6:270] <- paste(colnames(t_2018_Year_2017)[6:270], "_1", sep ="")
colnames(t_2018_Year_2016)[6:270] <- paste(colnames(t_2018_Year_2016)[6:270], "_2", sep ="")

# 2020
colnames(t_2020_Year_2019)[6:270] <- paste(colnames(t_2020_Year_2019)[6:270], "_1", sep ="")
colnames(t_2020_Year_2018)[6:270] <- paste(colnames(t_2020_Year_2018)[6:270], "_2", sep ="")

# bind site data and all years to reference climate years
df.2016 <- cbind(dat.1, t_2016_Year_2016[3:270], t_2016_Year_2015[6:270], t_2016_Year_2014[6:270])
df.2017 <- cbind(dat.2.3, t_2017_Year_2017[3:270], t_2017_Year_2016[6:270], t_2017_Year_2015[6:270])
df.2018 <- cbind(dat.4, t_2018_Year_2018[3:270], t_2018_Year_2017[6:270], t_2018_Year_2016[6:270])
df.2020 <- cbind(dat.5, t_2020_Year_2020[3:270], t_2020_Year_2019[6:270], t_2020_Year_2018[6:270])


# Create master dataset ----
df <- rbind(df.2016, df.2017, df.2018, df.2020)

# FORMAT master dataset
# update NA, 999 values and remove climate columns (radiation) with only -9999
is.na(df$Aspect) <- df$Aspect == 999
is.na(df$Slope) <- df$Slope == 999
df$Slope[is.na(df$Slope)] <- 0
df$Canopy.Cover[is.na(df$Canopy.Cover)] <- 0
df[df ==-9999] <- NA ; df <- df[colSums(!is.na(df)) > 0]

# Add BEC variants ----
# BEC layer accessed from the Forest Analysis and Inventory Branch (Licence - Open Government Licence - British Columbia)
#https://catalogue.data.gov.bc.ca/dataset/f358a53b-ffde-4830-a325-a5a03ff672c3
# export the df to perform intersection in QGIS (VERSION 3.34.11- Prizren)
write.csv(df, file = paste("C:/berries/GIS/data_df_N1900",Sys.Date(),".csv"),row.names=FALSE)
# perform the intersection in QGIS and then re import the data with the new BEC class

write.csv(df.shep, file = paste("C:/berries/GIS/data_df_shep_N353",Sys.Date(),".csv"),row.names=FALSE)
write.csv(df.vacc, file = paste("C:/berries/GIS/data_df_vacc_N864",Sys.Date(),".csv"),row.names=FALSE)

############################################ START HERE ----
############################################
# Create the directory structure ----
# create a folder in C:// drive named "GS25RMB0008" and add subfolder "2025"
# create the following working directories:

#path <- paste("C:/GS25RMB0008/2025/",Sys.Date(), sep = "")
#dir.create(path)
#setwd(path)

# create main directory
dir <- paste("C:/GS25RMB0008/2025/",Sys.Date(), sep = "")
time <-Sys.time() ; time <- substr(time, 11, 16)
time <- gsub(" ", "_",time) ; time <- gsub(":", "-",time)
path <- paste(dir, time, sep="")
dir.create(path)
setwd(path)
setwd(path)
getwd()

# create sub directories
dir.create(paste(path, "/data", sep = ""))
dir.create(paste(path, "/rpart", sep = ""))
dir.create(paste(path, "/plots", sep = ""))
dir.create(paste(path, "/tables", sep = ""))

##################################################################
# ---> ----> ----> ----> CHANGE THE WORKING DIRECTORY: DATA ----
setwd(paste(path, "/data", sep = ""))

##################################################################
# Import datasets with added BEC variables ----

intersect <- read.csv("C:/GS25RMB0008/2025/data_j_chandler/data_df_N1903 2025-01-10_INTERSECTION.csv")

# build new dataframe with selected variables
dat <- data.frame(intersect[9:11], intersect[2],intersect[1],
                  intersect[761:763], intersect[766],
                  intersect[768:770], intersect[772],
                  intersect[5:6], intersect[3:4], intersect[12],
                  intersect[8], intersect[7], intersect[16:759])

colnames(dat)[colnames(dat) == "VARIANT"] <- "VARIANT_CODE"
colnames(dat)[colnames(dat) == "MAP_LABEL"] <- "VARIANT"

# create the folded aspect (northern hemsiphere calculation, McCune 2007)
# dat$Heatload <- abs(180-abs(dat$Aspect-225))
dat$Aspect <- 180-abs(dat$Aspect-180)

# format the variables and factors
dat[1] <- lapply(dat[1], as.factor)
dat[4:14] <- lapply(dat[4:14], as.factor)
dat[15:764] <- lapply(dat[15:764], as.numeric)

write.csv(dat, file = paste("data_ALL_n1900_", Sys.Date(), ".csv"), row.names=FALSE)

dat.2spp <- subset(dat,
                   Species == "Buffaloberry (Shepherdia canadensis)" | Species == "Black Huckleberry (Vaccinium membranaceum)")

dat.2spp <- subset(dat.2spp, Species.Cover > 0)
dat.2spp$Productivity[is.na(dat.2spp$Productivity)] <- 0
dat.2spp<- droplevels(dat.2spp)
write.csv(dat.2spp, file = paste("data_2spp_n1217_", Sys.Date(), ".csv"), row.names=FALSE)

##################################################################
# Subset each species and export to a csv file ----

# shepherdia

df.shep <- droplevels(subset(dat.2spp, Species == "Buffaloberry (Shepherdia canadensis)"))
write.csv(df.shep, paste(file = "_data_2spp_shep_n353_", Sys.Date(), ".csv"),
          row.names=FALSE)

# vaccinium

df.vacc <- droplevels(subset(dat.2spp, Species == "Black Huckleberry (Vaccinium membranaceum)"))
write.csv(df.vacc, paste(file = "_data_2spp_vacc_n864_", Sys.Date(), ".csv"),
          row.names=FALSE)

##################################################################
# Define datasets and their labels ----

# shepherdia

shep.ZONE.counts <- data.frame(tapply(df.shep$Species, df.shep$ZONE, length))
#print(shep.ZONE.counts)

df.shep.sbs <- subset(df.shep, ZONE == "SBS")
df.shep.ms <- subset(df.shep, ZONE == "MS")
df.shep.bwbs <- subset(df.shep, ZONE == "BWBS")
df.shep.ich <- subset(df.shep, ZONE == "ICH")
df.shep.essf <- subset(df.shep, ZONE == "ESSF")
df.shep.idf <- subset(df.shep, ZONE == "IDF")

# remove observations where climate data was NA (MS=2 / IDF=2)
df.shep.ms <- df.shep.ms[complete.cases(df.shep.ms[21:764]), ]
df.shep.idf <- df.shep.idf[complete.cases(df.shep.idf[21:764]), ]

# vaccinium

vacc.ZONE.counts <- data.frame(tapply(df.vacc$Species, df.vacc$ZONE, length))
#print(vacc.ZONE.counts)

df.vacc.essf <- subset(df.vacc, ZONE == "ESSF")
df.vacc.ich <- subset(df.vacc, ZONE == "ICH")
df.vacc.sbs <- subset(df.vacc, ZONE == "SBS")

# remove observations where climate data was NA (ICH=1 observation)
df.vacc.ich <- df.vacc.ich[complete.cases(df.vacc.ich[21:764]), ]
# remove observations where climate data was NA (ESSF=1 observation)
df.vacc.essf <- df.vacc.essf[complete.cases(df.vacc.essf[18]), ]

# Define the input datasets (Zone level) ----

# the datasets
datasets <- list(df.shep.sbs, df.shep.ms, df.shep.bwbs,
                 df.shep.ich, df.shep.essf, df.shep.idf,
                 df.vacc.essf, df.vacc.ich, df.vacc.sbs)

# name each dataset
names <- list("Shepherdia - SBS", "Shepherdia - MS", "Shepherdia - BWBS",
              "Shepherdia - ICH", "Shepherdia - ESSF", "Shepherdia - IDF",
              "Vaccinium - ESSF", "Vaccinium - ICH", "Vaccinium - SBS")

# Export each of the zonal data files (for mapping) ----

# Loop through datasets to export the zonal data frames
for (i in seq_along(datasets)) {
  data <- datasets[[i]]
  name <- names[i]
  
  write.csv(data, paste(path,  "/data/", name, "_", Sys.Date(), ".csv", sep=""), row.names = FALSE)
  write.csv(data, paste("C:/GS25RMB0008/2025/GIS/zonal_data/zonal_data_", name,"_", Sys.Date(), ".csv"), row.names = FALSE)
}


##################################################################
# ---> ----> ----> ----> CHANGE THE WORKING DIRECTORY: RPART ----
setwd(paste(path, "/rpart", sep = ""))

##################################################################
# Rpart ANALYSIS ----

# Load required packages
require(rpart)
require(rpart.plot)

# Rpart - COVER

# Loop through datasets
for (i in seq_along(datasets)) {
  data <- datasets[[i]]
  name <- names[i]
  
  #convert the logging factor to numeric format
  data[14] <- lapply(data[14], as.numeric)
  
  # Fit the model (with the variant factor)
  mod.cov <- rpart(data$Species.Cover ~ ., method = "anova", data = data[c(9, 14:764)])
  
  # Save text output: model
  sink(paste0(name, "_rpart_cover_", Sys.Date(), ".txt"))
  print(mod.cov)
  print(summary(mod.cov))
  sink(file = NULL)
  
  # Save rpart tree plot
  png(paste0(name, "_rpart_cover_tree_", Sys.Date(), ".png"),
      width = 6,
      height = 6,
      units = "in",
      res = 1200,
      pointsize = 10)
  rpart.plot(mod.cov, cex = 1, type = 1,
             #main = "Species Cover (%)",
             extra = 1, under = TRUE, faclen = 0)
  #plotcp(mod.cov)
  dev.off()
 
  # Fit the model (without the variant factor for correlations and summary statistics)
  mod.cov <- rpart(data$Species.Cover ~ ., method = "anova", data = data[c(14:764)])
  
  # Save text output: site variable correlations
  sink(paste0(name, "_rpart_cover_site_correlations_", Sys.Date(), ".txt"))
  print(cor(data.frame(data[2:3], data[14:20])))
  sink(file = NULL)
  
  # Save text output: important variable correlations
  sink(paste0(name, "_rpart_cover_vars_correlations_", Sys.Date(), ".txt"))
  print(cor(data.frame(data[2:3], data[, names(mod.cov$variable.importance)[1:5]])))
  sink(file = NULL)
  
  # Save text output: important variable summary statistics
  sink(paste0(name, "_rpart_cover_vars_summary_stat_", Sys.Date(), ".txt"))
  print(summary(data.frame(data[2:3], data[, names(mod.cov$variable.importance)[1:5]])))
  sink(file = NULL)
  
  # Export the xy plot for the most important variable
  png(paste0(name, "_rpart_cover_vars_xyplot_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data[, names(mod.cov$variable.importance)[1]]), data$Species.Cover,
       #main = paste(name),
       xlab = names(mod.cov$variable.importance)[1],
       ylab = "Species Cover (%)",
       sub = paste("( n =", nrow(data), " / ", "r =", round(cor(data[names(mod.cov$variable.importance)[1]], data$Species.Cover), digits = 2),")"), cex.sub=0.9)
  dev.off()
}

# the extra plot for SBS cover PAS
png(paste0("Vaccinium - SBS_rpart_cover_vars_xyplot_PAS09-01_", Sys.Date(), ".png"),
    width = 4,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
plot(jitter(df.vacc.sbs$PAS09_1), df.vacc.sbs$Species.Cover,
     #main = "Vaccinium − SBS",
     xlab = "PAS09_1",
     ylab = "Species Cover (%)",
     sub = paste("( n =", nrow(data), " / ",
                 "r =", round(cor(data$PAS09_1, data$Species.Cover),
                              digits = 2),")"), cex.sub=0.9)
dev.off()


# Rpart - ABUNDANCE

# Loop through datasets
for (i in seq_along(datasets)) {
  data <- datasets[[i]]
  name <- names[i]
  
  #convert the logging factor to numeric format
  data[14] <- lapply(data[14], as.numeric)
  
  # Fit the model (with the variant factor)
  mod.pro <- rpart(data$Productivity ~ ., method = "anova", data = data[c(9, 14:764)])
  
  # Save text output: model
  sink(paste0(name, "_rpart_abundance_", Sys.Date(), ".txt"))
  print(mod.pro)
  print(summary(mod.pro))
  sink(file = NULL)
  
  # Save rpart tree plot
  png(paste0(name, "_rpart_abundance_tree_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 10)
  rpart.plot(mod.pro, cex = 0.9, type = 1,
             #main = "Abundance",
             extra = 1, under = TRUE, faclen = 0)
  #plotcp(mod.pro)
  dev.off()
  
  # Fit the model (without the variant factor)
  mod.pro <- rpart(data$Productivity ~ ., method = "anova", data = data[c(14:764)])
  
  # Save text output: important variable correlations
  sink(paste0(name, "_rpart_abundance_vars_correlations_", Sys.Date(), ".txt"))
  print(cor(data.frame(data[2:3], data[, names(mod.pro$variable.importance)[1:5]])))
  sink(file = NULL)
  
  # Save text output: important variable summary statistics
  sink(paste0(name, "_rpart_abundance_vars_summary_stat_", Sys.Date(), ".txt"))
  print(summary(data.frame(data[2:3], data[, names(mod.pro$variable.importance)[1:5]])))
  sink(file = NULL)
  
  # Export the xy plot for the most important variable
  png(paste0(name, "_rpart_abundance_vars_xyplot_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data[, names(mod.pro$variable.importance)[1]]), data$Productivity,
       #main = paste(name),
       xlab = names(mod.pro$variable.importance)[1],
       ylab = "Abundance (berry count)",
       sub = paste("( n =", nrow(data), " / ", "r =", round(cor(data[names(mod.pro$variable.importance)[1]], data$Productivity), digits = 2),")"), cex.sub=0.9)
  dev.off()
}


#mod.pro.site <- rpart(df.shep.bin.lesstop17$Productivity ~ ., method = "anova", data = df.shep.bin.lesstop17[c(4, 14, 15, 18:20)])
#rpart.plot(mod.pro.site)
#summary(mod.pro.site)

#mod.cov.site <- rpart(df.shep.bin.lesstop17$Species.Cover ~ ., method = "anova", data = df.shep.bin.lesstop17[c(4, 14, 15, 18:20)])
#rpart.plot(mod.cov.site)
#summary(mod.cov.site)

##################################################################
# ---> ----> ----> ----> CHANGE THE WORKING DIRECTORY: PLOTS ----
setwd(paste(path, "/plots", sep = ""))

##################################################################
# XYPLOTS (BEC ZONE) #### ----
# xy plot: aspect ----

# Aspect requires removal of IDF zone for shepherdia (all NA) and 
datasets.ASP <- list(df.shep.sbs, df.shep.ms, df.shep.bwbs,
                     df.shep.ich, df.shep.essf,
                     df.vacc.essf, df.vacc.ich, df.vacc.sbs)

# name each dataset
names.ASP <- list("Shepherdia - SBS", "Shepherdia - MS", "Shepherdia - BWBS",
                  "Shepherdia - ICH", "Shepherdia - ESSF",
                  "Vaccinium - ESSF", "Vaccinium - ICH", "Vaccinium - SBS")

for (i in seq_along(datasets.ASP)) {
  data.ASP <- datasets.ASP[[i]]
  name.ASP <- names.ASP[i]
  
  data.ASP <- data.ASP[!is.na(data.ASP$Aspect),]
  
  # COVER
  png(paste0(name.ASP, "_xyplot_cover_site_aspect_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.ASP$Aspect), data.ASP$Species.Cover,
       #main = paste(name.ASP),
       xlab = "Aspect (folded: 0-180 degrees)",
       ylab = "Species Cover (%)",
       sub = paste("( n =", nrow(data.ASP), " / ", "r =", round(cor(data.ASP$Aspect, data.ASP$Species.Cover), digits = 2),")"), cex.sub=0.9)
  dev.off()
  
  # ABUNDANCE
  png(paste0(name.ASP, "_xyplot_abundance_site_aspect_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.ASP$Aspect),data.ASP$Productivity,
       #main = paste(name.ASP),
       xlab = "Aspect (folded: 0-180 degrees)",
       ylab = "Abundance (berry count)",
       sub = paste("( n =", nrow(data.ASP), " / ", "r =", round(cor(data.ASP$Aspect, data.ASP$Productivity), digits = 2),")"), cex.sub=0.9)
  dev.off()
}

# xy plot: canopy cover ----

# define the datasets
datasets <- list(df.shep.sbs, df.shep.ms, df.shep.bwbs,
                 df.shep.ich, df.shep.essf, df.shep.idf,
                 df.vacc.essf, df.vacc.ich, df.vacc.sbs)

# name each dataset
names <- list("Shepherdia - SBS", "Shepherdia - MS", "Shepherdia - BWBS",
              "Shepherdia - ICH", "Shepherdia - ESSF", "Shepherdia - IDF",
              "Vaccinium - ESSF", "Vaccinium - ICH", "Vaccinium - SBS")

# Loop through datasets
for (i in seq_along(datasets)) {
  data <- datasets[[i]]
  name <- names[i]  
  
  # COVER
  png(paste0(name, "_xyplot_cover_site_canopy_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data$Canopy.Cover),data$Species.Cover,
       #main = paste(name),
       xlab = "Canopy Cover (%)",
       ylab = "Species Cover (%)",
       sub = paste("( n =", nrow(data), " / ", "r =", round(cor(data$Canopy.Cover, data$Species.Cover), digits = 2),")"), cex.sub=0.9)
  dev.off()
  
  # ABUNDANCE
  png(paste0(name, "_xyplot_abundance_site_canopy_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data$Canopy.Cover),data$Productivity,
       #main = paste(name),
       xlab = "Canopy Cover (%)",
       ylab = "Abundance (berry count)",
       sub = paste("( n =", nrow(data), " / ", "r =", round(cor(data$Canopy.Cover, data$Productivity), digits = 2),")"), cex.sub=0.9)
  dev.off()
}


# xy plot: elevation ----
# define the datasets

datasets <- list(df.shep.sbs, df.shep.ms, df.shep.bwbs,
                 df.shep.ich, df.shep.essf, df.shep.idf,
                 df.vacc.essf, df.vacc.ich, df.vacc.sbs)

# name each dataset
names <- list("Shepherdia - SBS", "Shepherdia - MS", "Shepherdia - BWBS",
              "Shepherdia - ICH", "Shepherdia - ESSF", "Shepherdia - IDF",
              "Vaccinium - ESSF", "Vaccinium - ICH", "Vaccinium - SBS")

for (i in seq_along(datasets)) {
  data.ELV <- datasets[[i]]
  name.ELV <- names[i]
  
  data.ELV <- data.ELV[!is.na(data.ELV$Elevation),]
  
  # COVER
  png(paste0(name.ELV, "_xyplot_cover_site_elevation_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.ELV$Elevation), data.ELV$Species.Cover,
       #main = paste(name.ELV),
       xlab = "Elevation (m)",
       ylab = "Species Cover (%)",
       sub = paste("( n =", nrow(data.ELV), " / ", "r =", round(cor(data.ELV$Elevation, data.ELV$Species.Cover), digits = 2),")"), cex.sub=0.9)
  dev.off()
  
  # ABUNDANCE
  png(paste0(name.ELV, "_xyplot_abundance_site_elevation_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.ELV$Elevation),data.ELV$Productivity,
       #main = paste(name.ELV),
       xlab = "Elevation (m)",
       ylab = "Abundance (berry count)",
       sub = paste("( n =", nrow(data.ELV), " / ", "r =", round(cor(data.ELV$Elevation, data.ELV$Productivity), digits = 2),")"), cex.sub=0.9)
  dev.off()
}

# xy plot: canopy cover with curve ----

# loess curve model ----

#data <- df.vacc.essf
#data <- df.vacc.ich
#data <- df.vacc.sbs

#data.cov <- data.frame(values = data$Species.Cover)
#Q1 <- quantile(data.cov$values, 0.25)
#Q3 <- quantile(data.cov$values, 0.75)

#IQ <- Q3 - Q1
#lowerbound = Q1 - 1.5*IQ
#upperbound = Q1 + 1.5*IQ

#no.outliers <- subset(data.cov, values > lowerbound & values < upperbound)
#data <- data.frame(no.outliers[1], data[15])
#colnames(data) <- c("Species.Cover", "Canopy.Cover")

# define the datasets

df.vacc3zones <- rbind(df.vacc.essf, df.vacc.ich, df.vacc.sbs)
data.curve <- df.vacc3zones
#data.curve <- df.vacc.essf
#data.curve <- df.vacc.ich
#data.curve <- df.vacc.sbs

# COVER

M1 <- lm(Species.Cover ~ Canopy.Cover, data.curve) #abline(M1, col = "red") #coef(M1)
M2 <- lm(Species.Cover ~ Canopy.Cover + I(Canopy.Cover^2), data.curve) ; summary(M2) #coef(M2)
x.pred <- data.frame(Canopy.Cover = seq(min(data.curve$Canopy.Cover), max(data.curve$Canopy.Cover), length.out = nrow(data.curve)))
y.pred <- predict(M2, x.pred)

png(paste0("curve_xyplot_cover_canopy_curve_", Sys.Date(), ".png"),
    width = 4,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
plot(jitter(data.curve$Canopy.Cover),data.curve$Species.Cover,
     #main = paste(name),
     xlab = "Canopy Cover (%)",
     ylab = "Species Cover (%)",
     sub = paste("( n =", nrow(data.curve), " / ", "r =", round(cor(data.curve$Canopy.Cover, data.curve$Species.Cover), digits = 2),")"), cex.sub=0.9)
lines(x.pred$Canopy.Cover, y.pred, col = "red")
dev.off()

lm(formula = Species.Cover ~ Canopy.Cover + I(Canopy.Cover^2), data = data)
summary(M2)$coefficients[, "Pr(>|t|)"]
summary(M2)$coefficients

# adjusted r-squared
r_squared.adj <- summary(M2)$adj.r.squared

# p-value
# f_statistic <- summary(M2)$fstatistic
# p_value <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)

# result2 <- paste("Adjusted R-squared =", round(r_squared.adj, digits = 3))
# result3 <- paste("p-value =", round(p_value, digits = 3))


# ABUNDANCE

data.curve <- df.vacc.essf
data.curve <- df.vacc.ich
data.curve <- df.vacc.sbs

M1 <- lm(Productivity ~ Canopy.Cover, data.curve) #abline(M1, col = "red") #coef(M1)
M2 <- lm(Productivity ~ Canopy.Cover + I(Canopy.Cover^2), data.curve)
summary(M2) #coef(M2)
x.pred <- data.frame(Canopy.Cover = seq(min(data.curve$Canopy.Cover), max(data.curve$Canopy.Cover), length.out = 50))
y.pred <- predict(M2, x.pred)

png(paste0("curve_xyplot_abundance_canopy_curve_", Sys.Date(), ".png"),
    width = 4,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
plot(jitter(data.curve$Canopy.Cover),data.curve$Productivity,
     #main = paste(name),
     xlab = "Canopy Cover (%)",
     ylab = "Abundance (berry count)",
     sub = paste("( n =", nrow(data.curve), " / ", "r =", round(cor(data.curve$Canopy.Cover, data.curve$Productivity), digits = 2),")"), cex.sub=0.9)
lines(x.pred$Canopy.Cover, y.pred, col = "red")
dev.off()

# histograms ----

for (i in seq_along(datasets)) {
  data.ELV <- datasets[[i]]
  name.ELV <- names[i]
  
# COVER
png(paste0(name.ELV, "_xyplot_cover_site_elevation_hist_", Sys.Date(), ".png"),
    width = 4,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
hist(data.ELV$Species.Cover,
     #main = paste(name.ELV),
     xlab = "Elevation (m)",
     ylab = "Species Cover (%)")
dev.off()

# ABUNDANCE
png(paste0(name.ELV, "_xyplot_abundance_site_elevation_hist_", Sys.Date(), ".png"),
    width = 4,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
hist(data.ELV$Productivity,
     #main = paste(name.ELV),
     xlab = "Elevation (m)",
     ylab = "Abundance (berry count)")
dev.off()
}

# xy plot: logging ----
# define the datasets
# logging factor must be converted to numeric format

datasets <- list(df.shep.sbs, df.shep.ms, df.shep.bwbs,
                 df.shep.ich, df.shep.essf, df.shep.idf,
                 df.vacc.essf, df.vacc.ich, df.vacc.sbs)

# name each dataset
names <- list("Shepherdia - SBS", "Shepherdia - MS", "Shepherdia - BWBS",
              "Shepherdia - ICH", "Shepherdia - ESSF", "Shepherdia - IDF",
              "Vaccinium - ESSF", "Vaccinium - ICH", "Vaccinium - SBS")

for (i in seq_along(datasets)) {
  data.LOG <- datasets[[i]]
  name <- names[i]
  
  #convert logging factor to numeric
  data.LOG[14] <- lapply(data.LOG[14], as.numeric)
  
  # COVER
  png(paste0(name, "_xyplot_cover_site_logging_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.LOG$Logging),data.LOG$Species.Cover,
       #main = paste(name),
       xlab = "Logging",
       ylab = "Species Cover (%)",
       sub = paste("( n =", nrow(data.LOG), " / ", "r =", round(cor(data.LOG$Logging, data.LOG$Species.Cover), digits = 2),")"), cex.sub=0.9)
  dev.off()
  
  # ABUNDANCE
  png(paste0(name, "_xyplot_abundance_site_logging_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.LOG$Logging),data.LOG$Productivity,
       #main = paste(name),
       xlab = "Logging",
       ylab = "Abundance (berry count)",
       sub = paste("( n =", nrow(data.LOG), " / ", "r =", round(cor(data.LOG$Logging, data.LOG$Productivity), digits = 2),")"), cex.sub=0.9)
  dev.off()  
}


# xy plot: slope ----
# define the datasets
datasets <- list(df.shep.sbs, df.shep.ms, df.shep.bwbs,
                 df.shep.ich, df.shep.essf, df.shep.idf,
                 df.vacc.essf, df.vacc.ich, df.vacc.sbs)

# name each dataset
names <- list("Shepherdia - SBS", "Shepherdia - MS", "Shepherdia - BWBS",
              "Shepherdia - ICH", "Shepherdia - ESSF", "Shepherdia - IDF",
              "Vaccinium - ESSF", "Vaccinium - ICH", "Vaccinium - SBS")

# Loop through datasets
for (i in seq_along(datasets)) {
  data <- datasets[[i]]
  name <- names[i]  
  
  # COVER
  png(paste0(name, "_xyplot_cover_site_slope_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data$Slope),data$Species.Cover,
       #main = paste(name),
       xlab = "Slope (%)",
       ylab = "Species Cover (%)",
       sub = paste("( n =", nrow(data), " / ", "r =", round(cor(data$Slope, data$Species.Cover), digits = 2),")"), cex.sub=0.9)
  dev.off()
  
  # ABUNDANCE
  png(paste0(name, "_xyplot_abundance_site_slope_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data$Slope),data$Productivity,
       #main = paste(name),
       xlab = "Slope (%)",
       ylab = "Abundance (berry count)",
       sub = paste("( n =", nrow(data), " / ", "r =", round(cor(data$Slope, data$Productivity), digits = 2),")"), cex.sub=0.9)
  dev.off()
}

# xy plot: abundance x species cover ----

# define the datasets
datasets <- list(df.shep.sbs, df.shep.ms, df.shep.bwbs,
                 df.shep.ich, df.shep.essf, df.shep.idf,
                 df.vacc.essf, df.vacc.ich, df.vacc.sbs)

# name each dataset
names <- list("Shepherdia - SBS", "Shepherdia - MS", "Shepherdia - BWBS",
              "Shepherdia - ICH", "Shepherdia - ESSF", "Shepherdia - IDF",
              "Vaccinium - ESSF", "Vaccinium - ICH", "Vaccinium - SBS")

# Loop through datasets
for (i in seq_along(datasets)) {
  data <- datasets[[i]]
  name <- names[i]
  
  # ABUNDANCE
  png(paste0(name, "_xyplot_abundance_speciescover_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data$Species.Cover),data$Productivity,
       #main = paste(name),
       xlab = "Species Cover (%)",
       ylab = "Abundance (berry count)",
       sub = paste("( n =", nrow(data), " / ", "r =", round(cor(data$Species.Cover, data$Productivity), digits = 2),")"), cex.sub=0.9)
  dev.off()
}


##################################################################
# XYPLOTS (VACCINIUM BEC VARIANT) #### ----
# xy plot: logging by variant over canopy cover ----

# Create and define selected variant datasets
vacc.ESSFdc1 <- df.vacc[c(df.vacc$VARIANT == "ESSFdc1"),]
vacc.ESSFdk1 <- df.vacc[c(df.vacc$VARIANT == "ESSFdk1"),]
vacc.ESSFmh <- df.vacc[c(df.vacc$VARIANT == "ESSFmh"),]
vacc.ESSFvc <- df.vacc[c(df.vacc$VARIANT == "ESSFvc"),]
vacc.ESSFvcw <- df.vacc[c(df.vacc$VARIANT == "ESSFvcw"),]

datasets.ESSF.VARIANTS <- list(vacc.ESSFdc1, vacc.ESSFdk1, vacc.ESSFmh, vacc.ESSFvc, vacc.ESSFvcw)

# name each dataset
#names.ESSF.VARIANTS <- list(
#  "Engelmann Spruce-Subalpine Fir (Dry Cold - Monashee)",
#  "Engelmann Spruce-Subalpine Fir (Dry Cool - Elk)",
#  "Engelmann Spruce-Subalpine Fir (Moist Hot)",
#  "Engelmann Spruce-Subalpine Fir (Very Wet Cold)",
#  "Engelmann Spruce-Subalpine Fir (Very Wet Cold Woodland)")


names.ESSF.VARIANTS <- list("ESSFdc1", "ESSFdk1", "ESSFmh", "ESSFvc", "ESSFvcw")

# define colours and shapes
colors <- c("black", "black")
shapes <- c(17, 1)

# Create the plot
for (i in seq_along(datasets.ESSF.VARIANTS)) {
  data.VARIANTS <- datasets.ESSF.VARIANTS[[i]]
  name.VARIANTS <- names.ESSF.VARIANTS[i]
  
  # COVER
  png(paste0("Vaccinium - ", name.VARIANTS, "_xyplot_variant_species_cover_canopy_cover_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.VARIANTS$Canopy.Cover), data.VARIANTS$Species.Cover,
       col = colors[as.numeric(data.VARIANTS$Logging)],
       pch = shapes[as.numeric(data.VARIANTS$Logging)],
       #main = paste(name.VARIANTS),
       xlab = "Canopy Cover (%)",
       ylab = "Species Cover (%)")
  legend("topright", legend = c("No Logging", "Logging"), col = colors, pch = shapes)
  dev.off()
}

for (i in seq_along(datasets.ESSF.VARIANTS)) {
  data.VARIANTS <- datasets.ESSF.VARIANTS[[i]]
  name.VARIANTS <- names.ESSF.VARIANTS[i]
  
  #ABUNDANCE
  png(paste0("Vaccinium - ", name.VARIANTS, "_xyplot_variant_abundance_canopy_cover_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.VARIANTS$Canopy.Cover), data.VARIANTS$Productivity,
       col = colors[as.numeric(data.VARIANTS$Logging)],
       pch = shapes[as.numeric(data.VARIANTS$Logging)],
       #main = paste(name.VARIANTS),
       xlab = "Canopy Cover (%)",
       ylab = "Abundance (berry count)")
  legend("topright", legend = c("No Logging", "Logging"), col = colors, pch = shapes)
  dev.off()
}  

for (i in seq_along(datasets.ESSF.VARIANTS)) {
  data.VARIANTS <- datasets.ESSF.VARIANTS[[i]]
  name.VARIANTS <- names.ESSF.VARIANTS[i]
  
  #ABUNDANCE X SPECIES COVER
  png(paste0("Vaccinium - ", name.VARIANTS, "_xyplot_variant_abundance_species_cover_", Sys.Date(), ".png"),
      width = 4,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(data.VARIANTS$Species.Cover), data.VARIANTS$Productivity,
       col = colors[as.numeric(data.VARIANTS$Logging)],
       pch = shapes[as.numeric(data.VARIANTS$Logging)],
       #main = paste(name.VARIANTS),
       xlab = "Species Cover (%)",
       ylab = "Abundance (berry count)")
  legend("topright", legend = c("No Logging", "Logging"), col = colors, pch = shapes)
  dev.off()
}



# define dataframe
df.shep6zones <- rbind(df.shep.bwbs, df.shep.essf, df.shep.ich, df.shep.idf, df.shep.ms, df.shep.sbs)

# define colours and shapes
colors <- c("black", "black")
shapes <- c(2, 1)

# COVER

png(paste0("Shepherdia - xyplot_species_cover_canopy_cover_", Sys.Date(), ".png"),
    width = 8,
    height = 4,
    units = "in",
    res = 1200,
pointsize = 12)
par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
plot(jitter(df.shep6zones$Canopy.Cover), df.shep6zones$Species.Cover,
col = colors[as.numeric(df.shep6zones$Logging)],
pch = shapes[as.numeric(df.shep6zones$Logging)],
#main = 
xlab = "Canopy Cover (%)",
ylab = "Species Cover (%)")
legend("topright", legend = c("No Logging", "Logging"), col = colors, pch = shapes)
dev.off()

#ABUNDANCE

png(paste0("Shepherdia - xyplot_abundance_canopy_cover_", Sys.Date(), ".png"),
      width = 8,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(df.shep6zones$Canopy.Cover), df.shep6zones$Productivity,
       col = colors[as.numeric(df.shep6zones$Logging)],
       pch = shapes[as.numeric(df.shep6zones$Logging)],
       #main = 
       xlab = "Canopy Cover (%)",
       ylab = "Abundance (berry count)")
  legend("topright", legend = c("No Logging", "Logging"), col = colors, pch = shapes)
  dev.off()

#ABUNDANCE X SPECIES COVER

png(paste0("Shepherdia - xyplot_abundance_species_cover_", Sys.Date(), ".png"),
      width = 8,
      height = 4,
      units = "in",
      res = 1200,
      pointsize = 12)
  par(mai = c(1, 0.8, 0.1, 0.5) + 0.1)
  plot(jitter(df.shep6zones$Species.Cover), df.shep6zones$Productivity,
       col = colors[as.numeric(df.shep6zones$Logging)],
       pch = shapes[as.numeric(df.shep6zones$Logging)],
       #main = 
       xlab = "Species Cover (%)",
       ylab = "Abundance (berry count)")
  legend("topright", legend = c("No Logging", "Logging"), col = colors, pch = shapes)
  dev.off()


##################################################################
# BARPLOTS of response to logging by variant for each species----

df.shep2 <- droplevels(subset(dat.2spp, Species == "Buffaloberry (Shepherdia canadensis)"))
df.vacc2 <- droplevels(subset(dat.2spp, Species == "Black Huckleberry (Vaccinium membranaceum)"))

df.shep2$Logging <- factor(df.shep2$Logging)
df.vacc2$Logging <- factor(df.vacc2$Logging)

# load ggplot2 package to produce boxplots

library(ggplot2)

# The lower and upper hinges correspond to the first and third quartiles (25th and 75th percentiles)
# the median, two hinges and two whiskers (95%)

# shepherdia ----

df.shep3 <- df.shep2[c(df.shep2$Productivity < 20000),]

# vertical ----

# COVER
png(paste0("Shepherdia_barplot_vertical_cover_", Sys.Date(), ".png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep3, aes(fill=Logging, y=Species.Cover, VARIANT)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Variant") +
  ylab("Species Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()
dev.off()

# ABUNDANCE
png(paste0("Shepherdia_barplot_vertical_abundance_", Sys.Date(), ".png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep3, aes(fill=Logging, y=Productivity, VARIANT)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Variant") +
  ylab("Abundance (berry count)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()
dev.off()

# CANOPY COVER
png(paste0("Shepherdia_barplot_vertical_canopycover_", Sys.Date(), ".png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep3, aes(fill=Logging, y=Canopy.Cover, VARIANT)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Variant") +
  ylab("Canopy Cover (%))") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()
dev.off()


# horizontal ----

# COVER
png(paste0("Shepherdia_barplot_horizontal_cover_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep3, aes(fill=Logging, y=Species.Cover, VARIANT)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Variant") +
  ylab("Species Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# ABUNDANCE
png(paste0("Shepherdia_barplot_horizontal_abundance_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep3, aes(fill=Logging, y=Productivity, VARIANT)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Variant") +
  ylab("Abundance (berry count)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# CANOPY COVER
png(paste0("Shepherdia_barplot_horizontal_canopycover_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep3, aes(fill=Logging, y=Canopy.Cover, VARIANT)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Variant") +
  ylab("Canopy Cover (%))") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# BARPLOTS of response to logging by zone for each species----
# vertical ----

# define dataframe
df.shep6zones <- rbind(df.shep.bwbs, df.shep.essf, df.shep.ich, df.shep.idf, df.shep.ms, df.shep.sbs)

df.shep6zones$Logging <- gsub("FALSE", "No Logging", df.shep6zones$Logging)
df.shep6zones$Logging <- gsub("TRUE", "Logging", df.shep6zones$Logging)

# COVER
png(paste0("Shepherdia_barplot_ZONE_cover_", Sys.Date(), ".png"),
    width = 7,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep6zones, aes(fill=Logging, y=Species.Cover, ZONE)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("BEC Zone") +
  ylab("Species Cover (%)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip() +
  theme(legend.position = c(0.9, 0.35),
        legend.background = element_rect(fill = "white", color = "black"))
dev.off()

# ABUNDANCE
df.shep6zones.lesstop17 <- df.shep6zones[c(df.shep6zones$Productivity < 11000),]

png(paste0("Shepherdia_barplot_ZONE_abundance_", Sys.Date(), ".png"),
    width = 7,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep6zones.lesstop17, aes(fill=Logging, y=Productivity, ZONE)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("BEC Zone") +
  ylab("Abundance (berry count)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip() +
  theme(legend.position = c(0.9, 0.35),
        legend.background = element_rect(fill = "white", color = "black"))
dev.off()

# CANOPY COVER
png(paste0("Shepherdia_barplot_ZONE_canopycover_", Sys.Date(), ".png"),
    width = 7,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep6zones, aes(fill=Logging, y=Canopy.Cover, ZONE)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("BEC Zone") +
  ylab("Canopy Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip() +
  theme(legend.position = c(0.9, 0.77),
      legend.background = element_rect(fill = "white", color = "black"))
dev.off()


# vaccinium ----

df.vacc3 <- df.vacc2[c(df.vacc2$Productivity < 20000),]

# vertical ----

# COVER
png(paste0("Vaccinium_barplot_vertical_cover_", Sys.Date(), ".png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3, aes(fill=Logging, y=Species.Cover, VARIANT)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("Variant") +
  ylab("Species Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()
dev.off()

# ABUNDANCE
png(paste0("Vaccinium_barplot_vertical_abundance_", Sys.Date(), ".png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3, aes(fill=Logging, y=Productivity, VARIANT)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("Variant") +
  ylab("Abundance (berry count)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()
dev.off()

# CANOPY COVER
png(paste0("Vaccinium_barplot_vertical_canopycover_", Sys.Date(), ".png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3, aes(fill=Logging, y=Canopy.Cover, VARIANT)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("Variant") +
  ylab("Canopy Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()
dev.off()

# horizontal ----

# COVER
png(paste0("Vaccinium_barplot_horizontal_cover_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3, aes(fill=Logging, y=Species.Cover, VARIANT)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("Variant") +
  ylab("Species Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# ABUNDANCE
png(paste0("Vaccinium_barplot_horizontal_abundance_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3, aes(fill=Logging, y=Productivity, VARIANT)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("Variant") +
  ylab("Abundance (berry count)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# CANOPY COVER
png(paste0("Vaccinium_barplot_horizontal_canopycover_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3, aes(fill=Logging, y=Canopy.Cover, VARIANT)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("Variant") +
  ylab("Canopy Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# log transformed ----

# shepherdia

# COVER
png(paste0("Shepherdia_barplot_horizontal_log_cover_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep2, aes(fill=Logging, y=log(Species.Cover), VARIANT)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Variant") +
  ylab("Species Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# ABUNDANCE
png(paste0("Shepherdia_barplot_horizontal_log_abundance_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep2, aes(fill=Logging, y=log(Productivity), VARIANT)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Variant") +
  ylab("Abundance (berry count)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()


# vaccinium

# COVER
png(paste0("Vaccinium_barplot__horizontal_log_cover_", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc2, aes(fill=Logging, y=log(Species.Cover), VARIANT)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("Variant") +
  ylab("Species Cover (%)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()

# ABUNDANCE
png(paste0("Vaccinium_barplot__horizontal_log_abundance", Sys.Date(), ".png"),
    width = 10,
    height = 6,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc2, aes(fill=Logging, y=log(Productivity), VARIANT)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("Variant") +
  ylab("Abundance (berry count)") + 
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
dev.off()


# BARPLOTS of response to logging by zone for each species----

# vertical ----
# df.vacc3zones <- rbind(df.vacc.essf, df.vacc.ich, df.vacc.sbs)
# COVER
png(paste0("Vaccinium_barplot_ZONE_cover_", Sys.Date(), ".png"),
    width = 8,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3zones, aes(fill=Logging, y=Species.Cover, ZONE)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("BEC Zone") +
  ylab("Species Cover (%)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()
dev.off()
    
# ABUNDANCE
png(paste0("Vaccinium_barplot_ZONE_abundance_", Sys.Date(), ".png"),
    width = 8,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3zones, aes(fill=Logging, y=Productivity, ZONE)) +
  #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
  xlab("BEC Zone") +
  ylab("Abundance (berry count)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()
dev.off()

# CANOPY COVER
    png(paste0("Vaccinium_barplot_ZONE_canopycover_", Sys.Date(), ".png"),
        width = 8,
        height = 5,
        units = "in",
        res = 1200,
        pointsize = 12)
    ggplot(df.vacc3zones, aes(fill=Logging, y=Canopy.Cover, ZONE)) +
      #ggtitle("Black Huckleberry", expression(italic("Vaccinium membranaceum"))) +
      xlab("BEC Zone") +
      ylab("Canopy Cover (%)") + 
      #geom_bar(position='dodge', stat='identity') +
      scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
      stat_boxplot(geom ='errorbar') +
      geom_boxplot() +
      theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
      coord_flip()
    dev.off()
    
# create categorical factors for canopy cover and elevation ----

# shepherdia
# BARPLOTS of response to logging by zone for all zones ----
# shepherdia ----
df.shep.bin <- df.shep

df.shep.bin$Logging <- gsub("FALSE", "No Logging", df.shep.bin$Logging)
df.shep.bin$Logging <- gsub("TRUE", "Logging", df.shep.bin$Logging)

df.shep.bin$Canopy.Cover.bin <- df.shep.bin$Canopy.Cover

df.shep.bin$Canopy.Cover.bin[df.shep.bin$Canopy.Cover.bin > -1 &  df.shep.bin$Canopy.Cover.bin < 11] <- "0-10"
df.shep.bin$Canopy.Cover.bin[df.shep.bin$Canopy.Cover.bin > 10 &  df.shep.bin$Canopy.Cover.bin < 21] <- "11-20"
df.shep.bin$Canopy.Cover.bin[df.shep.bin$Canopy.Cover.bin > 20 &  df.shep.bin$Canopy.Cover.bin < 31] <- "21-30"
df.shep.bin$Canopy.Cover.bin[df.shep.bin$Canopy.Cover.bin > 30 &  df.shep.bin$Canopy.Cover.bin < 41] <- "31-40"
df.shep.bin$Canopy.Cover.bin[df.shep.bin$Canopy.Cover.bin > 40 &  df.shep.bin$Canopy.Cover.bin < 51] <- "41-50"
df.shep.bin$Canopy.Cover.bin[df.shep.bin$Canopy.Cover.bin > 50 &  df.shep.bin$Canopy.Cover.bin < 61] <- "51-60"
df.shep.bin$Canopy.Cover.bin[df.shep.bin$Canopy.Cover.bin > 60 &  df.shep.bin$Canopy.Cover.bin < 71] <- "61-70"
df.shep.bin$Canopy.Cover.bin[df.shep.bin$Canopy.Cover.bin > 70 &  df.shep.bin$Canopy.Cover.bin < 81] <- "71-80"

# COVER

png(paste0("Shepherdia-barplot_canopy_bin_cover_", Sys.Date(), ".png"),
    width = 6,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep.bin, aes(fill=Logging, y=Species.Cover, Canopy.Cover.bin)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Canopy Cover (%)") +
  ylab("Species Cover (%)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(legend.position = c(0.115, 0.86),
      legend.background = element_rect(fill = "white", color = "black"))
dev.off()

# ABUNDANCE

df.shep.bin.lesstop17 <- df.shep.bin[c(df.shep.bin$Productivity < 11000),]

png(paste0("Shepherdia-barplot_canopy_bin_abundance", Sys.Date(), ".png"),
    width = 6,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep.bin.lesstop17, aes(fill=Logging, y=Productivity, Canopy.Cover.bin)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Canopy Cover (%)") +
  ylab("Abundance (berry count)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(legend.position = c(0.88, 0.86),
        legend.background = element_rect(fill = "white", color = "black"))
dev.off()

# ABUNDANCE VS SPECIES COVER

# this does not include one observation outlier: species cover = 31.5 / abundance = 12046

df.shep.bin.lesstop17$Species.Cover.bin <- df.shep.bin.lesstop17$Species.Cover

df.shep.bin.lesstop17$Species.Cover.bin[df.shep.bin.lesstop17$Species.Cover > 0.001 & df.shep.bin.lesstop17$Species.Cover < 4.999] <- "0-5"
df.shep.bin.lesstop17$Species.Cover.bin[df.shep.bin.lesstop17$Species.Cover > 4.999 &  df.shep.bin.lesstop17$Species.Cover < 9.999] <- "5-10"
df.shep.bin.lesstop17$Species.Cover.bin[df.shep.bin.lesstop17$Species.Cover > 9.999 &  df.shep.bin.lesstop17$Species.Cover < 14.999] <- "10-15"
df.shep.bin.lesstop17$Species.Cover.bin[df.shep.bin.lesstop17$Species.Cover > 14.999 &  df.shep.bin.lesstop17$Species.Cover < 19.999] <- "15-20"
df.shep.bin.lesstop17$Species.Cover.bin[df.shep.bin.lesstop17$Species.Cover > 19.999 &  df.shep.bin.lesstop17$Species.Cover < 24.999] <- "20-25"
df.shep.bin.lesstop17$Species.Cover.bin[df.shep.bin.lesstop17$Species.Cover > 24.999 &  df.shep.bin.lesstop17$Species.Cover < 32] <- "25-30"

df.shep.bin.lesstop17$Species.Cover.bin=factor(
  df.shep.bin.lesstop17$Species.Cover.bin, levels=c(
    "0-5", "5-10", "10-15", "15-20", "20-25", "25-30"))

png(paste0("Shepherdia-barplot_bin_abundance_vs_species_cover", Sys.Date(), ".png"),
    width = 6,
    height = 4,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep.bin.lesstop17, aes(fill=Logging, y=Productivity, Species.Cover.bin)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Buffaloberry Cover (%)") +
  ylab("Abundance (berry count)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(legend.position = c(0.88, 0.86),
        legend.background = element_rect(fill = "white", color = "black"))
dev.off()


# BARPLOTS of response to logging by zone for each zone ----

df.shep6zones <- rbind(df.shep.bwbs, df.shep.essf, df.shep.ich, df.shep.idf, df.shep.ms, df.shep.sbs)

# build a dataframe to bind to the dataframe to provide additional categories in the plots
# create vectors with sequences
# canopy cover
seq.shep.bec1 <- c(rep(c("ESSF"), each = 8), rep(c("ICH"), each = 8),
               rep(c("SBS"), each = 8), rep(c("SBWS"), each = 8),
               rep(c("MS"), each = 8), rep(c("IDF"), each = 8))
seq.shep.can <- c(5, 15, 25, 35, 45, 55, 65, 75)
seq.shep.canopy <- c(seq.shep.can, seq.shep.can, seq.shep.can, seq.shep.can, seq.shep.can, seq.shep.can)
# crate dataframe to add vectors to
df.shep6zones.bin2 <- df.shep6zones[1:48, ]
df.shep6zones.bin2[,] <- NA
df.shep6zones.bin2$ZONE <- seq.shep.bec1
df.shep6zones.bin2$Canopy.Cover <- seq.shep.canopy
df.shep6zones.bin <- rbind(df.shep6zones, df.shep6zones.bin2)

# create bin factor column : canopy cover 
df.shep6zones.bin <- df.shep6zones
df.shep6zones.bin$Canopy.Cover.bin <- df.shep6zones.bin$Canopy.Cover

df.shep6zones.bin$Canopy.Cover.bin[df.shep6zones.bin$Canopy.Cover.bin > -1 &  df.shep6zones.bin$Canopy.Cover.bin < 11] <- "0-10"
df.shep6zones.bin$Canopy.Cover.bin[df.shep6zones.bin$Canopy.Cover.bin > 10 &  df.shep6zones.bin$Canopy.Cover.bin < 21] <- "11-20"
df.shep6zones.bin$Canopy.Cover.bin[df.shep6zones.bin$Canopy.Cover.bin > 20 &  df.shep6zones.bin$Canopy.Cover.bin < 31] <- "21-30"
df.shep6zones.bin$Canopy.Cover.bin[df.shep6zones.bin$Canopy.Cover.bin > 30 &  df.shep6zones.bin$Canopy.Cover.bin < 41] <- "31-40"
df.shep6zones.bin$Canopy.Cover.bin[df.shep6zones.bin$Canopy.Cover.bin > 40 &  df.shep6zones.bin$Canopy.Cover.bin < 51] <- "41-50"
df.shep6zones.bin$Canopy.Cover.bin[df.shep6zones.bin$Canopy.Cover.bin > 50 &  df.shep6zones.bin$Canopy.Cover.bin < 61] <- "51-60"
df.shep6zones.bin$Canopy.Cover.bin[df.shep6zones.bin$Canopy.Cover.bin > 60 &  df.shep6zones.bin$Canopy.Cover.bin < 71] <- "61-70"
df.shep6zones.bin$Canopy.Cover.bin[df.shep6zones.bin$Canopy.Cover.bin > 70 &  df.shep6zones.bin$Canopy.Cover.bin < 81] <- "71-80"

# COVER

#[df.shep6zones.bin$ZONE == "BWBS", ]
png(paste0("Shepherdia_barplot_canopy_bin_cover_bwbs_", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep6zones.bin[df.shep6zones.bin$ZONE == "SBS", ], aes(fill=Logging, y=Species.Cover, Canopy.Cover.bin)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Canopy Cover (%)") +
  ylab("Species Cover (%)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()
dev.off()

# ABUNDANCE

df.shep6zones.bin.lesstop17 <- df.shep6zones.bin[c(df.shep6zones.bin$Productivity < 11000),]

png(paste0("Shepherdia_barplot_canopy_bin_abundance_bwbs_", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep6zones.bin.lesstop17[df.shep6zones.bin.lesstop17$ZONE == "SBS", ], aes(fill=Logging, y=Productivity, Canopy.Cover.bin)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Canopy Cover (%)") +
  ylab("Abundance (berry count)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()
dev.off()

# ABUNDANCE VS SPECIES COVER

# this does not include one observation outlier: species cover = 31.5 / abundance = 12046
df.shep6zones.binx <- df.shep6zones.bin.lesstop17
df.shep6zones.binx$Species.Cover.bin <- df.shep6zones.binx$Species.Cover

df.shep6zones.binx$Species.Cover.bin[df.shep6zones.binx$Species.Cover > 0.001 & df.shep6zones.binx$Species.Cover < 4.999] <- "0-5"
df.shep6zones.binx$Species.Cover.bin[df.shep6zones.binx$Species.Cover > 4.999 &  df.shep6zones.binx$Species.Cover < 9.999] <- "5-10"
df.shep6zones.binx$Species.Cover.bin[df.shep6zones.binx$Species.Cover > 9.999 &  df.shep6zones.binx$Species.Cover < 14.999] <- "10-15"
df.shep6zones.binx$Species.Cover.bin[df.shep6zones.binx$Species.Cover > 14.999 &  df.shep6zones.binx$Species.Cover < 19.999] <- "15-20"
df.shep6zones.binx$Species.Cover.bin[df.shep6zones.binx$Species.Cover > 19.999 &  df.shep6zones.binx$Species.Cover < 24.999] <- "20-25"
df.shep6zones.binx$Species.Cover.bin[df.shep6zones.binx$Species.Cover > 24.999 &  df.shep6zones.binx$Species.Cover < 32] <- "25-30"

df.shep6zones.binx$Species.Cover.bin=factor(
  df.shep6zones.binx$Species.Cover.bin, levels=c(
    "0-5", "5-10", "10-15", "15-20", "20-25", "25-30"))

#[df.shep6zones2$ZONE == "BWBS", ]
png(paste0("Shepherdia_barplot_bin_abundance_vs_species_cover_bwbs_", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep6zones.binx[df.shep6zones.binx$ZONE == "BWBS", ], aes(fill=Logging, y=Productivity, Species.Cover.bin)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Buffaloberry Cover (%)") +
  ylab("Abundance (berry count)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()
dev.off()

# ELEVATION

# build a dataframe to bind to the dataframe to provide additional categories in the plots
# create vectors with sequences

seq.shep.bec2 <- c(rep(c("ESSF"), each = 10), rep(c("ICH"), each = 10),
                   rep(c("SBS"), each = 10), rep(c("BWBS"), each = 10),
                   rep(c("MS"), each = 10), rep(c("IDF"), each = 10))
seq.shep.elev <- c(450, 600, 750, 900, 1050, 1200, 1350, 1500, 1650, 1800)
seq.shep.elevation <- c(seq.shep.elev, seq.shep.elev, seq.shep.elev, seq.shep.elev, seq.shep.elev, seq.shep.elev)
# crate dataframe to add vectors to
df.shep6zones.bin3 <- df.shep6zones[1:60, ]
df.shep6zones.bin3[,] <- NA
df.shep6zones.bin3$ZONE <- seq.shep.bec2
df.shep6zones.bin3$Elevation <- seq.shep.elevation
df.shep6zones.bin <- rbind(df.shep6zones, df.shep6zones.bin3)
#################################

df.shep6zones.bin <- df.shep6zones
df.shep6zones.bin$Elevation.bin <- df.shep6zones.bin$Elevation

df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 399 & df.shep6zones.bin$Elevation < 551] <- "400-550"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 549 & df.shep6zones.bin$Elevation < 701] <- "550-700"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 699 & df.shep6zones.bin$Elevation < 851] <- "700-850"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 849 & df.shep6zones.bin$Elevation < 1001] <- "850-1000"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 999 & df.shep6zones.bin$Elevation < 1151] <- "1000-1150"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 1149 & df.shep6zones.bin$Elevation < 1301] <- "1150-1300"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 1299 & df.shep6zones.bin$Elevation < 1451] <- "1300-1450"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 1449 & df.shep6zones.bin$Elevation < 1601] <- "1450-1600"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 1599 & df.shep6zones.bin$Elevation < 1751] <- "1600-1750"
df.shep6zones.bin$Elevation.bin[df.shep6zones.bin$Elevation > 1749 & df.shep6zones.bin$Elevation < 1901] <- "1750-1900"

# COVER

png(paste0("Shepherdia_barplot_elevation_bin_cover_", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep6zones.bin, aes(fill=Logging, y=Species.Cover, Elevation.bin)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Elevation") +
  ylab("Species Cover (%)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
dev.off()

# ABUNDANCE

df.shep6zones.bin.lesstop17 <- df.shep6zones.bin[c(df.shep6zones.bin$Productivity < 11000),]

png(paste0("Shepherdia_barplot_elevatio_bin_abundance", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.shep6zones.bin.lesstop17, aes(fill=Logging, y=Productivity, Elevation.bin)) +
  #ggtitle("Buffaloberry", expression(italic("Shepherdia canadensis"))) +
  xlab("Elevation") +
  ylab("Abundance (berry count)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
dev.off()


# vaccinium ----

# CANOPY COVER

# build a dataframe to bind to the dataframe to provide additional categories in the plots
# create vectors with sequences
seq.canopy.vacc <- c(5, 15, 25, 35, 45, 55, 65, 75, 85)

# crate dataframe to add class mean vector to
df.vacc.bin.canopyclassmean <- df.vacc3zones[1:9, ]
df.vacc.bin.canopyclassmean[,] <- NA
df.vacc.bin.canopyclassmean$Canopy.Cover <- seq.canopy.vacc

df.vacc.bin.essf <- rbind(df.vacc.essf, df.vacc.bin.canopyclassmean)
df.vacc.bin.ich <- rbind(df.vacc.ich, df.vacc.bin.canopyclassmean)
df.vacc.bin.sbs <- rbind(df.vacc.sbs, df.vacc.bin.canopyclassmean)
df.vacc.bin.all <- df.vacc3zones

x <- df.vacc.bin.all # select the zone and change the name of the output .png accordingly

df.vacc3zones.bin <- x

df.vacc3zones.bin$Canopy.Cover.bin <- df.vacc3zones.bin$Canopy.Cover


df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > -1 &  df.vacc3zones.bin$Canopy.Cover < 11] <- "0-10"
df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > 10 &  df.vacc3zones.bin$Canopy.Cover < 21] <- "10-20"
df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > 20 &  df.vacc3zones.bin$Canopy.Cover < 31] <- "20-30"
df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > 30 &  df.vacc3zones.bin$Canopy.Cover < 41] <- "30-40"
df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > 40 &  df.vacc3zones.bin$Canopy.Cover < 51] <- "40-50"
df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > 50 &  df.vacc3zones.bin$Canopy.Cover < 61] <- "50-60"
df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > 60 &  df.vacc3zones.bin$Canopy.Cover < 71] <- "60-70"
df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > 70 &  df.vacc3zones.bin$Canopy.Cover < 81] <- "70-80"
df.vacc3zones.bin$Canopy.Cover.bin[df.vacc3zones.bin$Canopy.Cover > 80 &  df.vacc3zones.bin$Canopy.Cover < 91] <- "80-90"


df.vacc3zones.bin$Canopy.Cover.bin=factor(
  df.vacc3zones.bin$Canopy.Cover.bin, levels=c(
    "0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"))

# COVER

png(paste0("Vaccinium_barplot_canopy_bin_cover_all_", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3zones.bin, aes(fill=Logging, y=Species.Cover, Canopy.Cover.bin)) +
  xlab("Canopy Cover (%)") +
  ylab("Species Cover (%)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()
  #theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  #theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))
dev.off()

# ABUNDANCE

# remove outliers (top 5% - 41 sites)

png(paste0("Vaccinium_barplot_canopy_bin_abundance_all_", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3zones.bin, aes(fill=Logging, y=Productivity, Canopy.Cover.bin)) +
  ylim(0, 8300) + # remove outliers (top 5% - 41 sites)
  xlab("Canopy Cover (%)") +
  ylab("Abundance (berry count)") +
  #geom_bar(position='dodge', stat='identity') +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()
  #theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  #theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))
dev.off()


# ELEVATION ----

# build a dataframe to bind to the dataframe to provide additional categories in the plots
# create vectors with sequences
seq.elev.vacc <- c(500, 700, 900, 1100, 1300, 1500, 1700, 1900, 2100, 2300)

# crate dataframe to add class mean vector to
df.vacc.bin.elevclassmean <- df.vacc3zones[1:10, ]
df.vacc.bin.elevclassmean[,] <- NA
df.vacc.bin.elevclassmean$Elevation <- seq.elev.vacc

# by zone ----

df.vacc.bin.essf <- rbind(df.vacc.essf, df.vacc.bin.elevclassmean)
df.vacc.bin.ich <- rbind(df.vacc.ich, df.vacc.bin.elevclassmean)
df.vacc.bin.sbs <- rbind(df.vacc.sbs, df.vacc.bin.elevclassmean)
df.vacc.bin.all <- df.vacc3zones

x <- df.vacc.bin.all # select the zone and change the name of the output .png accordingly

df.vacc3zones.bin <- x

df.vacc3zones.bin$Elevation.bin <- df.vacc3zones.bin$Elevation

df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 399 & df.vacc3zones.bin$Elevation < 601] <- "400-600"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 599 & df.vacc3zones.bin$Elevation < 801] <- "600-800"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 799 & df.vacc3zones.bin$Elevation < 1001] <- "800-1000"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 999 & df.vacc3zones.bin$Elevation < 1201] <- "1000-1200"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 1199 & df.vacc3zones.bin$Elevation < 1401] <- "1200-1400"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 1399 & df.vacc3zones.bin$Elevation < 1601] <- "1400-1600"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 1599 & df.vacc3zones.bin$Elevation < 1801] <- "1600-1800"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 1799 & df.vacc3zones.bin$Elevation < 2001] <- "1800-2000"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 1999 & df.vacc3zones.bin$Elevation < 2201] <- "2000-2200"
df.vacc3zones.bin$Elevation.bin[df.vacc3zones.bin$Elevation > 2199 & df.vacc3zones.bin$Elevation < 2401] <- "2200-2400"

df.vacc3zones.bin$Elevation.bin=factor(
  df.vacc3zones.bin$Elevation.bin, levels=c(
    "400-600", "600-800", "800-1000", "1000-1200", "1200-1400",
    "1400-1600", "1600-1800", "1800-2000", "2000-2200", "2200-2400"))

# COVER
png(paste0("_Vaccinium_barplot_elevation_bin_cover_all_", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3zones.bin, aes(y=Species.Cover, Elevation.bin, fill=Logging)) +
  xlab("Elevation") +
  ylab("Species Cover (%)") +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  #theme(legend.position = c(0.1, 0.87))
dev.off()

# ABUNDANCE

png(paste0("_Vaccinium_barplot_elevation_bin_abundance_all_", Sys.Date(), ".png"),
    width = 6,
    height = 5,
    units = "in",
    res = 1200,
    pointsize = 12)
ggplot(df.vacc3zones.bin, aes(y=Productivity, Elevation.bin, fill=Logging)) +
  ylim(0, 8300) + # remove outliers (top 5% - 41 sites)
  xlab("Elevation") +
  ylab("Abundance (berry count)") +
  scale_fill_manual('Logging', values=c('steelblue', 'lightgray')) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  #theme(legend.position = c(0.1, 0.87))
dev.off()


##################################################################
# ---> ----> ----> ----> CHANGE THE WORKING DIRECTORY: TABLES ----
setwd(paste(path, "/tables", sep = ""))

library(moments) # to calculate statistics (kurtosis and Jarque-Bera tests)

##################################################################
# the datasets
datasets <- list(df.shep.sbs, df.shep.ms, df.shep.bwbs,
                 df.shep.ich, df.shep.essf, df.shep.idf,
                 df.vacc.essf, df.vacc.ich, df.vacc.sbs)

# name each dataset
names <- list("Shepherdia - SBS", "Shepherdia - MS", "Shepherdia - BWBS",
              "Shepherdia - ICH", "Shepherdia - ESSF", "Shepherdia - IDF",
              "Vaccinium - ESSF", "Vaccinium - ICH", "Vaccinium - SBS")


# TABLES of summary statistics  ----

# Variant tables for response variables ----

# zone ----

# shepherdia

shep.zone.statistics.response <- data.frame(
  # calculate the number of observations
  tapply(df.shep$Species, df.shep2$ZONE, length),
  # species cover
  tapply(df.shep$Species.Cover, df.shep$ZONE, min),
  tapply(df.shep$Species.Cover, df.shep$ZONE, max),
  tapply(df.shep$Species.Cover, df.shep$ZONE, mean),
  tapply(df.shep$Species.Cover, df.shep$ZONE, median),
  tapply(df.shep$Species.Cover, df.shep$ZONE, sd),
  tapply(df.shep$Species.Cover, df.shep$ZONE, skewness),
  tapply(df.shep$Species.Cover, df.shep$ZONE, kurtosis),
  # abundance
  tapply(df.shep$Productivity, df.shep$ZONE, min),
  tapply(df.shep$Productivity, df.shep$ZONE, max),
  tapply(df.shep$Productivity, df.shep$ZONE, mean),
  tapply(df.shep$Productivity, df.shep$ZONE, median),
  tapply(df.shep$Productivity, df.shep$ZONE, sd),
  tapply(df.shep$Productivity, df.shep$ZONE, skewness),
  tapply(df.shep$Productivity, df.shep$ZONE, kurtosis))

# create a column for the zone name based on the row name
shep.zone.statistics.response <- 
  cbind(rownames(shep.zone.statistics.response), data.frame(shep.zone.statistics.response, row.names=NULL))

colnames(shep.zone.statistics.response) <- c("BEC Zone", "n",
                                             "min", "max", "mean", "median", "sd", "skewness", "kurtosis",
                                             "min", "max", "mean", "median", "sd", "skewness", "kurtosis")

sink(paste0("shepherdia_zone_statistics_response_", Sys.Date(), ".txt"))
print(shep.zone.statistics.response)
sink(file = NULL)


# vaccinium

vacc.zone.statistics.response <- data.frame(
  # calculate the number of observations
  tapply(df.vacc$Species, df.vacc2$ZONE, length),
  # species cover
  tapply(df.vacc$Species.Cover, df.vacc$ZONE, min),
  tapply(df.vacc$Species.Cover, df.vacc$ZONE, max),
  tapply(df.vacc$Species.Cover, df.vacc$ZONE, mean),
  tapply(df.vacc$Species.Cover, df.vacc$ZONE, median),
  tapply(df.vacc$Species.Cover, df.vacc$ZONE, sd),
  tapply(df.vacc$Species.Cover, df.vacc$ZONE, skewness),
  tapply(df.vacc$Species.Cover, df.vacc$ZONE, kurtosis),
  # abundance
  tapply(df.vacc$Productivity, df.vacc$ZONE, min),
  tapply(df.vacc$Productivity, df.vacc$ZONE, max),
  tapply(df.vacc$Productivity, df.vacc$ZONE, mean),
  tapply(df.vacc$Productivity, df.vacc$ZONE, median),
  tapply(df.vacc$Productivity, df.vacc$ZONE, sd),
  tapply(df.vacc$Productivity, df.vacc$ZONE, skewness),
  tapply(df.vacc$Productivity, df.vacc$ZONE, kurtosis))

# create a column for the zone name based on the row name
vacc.zone.statistics.response <- 
  cbind(rownames(vacc.zone.statistics.response), data.frame(vacc.zone.statistics.response, row.names=NULL))

colnames(vacc.zone.statistics.response) <- c("BEC Zone", "n",
                                             "min", "max", "mean", "median", "sd", "skewness", "kurtosis",
                                             "min", "max", "mean", "median", "sd", "skewness", "kurtosis")

sink(paste0("vaccinium_zone_statistics_response_", Sys.Date(), ".txt"))
print(vacc.zone.statistics.response)
sink(file = NULL)

# Zone tables for response variables and canopy cover in logged and not logged sites ----
# shepherdia

# All sites
# number of sites in each ZONE
a <- shep.ZONE.n.all <- data.frame(tapply(df.shep$Species, df.shep$ZONE, length))
# mean canopy cover for each ZONE
b <- shep.ZONE.mean.canopy.cover <- data.frame(tapply(df.shep$Canopy.Cover, df.shep$ZONE, mean))
# mean species cover for each ZONE
c <- shep.ZONE.mean.species.cover <- data.frame(tapply(df.shep$Species.Cover, df.shep$ZONE, mean))
# mean abundance for each ZONE
d <- shep.ZONE.mean.abundance <- data.frame(tapply(df.shep$Productivity, df.shep$ZONE, mean))

# Logging = true
df.shep.true <- df.shep[c(df.shep$Logging == "TRUE"),]
e <- shep.ZONE.n.log.true <- data.frame(tapply(df.shep.true$Species, df.shep.true$ZONE, length))
# mean canopy cover for each ZONE
f <- shep.ZONE.mean.canopy.cover.true <- data.frame(tapply(df.shep.true$Canopy.Cover, df.shep.true$ZONE, mean))
# mean species cover for each ZONE
g <- shep.ZONE.mean.species.cover.true <- data.frame(tapply(df.shep.true$Species.Cover, df.shep.true$ZONE, mean))
# mean abundance for each ZONE
h <- shep.ZONE.mean.abundance.true <- data.frame(tapply(df.shep.true$Productivity, df.shep.true$ZONE, mean))

# Logging = false
df.shep.false <- df.shep[c(df.shep$Logging == "FALSE"),]
i <- shep.ZONE.n.log.false <- data.frame(tapply(df.shep.false$Species, df.shep.false$ZONE, length))
# mean canopy cover for each ZONE
j <- shep.ZONE.mean.canopy.cover.false <- data.frame(tapply(df.shep.false$Canopy.Cover, df.shep.false$ZONE, mean))
# mean species cover for each ZONE
k <- shep.ZONE.mean.sp.cover.false <- data.frame(tapply(df.shep.false$Species.Cover, df.shep.false$ZONE, mean))
# mean abundance for each ZONE
l <- shep.ZONE.mean.abundance.false <- data.frame(tapply(df.shep.false$Productivity, df.shep.false$ZONE, mean))

# create the data frame for all / canopy cover / true / false
a <- cbind(rownames(a), data.frame(a, row.names=NULL))
z <- data.frame(a, b, c, d, e, f, g, h, i, j, k, l, row.names=NULL)

#Format the data frame
z[is.na(z)] <- 0
z[2:13] <- round(z[2:13], digits = 0)
colnames(z) <- c("Variant",
                 "n. All", "Mean.Canopy.Cover.All", "Mean.Species.Cover.All", "Mean.Productivity.All",
                 "n. true", "Mean.Canopy.Cover.true", "Mean.Species.Cover.true", "Mean.Productivity.true",
                 "n. false", "Mean.Canopy.Cover.false", "Mean.Species.Cover.false", "Mean.Productivity.false")

sink(paste0("shepherdia_zone_statistics_response_logging_", Sys.Date(), ".txt"))
print(z)
sink(file = NULL)


# variant ----
shep2.variant.statistics.response <- data.frame(
  # calculate the number of observations
  tapply(df.shep2$Species, df.shep2$VARIANT, length),
  # species cover
  tapply(df.shep2$Species.Cover, df.shep2$VARIANT, min),
  tapply(df.shep2$Species.Cover, df.shep2$VARIANT, max),
  tapply(df.shep2$Species.Cover, df.shep2$VARIANT, mean),
  tapply(df.shep2$Species.Cover, df.shep2$VARIANT, median),
  tapply(df.shep2$Species.Cover, df.shep2$VARIANT, sd),
  tapply(df.shep2$Species.Cover, df.shep2$VARIANT, skewness),
  tapply(df.shep2$Species.Cover, df.shep2$VARIANT, kurtosis),
  # abundance
  tapply(df.shep2$Productivity, df.shep2$VARIANT, min),
  tapply(df.shep2$Productivity, df.shep2$VARIANT, max),
  tapply(df.shep2$Productivity, df.shep2$VARIANT, mean),
  tapply(df.shep2$Productivity, df.shep2$VARIANT, median),
  tapply(df.shep2$Productivity, df.shep2$VARIANT, sd),
  tapply(df.shep2$Productivity, df.shep2$VARIANT, skewness),
  tapply(df.shep2$Productivity, df.shep2$VARIANT, kurtosis))

shep2.variant.statistics.response <- 
  cbind(rownames(shep2.variant.statistics.response), data.frame(shep2.variant.statistics.response, row.names=NULL))

colnames(shep2.variant.statistics.response) <- c("Variant", "n",
                                                 "min", "max", "mean", "median", "sd", "skewness", "kurtosis",
                                                 "min", "max", "mean", "median", "sd", "skewness", "kurtosis")

sink(paste0("shepherdia_variant_statistics_response_", Sys.Date(), ".txt"))
print(shep2.variant.statistics.response)
sink(file = NULL)


# vaccinium

vacc2.variant.statistics.response <- data.frame(
  # calculate the number of observations
  tapply(df.vacc2$Species, df.vacc2$VARIANT, length),
  # species cover
  tapply(df.vacc2$Species.Cover, df.vacc2$VARIANT, min),
  tapply(df.vacc2$Species.Cover, df.vacc2$VARIANT, max),
  tapply(df.vacc2$Species.Cover, df.vacc2$VARIANT, mean),
  tapply(df.vacc2$Species.Cover, df.vacc2$VARIANT, median),
  tapply(df.vacc2$Species.Cover, df.vacc2$VARIANT, sd),
  tapply(df.vacc2$Species.Cover, df.vacc2$VARIANT, skewness),
  tapply(df.vacc2$Species.Cover, df.vacc2$VARIANT, kurtosis),
  # abundance
  tapply(df.vacc2$Productivity, df.vacc2$VARIANT, min),
  tapply(df.vacc2$Productivity, df.vacc2$VARIANT, max),
  tapply(df.vacc2$Productivity, df.vacc2$VARIANT, mean),
  tapply(df.vacc2$Productivity, df.vacc2$VARIANT, median),
  tapply(df.vacc2$Productivity, df.vacc2$VARIANT, sd),
  tapply(df.vacc2$Productivity, df.vacc2$VARIANT, skewness),
  tapply(df.vacc2$Productivity, df.vacc2$VARIANT, kurtosis))

vacc2.variant.statistics.response <- 
  cbind(rownames(vacc2.variant.statistics.response), data.frame(vacc2.variant.statistics.response, row.names=NULL))

colnames(vacc2.variant.statistics.response) <- c("Variant", "n",
                                                 "min", "max", "mean", "median", "sd", "skewness", "kurtosis",
                                                 "min", "max", "mean", "median", "sd", "skewness", "kurtosis")

sink(paste0("vaccinium_variant_statistics_response_", Sys.Date(), ".txt"))
print(vacc2.variant.statistics.response)
sink(file = NULL)


# Variant tables for site variables ----

# calculate the number of observations for logged sites

# vaccinium

df.vacc2.log.fals <- subset(df.vacc2, Logging == "FALSE")
df.vacc2.log.fals.n <- data.frame(tapply(df.vacc2.log.fals$Species, df.vacc2.log.fals$VARIANT, length))

df.vacc2.log.true <- subset(df.vacc2, Logging == "TRUE")
df.vacc2.log.true.n <- data.frame(tapply(df.vacc2.log.true$Species, df.vacc2.log.true$VARIANT, length))

vacc_variant_log <- cbind(df.vacc2.log.fals.n, df.vacc2.log.true.n)
vacc_variant_log[is.na(vacc_variant_log)] <- 0

df.vacc2.aspe.min <- tapply(df.vacc2$Aspect, df.vacc2$VARIANT, min)
df.vacc2.aspe.max <- tapply(df.vacc2$Aspect, df.vacc2$VARIANT, max)
df.vacc2.aspe.mean <- tapply(df.vacc2$Aspect, df.vacc2$VARIANT, mean)
df.vacc2.elev.min <- tapply(df.vacc2$Elevation, df.vacc2$VARIANT, min)
df.vacc2.elev.max <- tapply(df.vacc2$Elevation, df.vacc2$VARIANT, max)
df.vacc2.elev.mean <- tapply(df.vacc2$Elevation, df.vacc2$VARIANT, mean)
df.vacc2.slop.min <- tapply(df.vacc2$Slope, df.vacc2$VARIANT, min)
df.vacc2.slop.max <- tapply(df.vacc2$Slope, df.vacc2$VARIANT, max)
df.vacc2.slop.mean <- tapply(df.vacc2$Slope, df.vacc2$VARIANT, mean)
df.vacc2.cano.min <- tapply(df.vacc2$Canopy.Cover, df.vacc2$VARIANT, min)
df.vacc2.cano.max <- tapply(df.vacc2$Canopy.Cover, df.vacc2$VARIANT, max)
df.vacc2.cano.mean <- tapply(df.vacc2$Canopy.Cover, df.vacc2$VARIANT, mean)

vacc2.variant.statistics.site <- data.frame(vacc_variant_log, df.vacc2.aspe.min, df.vacc2.aspe.max, df.vacc2.aspe.mean,
                                            df.vacc2.elev.min, df.vacc2.elev.max, df.vacc2.elev.mean, 
                                            df.vacc2.slop.min, df.vacc2.slop.max, df.vacc2.slop.mean,
                                            df.vacc2.cano.min, df.vacc2.cano.max, df.vacc2.cano.mean)

vacc2.variant.statistics.site <- cbind(rownames(vacc2.variant.statistics.site),
                                       data.frame(vacc2.variant.statistics.site,
                                                  row.names=NULL))

# format the data frame
colnames(vacc2.variant.statistics.site) <- c("Variant", "Not Logged (n)", "Logged (n)",
                                             "Aspect Minimum","Aspect Maximum", "Aspect Mean",
                                             "Elevation (m) Minimum","Elevation (m) Maximum", "Elevation (m) Mean",
                                             "Slope (%) Minimum","Slope (%) Maximum", "Slope (%) Mean",
                                             "Canopy Cover (%) Minimum","Canopy Cover (%) Maximum", "Canopy Cover (%) Mean")
vacc2.variant.statistics.site <- round(vacc2.variant.statistics.site[2:15], digits = 1)

sink(paste0("vaccinium_variant_statistics_site_", Sys.Date(), ".txt"))
print(vacc2.variant.statistics.site)
sink(file = NULL)

# Variant tables for response variables and canopy cover in logged and not logged sites ----

# shepherdia

# All sites
# number of sites in each VARIANT
a <- shep.VARIANT.n.all <- data.frame(tapply(df.shep$Species, df.shep$VARIANT, length))
# mean canopy cover for each VARIANT
b <- shep.VARIANT.mean.canopy.cover <- data.frame(tapply(df.shep$Canopy.Cover, df.shep$VARIANT, mean))
# mean species cover for each VARIANT
c <- shep.VARIANT.mean.species.cover <- data.frame(tapply(df.shep$Species.Cover, df.shep$VARIANT, mean))
# mean abundance for each VARIANT
d <- shep.VARIANT.mean.abundance <- data.frame(tapply(df.shep$Productivity, df.shep$VARIANT, mean))

# Logging = true
df.shep.true <- df.shep[c(df.shep$Logging == "TRUE"),]
e <- shep.VARIANT.n.log.true <- data.frame(tapply(df.shep.true$Species, df.shep.true$VARIANT, length))
# mean canopy cover for each VARIANT
f <- shep.VARIANT.mean.canopy.cover.true <- data.frame(tapply(df.shep.true$Canopy.Cover, df.shep.true$VARIANT, mean))
# mean species cover for each VARIANT
g <- shep.VARIANT.mean.species.cover.true <- data.frame(tapply(df.shep.true$Species.Cover, df.shep.true$VARIANT, mean))
# mean abundance for each VARIANT
h <- shep.VARIANT.mean.abundance.true <- data.frame(tapply(df.shep.true$Productivity, df.shep.true$VARIANT, mean))

# Logging = false
df.shep.false <- df.shep[c(df.shep$Logging == "FALSE"),]
i <- shep.VARIANT.n.log.false <- data.frame(tapply(df.shep.false$Species, df.shep.false$VARIANT, length))
# mean canopy cover for each VARIANT
j <- shep.VARIANT.mean.canopy.cover.false <- data.frame(tapply(df.shep.false$Canopy.Cover, df.shep.false$VARIANT, mean))
# mean species cover for each VARIANT
k <- shep.VARIANT.mean.sp.cover.false <- data.frame(tapply(df.shep.false$Species.Cover, df.shep.false$VARIANT, mean))
# mean abundance for each VARIANT
l <- shep.VARIANT.mean.abundance.false <- data.frame(tapply(df.shep.false$Productivity, df.shep.false$VARIANT, mean))

# create the data frame for all / canopy cover / true / false
a <- cbind(rownames(a), data.frame(a, row.names=NULL))
z <- data.frame(a, b, c, d, e, f, g, h, i, j, k, l, row.names=NULL)

#Format the data frame
z[is.na(z)] <- 0
z[2:13] <- round(z[2:13], digits = 0)
colnames(z) <- c("Variant",
                 "n. All", "Mean.Canopy.Cover.All", "Mean.Species.Cover.All", "Mean.Productivity.All",
                 "n. true", "Mean.Canopy.Cover.true", "Mean.Species.Cover.true", "Mean.Productivity.true",
                 "n. false", "Mean.Canopy.Cover.false", "Mean.Species.Cover.false", "Mean.Productivity.false")

sink(paste0("shepherdia_variant_statistics_logging_", Sys.Date(), ".txt"))
print(z)
sink(file = NULL)

# vaccinium

# All sites
# number of sites in each VARIANT
a <- vacc.VARIANT.n.all <- data.frame(tapply(df.vacc$Species, df.vacc$VARIANT, length))
# mean canopy cover for each VARIANT
b <- vacc.VARIANT.mean.canopy.cover <- data.frame(tapply(df.vacc$Canopy.Cover, df.vacc$VARIANT, mean))
# mean species cover for each VARIANT
c <- vacc.VARIANT.mean.species.cover <- data.frame(tapply(df.vacc$Species.Cover, df.vacc$VARIANT, mean))
# mean abundance for each VARIANT
d <- vacc.VARIANT.mean.abundance <- data.frame(tapply(df.vacc$Productivity, df.vacc$VARIANT, mean))

# Logging = true
df.vacc.true <- df.vacc[c(df.vacc$Logging == "TRUE"),]
e <- vacc.VARIANT.n.log.true <- data.frame(tapply(df.vacc.true$Species, df.vacc.true$VARIANT, length))
# mean canopy cover for each VARIANT
f <- vacc.VARIANT.mean.canopy.cover.true <- data.frame(tapply(df.vacc.true$Canopy.Cover, df.vacc.true$VARIANT, mean))
# mean species cover for each VARIANT
g <- vacc.VARIANT.mean.species.cover.true <- data.frame(tapply(df.vacc.true$Species.Cover, df.vacc.true$VARIANT, mean))
# mean abundance for each VARIANT
h <- vacc.VARIANT.mean.abundance.true <- data.frame(tapply(df.vacc.true$Productivity, df.vacc.true$VARIANT, mean))

# Logging = false
df.vacc.false <- df.vacc[c(df.vacc$Logging == "FALSE"),]
i <- vacc.VARIANT.n.log.false <- data.frame(tapply(df.vacc.false$Species, df.vacc.false$VARIANT, length))
# mean canopy cover for each VARIANT
j <- vacc.VARIANT.mean.canopy.cover.false <- data.frame(tapply(df.vacc.false$Canopy.Cover, df.vacc.false$VARIANT, mean))
# mean species cover for each VARIANT
k <- vacc.VARIANT.mean.sp.cover.false <- data.frame(tapply(df.vacc.false$Species.Cover, df.vacc.false$VARIANT, mean))
# mean abundance for each VARIANT
l <- vacc.VARIANT.mean.abundance.false <- data.frame(tapply(df.vacc.false$Productivity, df.vacc.false$VARIANT, mean))

# create the data frame for all / canopy cover / true / false
a <- cbind(rownames(a), data.frame(a, row.names=NULL))
z <- data.frame(a, b, c, d, e, f, g, h, i, j, k, l, row.names=NULL)

#Format the data frame
z[is.na(z)] <- 0
z[2:13] <- round(z[2:13], digits = 0)
colnames(z) <- c("Variant",
                 "n. All", "Mean.Canopy.Cover.All", "Mean.Species.Cover.All", "Mean.Productivity.All",
                 "n. true", "Mean.Canopy.Cover.true", "Mean.Species.Cover.true", "Mean.Productivity.true",
                 "n. false", "Mean.Canopy.Cover.false", "Mean.Species.Cover.false", "Mean.Productivity.false")

sink(paste0("vaccinium_variant_statistics_logging_", Sys.Date(), ".txt"))
print(z)
sink(file = NULL)

##################################################################
# Create correlation tables for response and site variables: No LOGGING vs LOGGING ----
for (i in seq_along(datasets)) {
  data.COR <- datasets[[i]]
  name <- names[i]
  
  #convert the logging factor to numeric format
  data.COR[14] <- lapply(data.COR[14], as.numeric)
  #convert the NA in Aspect to a numeric format
  data.COR$Aspect[is.na(data.COR$Aspect)] <- 181
  
  # Save text output: important variable correlations
  sink(paste0(name, "_corr_site_", Sys.Date(), ".txt"))
  print(cor(data.frame(data.COR[2:3], data.COR[14:20])))
  sink(file = NULL)
  
  # NO LOGGING
  # subset data to LOGGING = FALSE
  data.COR.1 <- droplevels(subset(data.COR, Logging == 1))
  # Save text output: important variable correlations
  sink(paste0(name, "_corr_site_logged_FALSE_", Sys.Date(), ".txt"))
  print(cor(data.frame(data.COR.1[2:3], data.COR.1[15:20])))
  sink(file = NULL)
  
  # LOGGING
  # subset data to LOGGING = TRUE
  data.COR.2 <- droplevels(subset(data.COR, Logging == 2))
  # Save text output: important variable correlations
  sink(paste0(name, "_corr_site_logged_TRUE_", Sys.Date(), ".txt"))
  print(cor(data.frame(data.COR.2[2:3], data.COR.2[15:20])))
  sink(file = NULL)
}

##################################################################
# statistical tests ----
# Create datasets for variants with > 30 observations for each species ----

# shepherdia
# BWBSmk (60), MSdw (35), SBSmk1 (66)

df.shep.BWBSmk <- droplevels(subset(df.shep, VARIANT == "BWBSmk"))
df.shep.MSdw <- droplevels(subset(df.shep, VARIANT == "MSdw"))
df.shep.SBSmk1 <- droplevels(subset(df.shep, VARIANT == "SBSmk1"))

# vaccinium
# ESSFdc1 (79), ESSFdk1	(37), ESSFmh (42), ESSFmm3 (73), ESSFunp (35), ESSFvc (46),
# ESSFvcw (35), ESSFwc2 (32), ESSFwc4 (43), ESSFwcw (35), ESSFwh1 (34),
# SBSmk1 (32), SBSvk (31)

df.vacc.ESSFdc1 <- droplevels(subset(df.vacc, VARIANT == "ESSFdc1"))
df.vacc.ESSFdk1 <- droplevels(subset(df.vacc, VARIANT == "ESSFdk1"))
df.vacc.ESSFmh <- droplevels(subset(df.vacc, VARIANT == "ESSFmh"))
df.vacc.ESSFmm3 <- droplevels(subset(df.vacc, VARIANT == "ESSFmm3"))
df.vacc.ESSFunp <- droplevels(subset(df.vacc, VARIANT == "ESSFunp"))
df.vacc.ESSFvc <- droplevels(subset(df.vacc, VARIANT == "ESSFvc"))
df.vacc.ESSFvcw <- droplevels(subset(df.vacc, VARIANT == "ESSFvcw"))
df.vacc.ESSFwc2 <- droplevels(subset(df.vacc, VARIANT == "ESSFwc2"))
df.vacc.ESSFwc4 <- droplevels(subset(df.vacc, VARIANT == "ESSFwc4"))
df.vacc.ESSFwcw <- droplevels(subset(df.vacc, VARIANT == "ESSFwcw"))
df.vacc.ESSFwh1 <- droplevels(subset(df.vacc, VARIANT == "ESSFwh1"))
df.vacc.SBSmk1 <- droplevels(subset(df.vacc, VARIANT == "SBSmk1"))
df.vacc.SBSvk <- droplevels(subset(df.vacc, VARIANT == "SBSvk"))

# Define the input datasets (Variant level) ----

# shepherdia

datasets.shep.variant <- list(df.shep.BWBSmk, df.shep.MSdw, df.shep.SBSmk1)

# name each dataset
names.shep.variant <- list("Boreal White and Black Spruce - Moist Cool",
                           "Montane Spruce - Dry Warm",
                           "Sub-Boreal Spruce - Moist Cool")
# vaccinium

datasets.vacc.variant <- list("df.vacc.ESSFdc1", "df.vacc.ESSFdk1", "df.vacc.ESSFmh",
                              "df.vacc.ESSFmm3","df.vacc.ESSFunp","df.vacc.ESSFvc",
                              "df.vacc.ESSFvcw","df.vacc.ESSFwc2","df.vacc.ESSFwc4",
                              "df.vacc.ESSFwcw","df.vacc.ESSFwh1","df.vacc.SBSmk1",
                              "df.vacc.SBSvk")


# Conduct Jarque-Bera test For variants with > 30 observations ----

# The null hypothesis for the test is that the data is normally distributed

jarque.BWBSmk.shep.cov <- jarque.test(df.shep.BWBSmk$Species.Cover) #p-value = 0
jarque.BWBSmk.shep.pro <- jarque.test(df.shep.BWBSmk$Productivity) #p-value = 5.22e-06
jarque.BWBSmk.shep.can <- jarque.test(df.shep.BWBSmk$Canopy.Cover) #p-value = 0.562

jarque.MSdw.shep.cov <- jarque.test(df.shep.MSdw$Species.Cover) #p-value = 0.384
jarque.MSdw.shep.pro <- jarque.test(df.shep.MSdw$Productivity) #p-value = 0
jarque.MSdw.shep.can <- jarque.test(df.shep.MSdw$Canopy.Cover) #p-value = 0.126

jarque.SBSmk1.shep.cov <- jarque.test(df.shep.SBSmk1$Species.Cover) #p-value = 0
jarque.SBSmk1.shep.pro <- jarque.test(df.shep.SBSmk1$Productivity) #p-value = 0
jarque.SBSmk1.shep.can <- jarque.test(df.shep.SBSmk1$Canopy.Cover) #p-value = 0.186


# vaccinium

jarque.ESSFdc1.cov <- jarque.test(df.vacc.ESSFdc1$Species.Cover) #p-value = 0.963
jarque.ESSFdc1.pro <- jarque.test(df.vacc.ESSFdc1$Productivity) #p-value = 0
jarque.ESSFdc1.can <- jarque.test(df.vacc.ESSFdc1$Canopy.Cover) #p-value = 0.0145

jarque.ESSFdk1.cov <- jarque.test(df.vacc.ESSFdk1$Species.Cover) #p-value = 0.0732
jarque.ESSFdk1.pro <- jarque.test(df.vacc.ESSFdk1$Productivity) #p-value = 0
jarque.ESSFdk1.can <- jarque.test(df.vacc.ESSFdk1$Canopy.Cover) #p-value = 0.782

jarque.ESSFmh.cov <- jarque.test(df.vacc.ESSFmh$Species.Cover) #p-value = 0.266
jarque.ESSFmh.pro <- jarque.test(df.vacc.ESSFmh$Productivity) #p-value = 0
jarque.ESSFmh.can <- jarque.test(df.vacc.ESSFmh$Canopy.Cover) #p-value = 0.259

jarque.ESSFmm3.cov <- jarque.test(df.vacc.ESSFmm3$Species.Cover) #p-value = 0.00191
jarque.ESSFmm3.pro <- jarque.test(df.vacc.ESSFmm3$Productivity) #p-value = 0

jarque.ESSFunp.cov <- jarque.test(df.vacc.ESSFunp$Species.Cover) #p-value = 1.03e-07
jarque.ESSFunp.pro <- jarque.test(df.vacc.ESSFunp$Productivity) #p-value = 7.77e-16

jarque.ESSFvc.cov <- jarque.test(df.vacc.ESSFvc$Species.Cover) #p-value = 0.858
jarque.ESSFvc.pro <- jarque.test(df.vacc.ESSFvc$Productivity) #p-value = 1.27e-05
jarque.ESSFvc.can <- jarque.test(df.vacc.ESSFvc$Canopy.Cover) #p-value = 9.17e-06

jarque.ESSFvcw.cov <- jarque.test(df.vacc.ESSFvcw$Species.Cover) #p-value = 0.376
jarque.ESSFvcw.pro <- jarque.test(df.vacc.ESSFvcw$Productivity) #p-value = 0.17
jarque.ESSFvcw.can <- jarque.test(df.vacc.ESSFvcw$Canopy.Cover) #p-value = 0.113

jarque.ESSFwc2.cov <- jarque.test(df.vacc.ESSFwc2$Species.Cover) #p-value = 0.703
jarque.ESSFwc2.pro <- jarque.test(df.vacc.ESSFwc2$Productivity) #p-value = 0.00261

jarque.ESSFwc4.cov <- jarque.test(df.vacc.ESSFwc4$Species.Cover) #p-value = 0.35
jarque.ESSFwc4.pro <- jarque.test(df.vacc.ESSFwc4$Productivity) #p-value = 0

jarque.ESSFwcw.cov <- jarque.test(df.vacc.ESSFwcw$Species.Cover) #p-value = 0.708
jarque.ESSFwcw.pro <- jarque.test(df.vacc.ESSFwcw$Productivity) #p-value = 0

jarque.ESSFwh1.cov <- jarque.test(df.vacc.ESSFwh1$Species.Cover) #p-value = 0.353
jarque.ESSFwh1.pro <- jarque.test(df.vacc.ESSFwh1$Productivity) #p-value = 3.74e-07

jarque.SBSmk1.cov <- jarque.test(df.vacc.SBSmk1$Species.Cover) #p-value = 0
jarque.SBSmk1.pro <- jarque.test(df.vacc.SBSmk1$Productivity) #p-value = 0

jarque.SBSvk.cov <- jarque.test(df.vacc.SBSvk$Species.Cover) #p-value = 0.0949
jarque.SBSvk.pro <- jarque.test(df.vacc.SBSvk$Productivity) #p-value = 5.03e-06

# Conduct ttest/Welch's test (logging) ----
# by default R assumes that the variances of y1 and y2 are unequal (defaults to Welch's test)
# to toggle this we use the flag var.equal=TRUE

# shepherdia

# Jarque-Bera tests qualified COVER in: MSdw

shep.MSdw.test.w <- t.test(df.shep.MSdw$Species.Cover ~ df.shep.MSdw$Logging)
shep.MSdw.test.n <- data.frame(tapply(df.shep.MSdw$Species, df.shep.MSdw$Logging, length))
shep.MSdw.test.w.table <- data.frame(shep.MSdw.test.w$p.value, shep.MSdw.test.w$estimate[1], shep.MSdw.test.w$estimate[2])


# vaccinium

# Jarque-Bera tests qualified
# COVER in: # ESSFdc1, ESSFdk1, ESSFmh, ESSFvc, ESSFvcw, ESSFwc2, ESSFwc4,
# ESSFwcw, ESSFwh1 and SBSvk
# ABUNDANCE in: ESSFvcw

vacc.ESSFdc1.test.w <- t.test(df.vacc.ESSFdc1$Species.Cover ~ df.vacc.ESSFdc1$Logging)
vacc.ESSFdc1.test.n <- data.frame(tapply(df.vacc.ESSFdc1$Species, df.vacc.ESSFdc1$Logging, length))
vacc.ESSFdc1.test.w.table <- data.frame(vacc.ESSFdc1.test.w$p.value, vacc.ESSFdc1.test.w$estimate[1], vacc.ESSFdc1.test.w$estimate[2])

vacc.ESSFdk1.test.w <- t.test(df.vacc.ESSFdk1$Species.Cover ~ df.vacc.ESSFdk1$Logging)
vacc.ESSFdk1.test.n <- data.frame(tapply(df.vacc.ESSFdk1$Species, df.vacc.ESSFdk1$Logging, length))
vacc.ESSFdk1.test.w.table <- data.frame(vacc.ESSFdk1.test.w$p.value, vacc.ESSFdk1.test.w$estimate[1], vacc.ESSFdk1.test.w$estimate[2])

vacc.ESSFmh.test.w <- t.test(df.vacc.ESSFmh$Species.Cover ~ df.vacc.ESSFmh$Logging)
vacc.ESSFmh.test.n <- data.frame(tapply(df.vacc.ESSFmh$Species, df.vacc.ESSFmh$Logging, length))
vacc.ESSFmh.test.w.table <- data.frame(vacc.ESSFmh.test.w$p.value, vacc.ESSFmh.test.w$estimate[1], vacc.ESSFmh.test.w$estimate[2])

vacc.ESSFvc.test.w <- t.test(df.vacc.ESSFvc$Species.Cover ~ df.vacc.ESSFvc$Logging)
vacc.ESSFvc.test.n <- data.frame(tapply(df.vacc.ESSFvc$Species, df.vacc.ESSFvc$Logging, length))
vacc.ESSFvc.test.w.table <- data.frame(vacc.ESSFvc.test.w$p.value, vacc.ESSFvc.test.w$estimate[1], vacc.ESSFvc.test.w$estimate[2])

vacc.ESSFvcw.test.w <- t.test(df.vacc.ESSFvcw$Species.Cover ~ df.vacc.ESSFvcw$Logging)
vacc.ESSFvcw.test.n <- data.frame(tapply(df.vacc.ESSFvcw$Species, df.vacc.ESSFvcw$Logging, length))
vacc.ESSFvcw.test.w.table <- data.frame(vacc.ESSFvcw.test.w$p.value, vacc.ESSFvcw.test.w$estimate[1], vacc.ESSFvcw.test.w$estimate[2])

vacc.ESSFwc2.test.w <- t.test(df.vacc.ESSFwc2$Species.Cover ~ df.vacc.ESSFwc2$Logging)
vacc.ESSFwc2.test.n <- data.frame(tapply(df.vacc.ESSFwc2$Species, df.vacc.ESSFwc2$Logging, length))
vacc.ESSFwc2.test.w.table <- data.frame(vacc.ESSFwc2.test.w$p.value, vacc.ESSFwc2.test.w$estimate[1], vacc.ESSFwc2.test.w$estimate[2])

vacc.ESSFwc4.test.w <- t.test(df.vacc.ESSFwc4$Species.Cover ~ df.vacc.ESSFwc4$Logging)
vacc.ESSFwc4.test.n <- data.frame(tapply(df.vacc.ESSFwc4$Species, df.vacc.ESSFwc4$Logging, length))
vacc.ESSFwc4.test.w.table <- data.frame(vacc.ESSFwc4.test.w$p.value, vacc.ESSFwc4.test.w$estimate[1], vacc.ESSFwc4.test.w$estimate[2])

vacc.ESSFwcw.test.w <- t.test(df.vacc.ESSFwcw$Species.Cover ~ df.vacc.ESSFwcw$Logging)
vacc.ESSFwcw.test.n <- data.frame(tapply(df.vacc.ESSFwcw$Species, df.vacc.ESSFwcw$Logging, length))
vacc.ESSFwcw.test.w.table <- data.frame(vacc.ESSFwcw.test.w$p.value, vacc.ESSFwcw.test.w$estimate[1], vacc.ESSFwcw.test.w$estimate[2])

vacc.ESSFwh1.test.w <- t.test(df.vacc.ESSFwh1$Species.Cover ~ df.vacc.ESSFwh1$Logging)
vacc.ESSFwh1.test.n <- data.frame(tapply(df.vacc.ESSFwh1$Species, df.vacc.ESSFwh1$Logging, length))
vacc.ESSFwh1.test.w.table <- data.frame(vacc.ESSFwh1.test.w$p.value, vacc.ESSFwh1.test.w$estimate[1], vacc.ESSFwh1.test.w$estimate[2])

vacc.SBSvk.test.w <- t.test(df.vacc.SBSvk$Species.Cover ~ df.vacc.SBSvk$Logging)
vacc.SBSvk.test.n <- data.frame(tapply(df.vacc.SBSvk$Species, df.vacc.SBSvk$Logging, length))
vacc.SBSvk.test.w.table <- data.frame(vacc.SBSvk.test.w$p.value, vacc.SBSvk.test.w$estimate[1], vacc.SBSvk.test.w$estimate[2])

# ABUNDANCE
vacc.ESSFvcw.test.w.p <- t.test(df.vacc.ESSFvcw$Productivity ~ df.vacc.ESSFvcw$Logging)
vacc.ESSFvcw.test.n.p <- data.frame(tapply(df.vacc.ESSFvcw$Species, df.vacc.ESSFvcw$Logging, length))
vacc.ESSFvcw.test.w.table.p <- data.frame(vacc.ESSFvcw.test.w$p.value, vacc.ESSFvcw.test.w$estimate[1], vacc.ESSFvcw.test.w$estimate[2])

# CANOPY
vacc.ESSFdc1.test.w.c <- t.test(df.vacc.ESSFdc1$Canopy.Cover ~ df.vacc.ESSFdc1$Logging)
vacc.ESSFdc1.test.n.c <- data.frame(tapply(df.vacc.ESSFdc1$Species, df.vacc.ESSFdc1$Logging, length))
vacc.ESSFdc1.test.w.c.table <- data.frame(vacc.ESSFdc1.test.w$p.value, vacc.ESSFdc1.test.w$estimate[1], vacc.ESSFdc1.test.w$estimate[2])

vacc.ESSFdk1.test.w.c <- t.test(df.vacc.ESSFdk1$Canopy.Cover ~ df.vacc.ESSFdk1$Logging)
vacc.ESSFdk1.test.n.c <- data.frame(tapply(df.vacc.ESSFdk1$Species, df.vacc.ESSFdk1$Logging, length))
vacc.ESSFdk1.test.w.c.table <- data.frame(vacc.ESSFdk1.test.w$p.value, vacc.ESSFdk1.test.w$estimate[1], vacc.ESSFdk1.test.w$estimate[2])

vacc.ESSFmh.test.w.c <- t.test(df.vacc.ESSFmh$Canopy.Cover ~ df.vacc.ESSFmh$Logging)
vacc.ESSFmh.test.n.c <- data.frame(tapply(df.vacc.ESSFmh$Species, df.vacc.ESSFmh$Logging, length))
vacc.ESSFmh.test.w.c.table <- data.frame(vacc.ESSFmh.test.w$p.value, vacc.ESSFmh.test.w$estimate[1], vacc.ESSFmh.test.w$estimate[2])

vacc.ESSFvc.test.w.c <- t.test(df.vacc.ESSFvc$Canopy.Cover ~ df.vacc.ESSFvc$Logging)
vacc.ESSFvc.test.n.c <- data.frame(tapply(df.vacc.ESSFvc$Species, df.vacc.ESSFvc$Logging, length))
vacc.ESSFvc.test.w.c.table <- data.frame(vacc.ESSFvc.test.w$p.value, vacc.ESSFvc.test.w$estimate[1], vacc.ESSFvc.test.w$estimate[2])

vacc.ESSFvcw.test.w.c <- t.test(df.vacc.ESSFvcw$Canopy.Cover ~ df.vacc.ESSFvcw$Logging)
vacc.ESSFvcw.test.n.c <- data.frame(tapply(df.vacc.ESSFvcw$Species, df.vacc.ESSFvcw$Logging, length))
vacc.ESSFvcw.test.w.table.c <- data.frame(vacc.ESSFvcw.test.w$p.value, vacc.ESSFvcw.test.w$estimate[1], vacc.ESSFvcw.test.w$estimate[2])

# Export the Welch tests
sink(paste0("vaccinium_ttests_welch_", Sys.Date(), ".txt"))
print("ESSF COVER")
print(vacc.ESSFdc1.test.w)
print(vacc.ESSFdk1.test.w)
print(vacc.ESSFmh.test.w)
print(vacc.ESSFvc.test.w)
print(vacc.ESSFvcw.test.w)
print(vacc.ESSFwc2.test.w)
print(vacc.ESSFwc4.test.w)
print(vacc.ESSFwcw.test.w)
print(vacc.ESSFwh1.test.w)
print("SBS COVER")
print(vacc.SBSvk.test.w)
print("ESSF ABUNDANCE")
print(vacc.ESSFvcw.test.w.p)
print("ESSF CANOPY")
print(vacc.ESSFdc1.test.w.c)
print(vacc.ESSFdk1.test.w.c)
print(vacc.ESSFmh.test.w.c)
print(vacc.ESSFvc.test.w.c)
print(vacc.ESSFvcw.test.w.c)
sink(file = NULL)

# Create the overall table
t.test.vacc <- data.frame(vacc.ESSFdc1.test.w.table, vacc.ESSFdk1.test.w.table, vacc.ESSFmh.test.w.table,
                          vacc.ESSFvc.test.w.table, vacc.ESSFvcw.test.w.table, vacc.ESSFwc2.test.w.table,
                          vacc.ESSFwc4.test.w.table, vacc.ESSFwcw.test.w.table, vacc.ESSFwh1.test.w.table,
                          vacc.SBSvk.test.w.table, vacc.ESSFvcw.test.w.table.p)

t.test.vacc.n <- data.frame(vacc.ESSFdc1.test.n, vacc.ESSFdk1.test.n, vacc.ESSFmh.test.n,
                            vacc.ESSFvc.test.n, vacc.ESSFvcw.test.n, vacc.ESSFwc2.test.n,
                            vacc.ESSFwc4.test.n, vacc.ESSFwcw.test.n, vacc.ESSFwh1.test.n,
                            vacc.SBSvk.test.n, vacc.ESSFvcw.test.n.p)

#write.csv(t.test.vacc, paste0("t.test.vacc", Sys.Date(), ".csv"))
#write.csv(t.test.vacc.n, paste0("t.test.vacc.n", Sys.Date(), ".csv"))
#write.csv(vacc2.variant.statistics.site[8:14], paste0("vaccinium_variant_statistics_site3_", Sys.Date(), ".csv"))

# Conduct ttest/Welch's test (canopy cover) ----
# by default R assumes that the variances of y1 and y2 are unequal (defaults to Welch's test)
# to toggle this we use the flag var.equal=TRUE

# shepherdia ----

# Jarque-Bera tests qualified COVER in: MSdw

shep.BWBS.test.w <- t.test(df.shep.MSdw$Species.Cover ~ df.shep.MSdw$Logging)
shep.MSdw.test.n <- data.frame(tapply(df.shep.MSdw$Species, df.shep.MSdw$Logging, length))
shep.MSdw.test.w.table <- data.frame(shep.MSdw.test.w$p.value, shep.MSdw.test.w$estimate[1], shep.MSdw.test.w$estimate[2])

# Jarque-Bera tests for ABUNDANCE in sites with different levels of species cover ----

df.shep.bin.lesstop17.bin1 <- subset(df.shep.bin.lesstop17, Species.Cover.bin == "0-5")
jarque.shep.species.cov1 <- jarque.test(df.shep.bin.lesstop17.bin1$Productivity) # p = 0

df.shep.bin.lesstop17.bin2 <- subset(df.shep.bin.lesstop17, Species.Cover.bin == "5-10")
jarque.shep.species.cov2 <- jarque.test(df.shep.bin.lesstop17.bin2$Productivity) #p = near 0

df.shep.bin.lesstop17.bin3 <- subset(df.shep.bin.lesstop17, Species.Cover.bin == "10-15")
jarque.shep.species.cov3 <- jarque.test(df.shep.bin.lesstop17.bin3$Productivity) #p = near 0.05 but only 22 sites

# Jarque-Bera tests for COVER in sites with different levels of canopy cover ----

df.shep.bin.lesstop17.bin1 <- subset(df.shep.bin.lesstop17, Canopy.Cover.bin == "0-10")
jarque.shep.species.cov1 <- jarque.test(df.shep.bin.lesstop17.bin1$Species.Cover) # p = 0 for abundance

df.shep.bin.lesstop17.bin2 <- subset(df.shep.bin.lesstop17, Canopy.Cover.bin == "11-20")
jarque.shep.species.cov2 <- jarque.test(df.shep.bin.lesstop17.bin2$Species.Cover) #p = near 0 for abundance

df.shep.bin.lesstop17.bin3 <- subset(df.shep.bin.lesstop17, Canopy.Cover.bin == "21-30")
jarque.shep.species.cov3 <- jarque.test(df.shep.bin.lesstop17.bin3$Species.Cover) #p = near 0 for abundance



##################################################################
# ---> ----> ----> ----> CHANGE THE WORKING DIRECTORY: MAIN DIRECTORY ----
setwd(path)

# Write script to file ----

#r_script <- file(paste("C:/GS25RMB0008/2025/R_script/berry project_GS25RMB0008_", Sys.Date(), ".txt")) # File name of output log
#r_script <- file(paste(path, "berry project_GS25RMB0008_", Sys.Date(), ".txt")) # File name of output log

# write script to R folder in project directory
r_script <- file(paste("C:/GS25RMB0008/2025/R_script/berry project_GS25RMB0008_", Sys.Date(), ".txt")) # File name of output log

# write script to working directory
r_script <- file(paste("berry project_GS25RMB0008_", Sys.Date(), ".txt")) # File name of output log
sink(r_script, append = TRUE, type = "output") # Writing console output to log file
sink(r_script, append = TRUE, type = "message")
cat(readChar(rstudioapi::getSourceEditorContext()$path, # Writing currently opened R script to file
             file.info(rstudioapi::getSourceEditorContext()$path)$size))
sink(file = NULL)

sink(paste0("berry project_GS25RMB0008_r_script", Sys.Date(), ".txt"))
print(r_script)
sink(file = NULL)

##################################################################
# END ###############
dev.off()

