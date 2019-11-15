#### MAINS REPAIR PREPARATION ####

# This script modifies the company mains repairs data (sap data tab)  
# so it can be processed by the mains repair to pipe script


#### PACKAGES ####
if (!require(pacman)) {
  install.packages("pacman", dependencies = TRUE, quiet = TRUE)
  require(pacman)
}

p_load(readxl, tidyr, dplyr, rgdal, sp)

#### GEOCODING FUNCTION ####
geokey <- ""  # add a bing api key
geocodeAddress <- function(df, address) {
  # takes an address, returns XY coordinates and postcode
  
  if(!require(jsonlite)) {require(jsonlite)}
  urlpt1 <- "http://dev.virtualearth.net/REST/v1/Locations?culture=en-GB&addressLine="
  urlpt2 <- "&userLocation=51.504360719046616,-0.12600176611298197&key=AmbO1l7vGnCAeg4vWolOAFsw5_W8PajuOIqeZQxjdGSbTbb_tczs-JhSEQo0joLt"
  url <- paste0(urlpt1, df[[address]], urlpt2)
  bingResult <- fromJSON(URLencode(url))
  if (bingResult$resourceSets$estimatedTotal > 0) {
    lat <- bingResult$resourceSets$resources[[1]]$point[, 2][[1]][1]
    lon <- bingResult$resourceSets$resources[[1]]$point[, 2][[1]][2]
    result <- data.frame(lat, lon, stringsAsFactors = FALSE)
  } else {
    result <- data.frame(lat = 0, lon = 0)
  }
  # print(result)
  result
}

#### DATA PROCESSING ####
# import the mains repair data
mrData <- read_excel("~/work/data/mains repairs/Mains Repair Reporting Latest.xlsx", sheet = "SAP Base Data")

# filter to mains repairs only
mrData <- mrData %>% filter(`Mains Repair` == "Yes")

# remove non-numeric characters from the house number column
mrData$`BGC_House Number - Short Text` <- gsub("[^0-9]", "", mrData$`BGC_House Number - Short Text`)

# remove the first two characters from the postcode field
mrData$`BGC_Post Code - Key` <- substr(mrData$`BGC_Post Code - Key`, 3, 11)

# make a merged address field
mrData$Address <- paste0(mrData$`BGC_House Number - Short Text`, " ", 
                         mrData$`BGC_Street - Long Text`, ", ", 
                         mrData$`BGC_City - Key`)

# make the AFR diameter column, converting imperial to metric
mrData$`BGC_Size - Short Text` <- as.double(mrData$`BGC_Size - Short Text`)
mrData <- mrData %>% 
  mutate(AFRDiameter = case_when(
    mrData$`BGC_Size Units` == "inch" ~ as.character(mrData$`BGC_Size - Short Text` * 25.4),
    mrData$`BGC_Size Units` == "MM" ~ as.character(mrData$`BGC_Size - Short Text`),
    is.na(mrData$`BGC_Size - Short Text`) ~ "")
    )

# make the material lookup table and add the AFR material column
materialLookup <- tribble(
~ "AFRMaterial",	~"sapMaterial",
"AC",	"Asbestos / Cement",
"BP", "Barrier Pipe(Coated Aluminium",
"BR",	"Brick",
"CI", "Cast Iron",
"CN",	"Concrete",
"CP", "Copper",
"CS",	"Coated Steel",
"DI",	"Ductile Iron",
"FG",	"Fibreglass",
"GI",	"Galvanised Iron",
"GRP",	"Glass Reinforced Plastic",
"GS",	"Galavanized Stell",
"LD",	"Lead",
"HPPE",	"HPPE (Dark Blue) High Performance Polyethylene",
"MDPE",	"MDPE (Med Blue)",
"OTH", "Other",
"POL",	"Polyethylene (Black Poly)",
"PVC", "Poly Vinyl Chloride",
"SI",	"Spun Iron",
"ST",	"Steel",
"UNK",	"Not assigned",
"UPC",	"Unplasticised Polyvinyl Chloride")

# add AFR Material
mrData <- left_join(mrData, materialLookup, by = c("BGC_Material - Medium Text" = "sapMaterial"))


# create the empty columns
mrData <- mrData %>% 
  mutate(RegulatoryReported = "Yes", SourceSystem = "SAP", ActivityCodePipeSize = "",
          MatchedMainSourceLayer = "", MatchedMainGISID = "", MatchedMainX = "",
          MatchedMainY = "", MatchedMainDistance = "", MatchedMainRenovated = "", MatchType = "", CurrentMain = "", 
          CoordinateSource = "", FailYearMonth = "", TMDB_ID = "", MatchCheck = "")

#  keep only necessary columns
mrData <- mrData %>% select(`BGC_Operation Number - Key`, `BGC_Notification Number - Key`, 
                              `BGC_Work Order Number - Key`, `BGC_Actual OperationType - Key`, `Type`, 
                              `Reporting period`, `RegulatoryReported`, `BGC_Operation Created Date`, 
                              `Date Completed`, `Address`, `The Real DMA`, 
                              `BGC_Operation Number - Work Centre (Text)`, `BGC_Actual OperationType - Medium Text`, 
                              `BGC_X Coordinate - Text`, `BGC_Y Coordinate - Text`, `SourceSystem`, 
                               ActivityCodePipeSize = `BGC type size`, 
                              `AFRMaterial`, `AFRDiameter`, `MatchedMainSourceLayer`, `MatchedMainGISID`, `MatchedMainX`, 
                              `MatchedMainY`, `MatchedMainDistance`, `MatchedMainRenovated`, `MatchType`, `CurrentMain`, 
                              `BGC_Post Code - Key`, `CoordinateSource`, `ImperialDiameter` = `BGC_Size - Short Text`,
                              `Mains Repair`, `FailYearMonth`, `TMDB_ID`, `MatchCheck`)

# change the column names
names(mrData) <- c("Activity", "NotificationNumber", "OrderNumber", "ActivityCode", "ActivityType", "ReportingPeriod", 
                    "RegulatoryReported", "CreatedDate", "CompletedDate", "Address", "DMA", "WorkCentre", "Comments", 
                    "X", "Y", "SourceSystem", "ActivityCodePipeSize", "AFRMaterial", "AFRDiameter", "MatchedMainSourceLayer", 
                    "MatchedMainGISID", "MatchedMainX", "MatchedMainY", "MatchedMainDistance", "MatchedMainRenovated", 
                    "MatchType", "CurrentMain", "Postcode", "CoordinateSource", "ImperialDiameter", "MainsRepair", 
                    "FailYearMonth", "TMDB_ID", "MatchCheck")

mrData$X <- as.double(mrData$X)
mrData$Y <- as.double(mrData$Y)


#### REGION TEST ####
# import the TW region (if it is not already imported)
if (!exists("twRegion")) {
twRegion <- readOGR(dsn = "C:/Users/dylan/Documents/work/maps", layer = "twRegionDissolvedSimplified")
twRegion <- spTransform(twRegion, CRSobj = "+init=epsg:27700")
}

# separate the latest records - test against stored activity code
mrLatest <- mrData %>% filter(!Activity %in% alreadyProcessed)

# if there are latest records to be processed do this
if(nrow(mrLatest)>0) {
  # make the latest records spatial
  mrLatest <- SpatialPointsDataFrame(coords = mrLatest[c("X", "Y")], mrLatest, proj4string = CRS("+init=epsg:27700"))
  
  
  # test whether any records are outside the TW area
  regionTest <- over(mrLatest, twRegion)
  
  # reduce the table to only records which need geocoding
  mrLatest <- mrLatest@data[is.na(regionTest$OBJECTID),]
  
  # create an address field for the geocode function
  mrLatest$geocodeAddress <- paste0(mrLatest$Address, ", ", mrLatest$Postcode)
  mrLatest$geocodeAddress <- gsub("#", "", mrLatest$geocodeAddress)

  # geocode
  geocodingResults <- apply(mrLatest, 1, geocodeAddress, "geocodeAddress")
  geocodingResults <- bind_rows(geocodingResults)
  geocodingResults <- SpatialPointsDataFrame(coords = geocodingResults[c("lon", "lat")], 
                                             data = geocodingResults, proj4string = CRS("+init=epsg:4326"))
  geocodingResults <- spTransform(geocodingResults, CRSobj = "+init=epsg:27700")
  geocodingResults <- data.frame(geocodingResults@data, 
                                 x = geocodingResults@coords[, 1], y = geocodingResults@coords[, 2])
  
} else {
  print("there are no new records to geocode")
}

  # write the geocoded X Y back to the full data set
  if(nrow(mrLatest) > 0){
    for (i in 1:nrow(mrLatest)) {
      if(geocodingResults[i, "lat"] > 0) {
      lActivity <- mrLatest$Activity[i]
      mrData[mrData$Activity == lActivity, "X"] <- geocodingResults[i, "x"]
      mrData[mrData$Activity == lActivity, "Y"] <- geocodingResults[i, "y"]
      mrData[mrData$Activity == lActivity, "CoordinateSource"] <- "Bing"
      }
    }
  }

  mrLatest <- cbind(mrLatest["Activity"], geocodingResults)

  # export the ungeocoded records
  write.table(mrLatest[mrLatest$lat == 0,], "geocodeFailed.csv", sep = ",", row.names = FALSE, col.names = TRUE)
  
  # export the geocoded records
  write.table(mrLatest[!mrLatest$lat == 0, ], "geocoded records.csv", sep = ",", row.names = FALSE, col.names = TRUE)
  
  
#### EXPORT THE PREPARED DATA ####
# export the prepared data
write.table(mrData, "mainsRepairToPipeTable.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# update the 'processed' list
alreadyProcessed <- unique(mrData$Activity)

rm(regionTest)
rm(mrData)
rm(mrLatest)
rm(i)
rm(lActivity)
rm(geocodingResults)

save.image("~/R/mains repairs/.RData")