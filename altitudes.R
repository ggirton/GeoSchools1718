# First Flight Elementary

### https://nationalmap.gov/epqs/pqs.php?

# Get elevations (in meters) for the public schools in NC's 3rd district (Kitty Hawk, Kill Devil Hills, Albemarle Sound, inner banks)

library(httr)
library(jsonlite)
library(readr)

## Interactive readout from putting the values directly into the above page
# Washington High
# 
# https://nationalmap.gov/epqs/pqs.php?x=-77.013522&y=35.557454&units=Meters&output=json
# 
# {"USGS_Elevation_Point_Query_Service":{"Elevation_Query":{"x":-77.013522,"y":35.557454,"Data_Source":"3DEP 1\/3 arc-second","Elevation":10.72,"Units":"Meters"}}}
# 
# Nags Head Elementary 
# 
# {"USGS_Elevation_Point_Query_Service":{"Elevation_Query":{"x":-75.63868,"y":35.975804,"Data_Source":"3DEP 1\/3 arc-second","Elevation":3.91,"Units":"Meters"}}}
# 

#### Trying with GET

bffurl <- "https://nationalmap.gov/epqs/"
bfpath <- "pqs.php"

options(stringsAsFactors = FALSE)

## Try once with Washington High
longitude=-77.013522
latitude=35.557454

returnformat = c("json")
altitude_units = c("Meters")

send <- paste0(bffurl,bfpath, "?", "x=", longitude, 
               "&y=",  latitude, "&units=", altitude_units, "&output=", returnformat)

### Interactive examination
send

rawres <- GET(send)

names(rawres)

rawres$status_code

payload <- rawToChar(rawres$content)

jsonenvelope <- fromJSON(payload)

print(jsonenvelope$USGS_Elevation_Point_Query_Service$Elevation_Query$Elevation)


### ready to process the 209 schools in NC's 3rd district
outerbanks <- read_csv("NC3PublicLunch.csv",
                         col_types= cols(
                           key = col_character(),
                           Name = col_character(),
                           Level = col_character(),
                           CongDist = col_character(),
                           teachers = col_double(),
                           students = col_double(),
                           NSLP = col_character(),
                           free_lunch = col_double(),
                           reduced_price_lunch = col_double(),
                           aided_lunch_total = col_double(),
                           direct_cert_total = col_double(),
                           latitude = col_double(),
                           longitude = col_double()
                         )
                       )

str(outerbanks)

coord_recs <- outerbanks
thisrun <- nrow(coord_recs)

startfile=TRUE
for(index in 1:thisrun){
  schoolkey=coord_recs[index,]$key
  longitude=coord_recs[index,]$longitude
  latitude=coord_recs[index,]$latitude
  
  sendit <- send <- paste0(bffurl,bfpath, "?", "x=", longitude, 
                   "&y=",  latitude, "&units=", altitude_units, "&output=", returnformat)
  print(sendit)  
  rawres <- GET(send)
  if(rawres$status_code == 200)
  {
    payload <- rawToChar(rawres$content)
    jsonenvelope <- fromJSON(payload)
    
    Elevation <- jsonenvelope$USGS_Elevation_Point_Query_Service$Elevation_Query$Elevation
    Units <- jsonenvelope$USGS_Elevation_Point_Query_Service$Elevation_Query$Units
    DataSource <- jsonenvelope$USGS_Elevation_Point_Query_Service$Elevation_Query$Data_Source
    
    result <- tribble(~key, ~latitude, ~longitude, ~Elevation, ~Source,
                      schoolkey,
                      latitude, longitude, Elevation, DataSource)
                      
    
    write_csv(as.data.frame(result),"elevationresult.csv", append=!startfile)
    startfile=FALSE
  } else {
    print(rawres$status_code) # what error num?
  }
}

### as it turned out, batch run failed due to 400 errors ... not sure why.



##### succeeding with POST

library(httr)

altitude_units <- "Meters"
returnformat <- "json"
latitude <- 33.970797
longitude <- -118.382728  
latitude <- as.character(latitude)
longitude <- as.character(longitude)

### elquery is 'elevation query'
elquery <- list(
  x= longitude, 
  y=  latitude, 
  units= altitude_units, 
  output= returnformat
)

#### Testing WORKED: Interactive part

elquery <- POST("https://nationalmap.gov/epqs/pqs.php", body = elquery, encode = "form", verbose())
names(elquery)
elquery$status_code

payload <- rawToChar(elquery$content)
# payload
jsonenvelope <- fromJSON(payload)
# 
print(jsonenvelope$USGS_Elevation_Point_Query_Service$Elevation_Query$Elevation)


## We'll need the function to call & set values repeatedly
setelquery <- function(latitude, longitude, 
                      altitude_units = "Meters",  returnformat = "json"){
  latitude <- as.character(latitude)
  longitude <- as.character(longitude)
  elquery <- list(
    x= longitude, 
    y=  latitude, 
    units= altitude_units, 
    output= returnformat
  )
  elquery
}

elquery <- setelquery(latitude = 38.8977, longitude = -77.0365)  # the white house
## Go back and try this ...
## The White House is 18.81 meters above sea level.  No problem!!!



coord_recs <- outerbanks
thisrun <- nrow(coord_recs)

## Create the file on the 1st write
startfile=TRUE

for(index in 1:thisrun){

  ## So weird to do iterative code once you're used to vectorized!
  schoolkey = coord_recs[index,]$key
  the_longitude = coord_recs[index,]$longitude
  the_latitude = coord_recs[index,]$latitude
  
  elquery <- setelquery(latitude = the_latitude, longitude = the_longitude)
  print(elquery)  ### ran this once and commented out the POST to make sure queries looked good
  
  rawres <- POST("https://nationalmap.gov/epqs/pqs.php", body = elquery, encode = "form", verbose())
  
  if(rawres$status_code == 200)
  {
    payload <- rawToChar(rawres$content)
    jsonenvelope <- fromJSON(payload)
    
    Elevation <- jsonenvelope$USGS_Elevation_Point_Query_Service$Elevation_Query$Elevation
    Units <- jsonenvelope$USGS_Elevation_Point_Query_Service$Elevation_Query$Units
    DataSource <- jsonenvelope$USGS_Elevation_Point_Query_Service$Elevation_Query$Data_Source
    
    result <- tribble(~key, ~latitude, ~longitude, ~Elevation, ~Source,
                      schoolkey,
                      latitude, longitude, Elevation, DataSource)
    
    
    write_csv(as.data.frame(result),"elevationresult.csv", append=!startfile)
    startfile=FALSE
  } else {
    print(rawres$status_code) # what error num?
  }
  Sys.sleep(0.3)  ### Since it was verbose, above, probably did not need to do this   
  
}

### So, let's get elevation back in here to match with the schools data

NC3elev <- read_csv("elevationresult.csv", col_types =
                      cols(
                        key = col_character(),
                        latitude = col_double(),
                        longitude = col_double(),
                        Elevation = col_double(),
                        Source = col_character()
                      ))

table(NC3elev$Source)   ### it's all good
summary(NC3elev$Elevation)  ## can't wait to see it

NC3Eonly <- NC3elev %>% select(key,Elevation)
NC3Esource <- NC3elev %>% select(key,Elevation, Source)

NC3E <- NC3 %>%  left_join(NC3Eonly, by="key")
foreign::write.dbf(NC3E,"~/Downloads/NC3SchoolElevations.dbf")
## Just test the elevations in the other program

NC3combined <- NC3all %>%  left_join(NC3Esource, by="key")
foreign::write.dbf(NC3combined,"~/Downloads/NC3SchoolsCombined.dbf")
## Just test the data in the other program

write_csv(NC3combined,"NC3SchoolsCombined.csv")
## make a copy for github


#tt <- "x=-118.382728&y=33.970797&units=Meters&output=JSON"
#str_length(tt) # yes it was 50


