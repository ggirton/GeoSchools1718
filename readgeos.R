#ReadSchools geographc coordinates into 1 geo schools file: public, private, post secondary ####
library(readxl)
library(naniar)
library(tidyverse)
library(visdat)

# Whenever possible, begin your script with a tribble that you will need later on ####
fipskey2statekey <- tribble(
  ~ ANSIKEY, ~STATEKEY,
  "01","AL",
  "02","AK",
  "04","AZ",
  "05","AR",
  "06","CA",
  "08","CO",
  "09","CT",
  "10","DE",
  "11","DC",
  "12","FL",
  "13","GA",
  "15","HI",
  "16","ID",
  "17","IL",
  "18","IN",
  "19","IA",
  "20","KS",
  "21","KY",
  "22","LA",
  "23","ME",
  "24","MD",
  "25","MA",
  "26","MI",
  "27","MN",
  "28","MS",
  "29","MO",
  "30","MT",
  "31","NE",
  "32","NV",
  "33","NH",
  "34","NJ",
  "35","NM",
  "36","NY",
  "37","NC",
  "38","ND",
  "39","OH",
  "40","OK",
  "41","OR",
  "42","PA",
  "44","RI",
  "45","SC",
  "46","SD",
  "47","TN",
  "48","TX",
  "49","UT",
  "50","VT",
  "51","VA",
  "53","WA",
  "54","WV",
  "55","WI",
  "56","WY",
  "60","AS",
  "66","GU",
  "69","MP",
  "72","PR",
  "74","UM",
  "78","VI"
)

# Load Schools geography files ####
EDGE_GEOCODE_PUBLICSCH_1718 <- read_excel("EDGE_GEOCODE_PUBLICSCH_1718/EDGE_GEOCODE_PUBLICSCH_1718.xlsx")
View(EDGE_GEOCODE_PUBLICSCH_1718, "Public School Coords")

pubgeo <- EDGE_GEOCODE_PUBLICSCH_1718 %>% 
  select(key=NCESSCH, name=NAME, state=STATE, CD=CD, latitude=LAT, longitude=LON)
pubgeo$type <- "PUB"
# vis_dat(pubgeo)  # make sure they are all there  (looking good)

# test the state code improvement
wytestpubgeo <- c("560123")
goodstatecode <- function(fipschar) {
  fipskey2statekey$STATEKEY[fipskey2statekey$ANSIKEY==fipschar] 
}
goodstatecode(str_sub(wytestpubgeo,1,2)) == "WY"

#improve the state codes
pubgeo_m1 <- mutate(pubgeo,statefips2=str_sub(key,1,2))
pubgeo_m1 <- merge(pubgeo_m1,fipskey2statekey,by.x="statefips2", by.y = "ANSIKEY")
pubgeo <- select(pubgeo_m1, key,name,state=STATEKEY,CD,latitude,longitude,type)

EDGE_GEOCODE_POSTSECONDARYSCH_1718 <- read_excel("EDGE_GEOCODE_POSTSECONDARYSCH_1718/EDGE_GEOCODE_POSTSECSCH_1718.xlsx")
View(EDGE_GEOCODE_POSTSECONDARYSCH_1718, "PostSecondary Coords")

postgeo <- EDGE_GEOCODE_POSTSECONDARYSCH_1718  %>% 
  select(key=UNITID,name=NAME, state=STATE, CD=CD, latitude=LAT, longitude=LON)
postgeo$type <- "POST"


pss1718_pu <- read_excel("EDGE_GEOCODE_PRIVATESCH_17_18/EDGE_GEOCODE_PRIVATESCH_1718.xlsx")
View(pss1718_pu, "Private School Coords")

prigeo <- pss1718_pu %>% 
  select(key=PPIN,name=NAME, state=STATE, CD=CD, latitude=LAT, longitude=LON)
prigeo$type <- "PRIV"

# Tribble time: Improve the state coding of the prigeo file ####
# Another day, another tribble, there's no trouble with tribbles
fip2state <- tribble(
  ~ ANSI, ~STATE,
  1,"AL",
  2,"AK",
  4,"AZ",
  5,"AR",
  6,"CA",
  8,"CO",
  9,"CT",
  10,"DE",
  11,"DC",
  12,"FL",
  13,"GA",
  15,"HI",
  16,"ID",
  17,"IL",
  18,"IN",
  19,"IA",
  20,"KS",
  21,"KY",
  22,"LA",
  23,"ME",
  24,"MD",
  25,"MA",
  26,"MI",
  27,"MN",
  28,"MS",
  29,"MO",
  30,"MT",
  31,"NE",
  32,"NV",
  33,"NH",
  34,"NJ",
  35,"NM",
  36,"NY",
  37,"NC",
  38,"ND",
  39,"OH",
  40,"OK",
  41,"OR",
  42,"PA",
  44,"RI",
  45,"SC",
  46,"SD",
  47,"TN",
  48,"TX",
  49,"UT",
  50,"VT",
  51,"VA",
  53,"WA",
  54,"WV",
  55,"WI",
  56,"WY",
  72,"PR"
)  # This database has no other keys, like VI GUam, etc.


newstate <- function(fips) {
  fip2state$STATE[fip2state$ANSI==fips] 
}
newstate(51) == "VA"

vis_dat(prigeo)
vis_dat(pubgeo)
vis_dat(postgeo)
#betterprigeo <- prigeo %>% mutate_at(vars(stateansi),funs(newstate(.)))
## well, that totally did not work

betterprigeo <- merge(prigeo, fip2state, by.x="stateansi", by.y = "ANSI")
prigeo <- select(betterprigeo,key,name,state=STATE,latitude,longitude,type)

betterprigeo
# Combine schools geography files ####
schoolsgeo <- rbind(pubgeo, prigeo, postgeo)

length(unique(schoolsgeo$key)) == nrow(schoolsgeo)

table(schoolsgeo$state)
## Filtering
states <- c("AL",
            "AR", "AK", "CA", "CO", "AZ", "CT", "DE", "DC", "MD", "FL",
            "GA", "HI", "ID", "OR", "IL", "IN", "IA", "KS", "KY", "LA", 
            "ME", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
            "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
#(statesm <- c(states,"M")  ) # NO MORE MISSING! (wasIncluding "missing")

# Puerto Rico ####
schoolsgeo %>% filter(state=="PR") -> prschools1718
# look at it (just for grins)
plot(prschools1718$longitude,prschools1718$latitude)

foreign::write.dbf(as.data.frame(prschools1718), "prschools1718.dbf", factor2char = TRUE)

#  Write The 51 states ####
unique(schoolsgeo$state) -> allstates
allstates  # Check it out!

schoolsus <- filter(schoolsgeo, state %in% states) 

nrow(schoolsus)
#mschools <- filter(schoolsgeo, state =="M")
# plot(x=mschools$longitude, y=mschools$latitude)
# look at the missing schools  (from when they had not been correct for)


#library(foreign)
foreign::write.dbf(as.data.frame(schoolsus), "schools1718.dbf", factor2char = TRUE)

#write_excel_csv(schoolsgeo,"schools1718.csv")
table(schoolsus$state)

table(schoolsus$type)
View(schoolus,"US Schools")

schoolsus %>% filter(type=="POST") %>% View("Post")

pubschools <- schoolsus %>% filter(type=="PUB") 


