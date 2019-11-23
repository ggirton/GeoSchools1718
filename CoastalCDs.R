library(tidyverse)


### Get coastal schools
## These are the schools in coastal COUNTIES of the United States
coastal <- read_csv("~/Downloads/CoastalCDs.csv")

coastalCDs <- unique(coastal$cd)
# coastalCDs
unique(coastal$state)

ctyschools <- nrow(coastal)
## should be more schools in the CDs than in the counties 16438

library(foreign)
options(stringsAsFactors = FALSE)
p19B <- read.dbf("~/Downloads/USpublicschoolsdata1718DBF.dbf")

p19Coastal <- p19B %>% filter(CD %in%  coastalCDs)
nrow(p19Coastal) > ctyschools   ## TRUE!

unique(p19Coastal$state)

filter(p19Coastal,state=="CA") %>% nrow()

  write_csv(p19Coastal,"coastalCDschools.csv")

write.dbf(p19Coastal,"~/Downloads/USCoastalSchoolsdata1718.dbf")

plot(p19Coastal$longitude,p19Coastal$latitude)

## Don't really need infinite precision on coordinates
p19Coastal$latitude <- format(p19Coastal$latitude,digits = 7)
p19Coastal$longitude <- format(p19Coastal$longitude,digits = 8)

write_csv(p19Coastal,"coastalCDschools.csv")

?format
plot(p19Coastal$lng,p19Coastal$lat, type='p', pch=22)
