library("tidyverse")

# KSPS <- read_csv("~/Downloads/KansasPublicSchools.csv")

## The 4 congressional districts of state number 20
KSPS <- p19A %>% filter(CD=="2001" | CD == "2002" | CD == "2003" | CD == "2004") %>% 
  filter(!(students==0 & teachers==0)) ## no teachers, no students, no schooling


websites <- KSPS %>% group_by(CD,WEBSITE) %>% select(Website=WEBSITE,CongDist=CD) %>% summarise(schools=n())

# table(websites)
unique(websites$CongDist)

### Websites by congressional district, by number of schools in each website
write_csv(websites,"kswebsitesbycd.csv")
### thus, 2002,http://www.usd246.org,2   USD 246 is a school district located in Kansas 2nd 2nd congressional district (20-02) Arma, KS,  & 2 schools

View(KSPS,"Kansas public schools")

view(websites,'KS school websites')

nrow(websites) == 463   ## Some of the websites are 404 due to subsequent URL re-org by the districts
