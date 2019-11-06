#readschooldata.r

## read the schools data & associate it with the geography objects generated
## in readgeos.R, which must be run first in the same session for the joins to work

#library(readr)   (Part of the tidyverse)
library(tidyverse)
# and using a call from library(foreign), which must be install.packages("foreign)

# Public schools read 5 data files ####
# directory
ccd_sch_029_1718_w_1a_083118 <- read_csv("Data/ccd_sch_029_1718_w_1a_083118.csv")
glimpse(ccd_sch_029_1718_w_1a_083118)
#Some errors in the 2nd mailing address field not going to worry about it

directory <- ccd_sch_029_1718_w_1a_083118 %>%
  select(key=NCESSCH, name=SCH_NAME, type=SCH_TYPE_TEXT, GSLO, GSHI,LEVEL, MZIP, MZIP4, PHONE,WEBSITE,CHARTER=CHARTER_TEXT)

#membership ... woa this is 1 big file
ccd_sch_052_1718_w_1a_083118 <- read_csv("Data/ccd_SCH_052_1718_l_1a_083118.csv")
#View(ccd_sch_052_1718_w_1a_083118)
# Some parsing erros in the UNION field we don't use that. Probably put the county code into the wrong place. So ... NA
View(ccd_sch_052_1718_w_1a_083118, "Membership")

ccd_sch_052_1718_w_1a_083118 %>% group_by(SCH_NAME,GRADE) %>% summarise(TOTAL=sum(STUDENT_COUNT)) -> classrooms
classrooms
## Not the best way to go


#### Students in grades (membership)
membership <- ccd_sch_052_1718_w_1a_083118 %>% 
  select(key=NCESSCH, GRADE, TOTAL_INDICATOR,  STUDENTS=STUDENT_COUNT)
# PK,KG,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12,G13,UG,AE,
unique(membership$TOTAL_INDICATOR)
## We will be using 'education unit total' and 'subtotal 4 by grade

memberunittotals <- membership %>% select(-GRADE) %>% filter(TOTAL_INDICATOR=="Education Unit Total"  )
## got this, but decided to add up the students from all the grades instead

membergradetotals <- membership %>% filter(TOTAL_INDICATOR=="Subtotal 4 - By Grade"  )
membergradetotals <- membergradetotals %>% filter(STUDENTS > 0)

gradetotals <- membergradetotals %>% pivot_wider(names_from = GRADE, values_from = STUDENTS)
 View(gradetotals,"Students by grade")
# Note we will have to put these in order later on
 
#### Teachers  (Staff)

ccd_sch_059_1718_w_1a_083118 <- read_csv("Data/ccd_sch_059_1718_l_1a_083118.csv")
#View(ccd_sch_059_1718_w_1a_083118)                                                          

# Just get the school total records
staff <-  ccd_sch_059_1718_w_1a_083118 %>% 
  select(key=NCESSCH, teachers=TEACHERS, TOTAL_INDICATOR)
  

#### Magnet, vvirtual, NSLP (national school lunch program)
ccd_sch_129_1718_w_1a_083118 <- read_csv("Data/ccd_sch_129_1718_w_1a_083118.csv")
#View(ccd_sch_129_1718_w_1a_083118)
school <- ccd_sch_129_1718_w_1a_083118 %>% 
  select(key = NCESSCH, magnet = MAGNET_TEXT, VIRTUAL= VIRTUAL, NSLP = NSLP_STATUS)
 

#### Free Lunch, reduced price Lunch 
ccd_sch_033_1718_w_1a_083118 <- read_csv("Data/ccd_sch_033_1718_l_1a_083118.csv")
#View(ccd_sch_033_1718_w_1a_083118)

lunch_pre <- ccd_sch_033_1718_w_1a_083118 %>% 
  select (key = NCESSCH, TOTAL_INDICATOR, DATA_GROUP, LUNCH_PROGRAM, STUDENT_COUNT)
View(lunch_pre,"lunch pre processing")
          # totalfree = TOTFRL, free = FRELCH, reduced = REDLCH)
lunch_post <- lunch_pre %>% pivot_wider(names_from = LUNCH_PROGRAM, values_from = STUDENT_COUNT)

lunch_direct <- lunch_post %>% filter(DATA_GROUP == "Direct Certification") %>% 
  select(key,direct_cert_total='Not Applicable')

lunch_total <- lunch_post %>% filter(DATA_GROUP == "Free and Reduced-price Lunch Table",TOTAL_INDICATOR=="Education Unit Total") %>% 
  select(key, aided_lunch_total="No Category Codes")

lunch_post <- lunch_post %>% filter(TOTAL_INDICATOR=="Category Set A") %>% select(key,free_lunch = `Free lunch qualified`, reduced_price_lunch =`Reduced-price lunch qualified`)

## I mean, you have to call it 'lunch table'
lunch_table <- inner_join(school, lunch_post) %>% 
  inner_join(lunch_direct, by="key") %>% 
  inner_join(lunch_total, by="key")

#### pubschools comes from the readgeos.R file
pub19data <- left_join(pubschools,gradetotals, by="key") %>% 
  left_join(staff, by="key") %>% 
  left_join(directory, by="key") %>% 
  left_join(lunch_table, by="key") %>% select(-starts_with("total_indica"))

p19 <- pub19data %>% select(-c(type.x,name.y)) %>%  rename(type = type.y, name=name.x)


## PUt the names like they used to be in the olden days and re order them
p19A <- p19 %>% transmute(key, name, 
                          teachers,
                          students=0,  # leave space for summing at next step
                          type,
                          LEVEL,
                          NSLP, free_lunch, reduced_price_lunch, aided_lunch_total, direct_cert_total,
                          GSLO,
                          GSHI,
                          PreK=`Pre-Kindergarten`, K=Kindergarten,
                          G01 = `Grade 1`, 
                          G02 = `Grade 2`,
                          G03 = `Grade 3`,
                          G04 = `Grade 4`,
                          G05 = `Grade 5`,
                          G06 = `Grade 6`,
                          G07 = `Grade 7`,
                          G08 = `Grade 8`,
                          G09 = `Grade 9`,
                          G10 = `Grade 10`,
                          G11 = `Grade 11`,
                          G12 = `Grade 12`,
                          Adult = `Adult Education`,
                          UNG = `Ungraded`,
                          NS = `Not Specified`,
                          magnet,
                          CHARTER,
                          VIRTUAL,
                          MZIP, MZIP4, PHONE, WEBSITE,
                          CD, state, latitude, longitude) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  mutate_if(is.character, funs(ifelse(is.na(.), "", .)))
## After the transmutation, make all the NA's of both kinds into zeroe or empty field


## And make the total students the sum of the grades
p19A$students <- p19A$G01 + p19A$G02 + p19A$G03 +
  p19A$G04 + p19A$G05 + p19A$G06 + p19A$G07 +
  p19A$G08 + p19A$G09 + p19A$G10 + p19A$G11 +
  p19A$G12 + p19A$UNG + p19A$NS + p19A$Adult

sum(p19A$students)  # Does 45,417,790 seem reasonable?
  

foreign::write.dbf(as.data.frame(p19A), "~/Downloads/USpublicschoolsdata1718DBF.dbf", factor2char = FALSE)
## To test in the other program

write_csv(p19A,"~/Downloads/USpublicschoolsdata1718.csv", na="")   # In case we missed a NA above

p19lunch <- p19A %>% select(key,Name=name,Level=LEVEL,CongDist=CD,state,teachers,students,free_lunch,reduced_price_lunch,aided_lunch_total,direct_cert_total,latitude,longitude)

## Did we not get some of these?  (tracking down an oopsie)
tally(p19lunch,state=="UT")

### Less interested in schools with no students and no teachers
p19lunch <- p19lunch %>% filter(!(students==0 & teachers==0))
View(p19lunch)

tally(p19lunch,state=="UT")

tally(p19lunch,aided_lunch_total > 0)

#foreign::write.dbf(as.data.frame(p19lunch), "~/Downloads/USpublicLunch.dbf")
#write_csv(p19lunch,"~/Downloads/USpublicLunchCSV.csv")

visdat::vis_dat(p19lunch, warn_large_data=FALSE)


##### Add NSLP source

p19lunch <- p19A %>% select(key,Name=name,Level=LEVEL,CongDist=CD,teachers,students,NSLP,free_lunch,reduced_price_lunch,aided_lunch_total,direct_cert_total,latitude,longitude)

unique(p19lunch$NSLP)

NSLPCodes <- c("NSLPCEO","NSLPNO","Not reported", "NSLPWOPRO",    "NSLPPRO3",     "NSLPPRO2",     "NSLPPRO1",     "MISSING")

### Sometimes it is not missing and not not reported it is just ... gone!
lunchmoney <- function (x) {
  ifelse(x %in% NSLPCodes,x,"NOCODE")
}

lunchmoney("MISSING")    ## A test


## This was a booboo bug that eliminated all schools in 1 state?
p19lunch <- p19lunch %>% filter(!(students==0 | teachers==0)) %>% 
  mutate_at(c("NSLP"), funs(lunchmoney(.)))

### This was discovering the bug
p19luncha <- p19luncha %>% mutate_at(c("NSLP"), funs(lunchmoney(.)))

nocodes <- filter(p19luncha, p19luncha$NSLP=="NOCODE")
View(nocodes,"No NSLP Code")

table(p19lunch$NSLP)

foreign::write.dbf(as.data.frame(p19luncha), "~/Downloads/USpublicLunch.dbf")
write_csv(p19luncha,"~/Downloads/USpublicLunchCSV.csv")

## Finally, we get just the schools with students and teachers, in NC's 3rd district
NC3 <- p19luncha %>% filter(CongDist=="3703") %>% 
  filter(!(students==0 & teachers==0)) 

write_csv(NC3, "~/Downloads/NC3PublicLunchK.csv")

write_csv(NC3, "NC3PublicLunch.csv")
foreign::write.dbf(as.data.frame(NC3), "~/Downloads/NC3LunchTest.dbf", factor2char = FALSE)

NC3$key <- paste0("C",NC3$key)
NC3 <- NC3 %>% select(-c("CongDist","NSLP","direct_cert_total"))
write_csv(NC3, "~/Downloads/NC3PublicLunchK.csv")


# def  should not have changed the name of CD to CongDist in just 1 place :-()

NC3all <- p19A %>% filter(CD=="3703") %>% 
  filter(!(students==0 & teachers==0)) 


