### Importing necessary libraries for my analysis
### Note: some of these libraries may not be utilized in this body of Code
### I made some adjustments to which packages I would be using
library(janitor)
library(tidyverse)
library(readxl)
library(fastDummies)
library(readr)
library(data.table)
library(ggmap)
library(devtools)
library(rgeos)
library(maptools)
library(geojsonio)
library(ggplot2)
library(ggthemes)
library(ggpubr)

### Cleaning for D-in-D Regression Analysis
### Import New York State Youth Tobacco Survey - https://health.data.ny.gov/Health/Youth-Tobacco-Survey-Beginning-2000/pbq7-ddg9
NewYorkYTSRaw <- read_csv("YTS2000_2020.csv") %>% 
  clean_names()

### Filter to only only include 2006-2018 range
NewYorkYTS <- NewYorkYTSRaw[!(NewYorkYTSRaw$year==2000) 
                            & !(NewYorkYTSRaw$year==2002) 
                            & !(NewYorkYTSRaw$year==2004)
                            & !(NewYorkYTSRaw$year==2020),] 

### Filter to only include responses from high school students
NewYorkYTS <- filter(NewYorkYTS, schlev==2) 

### Filtering for NYC Students
### [Defined by County FIPS (i.e., NYC if FIPS = 36005, 36047, 36061, 36081, or 36085)]
NYCYTS <- filter(NewYorkYTS, nyc==1)

### Filtering for NY State Students (excluding NYC) 
NYSYTS <- filter(NewYorkYTS, nyc==0)

### Running initial regression to determine if the parallel trends assumption was met
NewYorkYTS$location <- ifelse(NewYorkYTS$nyc == 1, 1, 0)
NewYorkYTS$yeardummy <- ifelse(NewYorkYTS$year <= 2015, 0, 1)
NewYorkYTS$policychange = NewYorkYTS$location * NewYorkYTS$yeardummy

### Subsetting data to output for figures
proof <- NewYorkYTS[,"storeproof", drop=FALSE]
proof <- proof[proof$storeproof !=1,]
proof <- na.omit(proof)
l <- sum(proof$storeproof == 2)
k <- sum(proof$storeproof == 3)
new <- cbind(k, l)
write.csv(new, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\proof.csv")

tobacco <- select(NewYorkYTS, c(year, smoke30))
ecig <- select(NewYorkYTS, c(year, vape30))
cigar <- select(NewYorkYTS, c(year, cigar30))
smokeless <- select(NewYorkYTS, c(year, smkls30))

test <- tobacco %>% 
  group_by(year) %>% 
  summarize(count=n()) 
test1 <- tobacco %>% 
  group_by(year) %>% 
  summarize(sumtobacco = sum(smoke30, na.rm=TRUE))
tt <- cbind(test, test1) %>% 
  subset(select=-c(3)) %>% 
  mutate(tobaccorate = sumtobacco/count)
tt$tobaccorate <- tt$tobaccorate *100

test2 <- ecig %>% 
  group_by(year) %>% 
  summarize(count=n()) 
test3 <- ecig %>% 
  group_by(year) %>% 
  summarize(sumecig = sum(vape30, na.rm=TRUE))
tt <- cbind(tt,test2, test3) %>% 
  subset(select=-c(7)) %>% 
  mutate(ecigrate = sumecig/count)
tt$ecigrate <- tt$ecigrate *100

test4 <- cigar %>% 
  group_by(year) %>% 
  summarize(count=n()) 
test5 <- cigar %>% 
  group_by(year) %>% 
  summarize(sumcigar = sum(cigar30, na.rm=TRUE))
tt <- cbind(tt,test4, test5) %>% 
  subset(select=-c(11)) %>% 
  mutate(cigarrate = sumcigar/count)
tt$cigarrate <- tt$cigarrate *100

test6 <- smokeless %>% 
  group_by(year) %>% 
  summarize(count=n()) 
test7 <- smokeless %>% 
  group_by(year) %>% 
  summarize(sumsmkls = sum(smkls30, na.rm=TRUE))
tt <- cbind(tt,test6, test7) %>% 
  subset(select=-c(15)) %>% 
  mutate(smklsrate = sumsmkls/count)
tt$smklsrate <- tt$smklsrate *100
tt <- tt[, c("year", "tobaccorate", "ecigrate", "cigarrate","smklsrate")]
write.csv(tt, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\NewYorkYTStobacco.csv")
### Created figures in DataWrapper

### More figures
whackytobacky <- select(NewYorkYTS, c("year","location","smoke30"))
whackytobacky1 <- whackytobacky[whackytobacky$location==1,]
whackytobacky2 <- whackytobacky[whackytobacky$location==0,]

try <- whackytobacky1 %>% 
  group_by(year) %>% 
  summarize(count=n()) 
try1 <- whackytobacky1 %>% 
  group_by(year) %>% 
  summarize(sumtobaccocity = sum(smoke30, na.rm=TRUE))
vv <- cbind(try, try1) %>% 
  subset(select=-c(3)) %>% 
  mutate(tobaccoratecity = sumtobaccocity/count)
vv$tobaccoratecity <- vv$tobaccoratecity *100 
vv = subset(vv, select=-c(2,3))

why <- whackytobacky2 %>% 
  group_by(year) %>% 
  summarize(count=n()) 
why1 <- whackytobacky2 %>% 
  group_by(year) %>%
  summarize(sumtobaccostate = sum(smoke30, na.rm=TRUE))
why1 = subset(why1, select=-c(1))
cc <- cbind(why, why1) %>% 
  mutate(tobaccoratestate = sumtobaccostate/count)
cc$tobaccoratestate <- cc$tobaccoratestate *100 
cc = subset(cc, select=-c(2,3))
cc = cbind(cc, vv)
cc = subset(cc, select=-c(3))
write.csv(cc, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\whackytobacky.csv")

### D-in-D - parallel trends assumption
### creating a "did" variable
### will use "policychange" as the difference in difference variable
### Pre-2014 NYC
lmptaNYC = lm(formula = smoke30~year + whatgrade+race+moneyall2, 
              data=subset(NewYorkYTS,policychange==0 & location==1),
              na.action=na.exclude)
summary(lmptaNYC)

### Pre-2014 NYS
lmptaNYS = lm(formula = smoke30~year + whatgrade+race+moneyall2, 
              data=subset(NewYorkYTS,policychange==0 & location==0), na.action=na.exclude)
summary(lmptaNYS)

### D-in-D Regression NYC
didNYC = lm(formula = smoke30~year+whatgrade+moneyall2+policychange, 
              data=NewYorkYTS, na.action=na.exclude)
summary(didNYC)


### Additional Line Graphs
### National Youth Survey Data for Tobacco Usage  
### https://www.cdc.gov/tobacco/data_statistics/surveys/nyts/data/index.html
National2011 <- read_excel("nyts2011.xlsx") %>% clean_names() %>% 
  select(qn13)
National2011$dv <- ifelse(National2011$qn13 > 1, 1, 0) 
National2011 <- subset(National2011, select = -c(qn13)) %>% 
  summarize(National2011$dv, "2011"=mean(National2011$dv, na.rm=TRUE)) %>% 
  select("2011") %>% 
  slice(1)

National2012 <- read_excel("nyts2012.xlsx") %>% clean_names() %>% 
  select(qn13)
National2012$dv <- ifelse(National2012$qn13 > 1, 1, 0) 
National2012 <- subset(National2012, select = -c(qn13)) %>% 
  summarize(National2012$dv, "2012"=mean(National2012$dv, na.rm=TRUE)) %>% 
  select("2012") %>% 
  slice(1)

National2013 <- read_excel("nyts2013.xlsx") %>% clean_names() %>% 
  select(qn15)
National2013$dv <- ifelse(National2013$qn15 > 1, 1, 0) 
National2013 <- subset(National2013, select = -c(qn15)) %>% 
  summarize(National2013$dv, "2013"=mean(National2013$dv, na.rm=TRUE)) %>% 
  select("2013") %>% 
  slice(1)

National2014 <- read_excel("nyts2014.xlsx") %>% clean_names() %>% 
  select(qn13)
National2014$dv <- ifelse(National2014$qn13 > 1, 1, 0) 
National2014 <- subset(National2014, select = -c(qn13)) %>% 
  summarize(National2014$dv, "2014"=mean(National2014$dv, na.rm=TRUE)) %>% 
  select("2014") %>% 
  slice(1)

National2015 <- read_excel("nyts2015.xlsx") %>% clean_names() %>% 
  select(qn12)
National2015$dv <- ifelse(National2015$qn12 > 1, 1, 0) 
National2015 <- subset(National2015, select = -c(qn12)) %>% 
  summarize(National2015$dv, "2015"=mean(National2015$dv, na.rm=TRUE)) %>% 
  select("2015") %>% 
  slice(1)

National2016 <- read_excel("nyts2016.xlsx") %>% clean_names() %>% 
  select(qn13)
National2016$dv <- ifelse(National2016$qn13 > 1, 1, 0) 
National2016 <- subset(National2016, select = -c(qn13)) %>% 
  summarize(National2016$dv, "2016"=mean(National2016$dv, na.rm=TRUE)) %>% 
  select("2016") %>% 
  slice(1)

National2017 <- read_excel("nyts2017.xlsx") %>% clean_names() %>% 
  select(qn11)
National2017$dv <- ifelse(National2017$qn11 > 1, 1, 0) 
National2017 <- subset(National2017, select = -c(qn11)) %>% 
  summarize(National2017$dv, "2017"=mean(National2017$dv, na.rm=TRUE)) %>% 
  select("2017") %>% 
  slice(1)

National2018 <- read_excel("nyts2018.xlsx") %>% clean_names() %>% 
  select(qn11)
National2018$dv <- ifelse(National2018$qn11 > 1, 1, 0) 
National2018 <- subset(National2018, select = -c(qn11)) %>% 
  summarize(National2018$dv, "2018"=mean(National2018$dv, na.rm=TRUE)) %>% 
  select("2018") %>% 
  slice(1)
Total <- cbind(National2011, National2012, National2013, National2014, National2015, National2016, National2017, National2018)
Total <- pivot_longer(Total, cols=c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'), names_to = 'year')

write.csv(Total, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\Total.csv")

### Ended up making this graph in DataWrapper.de 
### t <- ggplot(Total, aes(x=year, y=value, group=1)) + 
### geom_line() +
### ggtitle("National HS Youth Reported Tobacco Usage")

### Mapping and Visualizations
### Cleaning Data for Visualizations
### Import Data for active Tobacco Retailer Map - https://health.data.ny.gov/Health/Active-Tobacco-Retailer-Map/88k2-euek
retail <- read_csv("Active_Tobacco_Retailer_Map.csv") %>% 
  clean_names()
NYCretail <- filter(retail, county=="NEW YORK CITY" )

### Import Data for Tobacco Violations (FDA) - https://www.accessdata.fda.gov/scripts/oce/inspections/oce_insp_searching.cfm
### Filtered to only include data for violations involving sales to minors
violation <- read_csv("OCE_Inspection_Search_Report.csv", skip=8) %>% 
  clean_names()

### Getting the zip codes from the retail data to use to subset the violations data
NYCzip <- dplyr::filter(NYCretail, county=="NEW YORK CITY") %>% 
 select(zip)


### Filtering the "violate" and "retail" dataframes to only include addresses in NYC
### Created a vector of the NYC Zip Codes
vec <- NYCzip$zip

### Checking if the values in the "retail" dataframe, column "zip", include any vectors
### saving to dataframe for just NYC retailers
NYCretail <- NYCretail[NYCretail$zip %in% vec,]

### Realizing the location data included both the address and coordinates in one column
### Split that column into two columns, and appending to original dataframe
NYCretail[c("address", "coordinates")] <- str_split_fixed(NYCretail$location, "\\(",2)

### Splitting the coordinates into latitude and longitude
NYCretail[c("lat", "lon")] <- str_split_fixed(NYCretail$coordinates, ",",2)

### Removed the weird parenthesis
NYCretail$lon <- gsub("\\)", "", as.character(NYCretail$lon))

### Removing superfluous columns
NYCretail<-select(NYCretail, select= -c("location","coordinates"))

### Doing the same for violation
NYCviolations <- violation[violation$zip %in% vec,]

### Combining the address columns into one column that can be used for geocoding
NYCviolations$address <- paste(NYCviolations$street_address, NYCviolations$city, NYCviolations$state, NYCviolations$zip, sep=", ")

### Removing superfluous columns
NYCviolations<-select(NYCviolations, select = -c("street_address", "state", "city", "zip"))

### Providing ggmap with my API key from Google
### My API key has been redacted
### Get an API Key here: https://console.cloud.google.com/apis
register_google(key="AIzaSyCkvNYqNRU89gv3U4TtaRKgCvOsc9xs4RU")

### Geocoding to get latitude and longitude coordinates
origAddress <- select(NYCviolations, select="address")
# Initialize the data frame
geocoded <- data.frame(Address=origAddress, stringsAsFactors = FALSE)
### Using the geocode function to plot latitude and longitude
map <- geocode(location = geocoded$select, output="latlon", source="google")
merge <- cbind(geocoded, map)
NYCviolations <- cbind(NYCviolations, merge) 
NYCviolations <- select(NYCviolations, select = -c("select"))

### Realized I needed to change my date-time formats for the violations to create a temporal rendering
NYCviolations$newdate <- strptime(as.character(NYCviolations$decision_date), "%m/%d/%Y")
NYCviolations$year <- substr(NYCviolations$newdate, 1,4)

### Need to group my variables for graphs
NYCviolations <- subset(NYCviolations[-c(17,18)])
### After looking at the breakdown of different product types, I decided to group them
### E-liquid, ENDS, and ENDS / E-liquid = E-cigarettes & Accessories
### Cigar(s) and Single Cigarette = Cigar(s)
### Cigarette tobacco and Hookah tobacco = Tobacco
NYCviolations$product_type[NYCviolations$product_type == "E-liquid" | 
                            NYCviolations$product_type == "ENDS" | 
                            NYCviolations$product_type == "ENDS / E-liquid"] <- "E-cigarettes & Accessories"
NYCviolations$product_type[NYCviolations$product_type == "Cigar(s)" | 
                            NYCviolations$product_type == "Single cigarette"] <- "Cigar(s)"
NYCviolations$product_type[NYCviolation$product_type == "Cigarette tobacco" | 
                            NYCviolations$product_type == "Hookah tobacco"] <- "Tobacco"
NYCviolationstype <- dplyr::group_by(NYCviolations, product_type) %>% 
  count(product_type)
s <- dplyr::group_by(NYCviolations, year) %>% 
  count(product_type)
### I have absolutely no idea why I need to consolidate again...
s$product_type[s$product_type == "Hookah tobacco" | 
                                  s$product_type == "Tobacco"] <- "Tobacco"
s <-dplyr::group_by(s, product_type)

### creating rows with value 0 for final visualization consistency
yeardf <- c(2014, 2014, 2014, 2014, 2015, 2016, 2016, 2017, 2017, 2018,
            2018, 2019, 2020, 2021, 2021, 2022, 2022)
product_typedf <- c("Cigar(s)", "E-cigarettes & Accessories", "Smokeless tobacco", "Tobacco",
                    "E-cigarettes & Accessories", "E-cigarettes & Accessories", "Smokeless tobacco",
                    "Tobacco", "Smokeless tobacco", "Tobacco","Smokeless tobacco", "Smokeless tobacco", 
                    "Smokeless tobacco", "Tobacco","Smokeless tobacco", "Tobacco", "Smokeless tobacco")
ndf <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
new_rowsdf <- data.frame(yeardf, product_typedf, ndf)
names(new_rowsdf)[names(new_rowsdf) == "yeardf"] <- "year"
names(new_rowsdf)[names(new_rowsdf) == "product_typedf"] <- "product_type"
names(new_rowsdf)[names(new_rowsdf) == "ndf"] <- "n"
s <- rbind(s, new_rowsdf)
s <-dplyr::group_by(s, year)

### Over time trends for types of sale violations
g <- ggplot(s, aes(fill=product_type, y=n, x=year)) + geom_bar(position="dodge", stat="identity")
print(g)

### May just export to Datawrapp for ease

### Retail Visuals
NYCretailtype <- dplyr::group_by(NYCretail, vendor_type) %>% 
  count(vendor_type)
t <- ggplot(NYCretailtype, aes(x=vendor_type, y=n)) + geom_bar(stat="identity") +coord_flip()
print(t)

### May just export to Datawrapp for ease

### Writing to CSV
write.csv(NYCviolations, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\NYCviolations.csv")
write.csv(NYCviolationtype, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\NYCviolationtype.csv")
write.csv(NYCretail, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\NYCretail.csv")
write.csv(s, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\violationtypeyear.csv")
write.csv(NYCretailtype, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\retailtype.csv")



### Mapping in R
### For this step, I used QGIS (download here: https://www.qgis.org/en/site/forusers/download.html)
### The Shape file for NYC Community Districts was assembled via (https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycd_22a.zip)
### In QGIS, I created two maps, one for the number of violations by community district, and one for the number of retailers by community district
### Brought the data back into R so that it could be normalized for a final figure
QGISviolations <- read_csv("C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\QGIS\\community_districts_by_violation.csv") %>% 
  clean_names()
QGISretail <- read_csv("C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\QGIS\\retail_count_cd.csv") %>% 
  clean_names()

### Appending them together
QGISmerge <- merge(QGISviolations, QGISretail, by="boro_cd") %>% 
  mutate(normalize = numpoints.x/numpoints.y) %>% 
  mutate_all(~replace(., is.na(.), 0))
write.csv(QGISmerge, "C:\\Users\\micha\\Desktop\\R\\Project\\ProjectCode\\QGIS\\QGISmerge.csv")

### Exported to QGIS to complete the figures
### Also used datawrapper.de to create figures
