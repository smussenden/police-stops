# Fact-checking Lindsay Huth's story analysis. 

# Remove all previous objects
rm(list=ls(all=TRUE))

# Load libraries
library(tidyverse)
library(lubridate)
library(stringr)

# data are stored in separate csv files for each year
# import data for each year using tidyverse
d2009 <- read_csv("data/2009.csv", col_names = TRUE)
d2011 <- read_csv("data/2011.csv", col_names = TRUE)
d2012 <- read_csv("data/2012.csv", col_names = TRUE)
d2013 <- read_csv("data/2013.csv", col_names = TRUE)
d2015 <- read_csv("data/2015.csv", col_names = TRUE)

# Glimpse data to review 
glimpse(d2015)
glimpse(d2013)
glimpse(d2012)
glimpse(d2011)
glimpse(d2009)

# A few notes: varying number of columns by year.  2009, 2011 have 10, 2012 has 1 more.  24 in 2013, 27 in 2015.  Date and time of stop only present in later years (doesn't matter for this analysis).  Same with search length.  Common fields in all years appear to be: Agency that did stop, gender, race/ethnicity, stop reason, registration, whether a search was done and reason for search, disposition, outcome, and arrest reason.

# Exploring range of agencies.  Note different styles for departments in different years.

agency_2009 <- d2009 %>%
  group_by(Agency) %>%
  summarise(count= n()) %>%
  arrange(count)

agency_2011 <- d2011 %>%
  group_by(Agency) %>%
  summarise(count= n()) %>%
  arrange(count)

agency_2012 <- d2012 %>%
  group_by(Agency) %>%
  summarise(count= n()) %>%
  arrange(count)

agency_2013 <- d2013 %>%
  group_by(Agency) %>%
  summarise(count= n()) %>%
  arrange(count)

agency_2015 <- d2015 %>%
  group_by(AGENCY) %>%
  summarise(count= n()) %>%
  arrange(count)

# Exploring range of Genders. Generally clean and consistent. 

gender_2009 <- d2009 %>%
  group_by(Gender) %>%
  summarise(count= n()) %>%
  arrange(count)

gender_2011 <- d2011 %>%
  group_by(Gender) %>%
  summarise(count= n()) %>%
  arrange(count)

gender_2012 <- d2012 %>%
  group_by(Gender) %>%
  summarise(count= n()) %>%
  arrange(count)

gender_2013 <- d2013 %>%
  group_by(Gender) %>%
  summarise(count= n()) %>%
  arrange(count)

gender_2015 <- d2015 %>%
  group_by(GENDER) %>%
  summarise(count= n()) %>%
  arrange(count)

# Exploring race
race_2009 <- d2009 %>%
  group_by(Race) %>%
  summarise(count= n()) %>%
  arrange(count)

race_2011 <- d2011 %>%
  group_by(Race) %>%
  summarise(count= n()) %>%
  arrange(count)

race_2012 <- d2012 %>%
  group_by(Race) %>%
  summarise(count= n()) %>%
  arrange(count)

race_2013 <- d2013 %>%
  group_by(Race) %>%
  summarise(count= n()) %>%
  arrange(count)

race_2015 <- d2015 %>%
  group_by(ETHNICITY) %>%
  summarise(count= n()) %>%
  arrange(count)

# Exploring stopreason
stopreason_2009 <- d2009 %>%
  group_by(Stopreason) %>%
  summarise(count= n()) %>%
  arrange(count)

stopreason_2011 <- d2011 %>%
  group_by(Stopreason) %>%
  summarise(count= n()) %>%
  arrange(count)

stopreason_2012 <- d2012 %>%
  group_by(`Stop Reason`) %>%
  summarise(count= n()) %>%
  arrange(count)

stopreason_2013 <- d2013 %>%
  group_by(`Stop Reason (Abbreviated)`) %>%
  summarise(count= n()) %>%
  arrange(count)

stopreason_2015 <- d2015 %>%
  group_by(`STOP REASON (ABBR.)`) %>%
  summarise(count= n()) %>%
  arrange(count)

# examining search 

search_2009 <- d2009 %>%
  group_by(Search) %>%
  summarise(count= n()) %>%
  arrange(count)

search_2011 <- d2011 %>%
  group_by(Search) %>%
  summarise(count= n()) %>%
  arrange(count)

search_2012 <- d2012 %>%
  group_by(Search) %>%
  summarise(count= n()) %>%
  arrange(count)

search_2013 <- d2013 %>%
  group_by(Search) %>%
  summarise(count= n()) %>%
  arrange(count)

search_2015 <- d2015 %>%
  group_by(`SEARCH TYPE`) %>%
  summarise(count= n()) %>%
  arrange(count)

# Examine searchreason

searchreason_2009 <- d2009 %>%
  group_by(Searchreason) %>%
  summarise(count= n()) %>%
  arrange(count)

searchreason_2011 <- d2011 %>%
  group_by(Searchreason) %>%
  summarise(count= n()) %>%
  arrange(count)

searchreason_2012 <- d2012 %>%
  group_by(`Search Reason`) %>%
  summarise(count= n()) %>%
  arrange(count)

searchreason_2013 <- d2013 %>%
  group_by(`Search Reason`) %>%
  summarise(count= n()) %>%
  arrange(count)

searchreason_2015 <- d2015 %>%
  group_by(`SEARCH REASON`) %>%
  summarise(count= n()) %>%
  arrange(count)

# Agencies reported huge jumps in stop numbers between years. The Maryland Transportation Authority reported 150,000 stops one year and just 50,000 the next. SM CHECKED. ADJUST NUMBERS TO MATCH DATA. 

# Find MTA in each year by looking at above queries agency_YYYY
# 2009 = MDTA Police 130530
# 2011 = Maryland Transportation Authority 152395
# 2012 = Maryland Transportation Authority Police 52994
# 2013 = Maryland Transportation Authority Police 52497
# 2015 = MARYLAND TRANSPORTATION AUTHORITY 68836

# Other agencies reported no searches. YEP. 

agency_summary_2009 <- d2009 %>%
  group_by(Agency) %>%
  summarise_all(funs(sum(!is.na(.)))) 

agency_2009 <- d2009 %>%
  group_by(Agency) %>%
  summarise(total= n()) %>%
  arrange(total)

agency_summary_2009_join <- left_join(agency_2009, agency_summary_2009, by="Agency")

agency_summary_2009_join_sum <- agency_summary_2009_join %>%
  group_by(Agency) %>%
  mutate(pct_search = (Search/total)*100)

agency_summary_2011 <- d2011 %>%
  group_by(Agency) %>%
  summarise_all(funs(sum(!is.na(.)))) 

agency_2011 <- d2011 %>%
  group_by(Agency) %>%
  summarise(total= n()) %>%
  arrange(total)

agency_summary_2011_join <- left_join(agency_2011, agency_summary_2011, by="Agency")

agency_summary_2011_join_sum <- agency_summary_2011_join %>%
  group_by(Agency) %>%
  mutate(pct_search = (Search/total)*100)

agency_summary_2012 <- d2012 %>%
  group_by(Agency) %>%
  summarise_all(funs(sum(!is.na(.)))) 

agency_2012 <- d2012 %>%
  group_by(Agency) %>%
  summarise(total= n()) %>%
  arrange(total)

agency_summary_2012_join <- left_join(agency_2012, agency_summary_2012, by="Agency")

agency_summary_2012_join_sum <- agency_summary_2012_join %>%
  group_by(Agency) %>%
  mutate(pct_search = (Search/total)*100)  

agency_summary_2013 <- d2013 %>%
  group_by(Agency) %>%
  summarise_all(funs(sum(!is.na(.)))) 

agency_2013 <- d2013 %>%
  group_by(Agency) %>%
  summarise(total= n()) %>%
  arrange(total)

agency_summary_2013_join <- left_join(agency_2013, agency_summary_2013, by="Agency")

agency_summary_2013_join_sum <- agency_summary_2013_join %>%
  group_by(Agency) %>%
  mutate(pct_search = (Search/total)*100)  

# Or they reported thousands of searches, but not one that turned up contraband. YEP - in 2013, especially bad

#2009
# Get a list of departments with more than 1K searches.
bigagency_2009 <- d2009 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency) %>%
  summarise(count=n()) %>%
  filter(count >= 1000) 

# Get a complete list of all cases by departements with more 1K searches
bigagency_2009 <- left_join(bigagency_2009, d2009, by="Agency")

# Filter only searches
bigagency_2009 <- bigagency_2009 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency, Disposition) %>%
  summarise(count=n()) %>%
  spread(Disposition, count)

#2011
# Get a list of departments with more than 1K searches.
bigagency_2011 <- d2011 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency) %>%
  summarise(count=n()) %>%
  filter(count >= 1000) 

# Get a complete list of all cases by departements with more 1K searches
bigagency_2011 <- left_join(bigagency_2011, d2011, by="Agency")

# Filter only searches
bigagency_2011 <- bigagency_2011 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency, Disposition) %>%
  summarise(count=n()) %>%
  spread(Disposition, count)

#2012
# Get a list of departments with more than 1K searches.
bigagency_2012 <- d2012 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency) %>%
  summarise(count=n()) %>%
  filter(count >= 1000) 

# Get a complete list of all cases by departements with more 1K searches
bigagency_2012 <- left_join(bigagency_2012, d2012, by="Agency")

# Filter only searches
bigagency_2012 <- bigagency_2012 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency, Disposition) %>%
  summarise(count=n()) %>%
  spread(Disposition, count)

#2013

# Get a list of departments with more than 1K searches.
bigagency_2013 <- d2013 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency) %>%
  summarise(count=n()) %>%
  filter(count >= 1000) 

# Get a complete list of all cases by departements with more 1K searches
bigagency_2013 <- left_join(bigagency_2013, d2013, by="Agency")

# Filter only searches
bigagency_2013 <- bigagency_2013 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency, Disposition) %>%
  summarise(count=n()) %>%
  spread(Disposition, count)


contraband_2011 <- d2011 %>%
  filter(!is.na(Search)) %>%
  group_by(Agency, Disposition) %>%
  summarise(count = n())


# The Baltimore Police Department’s records are missing 17 of the 21 details about each stop that are required by law — including where the car was stopped and whether the person was arrested or received a warning, citation or repair order. UNCLEAR



# But the city department’s traffic stop data has large gaps, showing just 4,410 stops in 2009 (YEP) — and more than 35,000 the next year reported (YEP). The total climbed to 50,000, then 80,000 in the following years. YEP AND YEP
baltimore_2009 <- d2009 %>%
  filter(Agency == "Baltimore PD") %>%
  group_by(Agency) %>%
  summarise(count=n())

baltimore_2011 <- d2011 %>%
  filter(Agency == "Baltimore Police Department") %>%
  group_by(Agency) %>%
  summarise(count=n())

baltimore_2012 <- d2012 %>%
  filter(Agency == "Baltimore City Police Department") %>%
  group_by(Agency) %>%
  summarise(count=n())

baltimore_2013 <- d2013 %>%
  filter(Agency == "Baltimore Police Department") %>%
  group_by(Agency) %>%
  summarise(count=n())

baltimore_2015 <- d2015 %>%
  filter(AGENCY == "BALTIMORE POLICE DEPARTMENT") %>%
  group_by(AGENCY) %>%
  summarise(count=n())

#Those records show just 36 searches by the Baltimore police in 2009 (YEP)— and none that turned up drugs, weapons or other contraband. YEP
baltimore_contraband_2009 <- d2009 %>%
  filter(Agency == "Baltimore PD") %>%
  filter(!is.na(Search))

#In fact, in the five years of data analyzed, the data show just 55 searches that found those items.
baltimore_contraband_2009 <- d2009 %>%
  filter(Agency == "Baltimore PD") %>%
  filter(!is.na(Search)) %>%
  filter(!is.na(Disposition)) %>%
  filter(Disposition != "none")

baltimore_contraband_2011 <- d2011 %>%
  filter(Agency == "Baltimore Police Department") %>%
  filter(!is.na(Search)) %>%
  filter(!is.na(Disposition)) %>%
  filter(Disposition != "none")

baltimore_contraband_2012 <- d2012 %>%
  filter(Agency == "Baltimore City Police Department") %>%
  filter(!is.na(Search)) %>%
  filter(!is.na(Disposition)) %>%
  filter(Disposition != "none")

baltimore_contraband_2013 <- d2013 %>%
  filter(Agency == "Baltimore Police Department") %>%
  filter(!is.na(Search)) %>%
  filter(!is.na(Disposition)) %>%
  filter(Disposition != "none")

baltimore_contraband_2015 <- d2015 %>%
  filter(AGENCY == "BALTIMORE POLICE DEPARTMENT") %>%
  filter(!is.na(`SEARCH DISPOSITION`)) %>%
  filter(`SEARCH DISPOSITION` != "NONE")

balt2015 <- d2015 %>%
  filter(AGENCY == "BALTIMORE POLICE DEPARTMENT")

# subset stops made by baltimore police department each year
balt2009 <- subset(d2009, Agency == "Baltimore PD")
balt2011 <- subset(d2011, Agency == "Baltimore Police Department")
balt2012 <- subset(d2012, Agency == "Baltimore City Police Department")
balt2013 <- subset(d2013, Agency == "Baltimore Police Department")
balt2015 <- subset(d2015, AGENCY == "BALTIMORE POLICE DEPARTMENT")

# count number of stops each year, put in data frame
baltstops <- matrix(c("2009", nrow(balt2009),
                      "2011", nrow(balt2011),
                      "2012", nrow(balt2012),
                      "2013", nrow(balt2013),
                      "2015", nrow(balt2015)),
                    ncol=2, byrow=TRUE)
colnames(baltstops) <- c("Year", "Stops")

# create new T/F column for searches
balt2009$search_conducted <- ifelse(balt2009$Search %in% c("both","pers","prop"), "TRUE", "FALSE")
balt2011$search_conducted <- ifelse(balt2011$Search %in% c("both","pers","prop"), "TRUE", "FALSE")
balt2012$search_conducted <- ifelse(balt2012$Search %in% c("both","pers","prop"), "TRUE", "FALSE")
balt2013$search_conducted <- ifelse(balt2013$Search %in% c("both","pers","prop"), "TRUE", "FALSE")
balt2015$search_conducted <- ifelse(balt2015$SEARCH.TYPE %in% c("BOTH","PERS","PROP"), "TRUE", "FALSE")

# count search rate and number of searches each year, put in data frame
baltsearches <- matrix(c("2009", sum(as.logical(balt2009$search_conducted))/nrow(balt2009), sum(as.logical(balt2009$search_conducted)),
                         "2011", sum(as.logical(balt2011$search_conducted))/nrow(balt2011), sum(as.logical(balt2011$search_conducted)),
                         "2012", sum(as.logical(balt2012$search_conducted))/nrow(balt2012), sum(as.logical(balt2012$search_conducted)),
                         "2013", sum(as.logical(balt2013$search_conducted))/nrow(balt2013), sum(as.logical(balt2013$search_conducted)),
                         "2015", sum(as.logical(balt2015$search_conducted))/nrow(balt2015), sum(as.logical(balt2015$search_conducted))),
                       ncol=3, byrow=TRUE)
colnames(baltsearches) <- c("Year", "Search Rate", "Searches")

# create new T/F column for hits
balt2009$hit <- ifelse(balt2009$Disposition %in% c("both","contra","prop", "property"), "TRUE", "FALSE")
balt2011$hit <- ifelse(balt2011$Disposition %in% c("both","contra","prop"), "TRUE", "FALSE")
balt2012$hit <- ifelse(balt2012$Disposition %in% c("both","contra","prop"), "TRUE", "FALSE")
balt2013$hit <- ifelse(balt2013$Disposition %in% c("both","contra","prop"), "TRUE", "FALSE")
balt2015$hit <- ifelse(balt2015$SEARCH.DISPOSITION %in% c("BOTH","CONTRA","PROP"), "TRUE", "FALSE")

# count hit rate and number of hits each year, put in data frame
balthits <- matrix(c("2009", sum(as.logical(balt2009$hit))/sum(as.logical(balt2009$search_conducted)), sum(as.logical(balt2009$hit)),
                     "2011", sum(as.logical(balt2011$hit))/sum(as.logical(balt2011$search_conducted)), sum(as.logical(balt2011$hit)),
                     "2012", sum(as.logical(balt2012$hit))/sum(as.logical(balt2012$search_conducted)), sum(as.logical(balt2012$hit)),
                     "2013", sum(as.logical(balt2013$hit))/sum(as.logical(balt2013$search_conducted)), sum(as.logical(balt2013$hit)),
                     "2015", sum(as.logical(balt2015$hit))/sum(as.logical(balt2015$search_conducted)), sum(as.logical(balt2015$hit))),
                   ncol=3, byrow=TRUE)
colnames(balthits) <- c("Year", "Hit Rate", "Hits")

