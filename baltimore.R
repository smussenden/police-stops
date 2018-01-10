# separating and analyzing stops made by the baltimore police department

# data are stored in separate csv files for each year
# import data for each year
d2009 <- read.csv(file.choose(), header=TRUE)
d2011 <- read.csv(file.choose(), header=TRUE)
d2012 <- read.csv(file.choose(), header=TRUE)
d2013 <- read.csv(file.choose(), header=TRUE)
d2015 <- read.csv(file.choose(), header=TRUE)

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

