library(dplyr)
library(tidyr)
library(readxl)
library(readr)

 setwd("C:/Users/illus/Documents/GitHub/324-Indiv-Project/")

# Data Compilation

# Obtained from Kaggle: https://www.kaggle.com/prasertk/netflix-subscription-price-in-different-countries
feeinven <- read.csv("Base Datasets/Netflix subscription fee Dec-2021.csv")

# Obtained from World Bank: https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
gdp <- read.csv("Base Datasets/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3628616.csv", header = FALSE)

# Obtained from Harvard Dataverse: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LM4OWF
income <- read.csv("Base Datasets/swiid9_2_summary.csv")

# Obtained from IMDB: https://www.imdb.com/interfaces/
IMDB <- read_tsv("Base Datasets/data.tsv")

# Obtained from Netflix: https://top10.netflix.com/united-states
top <- read_excel("Base Datasets/all-weeks-countries.xlsx")

# Obtained from Comparitech: https://www.comparitech.com/tv-streaming/netflix-subscribers/

revsub <- read.csv("Base Datasets/Netflix Subscribers by Country - Update July 2021 - Subsciber Figures by Country.csv")

# Obtained from Kaggle: https://www.kaggle.com/andradaolteanu/iso-country-codes-global

countrycode <- read.csv("Base Datasets/wikipedia-iso-country-codes.csv")

# Create complete

# create the difference column for stacked bar graph

feeinven$basic_standard_diff = (feeinven$Cost.Per.Month...Standard.... - feeinven$Cost.Per.Month...Basic....)

feeinven$standard_premium_diff = (feeinven$Cost.Per.Month...Premium.... - feeinven$Cost.Per.Month...Standard....)

# combine with gdp dataset

names(gdp)[names(gdp) == 'V1'] <- 'Country'

feeinvengdp <- merge(feeinven, gdp, by="Country")

# obtain 2019's GDP in complete dataset

feeinvengdp2020 <- feeinvengdp[-c(11:72, 74, 75)] 

names(feeinvengdp2020)[names(feeinvengdp2020) == 'V64'] <- "2019 GDP (World Bank)"

# clean income disparity dataset

income <- income[, c(1:3)]

incomeyear <- income %>% group_by(country) %>% summarise(max = max(year, na.rm=TRUE))

# combine with complete dataset

income <- merge(income, incomeyear, by.x = c("country", "year"), by.y = c("country", "max"))

feeinvengdpincome2020 <- merge(feeinvengdp2020, income, by.x=c("Country"), by.y=c("country"))

# clean and combine revenue, subscription dataset with complete dataset

revsub <- revsub[,c(1, 23,24)]

complete <- merge(feeinvengdpincome2020, revsub, by=c("Country"))

# merge countrycode for choropleth map

countrycode <- countrycode[,c(1, 3)]

complete <- merge(complete, countrycode, by.x=c("Country"), by.y=c("English.short.name.lower.case"))

# export complete dataset

write.csv(complete, "Cleaned Datasets/Netflix subscription fee Dec-2021 with GDP and Income Dis 2019.csv", row.names = FALSE)

# Create top-genre dataset

# clean IMDB dataset

genre <- IMDB[,-c(1, 4:8)]

names(genre)[names(genre) == 'primaryTitle'] <- 'show_title'

# place genre into top 10 dataset

topgenre <- merge(top, genre, by = "show_title")

# clean topgenre so there's only 1 entry for each top 10 entry

topgenre <- topgenre[(topgenre$category == "Films" & topgenre$titleType == "movie") | (topgenre$category == "TV" & topgenre$titleType == "tvSeries"), ] 

topgenre <- distinct(topgenre, show_title, week, country_name, category, titleType,cumulative_weeks_in_top_10, .keep_all= TRUE)

# clean for relevant information

topgenrecountries <- topgenre[,-c(1, 3:9)]

#pivot longer

topgenrecountries <- separate(topgenrecountries, c("genres") , c("genre1", "genre2", "genre3"), sep = ",")

topgenrecountries <- pivot_longer(topgenrecountries, c("genre1", "genre2", "genre3"), names_to = "genre123", values_to = "genres")

# count number of genre

genrecount <- count(topgenrecountries, country_name, genres)

genrecount <- na.omit(genrecount)

genrecount <-subset(genrecount, genres!="\\N")

genrecount$n <- as.numeric(genrecount$n)

# export genre count

write.csv(genrecount, "Cleaned Datasets/Popular Genres in Different Countries Netflix.csv", row.names = FALSE)

# setup for Treemap

# take genrecount and format it for treemap graph
# (named sunburst because originally were to be used for sunburst graph)

sunburst <- rename(genrecount, label=country_name)

# remove - for formatting

sunburst$genres = sub("-", " ", sunburst$genres)

# add pathnames for formatting

sunburst$parent = c("total  - ")

sunburst$parent <- paste(sunburst$parent, sunburst$genres)

sunburst$id = c(" - ")

sunburst$id <- paste(sunburst$parent, sunburst$id)

sunburst$id <- paste(sunburst$id, sunburst$label)

sunburst$n <- as.numeric(sunburst$n)

# add all genres for aggregate

added <- aggregate(sunburst$n, list(sunburst$genres), FUN=sum)

added <- rename(added, label = Group.1)

added <- rename(added, n = x)

added$n <- as.numeric(added$n)

added$genres <- c(NA)

added$parent <- c("total")

added$id <- c(" - ")

added$id <- paste(added$parent, added$id)

added$id <- paste(added$id, added$label)

# add everything for total

total = sum(added$n)

# combine everything for treemap ready dataset

sunburst <- rbind(added, sunburst)

sunburst <- rbind(c("total", total, NA, NA, "total"), sunburst)

sunburst <- sunburst[,-c(3)]

sunburst$n <- as.numeric(sunburst$n)

# export treemap dataset

write.csv(sunburst, "Cleaned Datasets/Sunburst Genre.csv", row.names = FALSE)

#Treemap but top 10 in an attempt to make things more manageable

top10sunburst <- sunburst[-c(1:28),]

top10sunburst$n <- as.numeric(top10sunburst$n)

# get only top 10 of each country's genre

top10sunburst <- top10sunburst %>% 
  group_by(label) %>%
  top_n(10,n)

# recalculate totals

top10add <- aggregate(top10sunburst$n, list(top10sunburst$parent), FUN=sum)

top10add <- rename(top10add, id = Group.1)

top10add <- rename(top10add, n = x)

top10add$label = sub("total  -  ", "", top10add$id)

top10add$parent = c("total")

top10add$n <- as.numeric(top10add$n)

total = sum(top10add$n)

top10sunburst <- rbind(top10add, top10sunburst)

top10sunburst <- rbind(c("total", total, NA, NA, "total"), top10sunburst)

top10sunburst$n <- as.numeric(top10sunburst$n)

# export

write.csv(top10sunburst, "Cleaned Datasets/Short Sunburst.csv", row.names = FALSE)

#Treemap without Total

nototal <- sunburst[-c(1),]

nototal$parent = sub("total  -  ", "", nototal$parent)

nototal$parent = sub("total", NA, nototal$parent)

nototal$id = sub("total  -  ", "", nototal$id)

# remove total number for different formatting

write.csv(nototal, "Cleaned Datasets/No Center Sunburst.csv", row.names = FALSE)

#Treemap Countries 

# rewriting paths to fit country focused treemaps

countrytree <- nototal[-c(1:28),]

countrytree <- rename(countrytree, parents = label)

countrytree <- rename(countrytree, labels = parent)

countrytree$id = c(" - ")

countrytree$id <- paste(countrytree$parent, countrytree$id)

countrytree$id <- paste(countrytree$id, countrytree$label)

countries <- aggregate(countrytree$n, list(countrytree$parents), FUN = sum)

countries <- rename(countries, labels = Group.1)

countries <- rename(countries, n = x)

countries$n <- as.numeric(countries$n)

countries$id <- countries$label

countries$parents <- c(NA)

countrytree <- rbind(countrytree, countries)

# export

write.csv(countrytree, "Cleaned Datasets/Country Tree Map.csv", row.names = FALSE)

