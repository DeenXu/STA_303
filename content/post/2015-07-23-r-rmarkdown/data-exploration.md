---
title: "STA 303 Data Exploration"
author: "Deen Xu"
date: 2021-02-14

output:
  pdf_document: default
urlcolor: blue
header-includes:    
  - \usepackage{lastpage}
  - \usepackage{fancyhdr}
  - \pagestyle{fancy}
  - \fancyhead[CO, CE]{Deen Xu, 1003912769}
  - \fancyfoot[CO, CE]{\thepage \ of \pageref{LastPage}}
  
---




```{r setup, message = FALSE, echo=FALSE}
# Students: You probably shouldn't change any of the code in this chunk.

# These are the packages you will need for this activity
packages_needed <- c("tidyverse", "googledrive", "readxl", "janitor", 
                     "lubridate", "opendatatoronto", "ggthemes")

package.check <- lapply(
  packages_needed,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

# Credit: package.check based on a helpful post from Vikram Baliga https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

# Load tidyverse
library(tidyverse)
library(readxl)
library(janitor)
library(opendatatoronto)
library(ggthemes)

# Set so that long lines in R will be wrapped:
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=80), echo = FALSE)
```


```{r getdata, eval = FALSE, echo=FALSE}
# Students: You probably shouldn't change any of the code in this chunk BUT...

# This chunk loads the most recent data from Toronto City and the data from OpenToronto.

# You have to RUN this chunk by hand to update the data as 
#   eval is set to FALSE to limit unnecessary requsts on the site.

###################################################
# Step one: Get the COVID data from Toronto City. #
###################################################

googledrive::drive_deauth()

#url1 <- "https://drive.google.com/file/d/11KF1DuN5tntugNc10ogQDzFnW05ruzLH/view"
#googledrive::drive_download(url1, #path="data/CityofToronto_COVID-19_Daily_Public_Reporting.xlsx", overwrite = TRUE)

url2 <- "https://drive.google.com/file/d/1jzH64LvFQ-UsDibXO0MOtvjbL2CvnV3N/view"
googledrive::drive_download(url2, path = "data/CityofToronto_COVID-19_NeighbourhoodData.xlsx", overwrite = TRUE)

# this removes the url object that we don't need anymore
rm( url2)

#####################################################################
# Step two: Get the data neighbourhood data from Open Data Toronto. #
#####################################################################

nbhoods_shape_raw <- list_package_resources("neighbourhoods") %>% 
  get_resource()

saveRDS(nbhoods_shape_raw, "data/neighbourhood_shapefile.Rds")

nbhood_profile <- search_packages("Neighbourhood Profile") %>%
  list_package_resources() %>% 
  filter(name == "neighbourhood-profiles-2016-csv") %>% 
  get_resource()

saveRDS(nbhood_profile, "data/neighbourhood_profile.Rds")
```


```{r load_data, echo=FALSE}
######################################################
# Step three: Load the COVID data from Toronto City. #
######################################################

# Saving the name of the file as an object and then using the object name in the
# following code is a helpful practice. Why? If we change the name of the file 
# being used, we'll only have to change it in one place. This helps us avoid 
# 'human error'.

daily_data <- "data/CityofToronto_COVID-19_Daily_Public_Reporting.xlsx"

# Cases reported by date
reported_raw <- read_excel(daily_data, sheet = 5) %>% 
  clean_names()

# Cases by outbreak type
outbreak_raw <- read_excel(daily_data, sheet = 3) %>% 
  clean_names()

# When was this data updated?
date_daily <- read_excel(daily_data, sheet = 1) %>% 
  clean_names()

# By neighbourhood
neighbourood_data <- "data/CityofToronto_COVID-19_NeighbourhoodData.xlsx"

# Cases reported by date
nbhood_raw <- read_excel(neighbourood_data, sheet = 2) %>% 
  clean_names()

# Date the neighbourhood data was last updated
date_nbhood <- read_excel(neighbourood_data, sheet = 1) %>% 
  clean_names()

#don't need these anymore
rm(daily_data, neighbourood_data)

#############################################################
# Step four: Load the neighbourhood data from Toronto City. #
#############################################################

# Get neighbourhood profile data
nbhood_profile <- readRDS("data/neighbourhood_profile.Rds")

# Get shape data for mapping 
nbhoods_shape_raw <- readRDS("data/neighbourhood_shapefile.Rds") %>% 
  sf::st_as_sf() ## Makes sure shape info is in the most up to date format

```

Code last run `r Sys.Date()`.  
Daily: `r date_daily[1,1]`.   
Neighbourhood: `r date_nbhood[1,1]`. 

# Task 1: Daily cases
## Data wrangling

```{r cases_dw, echo=TRUE}
reported <- reported_raw %>% 
  mutate_if(is.numeric, replace_na, replace = 0)

reported$reported_date = date(reported$reported_date)

reported <- reported %>%
  rename_if(is.numeric, str_to_title)

reported <- reported %>% gather(type, count, c(Active, Recovered, Deceased)) 

reported$type <- factor(reported$type, levels = c("Active", "Recovered", "Deceased"))
```

\newpage
## Data visualization

```{r cases_vis, echo=TRUE}
reported %>% 
  ggplot(aes(reported_date,count ,fill = type)) +
  geom_bar(stat='identity') + 
  scale_fill_manual(values=c( "#003F5C","#86BCB6", "#B9CA5D")) + 
  ggtitle("Cases reported by day in Toronto, Canada", "Confirmed and probable cases") + 
  labs(x = "Date", y = "Case count")  +
  scale_x_date(date_labels = "%d %b %Y", limits = as.Date(c('2020-01-01',Sys.Date()))) + 
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = c(.15,.8)) +
  ylim(0, 2000) + 
  labs(caption = str_c("Created by: Deen Xu for STA303/1002, U of T\nSource: Ontario Ministry of Health, Integrated Public Health Information System and CORES\n", date_daily[1,1]))
```

\newpage
# Task 2: Outbreak type
## Data wrangling


```{r outbreak_dw, echo=TRUE}
outbreak <- outbreak_raw

outbreak$episode_week = date(outbreak$episode_week)

outbreak <- outbreak %>% 
  mutate(outbreak_or_sporadic = ifelse(outbreak_or_sporadic == 'Sporadic', 'Sporadic','Outbreak associated'))

outbreak$outbreak_or_sporadic <- 
  factor(outbreak$outbreak_or_sporadic, levels = c("Sporadic", 'Outbreak associated'))

outbreak <- outbreak %>% group_by(episode_week) %>%
  mutate(total_cases = sum(cases)) %>% ungroup()
```

\newpage
## Data visualization

```{r outbreak_vis, echo=TRUE}
outbreak %>% ggplot(aes(episode_week, cases, fill = outbreak_or_sporadic)) +
  geom_bar(stat='identity') + 
  scale_fill_manual(values=c("#86BCB6", "#B9CA5D")) + 
  ggtitle("Cases by outbreak type and week in Toronto, Canada", "Confirmed and probable cases") + 
  labs(x = "Date", y = "Case count")  +
  scale_x_date(date_labels = "%d %b %Y", limits = as.Date(c('2020-01-01',Sys.Date() + 7))) + 
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = c(.15,.8)) +
  ylim(0, max(outbreak$total_cases)) + 
  labs(caption = str_c("Created by: Deen Xu for STA303/1002, U of T\nSource: Ontario Ministry of Health, Integrated Public Health Information System and CORES\n", date_daily[1,1])) 
```

\newpage
# Task 3: Neighbourhoods
## Data wrangling: part 1

```{r nbhood_dw_1, echo=TRUE}
income <- nbhood_profile %>% filter(`City of Toronto` == '17.8')

#income[,c(6:ncol(income))] = income[,c(6:ncol(income))] %>% lapply(function(x) as.numeric(as.character(x)))

income <- as.data.frame(t(income)) %>% rename(percent = V1)%>%filter(!is.numeric(percent))
income <- tibble::rownames_to_column(income, "neighbourhood_name") %>% 
  filter(percent != '1143' & 
           percent!= 'Income' & 
           percent!= 'Low income in 2015' & 
           percent!= 'Census Profile 98-316-X2016001' & 
           neighbourhood_name!= 'Characteristic')

income$percent <- as.numeric(income$percent)
```

## Data wrangling: part 2

```{r nbhood_dw_2, echo=TRUE}
nbhoods_all <- nbhoods_shape_raw %>% rowwise() %>% mutate(neighbourhood_name = unlist(strsplit(AREA_NAME, ' \\('))[1])

nbhoods_all <- nbhoods_all %>% 
  mutate(neighbourhood_name = str_replace(neighbourhood_name, 'North St.James Town', 'North St. James Town')) %>%
  mutate(neighbourhood_name = str_replace(neighbourhood_name,'Cabbagetown-South St.James Town', 'Cabbagetown-South St. James Town')) %>%
  mutate(neighbourhood_name = str_replace(neighbourhood_name,'Weston-Pellam Park', 'Weston-Pelham Park')) %>%
  mutate(neighbourhood_name = str_replace(neighbourhood_name,'Mimico', 'Mimico (includes Humber Bay Shores)'))

nbhoods_all <- left_join(nbhoods_all, income, by = "neighbourhood_name")

nbhoods_all <- left_join(nbhoods_all, nbhood_raw, by = "neighbourhood_name")

nbhoods_all <- nbhoods_all %>% rename(rate_per_100000_people = rate_per_100_000_people)
```

## Data wrangling: part 3

```{r nbhood_dw_3, echo=TRUE}
med_rate = median(nbhoods_all$rate_per_100000_people)


med_inc = median(nbhoods_all$percent)

nbhoods_final <- nbhoods_all %>% mutate(med_inc = med_inc, med_rate = med_rate)

nbhoods_final <- nbhoods_final %>% mutate(nbhood_type = ifelse(percent >= med_inc & rate_per_100000_people >= med_rate, "Higher low income rate, higher case rate", ifelse(percent >= med_inc & rate_per_100000_people < med_rate, "Higher low income rate, lower case rate", ifelse(percent < med_inc & rate_per_100000_people >= med_rate, "Lower low income rate, higher case rate", ifelse(percent < med_inc & rate_per_100000_people < med_rate, "Lower low income rate, lower case rate", 0)))))
```

\newpage
## Data visualization

```{r neighbourhood_graphs_1, fig.height=4, echo=TRUE, message=FALSE}
ggplot(data = nbhoods_final) + 
  geom_sf(aes(geometry = geometry,fill = percent)) + 
  theme_map() + scale_fill_gradient(name = "% low income", low = "darkgreen", high = "lightgrey") + 
  theme_minimal() +coord_sf(datum = NA) + 
  ggtitle("Percentage of 18 to 64 year olds living in a low income family (2015)", "Neighbourhoods of Toronto, Canada") + 
  labs(caption = str_c("Created by: Deen Xu for STA303/1002, U of T\nSource: Census Profile 98-316-X2016001 via OpenData Toronto\n", date_daily[1,1]))
```

\newpage

```{r neighbourhood_graphs_2, fig.height=4, echo=TRUE, message=FALSE}
ggplot(data = nbhoods_final) + 
  geom_sf(aes(geometry = geometry,fill = rate_per_100000_people)) + 
  theme_map() + scale_fill_gradient(name = "Cases per 100,000 people", low = "white", high = 'darkorange') + 
  theme_minimal() +coord_sf(datum = NA) + 
  ggtitle("COVID-19 cases per 100,000, by neighbourhood in Toronto, Canada") + 
  labs(caption = str_c("Created by: Deen Xu for STA303/1002, U of T\nSource: Ontario Ministry of Health, Integrated Public Health Information System and CORES\n", date_daily[1,1]))
```

\newpage

```{r neighbourhood_graphs_3, fig.height=4, echo=TRUE, message=FALSE}
ggplot(data = nbhoods_final) + 
  geom_sf(aes(geometry = geometry,fill = nbhood_type)) + 
  theme_map()  + 
  theme_minimal() +coord_sf(datum = NA) + 
  ggtitle("COVID-19 cases per 100,000, by neighbourhood in Toronto, Canada") + 
  labs(caption = str_c("Created by: Deen Xu for STA303/1002, U of T\nIncome data source: Census Profile 98-316-X2016001 via OpenData Toronto\nCOVID data source: Ontario Ministry of Health, Integrated Public\nHealth Information System and CORES\n", date_daily[1,1])) +
  scale_fill_brewer(palette = 'Set1') +
  labs(fill = "% of 18 to 64 year-olds in
low income families and
COVID-19 case rates")
```


```{r, eval = FALSE}
# This chunk of code helps you prepare your assessment for submission on Crowdmark
# This is optional. If it isn't working, you can do it manually/take another approach.

# Run this chunk by hand after knitting your final version of your pdf for submission.
# A new file called 'to_submit' will appear in your working directory with each page of your assignment as a separate pdf.

# Install the required packages
if(!match("staplr", installed.packages()[,1], nomatch = FALSE))
  {install.packages("staplr")}

# Don't edit anything in this function
prep_for_crowdmark <- function(pdf=NULL){
  # Get the name of the file you're currently in. 
  this_file <- rstudioapi::getSourceEditorContext()$path
  pdf_name <- sub(".Rmd", ".pdf", sub('.*/', '', this_file))
  
  # Create a file called to_submit to put the individual files in
  # This will be in the same folder as this file is saved
  if(!match("to_submit", list.files(), nomatch = FALSE))
    {dir.create("to_submit")}
 
  # Split the files
  if(is.null(pdf)){
  staplr::split_pdf(pdf_name, output_directory = "to_submit", prefix = "page_")} else {
    staplr::split_pdf(pdf, output_directory = "to_submit", prefix = "page_") 
  }
}

prep_for_crowdmark()

```
