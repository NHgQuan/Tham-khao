#if you haven't installed "pacman" packages, please delete the character "#" right below
#install.packages("pacman")
#library("pacman")
pacman::p_load(
  rio,
  here,
  tibble
)
covid.data <- import(here("R", "owid-covid-data.csv"))
name.country <- c("Canada", "Greenland", "United States")
#############################################################
# 1)number of days without report

# NEW CASES#
# manipulative function
ndwithoutrpcases <- function(name) {
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)
  ndays <- 0
  for (i in 1:nrow(covid.data.location)) {
    if (is.na(covid.data.location$new_cases[i])|covid.data.location$new_cases[i]==0) {
      ndays <- ndays + 1
    }
  }
  ndays
}
# Canada
a<-ndwithoutrpcases(name.country[1])

# Greenland
b<-ndwithoutrpcases(name.country[2])

# United States
c<-ndwithoutrpcases(name.country[3])

# NEW DEATHS#
# manipulative function
ndwithoutrpdeaths <- function(name) {
  select <- covid.data$location == name
  ndays <- 0
  covid.data.location <- subset(covid.data, subset = select)
  for (i in 1:nrow(covid.data.location)) {
    if (is.na(covid.data.location$new_deaths[i])|covid.data.location$new_deaths[i]==0) {
      ndays <- ndays + 1
    }
  }
  ndays
}
# Canada
d<-ndwithoutrpdeaths(name.country[1])

# Greenland
e<-ndwithoutrpdeaths(name.country[2])

# United States
f<-ndwithoutrpdeaths(name.country[3])
cau1 <- tibble::tribble(
  ~Country,       ~New_deaths, ~New_cases,
  name.country[1], a, d,
  name.country[2], b, e,
  name.country[3], c, f
)
#############################################################
# 2)number of lowest days at that time

# NEW CASES#
# manipulative function
nlowestdayscases <- function(name) {
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)
  ndays <- 0
  min <- 0
  for (i in 1:nrow(covid.data.location))
  {
    if (!is.na(covid.data.location$new_cases[i])) 
      if(covid.data.location$new_cases[i]!=0){
      min <- covid.data.location$new_cases[i]
      break
    }
  }
  for (i in 1:nrow(covid.data.location))
  {
    if (is.na(covid.data.location$new_cases[i])) {
      next
    }
    if(covid.data.location$new_cases[i]!=0) {
      next
    }
    if (min > covid.data.location$new_cases[i]) {
      min <- covid.data.location$new_cases[i]
      ndays <- ndays + 1
    }
  }
  ndays
}
# Canada
a<-nlowestdayscases(name.country[1])
# Greenland
b<-nlowestdayscases(name.country[2])
# United States
c<-nlowestdayscases(name.country[3])

# NEW DEATHS#
# manipulative function
nlowestdaysdeaths <- function(name) {
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)
  ndays <- 0
  min <- 0
  for (i in 1:nrow(covid.data.location))
  {
    if (!is.na(covid.data.location$new_deaths[i])) 
      if(covid.data.location$new_deaths[i]!=0){
      min <- covid.data.location$new_deaths[i]
      break
    }
  }
  for (i in 1:nrow(covid.data.location))
  {
    if (is.na(covid.data.location$new_deaths[i])) {
      next
    }
    if(covid.data.location$new_deaths[i]!=0) {
      next
    }
    if (min > covid.data.location$new_deaths[i]) {
      min <- covid.data.location$new_deaths[i]
      ndays <- ndays + 1
    }
  }
  ndays
}
# Canada
d<-nlowestdaysdeaths(name.country[1])
# Greenland
e<-nlowestdaysdeaths(name.country[2])
# United States
f<-nlowestdaysdeaths(name.country[3])
cau2 <- tibble::tribble(
  ~Country,       ~New_deaths, ~New_cases,
  name.country[1], a, d,
  name.country[2], b, e,
  name.country[3], c, f
)
#############################################################
# 3)number of highest days at that time

# NEW CASES#
# manipulative function
nhighestdayscases <- function(name) {
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)
  ndays <- 0
  max <- 0
  for (i in 1:nrow(covid.data.location))
  {
    if (!is.na(covid.data.location$new_cases[i])) 
      if(covid.data.location$new_cases[i]!=0){
      max <- covid.data.location$new_cases[i]
      break
    }
  }
  for (i in 1:nrow(covid.data.location))
  {
    if (is.na(covid.data.location$new_cases[i])) {
      next
    }
    if(covid.data.location$new_cases[i]!=0){
      next
    }
    if (max < covid.data.location$new_cases[i]) {
      max <- covid.data.location$new_cases[i]
      ndays <- ndays + 1
    }
  }
  ndays
}
# Canada
a<-nhighestdayscases(name.country[1])
# Greenland
b<-nhighestdayscases(name.country[2])
# United States
c<-nhighestdayscases(name.country[3])

# NEW DEATH#
# manipulative function
nhighestdaysdeaths <- function(name) {
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)
  ndays <- 0
  max <- 0
  for (i in 1:nrow(covid.data.location))
  {
    if (!is.na(covid.data.location$new_deaths[i])) 
      if(covid.data.location$new_deaths[i]!=0){
      max <- covid.data.location$new_deaths[i]
      break
    }
  }
  for (i in 1:nrow(covid.data.location))
  {
    if (is.na(covid.data.location$new_deaths[i])) {
      next
    }
    if(covid.data.location$new_deaths[i]!=0){
      next
    }
    if (max < covid.data.location$new_deaths[i]) {
      max <- covid.data.location$new_deaths[i]
      ndays <- ndays + 1
    }
  }
  ndays
}
# Canada
d<-nhighestdaysdeaths(name.country[1])
# Greenland
e<-nhighestdaysdeaths(name.country[2])
# United States
f<-nhighestdaysdeaths(name.country[3])
cau3 <- tibble::tribble(
  ~Country,       ~New_deaths, ~New_cases,
  name.country[1], a, d,
  name.country[2], b, e,
  name.country[3], c, f
)
#############################################################
# 4)Tables

# NOT NEW REPORT#
covid.data.NA <- subset(covid.data, subset = (covid.data$location == name.country[1] | covid.data$location == name.country[2] | covid.data$location == name.country[3]) &
  (is.na(covid.data$new_cases) | is.na(covid.data$new_deaths)|covid.data$new_cases==0|covid.data$new_deaths==0))
covid.data.NA <- data.frame(covid.data.NA$location, covid.data.NA$new_cases, covid.data.NA$new_deaths)
names(covid.data.NA) <- c("Countries", "Infections", "Deaths")
cau4a<-covid.data.NA
# NEW REPORT#
covid.data.newreport <- subset(covid.data, subset = (covid.data$location == name.country[1] | covid.data$location == name.country[2] | covid.data$location == name.country[3]) &
  (!(is.na(covid.data$new_cases) | is.na(covid.data$new_deaths)|covid.data$new_cases==0|covid.data$new_deaths==0)))
covid.data.newreport <- data.frame(covid.data.newreport$location, covid.data.newreport$new_cases, covid.data.newreport$new_deaths)
names(covid.data.newreport) <- c("Countries", "Infections", "Deaths")
cau4b<-covid.data.newreport
#############################################################
# 5)the Shortest number of consecutive days with no data report

# NEW CASES#
# manipulative function
short.ds.no.report.cases <- function(name) {
  # select country
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)

  # set default
  inmissing <- FALSE
  length.of.missing <- nrow(covid.data.location)
  temp.length <- 0

  # check if don't have any missing value
  if (!ndwithoutrpcases(name)) length.of.missing <- 0

  # find shortest days
  for (i in 1:nrow(covid.data.location))
  {
    # check new missing string is shorter
    if (!is.na(covid.data.location$new_cases[i]) & inmissing) {
      inmissing <- FALSE
      if (length.of.missing > temp.length) {
        length.of.missing <- temp.length
      }
      temp.length <- 0
    }
    # set length of new missing string
    if (is.na(covid.data.location$new_cases[i])) {
      inmissing <- TRUE
      temp.length <- temp.length + 1
    }
  }
  length.of.missing
}
# Canada
a<-short.ds.no.report.cases(name.country[1])
# Greenland
b<-short.ds.no.report.cases(name.country[2])
# United States
c<-short.ds.no.report.cases(name.country[3])

# NEW DEATHS#
# manipulative function
short.ds.no.report.deaths <- function(name) {
  # select country
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)

  # set default
  inmissing <- FALSE
  length.of.missing <- nrow(covid.data.location)
  temp.length <- 0

  # check if don't have any missing value
  if (!ndwithoutrpdeaths(name)) length.of.missing <- 0

  # find shortest days
  for (i in 1:nrow(covid.data.location))
  {
    # check new missing string is shorter
    if (!is.na(covid.data.location$new_deaths[i]) & inmissing) {
      inmissing <- FALSE
      if (length.of.missing > temp.length) {
        length.of.missing <- temp.length
      }
      temp.length <- 0
    }
    # set length of new missing string
    if (is.na(covid.data.location$new_deaths[i])) {
      inmissing <- TRUE
      temp.length <- temp.length + 1
    }
  }
  length.of.missing
}
# Canada
d<-short.ds.no.report.deaths(name.country[1])
# Greenland
e<-short.ds.no.report.deaths(name.country[2])
# United States
f<-short.ds.no.report.deaths(name.country[3])
cau5 <- tibble::tribble(
  ~Country,       ~New_deaths, ~New_cases,
  name.country[1], a, d,
  name.country[2], b, e,
  name.country[3], c, f
)
#############################################################
# 6)the longest number of consecutive days with no data report

# NEW CASES#
# manipulative function
longest.ds.no.report.cases <- function(name) {
  # select country
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)

  # set default
  inmissing <- FALSE
  length.of.missing <- 0
  temp.length <- 0

  # find longest days
  for (i in 1:nrow(covid.data.location))
  {
    # check new missing string is longer
    if (!is.na(covid.data.location$new_cases[i]) & inmissing) {
      inmissing <- FALSE
      if (length.of.missing < temp.length) {
        length.of.missing <- temp.length
      }
      temp.length <- 0
    }
    # set length of new missing string
    if (is.na(covid.data.location$new_cases[i])) {
      inmissing <- TRUE
      temp.length <- temp.length + 1
    }
  }
  length.of.missing
}
# Canada
a<-longest.ds.no.report.cases(name.country[1])
# Greenland
b<-longest.ds.no.report.cases(name.country[2])
# United States
c<-longest.ds.no.report.cases(name.country[3])

# NEW DEATHS#
# manipulative function
longest.ds.no.report.deaths <- function(name) {
  # select country
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)

  # set default
  inmissing <- FALSE
  length.of.missing <- 0
  temp.length <- 0


  # find shortest days
  for (i in 1:nrow(covid.data.location))
  {
    # check new missing string is shorter
    if (!is.na(covid.data.location$new_deaths[i]) & inmissing) {
      inmissing <- FALSE
      if (length.of.missing < temp.length) {
        length.of.missing <- temp.length
      }
      temp.length <- 0
    }
    # set length of new missing string
    if (is.na(covid.data.location$new_deaths[i])) {
      inmissing <- TRUE
      temp.length <- temp.length + 1
    }
  }
  length.of.missing
}
# Canada
d<-longest.ds.no.report.deaths(name.country[1])
# Greenland
e<-longest.ds.no.report.deaths(name.country[2])
# United States
f<-longest.ds.no.report.deaths(name.country[3])
cau6 <- tibble::tribble(
  ~Country,       ~New_deaths, ~New_cases,
  name.country[1], a, d,
  name.country[2], b, e,
  name.country[3], c, f
)
#############################################################
# 7)the shortest number of consecutive days with no new infections
# change missing value to -1
for (i in 1:nrow(covid.data)) {
  if (is.na(covid.data$new_cases[i])) covid.data$new_cases[i] <- -1
  if (is.na(covid.data$new_deaths[i])) covid.data$new_deaths[i] <- -1
}

# manipulative function
shortest.ds.no.infections <- function(name) {
  # select country
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)

  # set default
  in.no.infection <- FALSE
  length.of.no.infections <- 0
  temp.length <- 0

  # check if don't have any day with no infection
  for (i in 1:nrow(covid.data.location)) {
    if (covid.data.location$new_cases[i] == 0) {
      length.of.no.infections <- nrow(covid.data.location)
      break
    }
  }

  # find shortest days
  for (i in 1:nrow(covid.data.location))
  {
    # check new no in string is shorter
    if (!(covid.data.location$new_cases[i] == 0) & in.no.infection) {
      in.no.infection <- FALSE
      if (length.of.no.infections > temp.length) {
        length.of.no.infections <- temp.length
      }
      temp.length <- 0
    }
    # set length of new no infections string
    if (covid.data.location$new_cases[i] == 0) {
      in.no.infection <- TRUE
      temp.length <- temp.length + 1
    }
  }
  length.of.no.infections
}
# Canada
a<-shortest.ds.no.infections(name.country[1])
# Greenland
b<-shortest.ds.no.infections(name.country[2])
# United States
c<-shortest.ds.no.infections(name.country[3])
cau7 <- tibble::tribble(
  ~Country,       ~New_cases,
  name.country[1], a,
  name.country[2], b,
  name.country[3], c,
)
#############################################################
# 8)the longest number of consecutive days with no new infections
# manipulative function
longest.ds.no.infections <- function(name) {
  # select country
  select <- covid.data$location == name
  covid.data.location <- subset(covid.data, subset = select)

  # set default
  in.no.infection <- FALSE
  length.of.no.infections <- 0
  temp.length <- 0

  # find longest days
  for (i in 1:nrow(covid.data.location))
  {
    # check new no in string is longer
    if (!(covid.data.location$new_cases[i] == 0) & in.no.infection) {
      in.no.infection <- FALSE
      if (length.of.no.infections < temp.length) {
        length.of.no.infections <- temp.length
      }
      temp.length <- 0
    }
    # set length of new no infections string
    if (covid.data.location$new_cases[i] == 0) {
      in.no.infection <- TRUE
      temp.length <- temp.length + 1
    }
  }
  length.of.no.infections
}
# Canada
a<-longest.ds.no.infections(name.country[1])
# Greenland
b<-longest.ds.no.infections(name.country[2])
# United States
c<-longest.ds.no.infections(name.country[3])
cau8 <- tibble::tribble(
  ~Country,       ~New_cases,
  name.country[1], a,
  name.country[2], b,
  name.country[3], c,
)
#############################################################
#Export results to CSV files
export(cau1, "Cau1_So_ngay_khong_co_du_lieu_bao_cao_moi.csv")
export(cau2, "Cau2_So_ngay_co_so_ca_nhiem_tu_vong_la_thap_nhat_duoc_bao_cao_moi.csv")
export(cau3, "Cau3_So_ngay_co_so_ca_nhiem_tu_vong_la_cao_nhat_duoc_bao_cao_moi.csv")
export(cau4a, "Cau4_Khong_duoc_bao_cao_moi.csv")
export(cau4b, "Cau4_Bao_cao_moi.csv")
export(cau5, "Cau5_So_ngay_ngan_nhat_lien_tiep_khong_co_du_lieu_bao_cao_moi.csv")
export(cau6, "Cau6_So_ngay_dai_nhat_lien_tiep_khong_co_du_lieu_bao_cao_moi.csv")
export(cau7, "Cau7_So_ngay_ngan_nhat_lien_tiep_khong_co_nguoi_nhiem_benh_moi.csv")
export(cau8, "Cau8_So_ngay_dai_nhat_lien_tiep_khong_co_nguoi_nhiem_benh_moi.csv")