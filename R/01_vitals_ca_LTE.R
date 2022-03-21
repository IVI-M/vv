# 01_vitals_ca_LTE.R



# 0. Global settings ----


# source ("LTE-vitals-ca.R")
# 1. Read cashed Table 13-10-0810-01 merged with GEO -----

# Canadian Vital Statistics Death (CVSD) Database
# Leading causes of death, total population 
# Provisional weekly death counts, by selected grouped causes of death
# Table: 13-10-0810-01
# Release date: 2021-11-08

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001
# https://www150.statcan.gc.ca/n1/tbl/csv/13100810-eng.zip


if (F) { # 1.a Read and cache live CANSIM data ----
  
  # NB: Works only if StatCan site is up and running.
  # You can test if it is running by clicking here: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001
  
  dt <- cansim::get_cansim("13-10-0810-01") %>%  setDT(dt)
  # Quick view of the data
  dt
  dt %>% names 
  # Remove unneeded columns and simplify values
  # dt[, (names(dt)[c(1,3:20,24)]):=NULL]
  dt <- dt[, c("Date", "GEO", "val_norm", "Cause of death (ICD-10)")]
  dt[, Date := ymd(Date)]; 
  dateMax <- dt$Date %>% max; dateMax # "2021-10-02"
  dt$GEO %>% unique() %>% sort
  dt[, GEO := gsub(", place of occurrence", "", GEO)]
  
  # 1.a.i Add timestamp to cached file----
  # dt[1, cached := dateToday]
  
  
  # dt <- dt[!is.na(val_norm)]
  
  dtTimeStamp <- dt[1][ , lapply(.SD, function(x) NA), .SDcols=names(dt)] [, Date:=dateToday]
  dt <- rbind(dt, dtTimeStamp)
  
  saveRDS(dt, paste0("13100810-20220110.Rds")) # save locally as compressed Rds file
  
  saveRDS(dt, paste0("13100810-cached.Rds")) # save locally as compressed Rds file
  fwrite(dt, paste0("13100810-cached.csv"), sep = "\t")
  fwrite(dt[Date >= ymd("2019-09-01")], paste0("13100810-after-20190901-20220110.csv"), sep = "\t")
}

if  (F)  { # 1.b Read cached from github.com/open-canada/datasets/statcan ----
  
  # downloader::download("https://github.com/open-canada/cansim-examples/raw/main/13100810-20211206.Rds", "13100810-20211206b.Rds") # order way
  curl::curl_download("https://github.com/open-canada/datasets/raw/main/statcan/13100810.Rds", "13100810.Rds")
  
  dt <- readRDS("13100810.Rds") 
  
  # Or load csv (for just last two years)
  dt <- fread ("https://github.com/open-canada/datasets/raw/main/statcan/13100810.csv") 
}

# * 1.c Read local cached copy ----

dtCached <- readRDS(paste0("13100810-20220110.Rds"))

# dtCached <- readRDS(paste0("13100810-cached.Rds"))
dtCached
dateCached <- "2022-01-10" %>% ymd
# dtCached <- dtCached[1:(.N-1)]
# if ( is.na(dt[.N]$GEO) ) { # CAUSES PROBLEM !!
#   dateCached <- dt[.N]$Date %>% ymd
# 
#   # # Remove last line in Cached Date by reference
#   # dt <- dt %>% dt.rmRow(nrow(dt))   # Much faster and memory efficient than than
#   dt <- dt[1:(.N-1)]
# }
# dateCached
# dtCached

dateMax <- dtCached$Date %>% max (na.rm=T) %>% ymd; dateMax
dtCached %>% names
dtCached[, GEO := gsub(", place of occurrence", "", GEO)]

choicesGEO <-  dtCached$GEO %>% unique(); choicesGEO
choicesCauses <- dtCached$`Cause of death (ICD-10)` %>% unique(); choicesCauses

# dt <- dt[Date >= ymd("2019-09-01")]

# 2. Merge with population, compute rates per million----

  dtGeo <- data.table(
    GEO = c(  "Ontario", "Quebec", "British Columbia", "Alberta",
              "Manitoba", "Saskatchewan", 
              "Nova Scotia", "New Brunswick",  "Newfoundland and Labrador", "Prince Edward Island",
              "Northwest Territories", "Nunavut", "Yukon", "Canada"  ),
    population = c( 14826276, 8604495, 
                    5214805, 4442879, 
                    1383765, 1179844, 
                    992055, 789225,  520553, 164318, 
                    45504, 39403, 42986, 38246108 )
  )

# We do it now in if input$per_million)
# dtCached <- dtGeo[dtCached, on="GEO"]
dtCached[, GEO:=fct_relevel(GEO, choicesGEO)]
# dtCached [, rate:=round(1000000*val_norm/population)]

dtCached <- dtCached[, c("Date", "GEO", "val_norm", "Cause of death (ICD-10)")]
dtCached[, Date := ymd(Date)]; 
dateMax <- dtCached$Date %>% max; dateMax # "2021-10-02"

setnames(dtCached, "val_norm", "value")
setcolorder(dtCached, c("Date",  "GEO", "Cause of death (ICD-10)", "value"))

# 3. Read Vaccination data -----

# Read cached or live data

# downloader::download("https://github.com/open-canada/cansim-examples/raw/main/vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds", "vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds")

if (F) {
  dtVac <- fread("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex-overTimeDownload.csv")
  fwrite(dtVac, "vaccination-coverage-byAgeAndSex-overTimeDownload.csv")
  saveRDS(dtVac, "vaccination-coverage-byAgeAndSex-overTimeDownload.Rds")
}

dtVac <- readRDS("vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds")

dtVac[.N]
dtVac %>% names
dtVac <- dtVac[, c(2, 4:7,9)]

dtVac[, week_end := ymd(week_end)]
setnames(dtVac, old=c("prename", "week_end"), new=c("GEO", "Date"))

dtVac %>% names
setnames(dtVac, 
            c("numtotal_atleast1dose","numtotal_fully" ),
            c("dose1_rate", "dose2_rate"))
colValues <- c("dose1_rate", "dose2_rate")
dtVac[, (colValues):=lapply(.SD, as.numeric), .SDcols=colValues]

dtVac$GEO  %>% unique() 
# dtVac[, prename:=fct_reorder(prename, numtotal_fully, min)]
dtVac[, GEO:=fct_relevel(GEO, choicesGEO)]

dtVac <- dtGeo[dtVac, on="GEO"]

dtVac [, dose1_rate:=round(100*dose1_rate/population)]
dtVac [, dose2_rate:=round(100*dose2_rate/population)]

dtVacAllAgesAllSexes <- 
  dtVac [sex == "All sexes", lapply(.SD, sum, na.rm=T), by=c("GEO", "Date"), .SD=colValues]


# 4.a Set parameters: static ----

in0 <- list(
  read_from="Cached Data",
  state = choicesGEO %wo% c("Yukon", "Northwest Territories", "Nunavut"), 
  # state <- choicesGEO %in% c("Canada", "Quebec" , "Ontario", "Alberta", "British Columbia" ), # Start with largest
  cause = choicesCauses[c(1,2,5, 7, 12, 15:16)], # 5:9, # Start with largest
  # live=F,
  # age=F,
  average = F,
  vaccination=T,
  vax="Total",
  lm=F,
  alternative_view=F,
  per_million = T,
  keep_scale = T,
  compare2past = F,
  
  date = c("2019-12-01", as.character(dateToday))
)

if ( !shiny::isRunning() ) { # DOES NOT WORK when Shiny is running?
  cat("Relax - Shiny is NOT Running :)")
  input <- in0
}


if (F) {
  
  
  
  # ```{r r.dt0 r.dtAll} -----
  
  if (input$read_from=="Cached Data") {
    dt = copy(dtCached)
  } 

  
  
  if (input$per_million==T) {
    dt <- dtGeo[dtCached, on="GEO"]
    dt [, GEO:=fct_relevel(GEO, choicesGEO)]
    dt [, value:=round(1000000*value/population)]
    setcolorder(dt, c("Date",  "GEO", "population", "Cause of death (ICD-10)", "value"))
  } 
  
  if (input$average==T) {
    convolution_window <- 3 
    dt[, value := frollmean(value, convolution_window, align = "right", fill = 0), by = .(GEO, `Cause of death (ICD-10)`)]
  }
  
  # if (input$clumpAllOtherCauses==T) {
  #   
  # }
  

  
  if (input$vaccination==F) {
    dtAll = dt 
  } else {
    dtVacAllAgesAllSexes <- # because causes don report age ad sex
      dtVac [sex == "All sexes", lapply(.SD, sum, na.rm=T), by=c("GEO", "Date"), .SD=colValues]
    
    # dtAll <- dtVacAllAgesAllSexes[dt, on=.(prename=GEO, week_end=Date)]
    dtAll <- dtVacAllAgesAllSexes[dt, on=c("GEO", "Date")]
    dtAll[, GEO:=fct_relevel(GEO, choicesGEO)]
    
    dtAll [, numtotal_atleast1dose:=round(100*numtotal_atleast1dose/population)]
    dtAll [, numtotal_fully:=round(100*numtotal_fully/population)]
  }
  
  
  

  q <- quote(Date >= input$date[1] & Date <= input$date[2] & GEO %in% input$state & as.character(`Cause of death (ICD-10)`) %in% input$cause)
  
  dt0 <- dtAll[eval(q)] # for testing offline
  # r.dtAll()[
  
  
  
  # ```{r r.dt0_wideGeo1 dygraphs} ----
  
  
  region1 <- dt0$GEO %>% unique() %>% .[1]; region1
  
  # dt0_wideGeo1 <- dt0[Date >= "2020-01-01" & GEO==region1, !c("GEO"), with=T] %>% dcast(... ~ `Cause of death (ICD-10)`)
  dt0_wideGeo1 <- dt0[Date >= "2020-01-01" & GEO == region1, !c("GEO"), with = T] %>% dcast( Date ~ `Cause of death (ICD-10)`, value.var = "value")
  # dt0_wideGeo1 <- dt0[Date >= "2020-01-01" & GEO == region1, !c("GEO"), with = T] %>% dcast( ... ~ `Cause of death (ICD-10)`, value.var = "value")
  
  dt0_wideGeo1
  
  
  setcolorder(dt0_wideGeo1, "Date")
  dts <- as.xts.data.table(dt0_wideGeo1)
  
  dygraph.title(dts, region1)
  
}