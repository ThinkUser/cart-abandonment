## Cart Abandonment 

library(tidyverse)
library(googleAnalyticsR)
library(janitor)
library(googleAuthR)
gar_auth("~/tu.httr-oauth")

account_list <- ga_account_list()
meta <- ga_meta()

# This Month
as.Date(start <- "2021-01-01")
as.Date(end <- "2021-06-30")

estee <- 160098300
mac <- 142253091
clinique <- 104892417
bobbi <- 171080712

#1.0 The Manual Way by date ----
df_ga <- google_analytics(mac, date_range = c(start, end),
                          dimensions = c("date"),
                          metrics = c("transactions", "transactionRevenue"),
                          max =-1,
                          anti_sample = TRUE)

total_transactions <- sum(df_ga$transactions)
total_revenue <- sum(df_ga$transactionRevenue)
aov <- round(total_revenue/total_transactions)
  
df_ga_events <- google_analytics(mac, date_range = c(start, end),
                          dimensions = c("date"),
                          metrics = c("uniqueEvents"),
                          filtersExpression = c("ga:eventCategory=~^(ecommerce|eCommerce)$;ga:eventAction=~^(add to bag|add to cart)$"),
                          max =-1,
                          anti_sample = TRUE)

total_add_to_carts <- sum(df_ga_events$uniqueEvents)
aband_rate <- round(1-(total_transactions/total_add_to_carts),3)
lost_per_1000 <- 1000*aband_rate
lost_revenue_per1000 <- round(lost_per_1000*aov)



#2.0 The Manual Way - Source Medium ----
df_ga <- google_analytics(mac, date_range = c(start, end),
                          dimensions = c("sourceMedium"),
                          metrics = c("transactions", "transactionRevenue"),
                          max =-1,
                          anti_sample = TRUE)

organic <- df_ga %>% filter(str_detect(sourceMedium, "organic"))


total_transactions <- sum(organic$transactions)
total_revenue <- sum(organic$transactionRevenue)
aov <- round(total_revenue/total_transactions)

df_ga_events <- google_analytics(mac, date_range = c(start, end),
                                 dimensions = c("sourceMedium"),
                                 metrics = c("uniqueEvents"),
                                 filtersExpression = c("ga:eventCategory=~^(ecommerce|eCommerce)$;ga:eventAction=~^(add to bag|add to cart)$"),
                                 max =-1,
                                 anti_sample = TRUE)

organic_events <- df_ga_events %>% filter(str_detect(sourceMedium, "organic"))

total_add_to_carts <- sum(organic_events$uniqueEvents)
aband_rate <- round(1-(total_transactions/total_add_to_carts),3)
lost_per_1000 <- 1000*aband_rate
lost_revenue_per1000 <- round(lost_per_1000*aov)




#3.0 The Function Way - sourceMedium ----
aband_function_source_medium <- function(ga_id,start,end,dimension) {
  df_ga <- google_analytics(ga_id, date_range = c(start, end),
                            dimensions = c("sourceMedium"),
                            metrics = c("transactions", "transactionRevenue"),
                            max =-1,
                            anti_sample = TRUE)
  
  filtered <- df_ga %>% filter(str_detect(sourceMedium, dimension))
  
  
  total_transactions <- sum(filtered$transactions)
  total_revenue <- sum(filtered$transactionRevenue)
  aov <- round(total_revenue/total_transactions)
  
  df_ga_events <- google_analytics(ga_id, date_range = c(start, end),
                                   dimensions = c("sourceMedium"),
                                   metrics = c("uniqueEvents"),
                                   filtersExpression = c("ga:eventCategory=~^(ecommerce|eCommerce)$;ga:eventAction=~^(add to bag|add to cart)$"),
                                   max =-1,
                                   anti_sample = TRUE)
  
  filtered_events <- df_ga_events %>% filter(str_detect(sourceMedium, dimension))
  
  total_add_to_carts <<- sum(filtered_events$uniqueEvents)
  aband_rate <<- round(1-(total_transactions/total_add_to_carts),3)
  lost_per_1000 <<- 1000*aband_rate
  lost_revenue_per1000 <<- round(lost_per_1000*aov)
  
}

aband_function_source_medium(mac,start,end,"organic")


#4.0 The Function Way - devices ----
aband_function_devices <- function(ga_id,start,end,dimension) {
  df_ga <- google_analytics(ga_id, date_range = c(start, end),
                            dimensions = c("deviceCategory"),
                            metrics = c("transactions", "transactionRevenue"),
                            max =-1,
                            anti_sample = TRUE)
  
  filtered <- df_ga %>% filter(str_detect(deviceCategory, dimension))
  
  
  total_transactions <- sum(filtered$transactions)
  total_revenue <- sum(filtered$transactionRevenue)
  aov <- round(total_revenue/total_transactions)
  
  df_ga_events <- google_analytics(ga_id, date_range = c(start, end),
                                   dimensions = c("deviceCategory"),
                                   metrics = c("uniqueEvents"),
                                   filtersExpression = c("ga:eventCategory=~^(ecommerce|eCommerce)$;ga:eventAction=~^(add to bag|add to cart)$"),
                                   max =-1,
                                   anti_sample = TRUE)
  
  filtered_events <- df_ga_events %>% filter(str_detect(deviceCategory, dimension))
  
  total_add_to_carts <<- sum(filtered_events$uniqueEvents)
  aband_rate <<- round(1-(total_transactions/total_add_to_carts),3)
  lost_per_1000 <<- 1000*aband_rate
  lost_revenue_per1000 <<- round(lost_per_1000*aov)
  
}

aband_function_devices(mac,start,end,"mobile")



#5.0 The Generalized way with choose a dimensions ----
aband_function <- function(ga_id,start,end,ga_dim,dimension) {
  df_ga <- google_analytics(ga_id, date_range = c(start, end),
                            dimensions = ga_dim,
                            metrics = c("transactions", "transactionRevenue"),
                            max =-1,
                            anti_sample = TRUE)
  
  filtered <- df_ga %>% filter(str_detect(.[[1]], dimension))
  
  
  total_transactions <- sum(filtered$transactions)
  total_revenue <- sum(filtered$transactionRevenue)
  aov <- round(total_revenue/total_transactions)
  
  df_ga_events <- google_analytics(ga_id, date_range = c(start, end),
                                   dimensions = ga_dim,
                                   metrics = c("uniqueEvents"),
                                   filtersExpression = c("ga:eventCategory=~^(ecommerce|eCommerce)$;ga:eventAction=~^(add to bag|add to cart)$"),
                                   max =-1,
                                   anti_sample = TRUE)
  
  filtered_events <- df_ga_events %>% filter(str_detect(.[[1]], dimension))
  
  total_add_to_carts <<- sum(filtered_events$uniqueEvents)
  aband_rate <<- round(1-(total_transactions/total_add_to_carts),3)
  lost_per_1000 <<- 1000*aband_rate
  lost_revenue_per1000 <<- round(lost_per_1000*aov)
  
}

aband_function(mac,start,end,"deviceCategory","mobile")
aband_function(mac,start,end,"deviceCategory","desktop")


aband_function(mac,start,end,"sourceMedium","organic")
aband_function(mac,start,end,"sourceMedium","cpc")



#6.0 Manual Data Frame deviceCategory ----

df_ga <- google_analytics(mac, date_range = c(start, end),
                          dimensions = c("deviceCategory"),
                          metrics = c("transactions", "transactionRevenue"),
                          max =-1,
                          anti_sample = TRUE)

df_ga <- df_ga %>% mutate(aov = (transactionRevenue/transactions) )


df_ga_events <- google_analytics(mac, date_range = c(start, end),
                                 dimensions = c("deviceCategory"),
                                 metrics = c("uniqueEvents"),
                                 filtersExpression = c("ga:eventCategory=~^(ecommerce|eCommerce)$;ga:eventAction=~^(add to bag|add to cart)$"),
                                 max =-1,
                                 anti_sample = TRUE)


df_ga_events <- df_ga_events %>% set_names(c("deviceCategory", "addtocart"))

joined_df <- df_ga %>% left_join(df_ga_events, by="deviceCategory")

joined_df_final <- joined_df %>% mutate(aband_rate =  round(1-(transactions/addtocart),3),
                     lost_per_1000 =  1000*aband_rate,
                     lost_revenue_per1000 = round(lost_per_1000*aov)
                     )


head(joined_df_final)



#7.0 Manual Data Frame Source Medium ----

df_ga <- google_analytics(mac, date_range = c(start, end),
                          dimensions = c("sourceMedium"),
                          metrics = c("transactions", "transactionRevenue"),
                          max =-1,
                          anti_sample = TRUE)

df_ga <- df_ga %>% group_by(sourceMedium) %>% summarise(transactions = sum(transactions),
                                                transactionRevenue = sum(transactionRevenue)) %>%
          ungroup()

df_ga <- df_ga %>% mutate(aov = (transactionRevenue/transactions) )


df_ga_events <- google_analytics(mac, date_range = c(start, end),
                                 dimensions = c("sourceMedium"),
                                 metrics = c("uniqueEvents"),
                                 filtersExpression = c("ga:eventCategory=~^(ecommerce|eCommerce)$;ga:eventAction=~^(add to bag|add to cart)$"),
                                 max =-1,
                                 anti_sample = TRUE)



df_ga_events <- df_ga_events%>% group_by(sourceMedium) %>% summarise(uniqueEvents = sum(uniqueEvents)) %>%
  ungroup()



df_ga_events <- df_ga_events %>% set_names(c("sourceMedium", "addtocart"))

joined_df <- df_ga %>% left_join(df_ga_events, by="sourceMedium")

joined_df_final <- joined_df %>% mutate(aband_rate =  round(1-(transactions/addtocart),3),
                                        lost_per_1000 =  1000*aband_rate,
                                        lost_revenue_per1000 = round(lost_per_1000*aov)
)

joined_df_final <- joined_df_final %>% arrange(desc(transactions))

head(joined_df_final)


#8.0 Generalize Data Frame Event Based----

aband_full_function <- function(ga_id,start,end,ga_dim) {

df_ga <- google_analytics(ga_id, date_range = c(start, end),
                          dimensions = ga_dim,
                          metrics = c("transactions", "transactionRevenue"),
                          max =-1,
                          anti_sample = TRUE)

df_ga <- df_ga %>% group_by_at(1) %>% summarise(transactions = sum(transactions),
                                                        transactionRevenue = round(sum(transactionRevenue))) %>%
  ungroup()
df_ga
df_ga <- df_ga %>% mutate(aov = (transactionRevenue/transactions) )
df_ga$aov[is.nan(df_ga$aov)]<-0

df_ga



df_ga_events <- google_analytics(ga_id, date_range = c(start, end),
                                 dimensions = ga_dim,
                                 metrics = c("uniqueEvents"),
                                 filtersExpression = c("ga:eventCategory=~^(ecommerce|eCommerce)$;ga:eventAction=~^(add to bag|add to cart)$"),
                                 max =-1,
                                 anti_sample = TRUE)

df_ga_events <- df_ga_events%>% group_by_at(1) %>% summarise(uniqueEvents = sum(uniqueEvents)) %>%
  ungroup()

df_ga_events

df_ga_events <- df_ga_events %>% set_names(c(names(df_ga_events[1]), "addtocart"))

joined_df <- df_ga %>% left_join(df_ga_events, by=names(df_ga_events[1]))

joined_df_final <- joined_df %>% mutate(aband_rate =  round(1-(transactions/addtocart),3),
                                        lost_per_1000 =  1000*aband_rate,
                                        lost_revenue_per1000 = round(lost_per_1000*aov))




joined_df_final <- joined_df_final %>% arrange(desc(transactions))

joined_df_final <- joined_df_final %>% replace(is.na(.), 0)


}


mac_abnd_rate_sourcemedium <- aband_full_function(mac,start,end,"sourceMedium")
mac_abnd_rate_devices <- aband_full_function(mac,start,end,"deviceCategory")
mac_abnd_rate_countries <- aband_full_function(mac,start,end,"country")


estee_abnd_rate_sourcemedium <- aband_full_function(estee,start,end,"sourceMedium")
estee_abnd_rate_devices <- aband_full_function(estee,start,end,"deviceCategory")
estee_abnd_rate_countries <- aband_full_function(estee,start,end,"country")


clinique_abnd_rate_sourcemedium <- aband_full_function(clinique,start,end,"sourceMedium")
clinique_abnd_rate_devices <- aband_full_function(clinique,start,end,"deviceCategory")
clinique_abnd_rate_countries <- aband_full_function(clinique,start,end,"country")


bobbi_abnd_rate_sourcemedium <- aband_full_function(bobbi,start,end,"sourceMedium")
bobbi_abnd_rate_devices <- aband_full_function(bobbi,start,end,"deviceCategory")
bobbi_abnd_rate_countries <- aband_full_function(bobbi,start,end,"country")





#9.0  Uploading to GCS and BQ ----

#9.1 Setting up BigQuery and GCS ----
library(googleCloudStorageR)
library(bigQueryR)

#9.2  Setting Project --
bqr_global_project("erez-bigquery")
bqr_get_global_project()

#9.3 get project list --
projects <- bqr_list_projects()
head(projects)

my_project <- projects[2,1]
my_project
my_project_id <- projects[2,3]
my_project_id

#9.4 for first project, get datasets --
datasets <- bqr_list_datasets(my_project)
head(datasets)
my_dataset <- datasets[4,2]
my_dataset

#10.0 Upload Data to gcs----
#10.1 Get Storage Buckets --
gcs_list_buckets(my_project_id)
#gcs_list_buckets("erez-bigquery")

#10.2 Set bucket to store the data --
gcs_global_bucket("cart_abandonment") ## This will be the folder

#10.3 Upload function To Storage --
f <- function(input, output) {
  write.table(input, sep = ",", col.names = FALSE, row.names = FALSE, 
              quote = FALSE, file = output, qmethod = "double",fileEncoding="UTF-8")}

filename=paste0("cart_abandonment.csv")
gcs_upload(mac_abnd_rate_sourcemedium, name = filename, object_function = f)
#gcs_upload(df, name = "easydf.csv", object_function = f)

#11.0 upload to BQ -----
path=paste0("gs://cart_abandonment/",filename) ##Make sure path is right
path

user_schema <- schema_fields(mac_abnd_rate_sourcemedium)

job <- bqr_upload_data(projectId = my_project_id, 
                datasetId = "elcalil_cart_abandonment",  ## CREATE IN BQ IF NEEDED
                tableId = "mac_by_source_medium", 
                upload_data = c(path), 
                create = c("CREATE_IF_NEEDED"), ##WILL CREATE TABLE ONLY
                # writeDisposition = c("WRITE_TRUNCATE"),
                schema = user_schema
                # autodetect = FALSE
)
job

bqr_get_job(projectId = my_project, jobId = job$jobReference$jobId)



## 12.0 Generalize Data Frame Goals Based----

aband_full_goals_function <- function(ga_id,start,end,ga_dim,goalnumber) {
  
  df_ga <- google_analytics(ga_id, date_range = c(start, end),
                            dimensions = ga_dim,
                            metrics = c("transactions", "transactionRevenue"),
                            max =-1,
                            anti_sample = TRUE)
  
  df_ga <- df_ga %>% group_by_at(1) %>% summarise(transactions = sum(transactions),
                                                  transactionRevenue = round(sum(transactionRevenue))) %>%
    ungroup()
  df_ga
  df_ga <- df_ga %>% mutate(aov = (transactionRevenue/transactions) )
  df_ga$aov[is.nan(df_ga$aov)]<-0
  
  df_ga
  
  
  
  df_ga_goals <- google_analytics(ga_id, date_range = c(start, end),
                                   dimensions = ga_dim,
                                   metrics = goalnumber,
                                   max =-1,
                                   anti_sample = TRUE)
  
  df_ga_goals <- df_ga_goals%>% group_by_at(1) %>% summarize_if(is.numeric, sum, na.rm=TRUE) %>%
    ungroup()
  
  df_ga_goals
  
  df_ga_goals <- df_ga_goals %>% set_names(c(names(df_ga_goals[1]), "addtocart"))
  
  joined_df <- df_ga %>% left_join(df_ga_goals, by=names(df_ga_goals[1]))
  
  joined_df_final <- joined_df %>% mutate(aband_rate =  round(1-(transactions/addtocart),3),
                                          lost_per_1000 =  1000*aband_rate,
                                          lost_revenue_per1000 = round(lost_per_1000*aov))
  
  
  
  
  joined_df_final <- joined_df_final %>% arrange(desc(transactions, transactionRevenue, addtocart, aov, aband_rate))
  
  joined_df_final <- joined_df_final %>% replace(is.na(.), 0)
  
  
}


## EVENTS BASED
mac_abnd_rate_sourcemedium <- aband_full_function(mac,start,end,"sourceMedium")
mac_abnd_rate_devices <- aband_full_function(mac,start,end,"deviceCategory")
mac_abnd_rate_countries <- aband_full_function(mac,start,end,"country")

## Goals BASED
mac_abnd_rate_sourcemedium_goals <- aband_full_goals_function(mac,start,end,"sourceMedium", "goal10Completions")
mac_abnd_rate_devices_goals <- aband_full_goals_function(mac,start,end,"deviceCategory", "goal10Completions")
mac_abnd_rate_countries_goals <- aband_full_goals_function(mac,start,end,"country", "goal10Completions")



## EVENTS BASED
estee_abnd_rate_sourcemedium <- aband_full_function(estee,start,end,"sourceMedium")
estee_abnd_rate_devices <- aband_full_function(estee,start,end,"deviceCategory")
estee_abnd_rate_countries <- aband_full_function(estee,start,end,"country")


## Goals BASED
estee_abnd_rate_sourcemedium_goals <- aband_full_goals_function(estee,start,end,"sourceMedium", "goal1Completions")
estee_abnd_rate_devices_goals <- aband_full_goals_function(estee,start,end,"deviceCategory", "goal1Completions")
estee_abnd_rate_countries_goals <- aband_full_goals_function(estee,start,end,"country", "goal1Completions")


## EVENTS BASED
clinique_abnd_rate_sourcemedium <- aband_full_function(clinique,start,end,"sourceMedium")
clinique_abnd_rate_devices <- aband_full_function(clinique,start,end,"deviceCategory")
clinique_abnd_rate_countries <- aband_full_function(clinique,start,end,"country")

## Goals BASED
clinique_abnd_rate_sourcemedium_goals <- aband_full_goals_function(clinique,start,end,"sourceMedium", "goal1Completions")
clinique_abnd_rate_devices_goals <- aband_full_goals_function(clinique,start,end,"deviceCategory","goal1Completions")
clinique_abnd_rate_countries_goals <- aband_full_goals_function(clinique,start,end,"country", "goal1Completions")


## EVENTS BASED
bobbi_abnd_rate_sourcemedium <- aband_full_function(bobbi,start,end,"sourceMedium")
bobbi_abnd_rate_devices <- aband_full_function(bobbi,start,end,"deviceCategory")
bobbi_abnd_rate_countries <- aband_full_function(bobbi,start,end,"country")

## Goals BASED
bobbi_abnd_rate_sourcemedium_goals <- aband_full_goals_function(bobbi,start,end,"sourceMedium", "goal10Completions")
bobbi_abnd_rate_devices_goals <- aband_full_goals_function(bobbi,start,end,"deviceCategory", "goal10Completions")
bobbi_abnd_rate_countries_goals <- aband_full_goals_function(bobbi,start,end,"country", "goal10Completions")


## FUTURE IDEAS ----

## ADD DATES ?
## UPLOAD ALL TO BQ ??
## CHANGE TO GOALS ?

