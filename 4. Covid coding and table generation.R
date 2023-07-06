# Last updated: July 5, 2023
# Last run: June 8, 2023
#
# Additional filtering based on data (as described in the published study)
#
## Unreviewed science in the news: The evolution of preprint media coverage from 2014-2021
#
#
#Alice Fleerackers
#
# Related Publication:
# Fleerackers, A., Shores, K., Chtena, N. & Alperin, J.P. (2023). Unreviewed science in the news: The evolution of preprint media coverage from 2014-2021. *bioarxiv*. 
# 
#
## Code for "Unreviewed science in the news: The evolution of preprint media coverage from 2014-2021"
#

library(tidyverse)

setwd(".")

preprint_mentions = read.csv("data/top outlets/preprints_mentions_final.csv")
wos_mentions = read.csv("data/top outlets/wos_mentions_final.csv")

## Code for Section 3.4 Identifying news mentions of COVID-19 research

## Identifying preprints and WoS outputs with a COVID-19-related term in the title

preprint_mentions$covid_title= grepl("coronavirus|covid-19|sars-cov|sars-cov-2|ncov-2019|2019-ncov|hcov-19|sars-2|pandemic|covid|Severe Acute Respiratory Syndrome Coronavirus 2|2019 ncov|sarscov2", preprint_mentions$alt_title, ignore.case=TRUE) 

wos_mentions$covid_title = grepl("coronavirus|covid-19|sars-cov|sars-cov-2|ncov-2019|2019-ncov|hcov-19|sars-2|pandemic|covid|Severe Acute Respiratory Syndrome Coronavirus 2|2019 ncov|sarscov2", wos_mentions$wos_title, ignore.case=TRUE) 

## Filtering COVID-19-related outputs to keep those from 2020-2021 only

preprint_mentions$pub_year=as.numeric(substr(preprint_mentions$best_preprint_pub_date,1,4))

preprint_mentions = preprint_mentions %>%
  mutate(covid_title = case_when(
    pub_year<2019 & covid_title==TRUE ~ FALSE,
    TRUE ~ covid_title
  ))

write.csv(preprint_mentions, "data/top outlets/preprint_mentions_final_w_covidcode.csv")

wos_mentions = wos_mentions %>%
  mutate(covid_title = case_when(
    wos_year<2019 & covid_title==TRUE ~ FALSE,
    TRUE ~ covid_title
  ))

write.csv(wos_mentions, "data/top outlets/wos_mentions_final_w_covidcode.csv")

## Code for Table 2: Number and share of preprint mentions 

# Calculating the total number of mentions of WoS and preprint research each year

wos_coverage_per_year = data.frame(table(wos_mentions$posted_on_year))
preprint_coverage_per_year = data.frame(table(preprint_mentions$posted_on_year))

colnames(wos_coverage_per_year) <- c("year","wos_mentions")
colnames(preprint_coverage_per_year) <- c("year","preprint_mentions")

## Calculating the number of mentions of each server each year 

ssrn_per_year = preprint_mentions %>% 
  filter(server == "ssrn")
ssrn_per_year=data.frame(table(ssrn_per_year$posted_on_year))
colnames(ssrn_per_year)=c("year","ssrn_mentions")
ssrn_per_year$year=as.character(ssrn_per_year$year)

arxiv_per_year = preprint_mentions %>% 
  filter(server == "arxiv")
arxiv_per_year=data.frame(table(arxiv_per_year$posted_on_year))
colnames(arxiv_per_year)=c("year","arxiv_mentions")
arxiv_per_year$year=as.character(arxiv_per_year$year)

biorxiv_per_year = preprint_mentions %>% 
  filter(server == "biorxiv")
biorxiv_per_year=data.frame(table(biorxiv_per_year$posted_on_year))
colnames(biorxiv_per_year)=c("year","biorxiv_mentions")
biorxiv_per_year$year=as.character(biorxiv_per_year$year)

medrxiv_per_year = preprint_mentions %>% 
  filter(server == "medrxiv")
medrxiv_per_year=data.frame(table(medrxiv_per_year$posted_on_year))
colnames(medrxiv_per_year)=c("year","medrxiv_mentions")
medrxiv_per_year$year=as.character(medrxiv_per_year$year)
medrxiv_per_year[4,]=c("2018",NA)
medrxiv_per_year[5,]=c("2017",NA)
medrxiv_per_year[6,]=c("2016",NA)
medrxiv_per_year[7,]=c("2015",NA)
medrxiv_per_year[8,]=c("2014",NA)
medrxiv_per_year$medrxiv_mentions=as.numeric(medrxiv_per_year$medrxiv_mentions)

annual_coverage_servers = list(arxiv_per_year, ssrn_per_year, biorxiv_per_year, medrxiv_per_year)
annual_coverage_servers = annual_coverage_servers %>% reduce(inner_join, by="year")

## Calculating the annual share of all research mentions for each server each year 

annual_coverage_servers$wos_mentions = annual_coverage$wos_mentions 
annual_coverage_servers$preprint_mentions = annual_coverage$preprint_mentions
annual_coverage_servers$arxiv_proportion = annual_coverage_servers$arxiv_mentions/(annual_coverage_servers$preprint_mentions+annual_coverage_servers$wos_mentions)
annual_coverage_servers$ssrn_proportion = annual_coverage_servers$ssrn_mentions/(annual_coverage_servers$preprint_mentions+annual_coverage_servers$wos_mentions)
annual_coverage_servers$biorxiv_proportion = annual_coverage_servers$biorxiv_mentions/(annual_coverage_servers$preprint_mentions+annual_coverage_servers$wos_mentions)
annual_coverage_servers$medrxiv_proportion = annual_coverage_servers$medrxiv_mentions/(annual_coverage_servers$preprint_mentions+annual_coverage_servers$wos_mentions)

## Calculating the average share of preprint mentions across all four servers for each year 

annual_coverage_servers$preprint_proportion=annual_coverage_servers$preprint_mentions/(annual_coverage_servers$preprint_mentions+annual_coverage_servers$wos_mentions)

## Calculating the total number and share of preprint and WoS mentions (across entire study period)

n_preprint_mentions = sum(annual_coverage_servers$preprint_mentions) ## total preprint mentions 
n_wos_mentions = sum(annual_coverage_servers$wos_mentions) ## total WoS mentions
n_mentions = n_preprint_mentions + n_wos_mentions ## total mentions of research
total_preprint_proportion=n_preprint_mentions/n_mentions ## total share of preprint mentions

## Calculating the total share of coverage for each server (across entire study period)

total_p_medrxiv = sum(annual_coverage_servers$medrxiv_mentions[c(6:8)])/n_mentions
total_p_biorxiv = sum(annual_coverage_servers$biorxiv_mentions)/n_mentions
total_p_ssrn = sum(annual_coverage_servers$ssrn_mentions)/n_mentions
total_p_arxiv = sum(annual_coverage_servers$arxiv_mentions)/n_mentions

## Calculating the total share of preprint coverage for each server (across entire study period)

preprints_p_medrxiv= sum(annual_coverage_servers$medrxiv_mentions[c(6:8)])/n_preprint_mentions
preprints_p_biorxiv= sum(annual_coverage_servers$biorxiv_mentions)/n_preprint_mentions
preprints_p_ssrn= sum(annual_coverage_servers$ssrn_mentions)/n_preprint_mentions
preprints_p_arxiv= sum(annual_coverage_servers$arxiv_mentions)/n_preprint_mentions

servers=c("arxiv","ssrn","biorxiv","medrxiv","all_preprints")
total_proportion=c(total_p_arxiv,total_p_ssrn,total_p_biorxiv,total_p_medrxiv, total_preprint_proportion)
preprints_proportion=c(preprints_p_arxiv, preprints_p_ssrn, preprints_p_biorxiv, preprints_p_medrxiv,1.0)

## Final results: 

## Annual share of preprint mentions by server:
annual_coverage_df=annual_coverage_servers[,c(1,6:12)] 
## Total share of preprint mentions by server:
preprint_proportions_df = data.frame(servers,total_proportion, preprints_proportion) 
