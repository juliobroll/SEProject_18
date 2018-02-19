## Here we get the Patent Data from NBER/USPTO and prep it for the distribution analysis
## NB: I'm keeping some commented "trash code" for now, for reference (DO NOT uncomment them)

## this will clean the environment

rm(list=ls())

## some libraries, not all of them are used here

library(readr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)
library(lattice)
library(ggthemes)
library(latticeExtra)
library(plm)
library(corrplot)
library(astsa)
library(foreign)
#library(ggplot2)

## Data upload:
## Patent_data: file with data for each patent (nb citations, assignee, dates...)
## Citation_data: data on every citation, by citing and cited patents
## Match_data: Patent-company matching data
## Match_data2: Company-Compustat matching data

Patent_data <- read.csv("C:/Users/Julio Roll/MAPSS/Classes/Thesis/Data/pat76_06_assg.asc", sep="\t")
Citation_data <- read.dta("C:/Users/Julio Roll/MAPSS/Classes/Thesis/Data/cite76_06.dta")
Match_data <- read.csv("C:/Users/Julio Roll/MAPSS/Classes/Thesis/Data/dynass.dat", sep="\t")
Match_data2 <- read.csv("C:/Users/Julio Roll/MAPSS/Classes/Thesis/Data/pdpcohdr.dat", sep="\t")

## some cleaning: US only (unreported comes with US State data, so it's US)
## also, adjusting the number of citations with Hall's correction term

Patent_data <- subset(Patent_data, country == "US" | country == "")
Patent_data <- Patent_data %>% mutate(cit_correct = allcites * hjtwt)

#hist(subset(Patent_data$cit_correct, Patent_data$cit_correct > 0), breaks=100)
#hist(subset(Patent_data$cit_correct, Patent_data$icl_class == "A41D" & Patent_data$cit_correct > 0), breaks=100)

## now, we will create our Citation_data main DF: we will match each citing and cited patent
## with its assignee number, application year, gvkey, class, maingroup and company (dynamically)
## we will also see if the citation is from a different company, class and/or maingroup

## first, some merging

aux_pat_cit <- data.frame(Patent_data$patent, Patent_data$pdpass)
colnames(aux_pat_cit) <- c("citing", "assign_citing")
Citation_data <- merge(Citation_data, aux_pat_cit, by = ("citing"))

colnames(aux_pat_cit) <- c("cited", "assign_cited")
Citation_data <- merge(Citation_data, aux_pat_cit, by = ("cited"))

aux_match_cit <- data.frame(Match_data$pdpass, Match_data$begyr1, Match_data$gvkey1,
                            Match_data$endyr1, Match_data$begyr2, Match_data$gvkey2, 
                            Match_data$endyr2, Match_data$begyr3, Match_data$gvkey3, 
                            Match_data$endyr3, Match_data$begyr4, Match_data$gvkey4,
                            Match_data$endyr4, Match_data$begyr5, Match_data$gvkey5,
                            Match_data$endyr5)
colnames(aux_match_cit) <- c("assign_citing", "citing_begyr1", "citing_gvkey1", "citing_endyr1",
                             "citing_begyr2", "citing_gvkey2", "citing_endyr2", "citing_begyr3",
                             "citing_gvkey3", "citing_endyr3", "citing_begyr4", "citing_gvkey4",
                             "citing_endyr4", "citing_begyr5", "citing_gvkey5", "citing_endyr5")
Citation_data <- merge(Citation_data, aux_match_cit, by = ("assign_citing"))

colnames(aux_match_cit) <- c("assign_cited", "cited_begyr1", "cited_gvkey1", "cited_endyr1",
                             "cited_begyr2", "cited_gvkey2", "cited_endyr2", "cited_begyr3",
                             "cited_gvkey3", "cited_endyr3", "cited_begyr4", "cited_gvkey4",
                             "cited_endyr4", "cited_begyr5", "cited_gvkey5", "cited_endyr5")
Citation_data <- merge(Citation_data, aux_match_cit, by = ("assign_cited"))

aux_appyear <- data.frame(Patent_data$patent, Patent_data$appyear)
colnames(aux_appyear) <- c("citing", "citing_appyear")
Citation_data$citing_appyear <- aux_appyear[match(Citation_data$citing, aux_appyear$citing), "citing_appyear"]

colnames(aux_appyear) <- c("cited", "cited_appyear")
Citation_data$cited_appyear <- aux_appyear[match(Citation_data$cited, aux_appyear$cited), "cited_appyear"]

## dynamic matching: assuming that a citation happens when a patent application arrives,
## we can track the company that owned the patent on that year

Citation_data <- Citation_data %>% mutate(citing_gvkey = citing_gvkey1)
Citation_data$cited_gvkey <-if_else(with(Citation_data, cited_appyear <= cited_endyr1 | is.na(cited_gvkey2)), Citation_data$cited_gvkey1,
                                    if_else(with(Citation_data, cited_appyear <= cited_endyr2 &
                                                   cited_appyear >= cited_begyr2 | is.na(cited_gvkey3)), Citation_data$cited_gvkey2,
                                            if_else(with(Citation_data, cited_appyear <= cited_endyr3 &
                                                           cited_appyear >= cited_begyr3 | is.na(cited_gvkey4)), Citation_data$cited_gvkey3,
                                            if_else(with(Citation_data, cited_appyear <= cited_endyr4 &
                                                           cited_appyear >= cited_begyr4 | is.na(cited_gvkey5)), Citation_data$cited_gvkey4, Citation_data$cited_gvkey5))))
#Citation_data <- Citation_data %>% mutate(cited_gvkey = ifelse(
#  is.na(cited_gvkey2), cited_gvkey1, ifelse(is.na(cited_gvkey3), cited_gvkey2, ifelse(
#    is.na(cited_gvkey4), cited_gvkey3, ifelse(is.na(cited_gvkey5), cited_gvkey4, cited_gvkey5)))))

#Citation_data <- Citation_data %>% mutate(cited_gvkey = ifelse(
#  inrange(cited_appyear, cited_begyr1, cited_endyr1, incbounds=TRUE), cited_gvkey1, ifelse(
#    inrange(cited_appyear, cited_begyr2, cited_endyr2, incbounds=TRUE), cited_gvkey2, ifelse(
#      inrange(cited_appyear, cited_begyr3, cited_endyr3, incbounds=TRUE), cited_gvkey3, ifelse(
#        inrange(cited_appyear, cited_begyr4, cited_endyr4, incbounds=TRUE), cited_gvkey4, ifelse(
#          inrange(cited_appyear, cited_begyr5, cited_endyr5, incbounds=TRUE), cited_gvkey5, NA))))
#  ))

Citation_data <- Citation_data %>% mutate(external = ifelse(citing_gvkey == cited_gvkey, 0, 1))

## we'll fetch some of the data in Patent_data with matching

aux_class <- data.frame(Patent_data$pdpass, Patent_data$icl_class, Patent_data$icl_maingroup)
colnames(aux_class) <- c("assign_citing", "citing_icl_class", "citing_icl_maingroup")
Citation_data$citing_icl_class <- aux_class[match(Citation_data$assign_citing, aux_class$assign_citing), "citing_icl_class"]
Citation_data$citing_icl_maingroup <- aux_class[match(Citation_data$assign_citing, aux_class$assign_citing), "citing_icl_maingroup"]

colnames(aux_class) <- c("assign_cited", "cited_icl_class", "cited_icl_maingroup")
Citation_data$cited_icl_class <- aux_class[match(Citation_data$assign_cited, aux_class$assign_cited), "cited_icl_class"]
Citation_data$cited_icl_maingroup <- aux_class[match(Citation_data$assign_cited, aux_class$assign_cited), "cited_icl_maingroup"]

## check if it's the same class or maingroup

Citation_data <- Citation_data %>% mutate(external_class = ifelse(citing_icl_class == cited_icl_class, 0, 1))
Citation_data <- Citation_data %>% mutate(external_maingroup = ifelse(citing_icl_maingroup == cited_icl_maingroup, 0, 1))

## With the above data, we will create the cit_external_comn_share: it's a summary DF that
## contains the shares of external citation (be it by company, class or maingroup)
## NB: because of our previous cleaning, I'm recounting the total number of citations
## (instead of using the one that was provided)

## first, for citations from different companies

cit_external_comn_share <- data.frame(Patent_data$patent)
colnames(cit_external_comn_share) <- "patent"
aux_external_comn_share <- as.data.frame(table(Citation_data$cited))
colnames(aux_external_comn_share) <- c("patent","tot_cit")
cit_external_comn_share$tot_cit <- aux_external_comn_share[match(cit_external_comn_share$patent, aux_external_comn_share$patent), "tot_cit"]
#cit_external_comn_share[is.na("tot_cit")] <- 0
cit_external_comn_share$tot_cit <- ifelse(is.na(cit_external_comn_share$tot_cit), 0, cit_external_comn_share$tot_cit)
cit_external_comn_share$hjtwt <- Patent_data[match(cit_external_comn_share$patent, Patent_data$patent), "hjtwt"]
cit_sum_external <- aggregate(Citation_data$external, by = list(Citation_data$cited), FUN = sum)
colnames(cit_sum_external) <- c("patent","tot_external")
cit_external_comn_share$tot_external <- cit_sum_external[match(cit_external_comn_share$patent, cit_sum_external$patent), "tot_external"]
cit_external_comn_share$tot_external <- ifelse(is.na(cit_external_comn_share$tot_external), 0, cit_external_comn_share$tot_external)
cit_external_comn_share <- cit_external_comn_share %>% mutate(external_share = ifelse(tot_cit != 0, tot_external/tot_cit, NA))

#plot(ecdf(subset(cit_external_comn_share$tot_external_correct, cit_external_comn_share$external_share >= 0.5 & cit_external_comn_share$tot_external_correct < 30 & cit_external_comn_share$tot_external_correct > 0)))

## by class

cit_sum_external_class <- aggregate(Citation_data$external_class, by = list(Citation_data$cited), FUN = sum)
colnames(cit_sum_external_class) <- c("patent","tot_external_class")
cit_external_comn_share$tot_external_class <- cit_sum_external_class[match(cit_external_comn_share$patent, cit_sum_external_class$patent), "tot_external_class"]
cit_external_comn_share$tot_external_class <- ifelse(is.na(cit_external_comn_share$tot_external_class), 0, cit_external_comn_share$tot_external_class)
cit_external_comn_share <- cit_external_comn_share %>% mutate(external_class_share = ifelse(tot_cit != 0, tot_external_class/tot_cit, NA))

## by maingroup

cit_sum_external_maingroup <- aggregate(Citation_data$external_maingroup, by = list(Citation_data$cited), FUN = sum)
colnames(cit_sum_external_maingroup) <- c("patent","tot_external_maingroup")
cit_external_comn_share$tot_external_maingroup <- cit_sum_external_maingroup[match(cit_external_comn_share$patent, cit_sum_external_maingroup$patent), "tot_external_maingroup"]
cit_external_comn_share$tot_external_maingroup <- ifelse(is.na(cit_external_comn_share$tot_external_maingroup), 0, cit_external_comn_share$tot_external_maingroup)
cit_external_comn_share <- cit_external_comn_share %>% mutate(external_maingroup_share = ifelse(tot_cit != 0, tot_external_maingroup/tot_cit, NA))

## correction terms: here, irrelevant, since ratio is constant

cit_external_comn_share <- cit_external_comn_share %>% mutate(tot_cit_correct = tot_cit * hjtwt)
cit_external_comn_share <- cit_external_comn_share %>% mutate(tot_external_correct = tot_external * hjtwt)
cit_external_comn_share <- cit_external_comn_share %>% mutate(tot_external_class_correct = tot_external_class * hjtwt)
cit_external_comn_share <- cit_external_comn_share %>% mutate(tot_external_maingroup_correct = tot_external_maingroup * hjtwt)

## extra: we're fetching the companies' names

Citation_data$citing_conm <- Match_data2[match(Citation_data$citing_gvkey, Match_data2$gvkey), "name"]
Citation_data$cited_conm <- Match_data2[match(Citation_data$cited_gvkey, Match_data2$gvkey), "name"]

##########
## Citation time evolution: we want ot create a DF that shows us, through time, the number
## of citations that were given to each patent

#cit_evolution <- data.frame(matrix(ncol = max(Patent_data$appyear) - min(Patent_data$appyear) + 1, nrow = nrow(Patent_data)))
#colnames(cit_evolution) <- seq(min(Patent_data$appyear), max(Patent_data$appyear), 1)
#cit_evolution <- cbind(Patent_data$appyear, cit_evolution)
#colnames(cit_evolution)[1] <- "appyear"
#cit_evolution <- cbind(Patent_data$patent, cit_evolution)
#colnames(cit_evolution)[1] <- "patent"

## we create a list that has, for each element (named by patent), the year that the particular
## patent got a citation

aux_cit_evolution <- data.frame(Citation_data$cited, Citation_data$citing_appyear)
aux_cit_evolution <- split(seq(nrow(aux_cit_evolution)), as.factor(aux_cit_evolution$Citation_data.cited))
aux_cit_evolution_year <- lapply(aux_cit_evolution, function(.indx){ 
  aux <- Citation_data$citing_appyear[.indx]
}) 

## table() allows us to count the occurrences (lapply: by element)
## rbindlist is a heavy one: he's going to create our DF from that list by joining one by one
## and placing each yearly sum on its respective column

aux_cit_evolution_year_count <- lapply(aux_cit_evolution_year, table)
#require(plyr)
#aux_cit_evolution_year_count_df <- rbind.fill(lapply(aux_cit_evolution_year_count,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
#aux_cit_evolution_year_count_df <- do.call(rbind, aux_cit_evolution_year_count_reord)
#aux_cit_evolution_year_count_reord <- c(list(seq(1950, 2006, 1)),aux_cit_evolution_year_count)
aux_cit_evolution_year_count_df <- rbindlist(lapply(aux_cit_evolution_year_count, as.data.frame.list), use.names=TRUE, fill=TRUE)
#aux_cit_evolution_year_count_df <- data.table::as.data.table(purrr::transpose(aux_cit_evolution_year_count_reord))
#colnames(aux_cit_evolution_year_count_df) <- sprintf('y%i', as.numeric(unlist(aux_cit_evolution_year_count_df[1,])))
#aux_cit_evolution_year_count_df <- aux_cit_evolution_year_count_df[-1,]
aux_cit_evolution_year_count_df <- cbind(names(aux_cit_evolution_year_count), aux_cit_evolution_year_count_df)
colnames(aux_cit_evolution_year_count_df)[1] <- "patent"

## because of the way it was created, each element in the DF is a list, which is not ideal
## we will convert everyone, then order the columns in chronological order

aux_cit_evolution_year_count_df <- data.matrix(aux_cit_evolution_year_count_df)
aux_cit_evolution_year_count_df <- as.data.frame(aux_cit_evolution_year_count_df)
aux_cit_evolution_year_count_df <- aux_cit_evolution_year_count_df[
  ,colnames(aux_cit_evolution_year_count_df)[order(colnames(aux_cit_evolution_year_count_df))]]
aux_cit_evolution_year_count_df <- cbind(Patent_data[match(
  aux_cit_evolution_year_count_df$patent, Patent_data$patent),"appyear"], aux_cit_evolution_year_count_df)
colnames(aux_cit_evolution_year_count_df)[1] <- "appyear"

#aux_cit_evolution_year_count_df <- as.matrix(aux_cit_evolution_year_count_df)
#aux_cit_evolution_year_count_df[aux_cit_evolution_year_count_df == "NULL"] <- 0
#aux_cit_evolution_year_count_df[is.na(aux_cit_evolution_year_count_df)] <- 0
#aux_cit_evolution_year_count_df <- as.data.frame(aux_cit_evolution_year_count_df)

#lengths <- lapply(aux_cit_evolution_year_count, length)
#aux_cit_evolution_year_count_df <- rbindlist(lapply(aux_cit_evolution_year_count, as.data.frame.list), fill = TRUE)
#aux_cit_evolution_year_df_split <- plyr::ldply(aux_cit_evolution_year, rbind)
#aux_cit_evolution_year_df_split1 <- rbindlist(lapply(aux_cit_evolution_year_split1, as.data.frame.list), fill = TRUE)
#aux_cit_evolution_year_df <- as.data.frame(do.call(rbind, aux_cit_evolution_year))
#lengths <- lapply(aux_cit_evolution_year, length)
#aux_cit_evolution_year_reord <- c(aux_cit_evolution_year[which.max(lengths)],aux_cit_evolution_year[-which.max(lengths)])
#aux_cit_evolution_year_df <- data.table::as.data.table(purrr::transpose(aux_cit_evolution_year_reord))