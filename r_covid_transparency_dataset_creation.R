# Creating dataset and xml files for the research project:
## Transparency indicators across the dentistry and oral health literature
## Authors: Ahmad Sofi-Mahmudi, Eero Raittio, KSergio E. Uribe
## Codes by: Ahmad Sofi-Mahmudi

# Loading required packages
pacman::p_load(dplyr,
               rtransparent, 
               metareadr, 
               europepmc,
               here)

# Creating the dataset
## Getting desired COVID-19 keywords
COVID_19_keywords <- read.csv(here("data", "covid_transparency_keywords.csv"))

### Some modifications are needed to create proper keywords for Title, Keywords
### and Results:
COVID_19_keywords <- COVID_19_keywords %>% mutate(
        search.term.title = paste0("TITLE:", '"', Keyword, '"'),
        search.term.keyword = paste0("KW:", '"', Keyword, '"'),
        search.term.results = paste0("RESULTS:", '"', Keyword, '"'))

TitleQuery <- paste(COVID_19_keywords$search.term.title,
                    collapse = " OR ")

KeywordQuery <- paste(COVID_19_keywords$search.term.keyword,
                      collapse = " OR ")

ResultQuery <- paste(COVID_19_keywords$search.term.results,
                     collapse = " OR ")

### Getting all open-access and EPMC COVID-19-related papers indexed in PubMed

#### As in this step we should download a large file that includes the 
#### characteristics of papers of our query, to ease the process, we divided our 
#### query to five separate queries based on the date: 1- first half of 2020;
#### 2- second half of 2020; 3- first half of 2021; 4- second half of 2021;
#### 5- the first two months of 2022.

db_2020_1 <- epmc_search(
        query = paste0("(", TitleQuery, " OR ", 
                       KeywordQuery, " OR ", 
                       ResultQuery, ") ", 
                       'AND (SRC:"MED") 
                       AND (LANG:"eng" OR LANG:"en" OR LANG:"us") 
                       AND (FIRST_PDATE:[2020-01-01 TO 2020-06-30]) 
                       AND (OPEN_ACCESS:y) 
                       AND (PUB_TYPE:"Journal Article" 
                            OR PUB_TYPE:"research-article" 
                            OR PUB_TYPE:"rapid-communication" 
                            OR PUB_TYPE:"product-review")'), 
        limit = 100000, 
        output = "parsed")

# total oa and non oa: 19467

db_2020_2 <- epmc_search(
        query = paste0("(", TitleQuery, " OR ", 
                       KeywordQuery, " OR ", 
                       ResultQuery, ") ", 
                       'AND (SRC:"MED") 
                       AND (LANG:"eng" OR LANG:"en" OR LANG:"us") 
                       AND (FIRST_PDATE:[2020-07-01 TO 2020-12-31]) 
                       AND (OPEN_ACCESS:y) 
                       AND (PUB_TYPE:"Journal Article" 
                            OR PUB_TYPE:"research-article" 
                            OR PUB_TYPE:"rapid-communication" 
                            OR PUB_TYPE:"product-review")'), 
        limit = 100000, 
        output = "parsed")

# total oa and non oa: 43107

db_2021_1 <- epmc_search(
        query = paste0("(", TitleQuery, " OR ", 
                       KeywordQuery, " OR ", 
                       ResultQuery, ") ", 
                       'AND (SRC:"MED") 
                       AND (LANG:"eng" OR LANG:"en" OR LANG:"us") 
                       AND (FIRST_PDATE:[2021-01-01 TO 2021-06-30]) 
                       AND (OPEN_ACCESS:y) 
                       AND (PUB_TYPE:"Journal Article" 
                            OR PUB_TYPE:"research-article" 
                            OR PUB_TYPE:"rapid-communication" 
                            OR PUB_TYPE:"product-review")'), 
        limit = 100000, 
        output = "parsed")

# total oa and non oa: 50512

db_2021_2 <- epmc_search(
        query = paste0("(", TitleQuery, " OR ", 
                       KeywordQuery, " OR ", 
                       ResultQuery, ") ", 
                       'AND (SRC:"MED") 
                       AND (LANG:"eng" OR LANG:"en" OR LANG:"us") 
                       AND (FIRST_PDATE:[2021-07-01 TO 2021-12-31]) 
                       AND (OPEN_ACCESS:y) 
                       AND (PUB_TYPE:"Journal Article" 
                            OR PUB_TYPE:"research-article" 
                            OR PUB_TYPE:"rapid-communication" 
                            OR PUB_TYPE:"product-review")'), 
        limit = 100000, 
        output = "parsed")

# total oa and non oa: 47975

db_2022 <- epmc_search(
        query = paste0("(", TitleQuery, " OR ", 
                       KeywordQuery, " OR ", 
                       ResultQuery, ") ", 
                       'AND (SRC:"MED") 
                       AND (LANG:"eng" OR LANG:"en" OR LANG:"us") 
                       AND (FIRST_PDATE:[2022-01-01 TO 2022-02-28]) 
                       AND (OPEN_ACCESS:y) 
                       AND (PUB_TYPE:"Journal Article" 
                            OR PUB_TYPE:"research-article" 
                            OR PUB_TYPE:"rapid-communication" 
                            OR PUB_TYPE:"product-review")'), 
        limit = 100000, 
        output = "parsed")

# total oa and non oa: 14272


### We then do the following for each of them
### Let's see how many duplicates are there:
table(duplicated(db_2020_1$pmid))
table(duplicated(db_2020_2$pmid))
table(duplicated(db_2021_1$pmid))
table(duplicated(db_2021_2$pmid))
table(duplicated(db_2022$pmid))

### Removing the duplicates:
db_2020_1 <- db_2020_1 %>% distinct(pmid, .keep_all = TRUE)
db_2020_2 <- db_2020_2 %>% distinct(pmid, .keep_all = TRUE)
db_2021_1 <- db_2021_1 %>% distinct(pmid, .keep_all = TRUE)
db_2021_2 <- db_2021_2 %>% distinct(pmid, .keep_all = TRUE)
db_2022 <- db_2022 %>% distinct(pmid, .keep_all = TRUE)


### Creating a new column from pmcid column and removing "PMC" from the cells:
db_2020_1$pmcid_ <- gsub("PMC", "", as.character(db_2020_1$pmcid))
db_2020_2$pmcid_ <- gsub("PMC", "", as.character(db_2020_2$pmcid))
db_2021_1$pmcid_ <- gsub("PMC", "", as.character(db_2021_1$pmcid))
db_2021_2$pmcid_ <- gsub("PMC", "", as.character(db_2021_2$pmcid))
db_2022$pmcid_ <- gsub("PMC", "", as.character(db_2022$pmcid))

###Now, we make five folders for xml format articles:
dir.create(c("pmc_ 2020_1", "pmc_2020_2", "pmc_2021_1", "pmc_2021_2", "pmc_2022"))

### Next, we download xmls in format accessible with metareadr.
### To skip errors (i.e., The metadata format 'pmc' is not supported by the
### item or by the repository.), first define a new function:
skipping_errors <- function(x) tryCatch(mt_read_pmcoa(x), error = function(e) e)

### Next, we download xmls in format accessible with rtransparent:
setwd("pmc_2020_1")
sapply(db_2020_1$pmcid_, skipping_errors)

setwd("../pmc_2020_2")
sapply(db_2020_2$pmcid_, skipping_errors)

setwd("../pmc_2021_1")
sapply(db_2021_1$pmcid_, skipping_errors)

setwd("../pmc_2021_2")
sapply(db_2021_2$pmcid_, skipping_errors)

setwd("../pmc_2022")
sapply(db_2022$pmcid_, skipping_errors)

### Now we run rtransparent (do for all others as well (f2-f5)):
filepath1 = dir(pattern=glob2rx("PMC*.xml"))

results_table_all_f1 <- sapply(filepath1, rt_all_pmc)

results_table_data_f1 <- rt_data_code_pmc_list(
        filepath1,
        remove_ns=F,
        specificity = "low")

### A list is created now. We should convert this list to a dataframe:
df1 <- data.table::rbindlist(results_table_all_f1, fill = TRUE)

### Merge data sharing results to database file:
setwd('..')
db <- db[!duplicated(db[, 4]),]
opendata <- merge(db_2020_1, results_table_data_f1, by = "pmid") %>% merge(df1)

### Selecting only needed columns:
opendata <- opendata %>% select(pmid,
                                pmcid,
                                doi,
                                title,
                                authorString,
                                journalTitle,
                                journalIssn,
                                publisher,
                                firstPublicationDate,
                                journalVolume,
                                pageInfo,
                                issue,
                                pubType,
                                citedByCount,
                                is_coi_pred,
                                coi_text,
                                is_fund_pred,
                                fund_text,
                                is_register_pred,
                                register_text,
                                is_open_data,
                                open_data_category,
                                is_open_code,
                                open_data_statements,
                                open_code_statements)

### Adding JIFs:
#### First, importing jif file:
jif <- read.csv(here("data", "covid_transparency_jif2020.csv"))

#### Merging it with opendata:
opendata$journalIssn <- toupper(opendata$journalIssn)

opendata <- opendata %>% separate(journalIssn, c("issn1", "issn2"), sep = "; ")

opendata <- merge(x = opendata, 
                  y = jif[, c("issn", "jif2020")],
                  by.x = "issn1",
                  by.y = "issn",
                  all.x = TRUE)

opendata <- merge(x = opendata, 
                  y = jif[, c("issn", "jif2020")],
                  by.x = "issn2",
                  by.y = "issn",
                  all.x = TRUE)

opendata <- opendata %>% distinct(pmid, .keep_all = TRUE) #Removing duplicates

opendata <- opendata %>% 
        mutate(jif2020 = coalesce(jif2020.x, jif2020.y)) %>%
        subset(select = -c(jif2020.x, jif2020.y))

opendata$jif2020 <- as.numeric(opendata$jif2020)

### Adding SCImago:
scimago <- read.csv(here("data", "covid_transparency_scimagojr_2020.csv"), sep = ";")

scimago <- scimago %>% separate(Issn, c("issn1", "issn2"))

scimago$issn1 <- gsub("(\\d{4})(\\d{4})$", "\\1-\\2", scimago$issn1)
scimago$issn2 <- gsub("(\\d{4})(\\d{4})$", "\\1-\\2", scimago$issn2)

#### Merging with opendata:
opendata <- merge(x = opendata, 
                  y = scimago[, c("issn1", "issn2", "SJR", "H.index", "Publisher")],
                  by.x = "issn1",
                  by.y = "issn1",
                  all.x = TRUE)

opendata <- merge(x = opendata, 
                  y = scimago[, c("issn1", "issn2", "SJR", "H.index", "Publisher")],
                  by.x = "issn2.x",
                  by.y = "issn1",
                  all.x = TRUE)

opendata <- merge(x = opendata, 
                  y = scimago[, c("issn1", "issn2", "SJR", "H.index", "Publisher")],
                  by.x = "issn1",
                  by.y = "issn2",
                  all.x = TRUE)

opendata <- opendata %>% distinct(pmid, .keep_all = TRUE) #Removing duplicates

opendata <- opendata %>% 
        mutate(sjr2020 = coalesce(SJR, SJR.x, SJR.y),
               scimago_publisher = coalesce(Publisher.x, Publisher.y, Publisher),
               scimago_hindex = coalesce(H.index.x, H.index.y, H.index)) %>%
        subset(select = -c(SJR.x, SJR.y, SJR, X.1, issn2.y, issn2.x, issn1.y, H.index.x, H.index.y, H.index, Publisher.x, Publisher.y, Publisher))

#### Changing commas to dots:
opendata$sjr2020 <- gsub(",", ".", as.character(opendata$sjr2020))

#### And changing type to numeric:
opendata$sjr2020 <- as.numeric(opendata$sjr2020)

opendata <- opendata[, c(2:7, 1, 27, 8:26, 28:30)]



### Random sample of search results for validation of our approach (100 refs):
set.seed(100)
randomsample1 <- db1[sample(nrow(db1), 100), ]


