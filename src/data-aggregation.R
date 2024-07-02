#### Call in data ####
##==================##
source("src/data-aggregation_functions.R")
##===================================================================##
#### Case Host - needed for host species, lat lon, symptoms, dates ####
##===================================================================##

ch <- read.csv(file = file.path(data_dir, "PSHB_case_hosts_29022024.csv")) ##CaseHosts

names(ch) <- tolower(names(ch))
dim(ch) ## 377191     42, 404685     42

unique(ch$treatment)
# 1] "", "1;#Tree removed", "2;#Limb or limbs removed", "5;#Treated at Sampling", "4;#SM - Limb or limbs removed", "3;#SM - Tree Removed"   

table(ch$treatment)
#                       1;#Tree removed                  2;#Limb or limbs removed          3;#SM - Tree Removed         4;#SM - Limb or limbs removed   5;#Treated at Sampling 
# 404506               99                                49                                 9                            17                             5 

# Types of identification codes
unique(ch$case)
unique(ch$casetxt)
unique(ch$hosttype.id)
unique(ch$id)

range(ch2$ch_id) ##15 - 456597, therefore use "id" for merging

length(ch$id) ##404685
length(ch$id[is.na(ch$id)]) ##0, apparently there are no NA's
ch$id[ch$id == ""] ## 0 apparently there is full coverage
unique(ch$id)

## Note, as indicated below "case" and "casetxt" are comparable, therefore only include one
head(data.frame(ch$case,
           ch$casetxt,
           ch$hosttype.id,
           ch$id))

# Types of date entry
unique(ch$created) ## use as date
unique(ch$auditdate) ## not complete
unique(ch$modified) ## keep - usefull

## Make a dates created and modified the consistent format
ch$created <- as.Date(x = ch$created, format = "%d/%m/%Y %H:%M")
ch$modified <- as.Date(x = ch$modified, format = "%d/%m/%Y %H:%M")

# Find intersects
intersect(names(ch), names(df)) ##"descriptionmultiplier" "samplecode"

## Host type (get species)
length(unique(ch$hosttype)) ##641

## Get the lat lon ##
# Extract all characters including the first number until the first space #### KEEP GOLD
ch$lon <- str_extract(ch$spatial, "\\d\\S+")
ch$lat <- str_extract(ch$spatial, "-[\\S\\d.,]+?(?=(\\s|,|\\)))|$")

## select vectors for merging
ch2 <- data.frame(
  ch_casetxt = ch$casetxt,
  ch_hosttype.id = ch$hosttype.id,
  ch_host_species = ch$hosttype,
  ch_id = ch$id,
  ch_date_created = ch$created,
  ch_date_modified = ch$modified,
  ch_symptoms = ch$symptoms,
  ch_lon = ch$lon,
  ch_lat = ch$lat)

##======================================##
#### Host Sample - needed for finding ####
##======================================##

# hs <- read.csv(file = "PSHB_HostSample_20112023.csv") ##CaseHosts
hs <- read.csv(file = file.path(data_dir, "PSHB_HostSample_29022024.csv")) ##CaseHosts
names(hs) <- tolower(names(hs))
st(hs) ## 7841   15 8564  15
head(hs)

## check range to decide how to merge
range(as.numeric(hs$id), na.rm = T)## 1 8965 1 9696
hs$casehost <- as.numeric(gsub("^(\\d+);#.*", "\\1", hs$casehost))
range(as.numeric(hs$casehost), na.rm = T)## 15 427107 15 456420: Note merge with case hose using hs_casehost derived from this

hs$casehost[is.na(hs$casehost)] ## 0, apparently there are no casehost that are NA
hs$casehost[hs$casehost == ""]


# Use str_extract to extract the first string of numbers
hs$casehost <- str_extract(hs$casehost, "^(\\d+)")
unique(hs$sampleid)
unique(hs$id)

## Check for contiguous id codes (just use sampleid)
data.frame(hs$casehost, hs$findsampleid, hs$sampleid, hs$id)

# Types of date entry
tail(unique(hs$inspecteddate)) ## use as date, equal to created????
tail(unique(hs$modified)) ## keep - usefull
unique((hs$created))

# pest presence/absence
hs$targetpestpresence
unique(hs$targetpestpresence)
# > unique(df$finding)
# [1] "Pending"                    "DDLS - NEG"                 "Triage - NEG"               "SYMPTOMATIC"                "DDLS - POS PCR"             "DDLS - Inconclusive"
# [7] "DDLS - POS MOR"             "RBL - NEG"                  "Triage - prior POS Discard"

unique(hs$targetpestpresence)

# Remove the numeric part and semicolon
hs$targetpestpresence <- gsub("^\\d+;#", "", hs$targetpestpresence)


## Make blank unknown
hs$targetpestpresence[hs$targetpestpresence == ""] <- "unknown"

# # Find intersects
# intersect(names(df), names(hs)) ##

## make df for merging
hs2 <- data.frame(hs_casehost = hs$casehost,
                  hs_sampleid = hs$sampleid,
                  hs_id = hs$id,
                  hs_finding = hs$targetpestpresence)

## Add in sample type for hosts (assumed to mean plant material checked in the lab)
hs2$sampletype = "host_sample"

head(hs2)

##===================================================##
#### Case trap - needed for trap lat lon and dates ####
##===================================================##

ct <- read.csv(file = file.path(data_dir,"PSHB_CaseTrap_29022024.csv")) ##CaseHosts
names(ct) <- tolower(names(ct))
st(ct) ## (7217 24) 8021 26
head(ct, 50)

## fist make everything date, the date format
# List of date columns
date_columns <- c("deploymentdate", "lurereplacementdate", "enddate", "created", "modified")

# Convert those columns to Date format
ct[date_columns] <- lapply(ct[date_columns], function(x) as.Date(x, format="%d/%m/%Y"))

## Find the most recent enddate for a "ended" lure
## note, you cannot easily base this off the enddate, or the lure replacement date, as there are lure replacement dates in the future

## Filter to only include the sample with an enddate
ct_with_enddate <- ct[!is.na(ct$enddate), ] #c("id", "deploymentdate", "lurereplacementdate", "enddate", "trapid")]
dim(ct_with_enddate) #4685

## now order by most recent lure change date
ct_with_enddate[order(ct_with_enddate$lurereplacementdate, decreasing = T), 
                c("id", "deploymentdate", "lurereplacementdate", "enddate", "trapid")][1:20, ]


ct_with_enddate[order(ct_with_enddate$enddate, decreasing = T), 
                c("id", "deploymentdate", "lurereplacementdate", "enddate", "trapid")][1:20, ]

## try to filter based on endreason
dim(ct[ct$endreason != "", ]) ##4681

#### note, there are less remaining traps using "endreason", that "enddate". 
##Therefor endreason is more stringent, use end reason. Add it to the main db

# Types of identification
unique(ct$casetxt)
unique(ct$trapid )
unique(ct$case) ## Comparable with casetxt, therefore don't use
unique(ct$id)

## Use ct$id for merging
range(ct$id) ##2 9459
dim(ct) ##8021 26
ct$id[is.na(ct$id)] ## Apparently there are no NA's
ct$id[ct$id == ""]

# Types of date entry
unique(ct$deploymentdate) ## use as date
unique(ct$enddate) ## not complete
unique(ct$lurereplacementdate)

## location
ct$spatial

# Extract all characters including the first number until the first space #### KEEP GOLD
ct$lon <- str_extract(ct$spatial, "\\d\\S+")
ct$lat <- str_extract(ct$spatial, "-[\\S\\d.,]+?(?=(\\s|,|\\)))|$")

## select vectors for merging

unique(ct$deploymentdate) ## use as date
unique(ct$enddate) ## not complete
unique(ct$lurereplacementdate)

## make dataframe for merging
ct2 <- data.frame(ct_casetxt = ct$casetxt,
                  ct_trapid = ct$trapid,
                  ct_id = ct$id,
                  ct_deploy_date = ct$deploymentdate,
                  ct_lure_date = ct$lurereplacementdate,
                  ct_enddate = ct$enddate,
                  ct_endreason = ct$endreason,
                  ct_lon = ct$lon,
                  ct_lat = ct$lat)

##===========================================================================================##
#### TrapSample - needed to link trapID with sampleID, trap_finding and get inspected date ####
##===========================================================================================##

ts <- read.csv(file.path(data_dir,"PSHB_Trap_Sample_29022024.csv"))
names(ts) <- tolower(names(ts))
st(ts) ## (116981 24) 135467 18
head(ts, 10)

# Types of identification
unique(ts$sampleid)
unique(ts$casetrap) ## equal to casetrap id
unique(ts$casetrap.findtrapid ) ## equal to trap_id
unique(ts$casetxt) ## Comparable with case
unique(ts$id)
range(ts$casetrap)

## Do to remove the duplicates
ts$casetrap <- as.numeric(gsub("^(\\d+);#.*", "\\1", ts$casetrap))
range(ts$casetrap) ##2 - 9366, therefor use case trap to merge
ts$casetrap[is.na(ts$casetrap)] ## Apparently there are no NA's
ts$casetrap[ts$casetrap == ""] ## Apparently there are no gaps


# Types of date entry
unique(ts$inspecteddate) ## use as date
unique(ts$created) ## not complete
unique(ts$modified)

## Make a dates created and modified the consistent format
ts$created <- as.Date(x = ts$created, format = "%d/%m/%Y %H:%M")
ts$modified <- as.Date(x = ts$modified, format = "%d/%m/%Y %H:%M")

## Trap finding
unique(ts$targetpestpresence)
# [1] ""                       "2;#DDLS - NEG"          "5;#DDLS - POS PCR"      "3;#DDLS - Inconclusive" "4;#Triage - NEG"

# Remove the numeric part and semicolon
ts$targetpestpresence <- gsub("^\\d+;#", "", ts$targetpestpresence)

## Make blank unknown
ts$targetpestpresence[ts$targetpestpresence == ""] <- "unknown"

## Make blank end reason and endate, for 
# ts$endreason <- ""
# ts$enddate <- ""

ts2 <- data.frame(ts_sampleid = ts$sampleid,
                  ts_trapid = ts$casetrap.findtrapid, ## equal to trap_id
                  ts_casetxt = ts$casetxt, ## Comparable with case
                  ts_id = ts$id,
                  ts_casetrap = ts$casetrap, ## CaseTrap number
                  ts_inspecteddate = ts$inspecteddate, ## use as date
                  # enddate = ts$enddate,
                  # endreason = ts$endreason,
                  ts_created = ts$created, ## not complete
                  ts_modified = ts$modified,
                  ts_finding = ts$targetpestpresence)


##===========##
#### Merge ####
##===========##

dim(ch2)   #377191   9, 404685      9
names(ch2)
head(ch2)

dim(hs2)   #7841     5, 8564 5
names(hs2)
head(hs2)

dim(ct2)   #7217     8, 8021 9
names(ct2)
head(ct2)

dim(ts2)   #116981   9, 135467 9
names(ts2)
head(ts2)

##=====================================##
#### Merge case host and host sample ####
##=====================================##

range(as.numeric(ch2$ch_id)) ## 15 456597, Note merge with host sample using ch_id derived from this
range(as.numeric(hs2$hs_casehost)) ## 15  456420

ch_hs <- merge(x = ch2, y = hs2, by.x = "ch_id", by.y = "hs_casehost", all = T)
dim(ch_hs) ## 405827 13 (Use this - close enough, error caused by updating inconsistency - keep)+
## There are no NA's or gaps in the id code
ch_hs$ch_id[is.na(ch_hs$ch_id)] ##0
ch_hs$ch_id[ch_hs$ch_id == ""] ##0

range(ch_hs$ch_id) ##15 456597
length(ch_hs$ch_id[is.na(ch_hs$ch_id)]) ##0
length(ch_hs$ch_id[ch_hs$ch_id == ""]) ##0
unique(ch_hs$ch_id)

# Update sample type to correct for case host entries were no sample was collected - possible field negative
ch_hs[is.na(ch_hs$sampletype), "sampletype"] <- "no_sample_collected"

## Add enddate and endreason, so you can row bind later
ch_hs$enddate <- ""
ch_hs$endreason <- ""

head(ch_hs, 20)

### Should be positive number of plants detected in total
ch_hs[is.na(ch_hs$sampletype), "sampletype"]

##=====================================##
#### Merge case trap and trap sample ####
##=====================================##

range(as.numeric(ct$id), na.rm = T) ## 2 8651, 2 9459
range(as.numeric(ts$casetrap), na.rm = T) ## 2 8569, 9366

dim(ct2) #8021    8
dim(ts2) #135467      9

ct_ts <- merge(x = ct2, y = ts2, by.x = "ct_id", by.y = "ts_casetrap", all = T)
dim(ct_ts) ##117397  , 135889     17 ## Perfect match, yay, merge trapsample and case trap
names(ct_ts)

## Add sample type to ct_ts
ct_ts$sampletype <- "trap_sample"

## Add host_species as "trap" for rbind consistency
ct_ts$host_species <- "trap"

## Add symptoms as "no_symptoms_trap" for rbind consistency
ct_ts$symptoms <- "no_symptoms_trap"

## Add ch_hs end date and endreason, so you can merge latter
ch_hs$enddate <- ""
ch_hs$endreason <- ""

head(ch_hs)

##===========================##
#### merge ch_hs and ct_ts ####
##===========================##

nrow(ch_hs) ##378192, 405827
nrow(ct_ts) ##117397, 135889

nrow(ch_hs)+nrow(ct_ts) ## 495589, 541716
names(ch_hs)
names(ct_ts) ##117397

length(unique(ch_hs$hs_sampleid)) ##7826,8550
length(unique(ct_ts$ts_sampleid)) ##116069, 134479

#### rbind ch_hs and ct_ts ####
### I don;t know how to merge ch_hs and ct_ts, therefore rbind
## Format for rbind
head(ch_hs)
dim(ch_hs) ##405827     15

ch_hs1 <- ch_hs[, c("ch_id", "ch_host_species", "ch_lat", "ch_lon", "ch_symptoms", "ch_date_created", "ch_date_modified", "hs_finding", "sampletype", "endreason", "enddate")]

names(ch_hs1) <- c("id","host_species", "lat", "lon", "symptoms", "date_created", "date_modified", "finding", "sampletype", "endreason", "enddate")
head(ch_hs1) ## Head looks appropriate
dim(ch_hs1) ## Note correct dimensions - 405827 11
dim(ch_hs1[is.na(ch_hs1$id), ]) ## ID has no NA's 0     11

ct_ts1 <- ct_ts[, c("ct_id", "host_species", "ct_lat", "ct_lon", "symptoms", "ts_created", "ts_modified", "ts_finding", "sampletype", "ct_endreason", "ct_enddate")]
names(ct_ts1)
dim(ct_ts1) ##135889     11
names(ct_ts1) <- c("id",  "host_species", "lat", "lon", "symptoms", "date_created", "date_modified", "finding", "sampletype", "endreason", "enddate")
head(ct_ts1)


df <- rbind(ch_hs1, ct_ts1) ##541716      11
dim(df)
head(df)

write.csv(df, file.path(data_dir,"df_pshb_containment_29022024.csv"), row.names = F)

#### Read in df for mapping ####

df <- read.csv(file.path(data_dir,"df_pshb_containment_29022024.csv"))
dim(df) ##541716     10
head(df)