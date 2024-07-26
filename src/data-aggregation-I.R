library(dplyr)
library(lubridate)
library(stringr)

# define the data directory
data_dir <- file.path(Sys.getenv("DATA_PATH"), "PSHB")

# read the data
ts <- read.csv(file.path(data_dir,"PSHB_Trap_Sample_29022024.csv"))
names(ts) <- make.names(names(ts))
ts <- ts %>% select(trapID = CaseTrap.FindTrapID, 
                    date = InspectedDate, 
                    sampleID = SampleID, 
                    abundance = Abundance) %>%
              mutate(date = dmy(date))

ct <- read.csv(file = file.path(data_dir,"PSHB_CaseTrap_29022024.csv")) 
names(ct) <- make.names(names(ct))
ct <- ct %>% select(spatial = Spatial, trapID = TrapID) %>%
              filter(trapID != "Unknown") %>%
              mutate(lat = as.numeric(str_extract(spatial, "-[\\S\\d.,]+?(?=(\\s|,|\\)))|$")),
                     lon = as.numeric(str_extract(spatial, "\\d\\S+"))) %>%
              select(-spatial) %>%
              distinct(trapID, .keep_all = TRUE)


od <- left_join(ts, ct) %>%
      filter(!is.na(abundance))

