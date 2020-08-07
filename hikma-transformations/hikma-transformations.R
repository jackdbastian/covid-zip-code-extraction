## Jack Bastian

## This script transforms the hikma health county level policy data into the format used for policy data in HHS Protect.

library(tidyverse)

hikma <- read_csv("https://raw.githubusercontent.com/hikmahealth/covid19countymap/gh-pages/all_county_policies.csv") 
# Historical dataset showing when policies begin (TRUE) and end (FALSE).

hikma_active <- read_csv("https://raw.githubusercontent.com/hikmahealth/covid19countymap/gh-pages/county_policies.csv") 
# Active policy dataset, only shows the current state of policies (whether currently enacted (TRUE), ended (FALSE) or NA)


# Active Policies Transformations -------------------------------------------

  ## The resulting "hikma_active_reformat" data frame includes all 'school', 'work' and 'event' policies 
  ## in the "hikma_active" df which were marked TRUE (i.e. currently in place).

hikma_active_reformat<-
  bind_rows(
    hikma_active %>% 
      transmute(County = fips, Start_Date = school_date, Source = school_url, `Childcare (K-12)` = school) %>% 
      gather("Policy_Type", "status", 4),
    hikma_active %>% 
      transmute(County = fips, Start_Date = work_date, Source = work_url, `Non-Essential Businesses` = work) %>% 
      gather("Policy_Type", "status", 4),
    hikma_active %>% 
      transmute(County = fips, Start_Date = event_date, Source = event_url, Entertainment = event) %>% 
      gather("Policy_Type", "status", 4)) %>% 
  mutate(Policy_Level = "county", Created_by = "Hikma", End_Date = NA, Name = "Hikma", 
         Policy_Details = case_when(Policy_Type == "Childcare (K-12)" ~ "Sourced from Hikma dataset. All schools are closed in the county",
                                    Policy_Type == "Non-Essential Businesses" ~ "Sourced from Hikma dataset. All “non-essential workplaces” are closed in the county",
                                    Policy_Type == "Entertainment" ~ "Sourced from Hikma dataset. Public events and gatherings of a certain size are restricted")) %>% 
  filter(status == TRUE) %>% 
  select(County, Start_Date, End_Date, Policy_Level, Policy_Type, Policy_Details, Source, Name, Created_by) %>% 
  arrange(County)


# Ended Policies Transformations ----------------------------------------

  ## "event_ended" contains all 'event' policies in the "hikma" df which have both a TRUE and a FALSE entry 
  ## (indicating that the policy was enacted and ended).

  ## "ns_biz_ended" contains all 'work' policies in the "hikma" df which have both a TRUE and a FALSE entry   
  ## (indicating that the policy was enacted and ended).

event_ended <- 
  hikma %>% 
  transmute(County = fips, date = event_date, Source = event_url, Entertainment = event) %>% 
  gather("Policy_Type", "status", 4) %>% 
  filter(!is.na(status)) %>% 
  group_by(County) %>% 
    filter(n() == 2) %>%
  count(status) %>%
    filter(n == 1) %>% 
  summarise(County = first(County)) %>% 
  left_join(hikma %>% 
              transmute(County = fips, date = event_date, Source = event_url, Entertainment = event) %>% 
              gather("Policy_Type", "status", 4) %>% 
              filter(!is.na(status)), by = "County") %>% 
  group_by(County) %>% 
  summarise(Start_Date = min(date),
            End_Date = max(date),
            Policy_Type = first(Policy_Type),
            Start_Source = first(Source),
            End_Source = last(Source)
            ) %>% 
  filter(!is.na(Start_Date), !is.na(End_Date)) %>% 
  unite(Source, c(Start_Source, End_Source), sep = "; ") %>% 
  mutate(Policy_Details = "Sourced from Hikma dataset. Public events and gatherings of a certain size are restricted",
         Name = "Hikma",
         Created_by = "Hikma",
         Policy_Level = "county") %>% 
  select(County, Start_Date, End_Date, Policy_Level, Policy_Type, Policy_Details,
         Source, Name, Created_by)

ns_biz_ended <- 
  hikma %>% 
  transmute(County = fips, date = work_date, Source = work_url, `Non-Essential Businesses` = work) %>% 
  gather("Policy_Type", "status", 4) %>% 
  filter(!is.na(status)) %>% 
  group_by(County) %>% 
    filter(n() == 2) %>%
  count(status) %>%
    filter(n == 1) %>% 
  summarise(County = first(County)) %>% 
  left_join(hikma %>% 
              transmute(County = fips, date = work_date, Source = work_url, `Non-Essential Businesses` = work) %>% 
              gather("Policy_Type", "status", 4) %>% 
              filter(!is.na(status)), by = "County") %>% 
  group_by(County) %>% 
  summarise(Start_Date = min(date),
            End_Date = max(date),
            Policy_Type = first(Policy_Type),
            Start_Source = first(Source),
            End_Source = last(Source)
            ) %>% 
  filter(!is.na(Start_Date), !is.na(End_Date)) %>% 
  unite(Source, c(Start_Source, End_Source), sep = "; ") %>% 
  mutate(Policy_Details = "Sourced from Hikma dataset. All “non-essential workplaces” are closed in the county",
         Name = "Hikma",
         Created_by = "Hikma",
         Policy_Level = "county") %>% 
  select(County, Start_Date, End_Date, Policy_Level, Policy_Type, Policy_Details,
         Source, Name, Created_by)


# Assembling Master Dataset -----------------------------------------------

hikma_active_and_ended <- 
  bind_rows(hikma_active_reformat, event_ended, ns_biz_ended) %>% 
  filter(!is.na(Start_Date), !is.na(Source))

hikma_active_and_ended %>% write_csv("output-data/hikma-all-county-policies.csv")
