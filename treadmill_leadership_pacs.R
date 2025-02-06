# 2024 Leadership PAC Analysis

# Packages 
library(tidyverse)

# Data

## Data - All leadership PAC contributions
## Link - TK

raising_118 <- read_csv("pac_data/committee_summary_2024.csv")

## Data - All leadership PACs
## Link - TK
leadership_pacs <- read_csv("pac_data/leadership_pacs_118.csv")

# Data for only leadership PACs
leadership_pac_totals <-
  raising_118 |> 
  filter(CMTE_ID %in% c(leadership_pacs$LEADERSHIP_PAC_1_ID, 
                        leadership_pacs$LEADERSHIP_PAC_2_ID, 
                        leadership_pacs$LEADERSHIP_PAC_3_ID, 
                        leadership_pacs$LEADERSHIP_PAC_4_ID))

# Checking FEC final filings for missing data

data_check <- 
  raising_118 |>
  mutate(CVG_END_DT = ymd(CVG_END_DT)) |> 
  filter(CVG_END_DT == "2024-12-31",
         CMTE_ID %in% c(leadership_pacs$LEADERSHIP_PAC_1_ID, 
                        leadership_pacs$LEADERSHIP_PAC_2_ID, 
                        leadership_pacs$LEADERSHIP_PAC_3_ID, 
                        leadership_pacs$LEADERSHIP_PAC_4_ID))

missing_data_1 <-
  leadership_pacs |> 
  filter(!is.na(LEADERSHIP_PAC_1_ID),
         !LEADERSHIP_PAC_1_ID %in% data_check$CMTE_ID) |> 
  select(LEADERSHIP_PAC_1_ID)


missing_data_2 <-
  leadership_pacs |> 
  filter(!is.na(LEADERSHIP_PAC_2_ID),
         !LEADERSHIP_PAC_2_ID %in% data_check$CMTE_ID) |> 
  select(LEADERSHIP_PAC_2_ID)


missing_data_3 <-
  leadership_pacs |> 
  filter(!is.na(LEADERSHIP_PAC_3_ID),
         !LEADERSHIP_PAC_3_ID %in% data_check$CMTE_ID) |> 
  select(LEADERSHIP_PAC_3_ID)

missing_data_4 <-
  leadership_pacs |> 
  filter(!is.na(LEADERSHIP_PAC_4_ID),
         !LEADERSHIP_PAC_4_ID %in% data_check$CMTE_ID) |> 
  select(LEADERSHIP_PAC_4_ID)


all_missing_data <-
  bind_rows(missing_data_1,
            missing_data_2,
            missing_data_3,
            missing_data_4)

rm(missing_data_1,
   missing_data_2,
   missing_data_3,
   missing_data_4)

raising_118 <-
  raising_118 |> 
  select(CMTE_ID, TTL_RECEIPTS, INDV_UNITEM_CONTB)

## STAT: PORTION OF MEMBERS OF 118TH CONGRESS WHO HAD A LEADERSHIP PAC
leadership_pacs |> 
  filter(VOTING_MEMBER == "Y") |> 
  summarize(count = n(),
            .by = LEADERSHIP_PAC_1)


## STAT: Total amount raised
total <- 
  raising_118 |> 
  filter(CMTE_ID %in% c(leadership_pacs$LEADERSHIP_PAC_1_ID, 
                        leadership_pacs$LEADERSHIP_PAC_2_ID, 
                        leadership_pacs$LEADERSHIP_PAC_3_ID, 
                        leadership_pacs$LEADERSHIP_PAC_4_ID)) |> 
  summarize(total_raised = sum(TTL_RECEIPTS, na.rm = TRUE)) 

total

## STAT: Small dollar donations
small_dollar <-
  raising_118 |> 
  filter(CMTE_ID %in% c(leadership_pacs$LEADERSHIP_PAC_1_ID, 
                        leadership_pacs$LEADERSHIP_PAC_2_ID, 
                        leadership_pacs$LEADERSHIP_PAC_3_ID, 
                        leadership_pacs$LEADERSHIP_PAC_4_ID)) |> 
  summarize(small_dollars = sum(INDV_UNITEM_CONTB, na.rm = TRUE))

small_dollar
            
## STAT: Portion of small dollar donors
small_dollar / total

## STAT: Median, by member, not by leadership PAC

# Members with ONE leadership PAC
leadership_pac1_join <- 
  raising_118 |>
  rename("LEADERSHIP_PAC_1_ID" = "CMTE_ID")

leadership_pac1_totals <- 
  left_join(leadership_pacs, leadership_pac1_join) |> 
  rename("LEADERSHIP_PAC_1_RECEIPTS" = "TTL_RECEIPTS") |> 
  select(NAME, LEADERSHIP_PAC_1_ID, LEADERSHIP_PAC_1_RECEIPTS)

# Members with TWO leadership PACs
leadership_pac_2_join <- 
  raising_118 |> 
  rename("LEADERSHIP_PAC_2_ID" = "CMTE_ID")

leadership_pac2_totals <- 
  left_join(leadership_pacs, leadership_pac_2_join) |> 
  rename("LEADERSHIP_PAC_2_RECEIPTS" = "TTL_RECEIPTS") |> 
  select(NAME, LEADERSHIP_PAC_2_ID, LEADERSHIP_PAC_2_RECEIPTS)

# Members with THREE leadership PACs
leadership_pac_3_join <- 
  raising_118 |> 
  rename("LEADERSHIP_PAC_3_ID" = "CMTE_ID")

leadership_pac3_totals <- 
  left_join(leadership_pacs, leadership_pac_3_join) |> 
  rename("LEADERSHIP_PAC_3_RECEIPTS" = "TTL_RECEIPTS") |> 
  select(NAME, LEADERSHIP_PAC_3_ID, LEADERSHIP_PAC_3_RECEIPTS)


# Members with FOUR leadership PACs
leadership_pac_4_join <- 
  raising_118 |> 
  rename("LEADERSHIP_PAC_4_ID" = "CMTE_ID")

leadership_pac4_totals <- 
  left_join(leadership_pacs, leadership_pac_4_join) |> 
  rename("LEADERSHIP_PAC_4_RECEIPTS" = "TTL_RECEIPTS") |> 
  select(NAME, LEADERSHIP_PAC_4_ID, LEADERSHIP_PAC_4_RECEIPTS)

# Joining all leadership PACs to members
joinA <- 
  left_join(
    leadership_pac1_totals, leadership_pac2_totals,
    by = "NAME")

joinB <- 
  left_join(
    joinA, leadership_pac3_totals,
    by = "NAME")

joinC <- 
  left_join(
    joinB, leadership_pac4_totals,
    by = "NAME")

# All leadership PAC raising
total_leadership_pac <-
  joinC |> 
  summarize(total_receipts = 
              sum(LEADERSHIP_PAC_1_RECEIPTS, 
                  LEADERSHIP_PAC_2_RECEIPTS, 
                  LEADERSHIP_PAC_3_RECEIPTS,
                  LEADERSHIP_PAC_4_RECEIPTS,
                  na.rm = TRUE),
            .by = NAME)

rm(leadership_pac1_join,
   leadership_pac_2_join,
   leadership_pac_3_join,
   leadership_pac_4_join,
   leadership_pac1_totals,
   leadership_pac2_totals,
   leadership_pac3_totals,
   leadership_pac4_totals,
   joinA,
   joinB,
   joinC)

# Median
total_leadership_pac |> 
  summarize(
    median = median(total_receipts, na.rm = TRUE))

