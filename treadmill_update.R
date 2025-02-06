
# 2024 Congressional Treadmill Fundraising Analysis

# Packages
library(tidyverse)

# Data

## Data - All campaign committee filings
## Link - https://github.com/miaminkin/IO-Congressional-Fundraising-Treadmill-2025/blob/main/data/congressional_treadmill.csv

all_filings <- 
  read_csv("data/congressional_treadmill.csv") |> 
  select(
    committee_id,
    committee_name,
    office,
    cycle,
    amendment_indicator,
    coverage_start_date,
    coverage_end_date,
    report_year,
    report_type,
    total_receipts,
    html_url)
  
## Data -  Sitting members of the 118th Congress
## Link -  https://github.com/miaminkin/IO-Congressional-Fundraising-Treadmill-2025/blob/main/data/members.csv

members <- 
  read_csv("data/members.csv") |> 
  select(
   -"...13",
    -"...14")

# Data cleaning

## Cleaning the committee data to only include filings from 2024 by those who
## are members of the 118th Congress. 

member_filings <- 
  all_filings |>
  filter(
    report_year %in% c("2023", "2024"),
    cycle == "2024",
    amendment_indicator == "N",
    committee_id %in% members$COMMITTEE_ID) 

## Creates a data frame of the total amount raised by each campaign committee

total_raised <- 
  member_filings |> 
  summarize(
    TOTAL_RECEIPTS = sum(total_receipts, na.rm = TRUE),
    .by = c("committee_id", "committee_name")) |> 
  rename(
    "COMMITTEE_ID" = "committee_id",
    "COMMITTEE_NAME" = "committee_name")

## Filters total amount raised by campaign committees to only include sitting members of Congress

members_raised <- 
  left_join(members, total_raised) |> 
  
  ## Filter to exclude non-voting members of Congress + vacant seats
  
  filter(
    !VOTING_MEMBER == "N",
    !NAME == "VACANT")

# Summary statistics

## STAT 1: TOTAL AMOUNT OF $ RAISED BY ALL HOUSE MEMBERS RUNNING FOR REELECTION COMBINED

members_raised |> 
  summarize(
    TOTAL_RAISED = sum(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = c(CHAMBER, RETIREMENT)) |> 
  filter(
    RETIREMENT == "N",
    CHAMBER == "House")

## STAT 2: MEDIAN AMOUNT OF $ RAISED BY A HOUSE MEMBER RUNNING FOR REELECTION IN NOVEMBER 2024

members_raised |> 
  summarize(
    MEDIAN_RAISED = median(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = c(CHAMBER, RETIREMENT)) |> 
  mutate(
    PER_DAY = (MEDIAN_RAISED / 731)) |> 
  filter(CHAMBER == "House")

## STAT 3: MEDIAN AMOUNT RAISED BY A FRESHMAN MEMBER OF THE 118TH CONGRESS

members_raised |> 
  summarize(
    MEDIAN_RAISED = median(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = c(EXPERIENCE, CHAMBER))|> 
  mutate(
    PER_DAY = (MEDIAN_RAISED / 731))

## STAT 4: MEDIAN AMOUNT RAISED BY A HOUSE INCUMBENT RUNNING FOR REELECTION IN 2024 IN A CPR TOSSUP RACE
  
members_raised |> 
  summarize(
    MEDIAN_RAISED = median(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = c(CHAMBER, EXPERIENCE, CPR_TOSSUP)) |> 
  mutate(
    PER_DAY = (MEDIAN_RAISED / 731)) |> 
  filter(CHAMBER == "House",
         EXPERIENCE == "incumbent")

## STAT 5: TOTAL AMOUNT OF $ RAISED BY ALL SENATORS RUNNING FOR REELECTION IN 2024 COMBINED

members_raised |> 
  summarize(
    TOTAL_RAISED = sum(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = c(CHAMBER, RETIREMENT, IN_CYCLE)) |> 
  filter(
    RETIREMENT == "N",
    CHAMBER == "Senate")
  

## STAT 6: MEDIAN AMOUNT OF $ RAISED BY A SITTING SENATOR RUNNING FOR REELECTION IN 2024

members_raised |> 
  filter(CHAMBER == "Senate") |> 
  summarize(
    MEDIAN_RAISED = median(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = c(CHAMBER, RETIREMENT, IN_CYCLE)) |> 
  mutate(
    PER_DAY = (MEDIAN_RAISED / 731))

## STAT 7: MEDIAN AMOUNT OF $ RAISED BY A SITTING SENATOR NOT UP FOR REELECTION IN 2024

members_raised |>
  filter(CHAMBER == "Senate",
         !IN_CYCLE == "Y") |> 
  summarize(
    MEDIAN_RAISED = median(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = RETIREMENT) |>  
  mutate(PER_DAY = (MEDIAN_RAISED / 731))


members_raised |>
  filter(CHAMBER == "Senate") |> 
  mutate(INCLUDE = 
           case_when(IN_CYCLE == "Y" ~ "exclude",
                     .default = "include")) |> 
  summarize(
    MEDIAN_RAISED = median(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = INCLUDE) |>  
  mutate(PER_DAY = (MEDIAN_RAISED / 731))

## STAT 8: Portion of total raised raised by top 5 Senators

members_raised |>
  
  # Add a variable of the total spent for each respective chamber 
  mutate(
    CHAMBER_TOTAL = sum(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = c(CHAMBER, IN_CYCLE)) |> 
  
  reframe(
    NAME,
    CHAMBER,
    IN_CYCLE,
    CHAMBER_PROPORTION = TOTAL_RECEIPTS/CHAMBER_TOTAL) |> 
  
  filter(CHAMBER == "Senate",
         IN_CYCLE == "Y") |> 
  
  slice_max(
    order_by = CHAMBER_PROPORTION,
    n = 5)

members_raised |> 
  filter(NAME %in% c("Brown, Sherrod", "Tester, Jon")) |> 
  summarize(
    per_day = TOTAL_RECEIPTS/731,
    .by = NAME)


members_raised |> 
  filter(
    !NAME == "Brown, Sherrod",
    !NAME == "Tester, Jon") |> 
  summarize(
    MEDIAN_RAISED = median(TOTAL_RECEIPTS, na.rm = TRUE),
    .by = c(CHAMBER, RETIREMENT, IN_CYCLE)) |> 
  mutate(
    PER_DAY = (MEDIAN_RAISED / 731)) |> 
  filter(CHAMBER == "Senate",
         IN_CYCLE == "Y")

## CLEAN UP

#Checking FEC final filings for missing data

data_check <- 
  member_filings |>
  filter(
    report_type == "YE",
    report_year == "2024")


missing_data <- 
  members |> 
  filter(!COMMITTEE_ID %in% data_check$committee_id)
