This repository contains the data and code needed to replicate Issue One's 2025 [analysis](https://issueone.org/articles/the-118th-congress-fundraising-treadmill/) of the money raised by the 118th Congress between January 2023 and December 2025 via campaign committees and leadership PACs. 

**Code for analysis**
+ treadmill_update.R: Contains code to analyze the fundraising of congressional campaign committees.
+ treadmill_leadership_pacs.R: Contains code to analyze the fundraising of leadership PACs.

**Data**
+ data/congressional_treadmill.csv: Comes from the FEC website, data from F3 candidate campaign committee filings, https://www.fec.gov/data/filings/?data_type=processed&cycle=2024&form_type=F3 
+ data/members.csv: Comes from internal data collection by Issue One. The file includes data about all sitting members at the end of the 118th Congress including their associated FEC campaign committee ID. 
+ pac_data/committee_summary_2024.csv: Comes from the FEC website, summary data file of the raising and spending of various committees, https://www.fec.gov/data/browse-data/?tab=committees
+ pac_data/leadership_pacs_118.csv: Comes from internal data collection by Issue One. The file contains data about the leadership PACs associated with all sitting members at the end of the 118th Congress.

