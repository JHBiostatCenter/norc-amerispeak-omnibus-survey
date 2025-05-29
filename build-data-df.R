# Load raw data
raw_data <- haven::read_sas("data/omnibusw2_november2024_jhu_20241127.sas7bdat")
data <- raw_data

source("data-labels.R")

# Apply labels to variables
for (var in names(labels)) {
  if (var %in% colnames(data)) {
    data <- apply_labels(data, !!sym(var), labels[[var]])
  }
}

# Helper functions for variable creation
create_demographic_vars <- function(data) {
  data |>
    mutate(
      race_eth_cat = RACETHNICITY,
      age_cat = AGE4,
      gender_cat = GENDER,
      region_cat = REGION4,
      education_cat = EDUC5,
      income_cat = INCOME4
    )
}

create_voting_vars <- function(data) {
  data |>
    mutate(
      vote_status_2020 = VOTE20,
      candidate_2020 = CANDI20,
      vote_status_2024 = VOTE20,
      candidate_2024 = CANDI24,
      candidate_2020_answered = case_when(
        candidate_2020 == "Joe Biden" ~ TRUE,
        candidate_2020 == "Donald Trump" ~ TRUE,
        candidate_2020 == "Someone else" ~ TRUE,
        candidate_2020 == "Did not vote in this race" ~ TRUE,
        TRUE ~ FALSE
      ),
      candidate_2024_answered = case_when(
        candidate_2024 == "Kamala Harris" ~ TRUE,
        candidate_2024 == "Donald Trump" ~ TRUE,
        candidate_2024 == "Someone else" ~ TRUE,
        candidate_2024 == "Did not vote in this race" ~ TRUE,
        TRUE ~ FALSE
      ),
      voted_2020 = case_when(
        VOTE20 == "I'm sure I voted in the November 2020 General Election." ~ TRUE,
        VOTE20 == "I did not vote in the November 2020 General Election." ~ FALSE,
        VOTE20 == "I thought about voting in the November 2020 General Election, but did not." ~ FALSE,
        VOTE20 == "I usually vote, but I did not in the November 2020 General Election." ~ FALSE,
        TRUE ~ FALSE
      ),
      voted_2024 = case_when(
        VOTE24 == "I'm sure I voted in the November 2024 General Election." ~ TRUE,
        VOTE24 == "I did not vote in the November 2024 General Election." ~ FALSE,
        VOTE24 == "I thought about voting in the November 2024 General Election, but did not." ~ FALSE,
        VOTE24 == "I usually vote, but I did not in the November 2024 General Election." ~ FALSE,
        TRUE ~ FALSE
      ),
      voted_2020_and_2024 = voted_2020 & voted_2024,
      voted_2020_or_2024 = voted_2020 | voted_2024,
      reported_candidates_2020_and_2024 = candidate_2020_answered & candidate_2024_answered,
      reported_candidates_2020_or_2024 = candidate_2020_answered | candidate_2024_answered,
      voted_and_reported_candidates_2020_2024 = voted_2020_and_2024 & reported_candidates_2020_and_2024
    )
}

create_party_vars <- function(data) {
  data |>
    mutate(
      party_cat = case_when(
        PartyID5 == "Democrat" ~ "Democrat",
        PartyID5 == "Lean Democrat" ~ "Democrat",
        PartyID5 == "Don't Lean/Independent/None" ~ "Independent",
        PartyID5 == "Lean Republican" ~ "Republican",
        PartyID5 == "Republican" ~ "Republican",
        TRUE ~ "Unknown"
      ),
      candidate_switch = case_when(
        candidate_2020 == "Joe Biden" & candidate_2024 == "Donald Trump" ~ "Joe Biden to Donald Trump",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Someone else" ~ "Joe Biden to Third Party",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Did not vote in this race" ~ "Joe Biden to Did Not Vote",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Kamala Harris" ~ "Joe Biden, Kamala Harris",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Kamala Harris" ~ "Donald Trump to Kamala Harris",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Someone else" ~ "Donald Trump to Third Party",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Did not vote in this race" ~ "Donald Trump to Did Not Vote",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Donald Trump" ~ "Donald Trump 2020, 2024",
        TRUE ~ "Skipped/Refused/Did Note Vote"
      ),
      party_switch_expanded = case_when(
        candidate_2020 == "Joe Biden" & candidate_2024 == "Kamala Harris" ~ "Stay Democrat",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Donald Trump" ~ "Democrat to Republican",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Did not vote in this race" ~ "Democrat to Non-voter",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Someone else" ~ "Democrat to Third Party",
        candidate_2020 == "Joe Biden" ~ "Democrat to non-response",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Donald Trump" ~ "Stay Republican",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Kamala Harris" ~ "Republican to Democrat",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Did not vote in this race" ~ "Republican to Non-voter",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Someone else" ~ "Republican to Third Party",
        candidate_2020 == "Donald Trump" ~ "Republican to non-response",
        candidate_2020 == "Someone else" & candidate_2024 == "Donald Trump" ~ "Third Party to Republican",
        candidate_2020 == "Someone else" & candidate_2024 == "Kamala Harris" ~ "Third Party to Democrat",
        candidate_2020 == "Someone else" & candidate_2024 == "Did not vote in this race" ~ "Third Party to Non-voter",
        candidate_2020 == "Someone else" & candidate_2024 == "Someone else" ~ "Stay Third Party",
        candidate_2020 == "Someone else" ~ "Third Party to non-response",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Donald Trump" ~ "Non-voter to Republican",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Kamala Harris" ~ "Non-voter to Democrat",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Someone else" ~ "Non-voter to Third Party",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Did not vote in this race" ~ "Stay Non-voter",
        candidate_2020 == "Did not vote in this race" ~ "Non-voter to non-response",
        TRUE ~ "Voted 2020 but non-response to candidate 2020 or 2024"
      ),
      party_switch = case_when(
        candidate_2020 == "Joe Biden" & candidate_2024 == "Kamala Harris" ~ "Stay Democrat",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Donald Trump" ~ "New Republican",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Did not vote in this race" ~ "New Non-voter",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Someone else" ~ "New Third Party",
        candidate_2020 == "Joe Biden" ~ "Democrat to non-response",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Donald Trump" ~ "Stay Republican",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Kamala Harris" ~ "New Democrat",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Did not vote in this race" ~ "New Non-voter",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Someone else" ~ "New Third Party",
        candidate_2020 == "Donald Trump" ~ "Republican to non-response",
        candidate_2020 == "Someone else" & candidate_2024 == "Donald Trump" ~ "New Republican",
        candidate_2020 == "Someone else" & candidate_2024 == "Kamala Harris" ~ "New Democrat",
        candidate_2020 == "Someone else" & candidate_2024 == "Did not vote in this race" ~ "New Non-voter",
        candidate_2020 == "Someone else" & candidate_2024 == "Someone else" ~ "Stay Third Party",
        candidate_2020 == "Someone else" ~ "Third Party to non-response",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Donald Trump" ~ "New Republican",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Kamala Harris" ~ "New Democrat",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Someone else" ~ "New Third Party",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Did not vote in this race" ~ "Stay Non-voter",
        candidate_2020 == "Did not vote in this race" ~ "Non-voter to non-response",
        TRUE ~ "Voted 2020 but non-response to candidate 2020 or 2024"
      ),
      party_switch_analysis = case_when(
        candidate_2020 == "Joe Biden" & candidate_2024 == "Kamala Harris" ~ "Democrat",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Donald Trump" ~ "Republican",
        candidate_2020 == "Joe Biden" & candidate_2024 == "Donald Trump" ~ "New Republican",
        candidate_2020 == "Someone else" & candidate_2024 == "Donald Trump" ~ "New Republican",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Donald Trump" ~ "New Republican",
        candidate_2020 == "Donald Trump" & candidate_2024 == "Kamala Harris" ~ "New Democrat",
        candidate_2020 == "Someone else" & candidate_2024 == "Kamala Harris" ~ "New Democrat",
        candidate_2020 == "Did not vote in this race" & candidate_2024 == "Kamala Harris" ~ "New Democrat",
        TRUE ~ NA
      ),
      voted_dem_2020 = candidate_2020 == "Joe Biden",
      voted_rep_2020 = candidate_2020 == "Donald Trump",
      voted_dem_2024 = candidate_2024 == "Kamala Harris",
      voted_rep_2024 = candidate_2024 == "Donald Trump",
      dem_2020_2024 = party_switch == "Stay Democrat",
      rep_2020_2024 = party_switch == "Stay Republican",
      new_dem_2024 = party_switch == "New Democrat",
      new_rep_2024 = party_switch == "New Republican",
      dem_to_rep_2024 = party_switch_expanded == "Democrat to Republican",
      rep_to_dem_2024 = party_switch_expanded == "Republican to Democrat"
    )
}

# Apply all transformations
data <- data |>
  create_demographic_vars() |>
  create_voting_vars() |>
  create_party_vars() |>
  mutate(across(
    all_of(meta$var),
    ~ recode_responses(., meta$response_set[meta$var == cur_column()]),
    .names = "{.col}_recoded"
  ))
