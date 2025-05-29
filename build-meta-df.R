# JHU1-3 = Extremely important...
# JHU4-6 = Strongly support...
# JHU7 = Yes/No
# Extract question names
meta <- readxl::read_xlsx("data/7783_OmniW2_1124_JHU_Codebook_20241127.xlsx", sheet = "Codebook") |>
  rename(
    var = Variable,
    question_raw_text = "Variable Label",
  ) |>
  dplyr::select(var, question_raw_text) |>
  filter(!is.na(question_raw_text)) |>
  rowwise() |>
  mutate(metadata = list(extract_question_metadata(pick(question_raw_text)))) |>
  unnest_wider(metadata) |>
  mutate(
    chosen_from_list = if_else(str_detect(question_raw_text, "Choose up to"), TRUE, FALSE),
    response_set = case_when(
      grepl("^JHU[123]", var) ~ "importance",
      grepl("^JHU[456]", var) ~ "support",
      grepl("^PartyID5", var) ~ "party_id",
      grepl("^JHU7", var) ~ "yes_no",
      TRUE ~ NA_character_ # Optional, for cases not matching the above
    )
  )
