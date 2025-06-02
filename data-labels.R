apply_labels <- function(df, var, labels) {
  df |>
    mutate({{ var }} := factor({{ var }}, levels = names(labels), labels = labels))
}

labels <- list(
  JHU1A = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU1B = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU1C = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU1D = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU1E = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU1F = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU1G = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU2A = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU2B = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU2C = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU2D = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU2E = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU2F = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU3A = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU3B = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU3C = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU3D = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU3E = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU3F = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU3G = c(
    `1` = "Extremely important",
    `2` = "Very important",
    `3` = "Somewhat important",
    `4` = "A little important",
    `5` = "Not important at all",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU4A = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU4B = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU4C = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU4D = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU4E = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU4F = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU4G = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU5A = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU5B = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU5C = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU5D = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU5E = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU5F = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU5G = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU5H = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU6A = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU6B = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU6C = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU6D = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU6E = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU6F = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU6G = c(
    `1` = "Strongly support",
    `2` = "Support",
    `3` = "Neither support nor oppose",
    `4` = "Oppose",
    `5` = "Strongly oppose",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  JHU7_1 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_2 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_3 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_4 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_5 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_6 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_7 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_8 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_9 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_10 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_11 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_12 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_13 = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_13_OE = c(
    "77" = "DON'T KNOW",
    "98" = "SKIPPED ON WEB",
    "99" = "REFUSED"
  ),
  JHU7_DK = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_SKP = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU7_REF = c(
    `0` = "No",
    `1` = "Yes"
  ),
  JHU8 = c(
    `1` = "Very well",
    `2` = "Somewhat well",
    `3` = "Not too well",
    `4` = "Not at all",
    `77` = "Don’t know",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  PartyID7 = c(
    `-1` = "Unknown",
    `1` = "Strong Democrat",
    `2` = "Not so strong Democrat",
    `3` = "Lean Democrat",
    `4` = "Don't Lean/Independent/None",
    `5` = "Lean Republican",
    `6` = "Not so Strong Republican",
    `7` = "Strong Republican"
  ),
  PartyID5 = c(
    `-1` = "Unknown",
    `1` = "Democrat",
    `2` = "Lean Democrat",
    `3` = "Don't Lean/Independent/None",
    `4` = "Lean Republican",
    `5` = "Republican"
  ),
  IDEO = c(
    `-1` = "Unknown",
    `1` = "Very liberal",
    `2` = "Somewhat liberal",
    `3` = "Moderate",
    `4` = "Somewhat conservative",
    `5` = "Very conservative"
  ),
  VOTE20 = c(
    `1` = "I did not vote in the November 2020 General Election.",
    `2` = "I thought about voting in the November 2020 General Election, but did not.",
    `3` = "I usually vote, but I did not in the November 2020 General Election.",
    `4` = "I'm sure I voted in the November 2020 General Election.",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  CANDI20 = c(
    `1` = "Joe Biden",
    `2` = "Donald Trump",
    `3` = "Someone else",
    `4` = "Did not vote in this race",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  VOTE24 = c(
    `1` = "I did not vote in the November 2024 General Election.",
    `2` = "I thought about voting in the November 2024 General Election, but did not.",
    `3` = "I usually vote, but I did not in the November 2024 General Election.",
    `4` = "I'm sure I voted in the November 2024 General Election.",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  CANDI24 = c(
    `1` = "Kamala Harris",
    `2` = "Donald Trump",
    `3` = "Someone else",
    `4` = "Did not vote in this race",
    `77` = "DON'T KNOW",
    `98` = "SKIPPED ON WEB",
    `99` = "REFUSED"
  ),
  DURATION_JHU = c(
    `0` = "Under 1 minute"
  ),
  SURV_MODE = c(
    `1` = "Phone interview",
    `2` = "Web Interview"
  ),
  SURV_LANG = c(
    `1` = "English",
    `2` = "Spanish"
  ),
  GENDER = c(
    `0` = "Unknown",
    `1` = "Male",
    `2` = "Female"
  ),
  AGE4 = c(
    `1` = "18-29",
    `2` = "30-44",
    `3` = "45-59",
    `4` = "60+",
    `99` = "Under 18"
  ),
  AGE7 = c(
    `1` = "18-24",
    `2` = "25-34",
    `3` = "35-44",
    `4` = "45-54",
    `5` = "55-64",
    `6` = "65-74",
    `7` = "75+",
    `99` = "Under 18"
  ),
  RACETHNICITY = c(
    `1` = "White, non-Hispanic",
    `2` = "Black, non-Hispanic",
    `3` = "Other, non-Hispanic",
    `4` = "Hispanic",
    `5` = "2+, non-Hispanic",
    `6` = "Asian-Pacific Islander, non-Hispanic"
  ),
  EDUC5 = c(
    `1` = "Less than high school",
    `2` = "High school graduate or equivalent",
    `3` = "Some college/associates degree",
    `4` = "Bachelor's degree",
    `5` = "Postgraduate study/professional degree"
  ),
  MARITAL = c(
    `1` = "Married",
    `2` = "Widowed",
    `3` = "Divorced",
    `4` = "Separated",
    `5` = "Never married",
    `6` = "Living with partner"
  ),
  EMPLOY = c(
    `1` = "Working - as a paid employee",
    `2` = "Working - self-employed",
    `3` = "Not working - on temporary layoff from a job",
    `4` = "Not working - looking for work",
    `5` = "Not working - retired",
    `6` = "Not working - disabled",
    `7` = "Not working - other"
  ),
  INCOME = c(
    `1` = "Less than $5,000",
    `2` = "$5,000 to $9,999",
    `3` = "$10,000 to $14,999",
    `4` = "$15,000 to $19,999",
    `5` = "$20,000 to $24,999",
    `6` = "$25,000 to $29,999",
    `7` = "$30,000 to $34,999",
    `8` = "$35,000 to $39,999",
    `9` = "$40,000 to $49,999",
    `10` = "$50,000 to $59,999",
    `11` = "$60,000 to $74,999",
    `12` = "$75,000 to $84,999",
    `13` = "$85,000 to $99,999",
    `14` = "$100,000 to $124,999",
    `15` = "$125,000 to $149,999",
    `16` = "$150,000 to $174,999",
    `17` = "$175,000 to $199,999",
    `18` = "$200,000 or more"
  ),
  INCOME4 = c(
    `1` = "Less than $30,000",
    `2` = "$30,000 to under $60,000",
    `3` = "$60,000 to under $100,000",
    `4` = "$100,000 or more"
  ),
  INCOME9 = c(
    `1` = "Under $10,000",
    `2` = "$10,000 to under $20,000",
    `3` = "$20,000 to under $30,000",
    `4` = "$30,000 to under $40,000",
    `5` = "$40,000 to under $50,000",
    `6` = "$50,000 to under $75,000",
    `7` = "$75,000 to under $100,000",
    `8` = "$100,000 to under $150,000",
    `9` = "$150,000 or more"
  ),
  STATE = c(
    "77" = "DON'T KNOW",
    "99" = "REFUSED",
    "CA" = "California",
    "GA" = "Georgia",
    "IA" = "Iowa",
    "LA" = "Louisiana",
    "MA" = "Massachusetts",
    "PA" = "Pennsylvania",
    "VA" = "Virginia",
    "WA" = "Washington",
    "DC" = "District of Columbia",
    "NC" = "North Carolina",
    "SC" = "South Carolina",
    "ID" = "Idaho",
    "MD" = "Maryland",
    "ND" = "North Dakota",
    "SD" = "South Dakota",
    "DE" = "Delaware",
    "ME" = "Maine",
    "NE" = "Nebraska",
    "NH" = "New Hampshire",
    "OH" = "Ohio",
    "HI" = "Hawaii",
    "MI" = "Michigan",
    "RI" = "Rhode Island",
    "VI" = "Virgin Islands",
    "WI" = "Wisconsin",
    "NJ" = "New Jersey",
    "AK" = "Alaska",
    "OK" = "Oklahoma",
    "AL" = "Alabama",
    "FL" = "Florida",
    "IL" = "Illinois",
    "NM" = "New Mexico",
    "IN" = "Indiana",
    "MN" = "Minnesota",
    "TN" = "Tennessee",
    "CO" = "Colorado",
    "MO" = "Missouri",
    "AR" = "Arkansas",
    "OR" = "Oregon",
    "KS" = "Kansas",
    "MS" = "Mississippi",
    "CT" = "Connecticut",
    "MT" = "Montana",
    "UT" = "Utah",
    "VT" = "Vermont",
    "NV" = "Nevada",
    "WV" = "West Virginia",
    "TX" = "Texas",
    "KY" = "Kentucky",
    "NY" = "New York",
    "WY" = "Wyoming",
    "AZ" = "Arizona"
  ),
  REGION4 = c(
    `1` = "Northeast",
    `2` = "Midwest",
    `3` = "South",
    `4` = "West"
  ),
  REGION9 = c(
    `1` = "New England",
    `2` = "Mid-Atlantic",
    `3` = "East North Central",
    `4` = "West North Central",
    `5` = "South Atlantic",
    `6` = "East South Central",
    `7` = "West South Central",
    `8` = "Mountain",
    `9` = "Pacific"
  ),
  METRO = c(
    `0` = "Non-Metro Area",
    `1` = "Metro Area"
  ),
  INTERNET = c(
    `0` = "Non-internet household",
    `1` = "Internet Household"
  ),
  HOUSING = c(
    `1` = "Owned or being bought by you or someone in your household",
    `2` = "Rented for cash",
    `3` = "Occupied without payment of cash rent"
  ),
  HOME_TYPE = c(
    `1` = "A one-family house detached from any other house",
    `2` = "A one-family house attached to one or more houses",
    `3` = "A building with 2 or more apartments",
    `4` = "A mobile home or trailer",
    `5` = "Boat, RV, van, etc"
  ),
  PHONESERVICE = c(
    `1` = "Landline telephone only",
    `2` = "Have a landline, but mostly use cellphone",
    `3` = "Have cellphone, but mostly use landline",
    `4` = "Cellphone only",
    `5` = "No telephone service"
  ),
  HHSIZE = c(
    `1` = "One person, I live by myself",
    `2` = "Two persons",
    `3` = "Three persons",
    `4` = "Four persons",
    `5` = "Five persons",
    `6` = "Six or more persons"
  )
)
