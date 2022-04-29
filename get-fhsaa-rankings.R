# Get FHSAA girl lacrosse rankings direct from origin and updated

# https://fhsaa_ftp.sidearmsports.com/custompages/rankings/glx_rankings.json


require(httr)
require(tidyjson)
require(dplyr)
require(janitor)
require(maps)
require(readr)
require(stringr)
require(lubridate)

glx_rankings_get <-
  GET("https://fhsaa_ftp.sidearmsports.com/custompages/rankings/glx_rankings.json")

glx_rankings_json <-
  content(glx_rankings_get, as = "text", encoding = "UTF-8")



rankingsDate <- glx_rankings_json %>%
  json_types() %>%
  gather_object() %>%
  json_types %>%
  filter(type == "object") %>%
  spread_all() %>%
  .$RankingsDate %>%
  ymd_hms()

write_file(glx_rankings_json,
           file = str_replace(paste(
             "glx_rankings_",
             format_ISO8601(rankingsDate),
             ".json",
             sep = ""
           ),":",""))

divisions_spread <- glx_rankings_json %>%
  json_types() %>%
  gather_object() %>%
  json_types() %>%
  filter(type == "array") %>%
  gather_array() %>%
  spread_all() %>%
  gather_object() %>%
  spread_all() %>%
  json_types() %>%
  filter(type == "array") %>%
  gather_array() %>%
  spread_all() %>%
  rename(Division = Name) %>%
  tidyr::separate(SchoolName,
                  c("School", "City", "State"),
                  sep = "[(),]",
                  remove = FALSE) %>%
  mutate_at(
    "Division",
    stringr::str_replace_all,
    c(
      "Division " = "",
      "Independent" = "Indep",
      "Unknown" = "Unkn"
    )
  )

# Get lat long
# geo_coded_schools <- geocode(as.character(divisions_spread$SchoolName))

# Load tibble with lat long
load("./geo_coded_school.RData")

divisions <- divisions_spread %>%
  mutate(geo_coded_schools) %>%
  select(-c(
    document.id,
    type,
    name,
    array.index,
    name.2,
    array.index.2,
    ..JSON
  ))

save(rankingsDate, divisions, file = "./divisions.RData")
