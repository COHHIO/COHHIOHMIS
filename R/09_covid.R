# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

covid <- function(
             clarity_api,
             app_env,
             e = rlang::caller_env()
            ) {
if (missing(clarity_api))
  clarity_api <- get_clarity_api(e = e)
if (missing(app_env))
  app_env <- get_app_env(e = e)

# library(sf)
# library(urbnmapr)
# library(choroplethrMaps)
# library(plotly)

# ohio_counties <- st_read("Ohio/counties/REFER_COUNTY.shp")
#
# counties <- get_urbn_map("counties", sf = TRUE)
#
# counties <- st_transform(counties, "+init=epsg:3857")
#
# counties <- counties %>%
#   mutate(county_name = str_remove(county_name, " County"))
#
# data(county.map)
#
# oh_counties <- county.map %>% filter(STATE == 39) %>% select(NAME, region)

# Pinpointing where Vaccines are Wanted -----------------------------------

# who's already been vaccinated?

one_dose_and_done <- doses %>%
  dplyr::filter(C19VaccineManufacturer == "Johnson & Johnson") %>%
  dplyr::select(PersonalID, C19DoseDate) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::slice_max(C19DoseDate) %>%
  dplyr::select(PersonalID, "LastDose" = C19DoseDate) %>%
  unique()

complete <- doses %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::summarise(Doses = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(Doses > 1) %>%
  dplyr::left_join(doses, by = "PersonalID") %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::mutate(LastDose = dplyr::lag(C19DoseDate, order_by = C19DoseDate)) %>%
  dplyr::filter(!is.na(LastDose)) %>%
  dplyr::mutate(DaysBetweenDoses = difftime(C19DoseDate, LastDose, units = "days")) %>%
  dplyr::filter(DaysBetweenDoses >= 20) %>%
  dplyr::select(PersonalID, LastDose) %>%
  unique() %>%
  rbind(one_dose_and_done) %>%
  dplyr::mutate(HasAllDoses = "Yes") %>%
  unique() %>%
  dplyr::mutate(
    FullyVaccinated = dplyr::case_when(
      lubridate::ymd(LastDose) >= lubridate::today() - lubridate::days(14) ~ "No",
      lubridate::ymd(LastDose) < lubridate::today() - lubridate::days(14) ~ "Yes"
    )
  )

# deduping enrollment data taking the most recent open enrollment
most_recent_entries <- co_clients_served %>%
  dplyr::filter(AgeAtEntry >= 16 &
           is.na(ExitDate) &
           (ProjectType %in% c(project_types$lh) |
              (ProjectType %in% c(project_types$ph) &
                 is.na(MoveInDateAdjust)))
  ) %>%
  dplyr::group_by(PersonalID) %>%
  dplyr::slice_max(EntryDate) %>%
  dplyr::slice_max(EnrollmentID) %>%
  dplyr::ungroup()

# cohort of clients = current, over 16, and literally homeless in any ptc
current_over16_lh <- most_recent_entries %>%
  dplyr::select(CountyServed, PersonalID, ProjectName) %>%
  dplyr::left_join(covid19[c("PersonalID", "C19ConsentToVaccine", "C19VaccineConcerns")],
            by = "PersonalID") %>%
  dplyr::left_join(complete, by = "PersonalID") %>%
  dplyr::mutate(HasAllDoses = dplyr::if_else(is.na(HasAllDoses),
                                   "Not acc. to HMIS",
                                   HasAllDoses))

# getting total clients included per county
total_lh_by_county <- current_over16_lh %>%
  dplyr::count(CountyServed) %>%
  dplyr::rename("TotalLH" = n) %>%
  dplyr::arrange(dplyr::desc(TotalLH))

# getting consent data on everyone, getting data ready to turn
consent_status <- current_over16_lh %>%
  dplyr::mutate(
    C19ConsentToVaccine = dplyr::if_else(is.na(C19ConsentToVaccine),
                               "Data not collected (HUD)",
                               C19ConsentToVaccine),
    Status = dplyr::case_when(
      HasAllDoses == "Yes" ~ "Has All Doses",
      C19ConsentToVaccine == "Yes (HUD)" ~ "Answered Yes to Consent question",
      !C19ConsentToVaccine %in% c("Yes (HUD)", "No (HUD)") ~ "Consent Unknown",
      C19ConsentToVaccine == "No (HUD)" ~ "Answered No to Consent question"))

# turning the data so each Status has its own column and it's by County
consent_status_by_county <- consent_status %>%
  dplyr::count(CountyServed, Status) %>%
  tidyr::pivot_wider(names_from = Status,
              values_from = n,
              values_fill = 0)

# putting all the data together
totals_by_county <- total_lh_by_county %>%
  dplyr::left_join(consent_status_by_county, by = "CountyServed") %>%
  janitor::clean_names() %>%
  dplyr::rename("county_name" = county_served)

# creating sf data object with the pre-shaped data
# vaccine_distribution_county <- counties %>%
#   filter(state_fips == 39, # Ohio
#          !county_fips %in% c(39113, # Montgomery
#                              39035, # Cuyahoga
#                              39049, # Franklin
#                              39153, # Summit
#                              39061, # Hamilton
#                              39095, # Lucas
#                              39151)) %>% # Stark
#   left_join(totals_by_county, by = "county_name") %>%
#   mutate(across(7:11, ~replace_na(.x, 0)),
#          hover = paste0(county_name, ": \n",
#                         consent_unknown,
#                         " + | Consent Unknown\n",
#                         answered_no_to_consent_question,
#                         " + | Would Not Consent\n",
#                         answered_yes_to_consent_question,
#                         " + | Would Consent\n",
#                         has_all_doses,
#                         " + | Has All Doses\n= ",
#                         total_lh,
#                         " | Total Over 16 and Literally Homeless"))
#
# creating plot
# consent_plot <- ggplot(counties %>% filter(state_fips == 39)) +
#   geom_sf() +
#   geom_sf(vaccine_distribution_county,
#           mapping = aes(fill = total_lh)) +
#   geom_sf_label(vaccine_distribution_county,
#                 mapping = aes(label = hover)) +
#   # geom_sf_text(counties %>% filter(state_fips == 39),
#   #              mapping = aes(label = county_name),
#   #              check_overlap = TRUE,
#   #              size = 3,
#   #              color = "slategray3") +
#   scale_fill_viridis_c(super = ScaleContinuous) +
#   labs(title = "Would Consent to Vaccine") +
#   theme_void()

# # making it usable
# ggplotly(consent_plot,
#          tooltip = "text")


# Trying Leaflet ----------------------------------------------------------


# Connecting Clients to their 2nd Doses -----------------------------------

vaccine_needs_second_dose <- dose_counts %>%
  dplyr::filter(Doses == 1) %>%
  dplyr::left_join(doses, by = "PersonalID") %>%
  dplyr::left_join(most_recent_entries, by = "PersonalID") %>%
  dplyr::filter(C19VaccineManufacturer != "Johnson & Johnson") %>%
  dplyr::mutate(
    NextDoseNeededDate = dplyr::case_when(
      C19VaccineManufacturer == "Moderna" ~
        lubridate::ymd(C19DoseDate) + lubridate::days(28),
      C19VaccineManufacturer == "Pfizer" ~
        lubridate::ymd(C19DoseDate) + lubridate::days(21),
      stringr::str_starts(C19VaccineManufacturer, "Client doesn't know") == TRUE ~
        lubridate::ymd(C19DoseDate) + lubridate::days(28)
    ),
    CurrentLocation = dplyr::case_when(
      is.na(EntryDate) ~ dplyr::if_else(
        is.na(VaccineContactInfo),
        "No contact info and not currently enrolled in any project.",
        VaccineContactInfo
      ),
      lubridate::today() >= lubridate::ymd(EntryDate) &
        (lubridate::ymd(ExitDate) > lubridate::today()) | is.na(ExitDate) ~
        paste(
          "Currently in",
          ProjectName,
          "Contact Info:",
          VaccineContactInfo
        ),
        ExitDate <= lubridate::today() ~ paste(
          "Exited",
          ProjectName,
          "on",
          ExitDate,
          "to",
          HMIS::hud_translations$`3.12.1 Living Situation Option List`(Destination),
          "Contact info:",
          VaccineContactInfo
        )
    ),
    DaysUntilNextDose = lubridate::ymd(NextDoseNeededDate) - lubridate::today(),
    VeteranStatus = dplyr::case_when(
      VeteranStatus == 0 ~ "No",
      VeteranStatus == 1 ~ "Yes",
      TRUE ~ "Unknown"
    ),
    AgeAtEntry = dplyr::case_when(
      AgeAtEntry < 12 ~ "0-11",
      AgeAtEntry < 16 ~ "12-15",
      AgeAtEntry < 25 ~ "16-24",
      AgeAtEntry < 65 ~ "25-59",
      AgeAtEntry < 75 ~ "60-74",
      AgeAtEntry < 85 ~ "75-84",
      AgeAtEntry < 120 ~ "85+",
      TRUE ~ "Unknown"
    ),
    HowSoon = dplyr::case_when(
      DaysUntilNextDose < 0 ~ "Overdue",
      DaysUntilNextDose > 7 ~ "Next Week",
      DaysUntilNextDose > 3 ~ "7 days",
      DaysUntilNextDose >= 0 ~ "3 days"
    )
  ) %>%
  dplyr::select(
    PersonalID,
    HouseholdID,
    CountyServed,
    C19VaccineManufacturer,
    AgeAtEntry,
    VeteranStatus,
    NextDoseNeededDate,
    HowSoon,
    DaysUntilNextDose,
    CurrentLocation
  )

# Client Statuses ---------------------------------------------------------

get_county_from_provider <- co_clients_served %>%
  dplyr::left_join(Project %>%
              dplyr::select(ProjectName, ProjectCounty), by = "ProjectName") %>%
  dplyr::mutate(
    CountyGuessed = dplyr::if_else(is.na(CountyServed) |
                              CountyServed == "--Outside of Ohio--", 1, 0),
    CountyServed = dplyr::case_when(
      CountyGuessed == 1 &
        ProjectName != "Unsheltered Clients - OUTREACH" ~ ProjectCounty,
      CountyGuessed == 0 ~ CountyServed,
      TRUE ~ "Unsheltered in unknown County"
    ),
    ProjectCounty = NULL
  )

co_clients_served_county_guesses <- get_county_from_provider %>%
  dplyr::left_join(Enrollment %>%
              dplyr::select(EnrollmentID, UserCreating), by = "EnrollmentID") %>%
  dplyr::mutate(
    UserID = gsub(pattern = '[^0-9\\.]', '', UserCreating, perl = TRUE)
  ) %>%
  dplyr::left_join(Users %>%
              dplyr::mutate(UserID = as.character(UserID)) %>%
              dplyr::select(UserID, UserCounty), by = "UserID") %>%
  dplyr::mutate(CountyServed = dplyr::if_else(CountyServed == "Unsheltered in unknown County",
                                UserCounty,
                                CountyServed)) %>%
  dplyr::select(-dplyr::starts_with("User"))

vaccine_status <- co_clients_served_county_guesses %>%
  dplyr::left_join(complete %>% dplyr::select(-LastDose), by = "PersonalID") %>%
  dplyr::mutate(HasAllDoses = dplyr::if_else(is.na(HasAllDoses), "No", HasAllDoses)) %>%
  dplyr::left_join(vaccine_needs_second_dose[c("PersonalID", "HouseholdID", "HowSoon")],
            by = c("HouseholdID", "PersonalID")) %>%
  dplyr::left_join(covid19[c("PersonalID", "C19ConsentToVaccine")],
            by = c("PersonalID")) %>%
  dplyr::mutate(
    C19ConsentToVaccine = dplyr::if_else(is.na(C19ConsentToVaccine),
                               "Data not collected",
                               C19ConsentToVaccine),
    AgeAtEntry = dplyr::case_when(
      AgeAtEntry < 12 ~ "0-11",
      AgeAtEntry < 16 ~ "12-15",
      AgeAtEntry < 25 ~ "16-24",
      AgeAtEntry < 65 ~ "25-59",
      AgeAtEntry < 75 ~ "60-74",
      AgeAtEntry < 85 ~ "75-84",
      AgeAtEntry < 120 ~ "85+",
      TRUE ~ "Unknown"
    ),
    VaccineStatus = dplyr::case_when(
      FullyVaccinated == "Yes" ~ "Fully vaccinated",
      HasAllDoses == "Yes" ~ "Has all doses",
      !is.na(HowSoon) ~ "Needs 2nd dose",
      C19ConsentToVaccine == "Yes (HUD)" ~ "Not vaccinated, would consent",
      C19ConsentToVaccine == "No (HUD)" ~ "Not vaccinated, would not consent",
      !C19ConsentToVaccine %in% c("Yes (HUD)", "No (HUD)", "Data not collected") ~
        "Not vaccinated, consent unknown",
      C19ConsentToVaccine == "Data not collected" ~ "Data not collected"
    ),
    VaccineStatus = factor(VaccineStatus, levels = c(
      "Fully vaccinated",
      "Has all doses",
      "Needs 2nd dose",
      "Not vaccinated, would consent",
      "Not vaccinated, would not consent",
      "Not vaccinated, consent unknown",
      "Data not collected"
    ))
  ) %>%
  dplyr::select(
    PersonalID,
    HouseholdID,
    CountyServed,
    ProjectName,
    AgeAtEntry,
    RelationshipToHoH,
    VeteranStatus,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    VaccineStatus
  )

 # Concerns ----------------------------------------------------------------
#
# concerns <- covid19 %>%
#   select(PersonalID, C19ConsentToVaccine, C19VaccineConcerns) %>%
#   filter(C19ConsentToVaccine != "Yes (HUD)" & !is.na(C19VaccineConcerns))
#
# text <- concerns$C19VaccineConcerns
#
# text <- tolower(text)
#
# text <- str_replace(text, "side affects", "side effects")
#
# text <-
#   removeWords(
#     text,
#     c(
#       "am",
#       "is",
#       "are",
#       "was",
#       "been",
#       "did",
#       "want",
#       "will",
#       "would",
#       "doesnt",
#       "dont",
#       "have",
#       "has",
#       "hasn't",
#       "hasnt",
#       "does",
#       "doesn't",
#       "don't",
#       "the",
#       "not",
#       "and",
#       "vaccine",
#       "vaccines",
#       "about",
#       "into",
#       "for",
#       "its",
#       "it's",
#       "that"
#     )
#   )
#
# cloud <- Corpus(VectorSource(text))
#
# cloud <- tm_map(cloud, content_transformer(tolower)) %>%
#   tm_map(removeNumbers) %>%
#   tm_map(removePunctuation) %>%
#   tm_map(stripWhitespace)
#
# vaccine_concerns_cloud <- wordcloud(
#   cloud,
#   colors = brewer.pal(8, "Dark2"),
#   random.order = FALSE,
#   random.color = FALSE,
#   scale = c(3, .2)
# )


# Who needs followup? -----------------------------------------------------


served_since_02052021 <- co_clients_served %>%
  dplyr::filter(HMIS::served_between(., rm_dates$hc$bos_start_vaccine_data, rm_dates$meta_HUDCSV$Export_End)) %>%
  dplyr::count(ProjectName) %>%
  dplyr::rename("totalserved" = n)

exited <- missing_vaccine_exited %>%
  dplyr::count(ProjectName) %>%
  dplyr::rename("missingexited" = n)

current <- missing_vaccine_current %>%
  dplyr::count(ProjectName) %>%
  dplyr::rename("missingcurrent" = n)

all <- dplyr::full_join(served_since_02052021, current, by = "ProjectName") %>%
  dplyr::full_join(exited, by = "ProjectName") %>%
  dplyr::mutate(missingcurrent = tidyr::replace_na(missingcurrent, 0),
         missingexited = tidyr::replace_na(missingexited, 0),
         allmissing = missingcurrent + missingexited,
         percentmissing = allmissing/totalserved)

readr::write_csv(all, "random_data/percentmissing.csv")

# to update this, I'm saving this to DB > HMIS > Covid-19 Data Analysis for EM

# cleanup -----------------------------------------------------------------

rm(list = ls()[!(
  ls() %in% c(
    "vaccine_needs_second_dose",
    "vaccine_status"
  )
)])
app_env$gather_deps()
app_env
}
