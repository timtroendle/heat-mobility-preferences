library(readxl)
library(tidyverse)
library(arrow)

##############################################################################################
# Load data
##

df <- read_excel(snakemake@input[["data"]])

##############################################################################################
# Data preparation
##

# filter out respondents who fall in the category "screended out" and "quota full", add duration in minutes
d <- df %>%
  filter(response_type == "complete") %>%
  mutate( 
    duration_minutes = `Duration (in seconds)`/60)

# analyze duration 
hist_q <- d %>% 
  filter(duration_minutes <= quantile(d$duration_minutes, probs=.90)) %>% 
  ggplot(aes(x=duration_minutes)) + 
  geom_histogram(bins = 60) +
  theme_bw() +
  labs(x = "Duration (minutes)", y = "Count") +
  theme(axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 20)))+
  geom_vline(xintercept = quantile(d$duration_minutes, probs = 0.05), color="#993333")

hist_q
ggsave(snakemake@output[["duration"]], device = png,
         width = 10, height = 6)  

# filter out speeders
d = d %>%
  filter(duration_minutes >= quantile(d$duration_minutes, probs=.05))

summary(d$duration_minutes)

# mutate ResponseIDs to start with 1 until nrow and rename preferences to fit with choice naming
d = d %>%
   mutate(ID =1:nrow(d))%>%
   rename("1_choice" = "2_pref_h",
          "2_choice" = "3_pref_h",
          "3_choice" = "4_pref_h",
          "4_choice" = "5_pref_h",
          "5_choice" = "6_pref_h",
          "6_choice" = "2_pref_t",
          "7_choice" = "3_pref_t",
          "8_choice" = "4_pref_t",
          "9_choice" = "5_pref_t",
          "10_choice" = "6_pref_t",
    ) %>%
  rename (
    "1_rating_1" = "2_support_h_1",
    "1_rating_2" = "2_support_h_2",
    "2_rating_1" = "3_support_h_1",
    "2_rating_2" = "3_support_h_2",
    "3_rating_1" = "4_support_h_1",
    "3_rating_2" = "4_support_h_2",
    "4_rating_1" = "5_support_h_1",
    "4_rating_2" = "5_support_h_2",    
    "5_rating_1" = "6_support_h_1",
    "5_rating_2" = "6_support_h_2",    
    "6_rating_1" = "2_support_t_1",
    "6_rating_2" = "2_support_t_2",
    "7_rating_1" = "3_support_t_1",
    "7_rating_2" = "3_support_t_2",
    "8_rating_1" = "4_support_t_1",
    "8_rating_2" = "4_support_t_2",
    "9_rating_1" = "5_support_t_1",
    "9_rating_2" = "5_support_t_2",    
    "10_rating_1" = "6_support_t_1",
    "10_rating_2" = "6_support_t_2",     
  )

# extract framing and rename
framing = d %>%
  select(c("ID", "Framing")) %>%
  mutate(Framing = fct_recode(Framing,
                           "Phase-Out"  = "P",
                           "Renewables" = "R"))

# extract sectors and rename
first_sector = d %>%
  select(c("ID", "First")) %>%
  mutate(First = fct_recode(First,
                              "Heating"  = "H",
                              "Transport" = "T"))
  
# extract respondent charactercistics and recode 
resp_char <- d %>%
  select(c("ID", "age", "gender", "education", "party_pref", 
           "number_adults", "number_children", "income", "residential_area", 
           "number_diesel", "number_gas", "number_hybrid", "number_plugin", "number_EV", "number_other", "car_days",
           "source_h", "building_type", "ownership", "influence_h")) %>%
  mutate_at(c("age", "gender", "education", "party_pref", 
              "number_adults", "number_children", "income", "residential_area", 
              "car_days",
              "source_h", "building_type", "ownership", "influence_h"), factor) %>%
  mutate(age = fct_recode(age, 
                          "18 - 29 years" = "2", 
                          "30 - 39 years" = "3", 
                          "40 - 49 years" = "4", 
                          "50 - 59 years" = "5", 
                          "older than 60 years"= "6" )) %>%
  mutate(gender = fct_recode(gender,
                             "Male" = "1",
                             "Female" = "2",
                             "Non-binary" = "3")) %>%
  mutate(education = fct_recode(education,
                                "No school diploma" = "1", 
                                "Volks- or Hauptschulabschluss" = "2", 
                                "Mittlere Reife" = "3", 
                                "Abitur" = "4", 
                                "Fachochschulabschluss" = "6", 
                                "University degree" = "7", 
                                "Other degree" = "8")) %>%
  mutate(party_pref = fct_recode(party_pref,
                                 "CDU/CSU" = "1",
                                 "SPD" = "2", 
                                 "FDP" = "3", 
                                 "Die Linke" = "4", 
                                 "Die Grünen" = "5", 
                                 "AfD" = "6", 
                                 "None of these parties" = "7")) %>%
  mutate(residential_area =fct_recode(residential_area,
                                      "<2'000 inh." = "1", 
                                      "2'000 - 5'000 inh." = "2", 
                                      "5'000 - 20'000 inh." = "3", 
                                      "20'000 - 50'000 inh." = "4", 
                                      "50'000 - 100'000 inh." = "5", 
                                      "100'000 - 500'000 inh." = "6", 
                                      " > 500'000 inh." = "7")) %>%
  mutate(number_adults = fct_recode(number_adults,
                                    ">4" = "5",
                                    ">4" = "6",
                                    ">4" = "7",
                                    ">4" = "9",
                                    ">4" = "14",
                                    ">4" = "20",
                                    ">4" = "28",
                                    ">4" = "30")) %>%
  mutate(number_children = fct_recode(number_children,
                                      ">4" = "5",
                                      ">4" = "9" )) %>%
  mutate(income = fct_recode(income,
                             "< 500 Euro" = "1", 
                             "500 - 1'000 Euro" = "2", 
                             "1'000 - 2'000 Euro" = "3", 
                             "2'000 - 3'000 Euro" = "4", 
                             "3'000 - 4'000 Euro" = "5", 
                             "4'000 - 5'000 Euro" = "6", 
                             "5'000 - 7'500 Euro" = "7", 
                             "7'500 - 10'000 Euro" = "8", 
                             "> 10'000 Euro" = "9", 
                             "No indication" = "10")) %>%
  mutate(car_days = fct_recode(car_days, 
                               "Never/ exceptionally" = "1",
                               "1 - 2 days" = "2",
                               "3 - 4 days" = "3",
                               "5 - 6 days" = "4",
                               "Every day" = "5"))%>%
  mutate(cardays_cat = case_when (
    car_days == "Never/ exceptionally" ~ "never / exceptionally",
    car_days == "1 - 2 days" ~ "1 - 4 days",
    car_days == "3 - 4 days" ~ "1 - 4 days",
    car_days == "5 - 6 days" ~ "5 - 7 days",
    car_days == "Every day" ~  "5 - 7 days" )) %>%
  mutate_at(c("cardays_cat"), factor) %>%
  mutate(number_diesel = replace(number_diesel, is.na(number_diesel) & (number_gas >= 1 | number_hybrid >= 1 | number_plugin >= 1 | number_EV >= 1 | number_other >= 1), 0))%>%
  mutate(number_gas = replace(number_gas, is.na(number_gas) & (number_diesel >= 1 | number_hybrid >= 1 | number_plugin >= 1 | number_EV >= 1 | number_other >= 1), 0)) %>%
  mutate(number_hybrid = replace(number_hybrid, is.na(number_hybrid) & (number_diesel >= 1 | number_gas >= 1 | number_plugin >= 1 | number_EV >= 1 | number_other >= 1), 0)) %>%
  mutate(number_plugin = replace(number_plugin, is.na(number_plugin) & (number_diesel >= 1 | number_gas >= 1 | number_hybrid >= 1 | number_EV >= 1 | number_other >= 1), 0)) %>%
  mutate(number_EV = replace(number_EV, is.na(number_EV) & (number_diesel >= 1 | number_gas >= 1 | number_hybrid >= 1 | number_plugin >= 1 | number_other >= 1), 0)) %>%
  mutate(number_other = replace(number_other, is.na(number_other) & (number_diesel >= 1 | number_gas >= 1 | number_hybrid >= 1 | number_plugin >= 1 | number_EV >= 1), 0)) %>%
  mutate(number_ff = number_diesel + number_gas,
         number_hyb = number_hybrid + number_plugin) %>%
  mutate(car_type = case_when(number_ff == 0 & number_hyb == 0 & number_EV == 0 & number_other == 0 ~ 0,
                              number_ff >= 1 & number_hyb == 0 & number_EV == 0 & number_other == 0 ~ 1,
                              number_hyb >= 1 & number_EV == 0 & number_other == 0 & number_ff == 0 ~ 2,
                              number_EV >= 1 & number_other == 0 & number_ff == 0 & number_hyb == 0 ~ 3,
                              number_other >= 1 & number_ff == 0 & number_hyb == 0 & number_EV == 0~ 4,
                              number_other >= 1 & number_ff >= 1  | number_other >= 1 & number_hyb >= 1 | number_other >= 1 & number_EV >= 1 |
                              number_ff >= 1 & number_hyb >= 1 | number_ff >= 1 & number_EV >= 1 |
                              number_hyb >= 1 & number_EV >= 1 ~ 5,)) %>%
  mutate_at("car_type",factor) %>%
  mutate(car_type = fct_recode(car_type,
                               "No car" = "0",
                               "ICEV " = "1",
                               "Hybrid" = "2",
                               "EV" = "3",
                               "Other" = "4",
                               "Multiple" = "5")) %>%
  mutate(source_h = fct_recode(source_h, 
                               "Oil" = "1", 
                               "Gas" = "2", 
                               "Wood" = "3", 
                               "Heat pump" = "4", 
                               "District heating" = "5", 
                               "Other" = "6",
                               "Other" = "7" )) %>%
  mutate(source_h_cat = case_when (
    source_h == "Oil" ~ "Fossil fuel",
    source_h == "Gas" ~ "Fossil fuel",
    source_h == "Wood" ~ "Low carbon",
    source_h == "Heat pump" ~ "Low carbon",
    source_h == "District heating" ~  "District heating",
    source_h == "Other" ~ "Other" )) %>%
  mutate_at("source_h_cat", factor) %>%
  mutate(building_type = fct_recode(building_type, 
                                    "New building" = "1", 
                                    "Modernized building" = "2", 
                                    "Old building" = "3")) %>%
  mutate(ownership = fct_recode(ownership, 
                                "Owning" = "1", 
                                "Renting" = "2")) %>%
  mutate(influence_h = fct_recode(influence_h, 
                                  "Yes" = "1",
                                  "No" = "2"))

# extract attitudes data and calculate mean values for climate beliefs and climate action
attitudes = d %>%
  select(c("ID", "climate_change_1", "climate_change_2", "climate_change_3", "climate_change_4", "climate_change_5", "climate_change_6",
           "responsibility_1", "responsibility_2", "responsibility_3",
           "trust_1", "trust_2", "trust_3")) %>%
  mutate_at(c("climate_change_1", "climate_change_2", "climate_change_3", "climate_change_4", "climate_change_5", "climate_change_6"), factor) %>%
  mutate(cceval = ((d$climate_change_1 + d$climate_change_2 + d$climate_change_3 + d$climate_change_4 + d$climate_change_5 + d$climate_change_6) / 6)) %>%
    mutate(cceval_cat = case_when(cceval < 3 ~ "do not agree",
                                  cceval >3 & cceval < 4 ~ "neither / nor",
                                  cceval > 4 ~ "agree")) %>%
  mutate(trust_gov_cat = case_when(trust_1 < 3 ~ "do not trust",
                                   trust_1 == 3 ~ "neither / nor",
                                   trust_1 > 3 ~ "trust")) %>%  
  mutate(trust_com_cat = case_when(trust_2 < 3 ~ "do not trust",
                                   trust_2 == 3 ~ "neither / nor",
                                   trust_2 > 3 ~ "trust")) %>% 
  mutate(trust_cit_cat = case_when(trust_3 < 3 ~ "do not trust",
                                   trust_3 == 3 ~ "neither / nor",
                                   trust_3 > 3 ~ "trust")) %>% 
  mutate(resp_gov_cat = case_when(responsibility_1 < 3 ~ "not responsible",
                                  responsibility_1 == 3 ~ "neither / nor",
                                  responsibility_1 > 3 ~ "responsible")) %>% 
  mutate(resp_com_cat = case_when(responsibility_2 < 3 ~ "not responsible",
                                  responsibility_2 == 3 ~ "neither / nor",
                                  responsibility_2 > 3 ~ "responsible")) %>% 
  mutate(resp_cit_cat = case_when(responsibility_3 < 3 ~ "not responsible",
                                  responsibility_3 == 3 ~ "neither / nor",
                                  responsibility_3 > 3 ~ "responsible")) %>%
  mutate_at(c("cceval_cat","trust_gov_cat", "trust_com_cat", "trust_cit_cat", "resp_gov_cat", "resp_com_cat","resp_cit_cat"),  factor)

##############################################################################################
# Data preparation for conjoint part
##

# reshape the attributes so each package gets its own row
d_package = d %>% select(ID, starts_with("choice"))
d_package = d_package %>%
  gather(variable, value, -ID) %>%
  mutate(
    choiceNum = as.numeric(gsub("[A-Za-z]|_.+", "", variable)),
    packNum   = as.numeric(gsub(".+(.$)", "\\1", variable)),
    attribute = gsub(".+_|.$", "", variable)
  ) %>%
  select(-variable) %>%
  spread(attribute, value)

# reshape the respondent 's preferences so each choice gets its own row .
d_choice = d %>% select(ID, ends_with("choice"))
d_choice = d_choice %>%
  gather(variable, choice, -ID) %>%
  mutate(
    choiceNum  = as.numeric(gsub("_choice", "", variable)),
    choice = as.numeric(gsub("Maßnahmenpaket", "", choice))
  ) %>%
  select(-variable)

# reshape the respondent 's preferences so each rating gets its own row
d_rate = d %>% select(ID, contains("_rating_"))
d_rate = d_rate %>%
  gather(variable, rating, -ID) %>%
  mutate(
    choiceNum  = as.numeric(gsub("_rating_1|_rating_2", "", variable)),
    packNum   = as.numeric(gsub(".+(.$)", "\\1", variable)),
  ) %>%
  select(-variable)

# recode rating to binary 1-0 (no = 1-3, yes = 4 & 5)
d_rate$bin_rate = ifelse(d_rate$rating == 1, 0,
                           ifelse(d_rate$rating == 2, 0, 
                                  ifelse(d_rate$rating == 3, 0,
                                         ifelse(d_rate$rating == 4, 1,
                                                ifelse(d_rate$rating == 5, 1, NA)))))

# merge the attributes and preferences.
stack_choice = left_join(d_package, d_choice)
stack_choice = stack_choice %>%
  mutate(Y = as.numeric(packNum == choice)) %>%
  left_join(framing, by = "ID")

stack_rating = left_join(d_package, d_rate) 
stack_rating = stack_rating %>%
  left_join(framing, by = "ID")

# check that there are no extra rows .
nrow(stack_choice) == (nrow(d) * max(as.numeric(stack_choice$packNum)) * max(as.numeric(stack_choice$choiceNum)))
nrow(stack_rating) == (nrow(d) * max(as.numeric(stack_rating$packNum)) * max(as.numeric(stack_rating$choiceNum)))

# drop rows for rating when tasks have not been answered
stack_choice = stack_choice %>% 
  drop_na(Y)

stack_rating = stack_rating %>% 
  drop_na(rating)

# divide into two tables for the two sectors, and drop rows when data for packages is missing
choice_h = stack_choice %>% 
  select(c("ID", "choiceNum", "packNum", "htiming", "hpurchase", "huse", "hcompensation", "choice","Y", "Framing")) %>%
  filter(choiceNum < 6) %>%
  drop_na("htiming")

choice_t = stack_choice %>%  
  select(c("ID", "choiceNum", "packNum", "ttiming", "tpurchase", "tuse", "tcompensation", "choice","Y", "Framing")) %>%
  filter(choiceNum >= 6) %>%
  drop_na("ttiming")

rating_h = stack_rating %>% 
  select(c("ID", "choiceNum", "packNum", "htiming", "hpurchase", "huse", "hcompensation", "rating", "bin_rate", "Framing")) %>%
  filter(choiceNum < 6) %>%
  drop_na("htiming")

rating_t = stack_rating %>%  
  select(c("ID", "choiceNum", "packNum", "ttiming", "tpurchase", "tuse", "tcompensation", "rating", "bin_rate", "Framing")) %>%
  filter(choiceNum >= 6) %>%
  drop_na("ttiming")

# convert results conjoint to factor
cols2fah <- c("htiming", "hpurchase", "huse", "hcompensation", "Framing", "choiceNum", "packNum")
choice_h[cols2fah] <- lapply(choice_h[cols2fah], factor)
rating_h[cols2fah] <- lapply(rating_h[cols2fah], factor)

cols2fat <- c("ttiming", "tpurchase", "tuse", "tcompensation", "Framing", "choiceNum", "packNum")
choice_t[cols2fat] <- lapply(choice_t[cols2fat], factor)
rating_t[cols2fat] <- lapply(rating_t[cols2fat], factor)

# convert choiceNum for transport to 1-5
choice_t = choice_t %>%
  mutate(choiceNum = fct_recode(choiceNum, "1" = "6", 
                               "2"= "7",
                               "3"= "8",
                               "4" = "9", 
                               "5" = "10"))

rating_t = rating_t %>%
  mutate(choiceNum = fct_recode(choiceNum, "1" = "6", 
                               "2"= "7",
                               "3"= "8",
                               "4" = "9", 
                               "5" = "10"))
# rename coloumns
choice_h = choice_h %>% 
  rename ( "Timing" = "htiming",
           "Purchase" = "hpurchase",
           "Use" = "huse",
           "Support" = "hcompensation") 

choice_t = choice_t %>% 
  rename ( "Timing" = "ttiming",
           "Purchase" = "tpurchase",
           "Use" = "tuse",
           "Support" = "tcompensation")

rating_h = rating_h %>% 
  rename ( "Timing" = "htiming",
           "Purchase" = "hpurchase",
           "Use" = "huse",
           "Support" = "hcompensation")

rating_t = rating_t %>% 
  rename ( "Timing" = "ttiming",
           "Purchase" = "tpurchase",
           "Use" = "tuse",
           "Support" = "tcompensation")

# recode instrument levels
choice_h = choice_h %>%
  mutate(Purchase = fct_recode(Purchase, "No purchase instrument" = "Keine Maßnahme", 
                             "Purchase tax on fossil fuel heating (10%)"= "Kaufsteuer auf fossile Heizungen (10% Kaufpreis)",
                             "Purchase tax on fossil fuel heating (20%)"= "Kaufsteuer auf fossile Heizungen (20% Kaufpreis)",
                             "Purchase ban for fossil fuel heating (2025)" = "Kaufverbot für fossile Heizungen ab 2025", 
                             "Purchase ban for fossil fuel heating (2030)" = "Kaufverbot für fossile Heizungen ab 2030")) %>%
  mutate(Use = fct_recode(Use, "No use instrument" = "Keine Maßnahme", 
                             "Tax on fossil fuels (20 ct/l)"= "Steuer auf fossile Energieträger (20 ct/l)",
                             "Tax on fossil fuels (50 ct/l)"= "Steuer auf fossile Energieträger (50 ct/l)",
                             "Replacement of fossil heating (> 30 years)" = "Ersatz fossile Heizungen (älter als 30 Jahre)", 
                             "Replacement of fossil heating (> 15 years)" = "Ersatz fossile Heizungen (älter als 15 Jahre)")) %>%
  mutate(Support = fct_recode(Support, "No supporting instrument" = "Keine unterstützende Maßnahme", 
                            "Subsidies for climate-friendly alternatives"= "Subvention für klimafreundliche Alternativen",
                            "Trade in bonus"= "Eintauschprämie",
                            "State-supported building renovation measures" = "Staatlich geförderte Gebäudesanierungsmaßnahmen",
                            "Preferential loan" = "Vorzugsdarlehen"))

choice_t = choice_t %>%
  mutate(Purchase = fct_recode(Purchase, "No purchase instrument" = "Keine Maßnahme", 
                               "Purchase tax on ICEV (10%)"= "Kaufsteuer auf Verbrenner (10% Kaufpreis)",
                               "Purchase tax on ICEV (20%)"= "Kaufsteuer auf Verbrenner (20% Kaufpreis)",
                               "Purchase ban for ICEV (2025)" = "Kaufverbot für Verbrenner ab 2025", 
                               "Purchase ban for ICEV (2030)" = "Kaufverbot für Verbrenner ab 2030")) %>%
  mutate(Use = fct_recode(Use, "No use instrument" = "Keine Maßnahme", 
                          "Tax on fossil fuels (20 ct/l)"= "Steuer auf fossile Energieträger (20 ct/l)",
                          "Tax on fossil fuels (50 ct/l)"= "Steuer auf fossile Energieträger (50 ct/l)",
                          "Weekday ban on ICEVs in city centers" = "Fahrverbot für Verbrenner in Innenstädten an Werktagen", 
                          "Daily ban on ICEVs in city centers" = "Tägliches Fahrverbot für Verbrenner in Innenstädten")) %>%
  mutate(Support = fct_recode(Support, "No supporting instrument" = "Keine unterstützende Maßnahme", 
                                   "Subsidies for climate-friendly alternatives"= "Subvention für klimafreundliche Alternativen",
                                   "Trade in bonus"= "Eintauschprämie",
                                   "State-supported infrastructure measures" = "Staatlich geförderte Infrastrukturmaßnahmen (Ladeinfrastruktur, ÖV)",
                                   "Preferential loan" = "Vorzugsdarlehen"))

rating_h = rating_h %>%
  mutate(Purchase = fct_recode(Purchase, "No purchase instrument" = "Keine Maßnahme", 
                               "Purchase tax on fossil fuel heating (10%)"= "Kaufsteuer auf fossile Heizungen (10% Kaufpreis)",
                               "Purchase tax on fossil fuel heating (20%)"= "Kaufsteuer auf fossile Heizungen (20% Kaufpreis)",
                               "Purchase ban for fossil fuel heating (2025)" = "Kaufverbot für fossile Heizungen ab 2025", 
                               "Purchase ban for fossil fuel heating (2030)" = "Kaufverbot für fossile Heizungen ab 2030")) %>%
  mutate(Use = fct_recode(Use, "No use instrument" = "Keine Maßnahme", 
                          "Tax on fossil fuels (20 ct/l)"= "Steuer auf fossile Energieträger (20 ct/l)",
                          "Tax on fossil fuels (50 ct/l)"= "Steuer auf fossile Energieträger (50 ct/l)",
                          "Replacement of fossil heating (> 30 years)" = "Ersatz fossile Heizungen (älter als 30 Jahre)", 
                          "Replacement of fossil heating (> 15 years)" = "Ersatz fossile Heizungen (älter als 15 Jahre)")) %>%
  mutate(Support = fct_recode(Support, "No supporting instrument" = "Keine unterstützende Maßnahme", 
                                   "Subsidies for climate-friendly alternatives"= "Subvention für klimafreundliche Alternativen",
                                   "Trade in bonus"= "Eintauschprämie",
                                   "State-supported building renovation measures" = "Staatlich geförderte Gebäudesanierungsmaßnahmen",
                                   "Preferential loan" = "Vorzugsdarlehen"))

rating_t = rating_t %>%
  mutate(Purchase = fct_recode(Purchase, "No purchase instrument" = "Keine Maßnahme", 
                               "Purchase tax on ICEV (10%)"= "Kaufsteuer auf Verbrenner (10% Kaufpreis)",
                               "Purchase tax on ICEV (20%)"= "Kaufsteuer auf Verbrenner (20% Kaufpreis)",
                               "Purchase ban for ICEV (2025)" = "Kaufverbot für Verbrenner ab 2025", 
                               "Purchase ban for ICEV (2030)" = "Kaufverbot für Verbrenner ab 2030")) %>%
  mutate(Use = fct_recode(Use, "No use instrument" = "Keine Maßnahme", 
                          "Tax on fossil fuels (20 ct/l)"= "Steuer auf fossile Energieträger (20 ct/l)",
                          "Tax on fossil fuels (50 ct/l)"= "Steuer auf fossile Energieträger (50 ct/l)",
                          "Weekday ban on ICEVs in city centers" = "Fahrverbot für Verbrenner in Innenstädten an Werktagen", 
                          "Daily ban on ICEVs in city centers" = "Tägliches Fahrverbot für Verbrenner in Innenstädten")) %>%
  mutate(Support = fct_recode(Support, "No supporting instrument" = "Keine unterstützende Maßnahme", 
                                   "Subsidies for climate-friendly alternatives"= "Subvention für klimafreundliche Alternativen",
                                   "Trade in bonus"= "Eintauschprämie",
                                   "State-supported infrastructure measures" = "Staatlich geförderte Infrastrukturmaßnahmen (Ladeinfrastruktur, ÖV)",
                                   "Preferential loan" = "Vorzugsdarlehen"))

write_feather(d, snakemake@output[["d"]])
write_feather(first_sector, snakemake@output[["first_sector"]])
write_feather(framing, snakemake@output[["framing"]])
write_feather(resp_char, snakemake@output[["resp_char"]])
write_feather(attitudes, snakemake@output[["attitudes"]])

write_feather(rating_t, snakemake@output[["rating_t"]])
write_feather(rating_h, snakemake@output[["rating_h"]])
write_feather(choice_t, snakemake@output[["choice_t"]])
write_feather(choice_h, snakemake@output[["choice_h"]])