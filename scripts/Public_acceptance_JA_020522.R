library(cjoint)
library(cregg)
library(forcats)
library(ggpubr)
library(janitor)
library(likert)
library(psych)
library(readxl)
library(tidyverse)
devtools::install_github("LukasWallrich/rNuggets@5dc76f1")

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
# Distribution of framing and first sectors
## 

#framing
freq_framing <- d %>% 
  mutate(Framing = fct_recode(Framing,
                              "Phase-Out"  = "P",
                              "Renewables" = "R")) %>%
  count(Framing) %>%
  mutate(Framing = fct_reorder(Framing, n, .desc = FALSE)) %>%
  ggplot(aes(x=Framing, y = n)) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18))
freq_framing

#first sector
freq_first <- d %>% 
  mutate(First = fct_recode(First,
                              "Heating"  = "H",
                              "Transport" = "T")) %>%
  count(First) %>%
  mutate(First = fct_reorder(First, n, .desc = FALSE)) %>%
  ggplot(aes(x=First, y = n)) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.y = element_text(size = 18, face ="bold"), axis.title.x = element_text(size = 18, face ="bold"))+
  ggtitle("Sequence sectors (first one)")
freq_first

#framing and sequence sectors
freq_firstfr <- d %>% 
  mutate(First = fct_recode(First,
                            "Heating"  = "H",
                            "Transport" = "T")) %>%
  mutate(Framing = fct_recode(Framing,
                              "Phase-Out"  = "P",
                              "Renewables" = "R")) %>%
  group_by(Framing, First) %>% 
  summarise(count=n()) %>%
  mutate(category = ifelse(Framing == "Phase-Out" & First == "Heating" , "Phase-Out & Heating",
                         ifelse(Framing == "Phase-Out" & First == "Transport" , "Phase-Out & Transport",
                                ifelse(Framing == "Renewables" & First == "Heating" , "Renewables & Heating",
                                       ifelse(Framing == "Renewables" & First == "Transport" , "Renewables & Transport", NA))))) %>%
  mutate(category = fct_reorder(category, count, .desc = FALSE)) %>%
  ggplot(aes(x=category, y = count/sum(count))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Share of respondents") +
  theme(axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18,margin = margin(t = 10, r = 0, b = 0, l = 0)))
freq_firstfr
ggsave("Frequency Framing & Sector", device=png, 
       path = "build/figures-and-tables/checks",
       width = 12 , height = 6 )

##############################################################################################
# Distribution of recoded group sizes for heating and transport
## 

# energy source heating (recoded groups)
dist_sourcehcat <- resp_char %>% 
  count(source_h_cat) %>%
  mutate(source_h_cat = fct_reorder(source_h_cat, n, .desc = FALSE)) %>%
  ggplot(aes(x=source_h_cat, y = n/sum(n))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Share of respondents") +
  theme(plot.title = element_text(size = 20, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ggtitle("a. Energy source heating")

dist_sourcehcat
ggsave("Distribution energy source heating", device=png, 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 6 )

# car use (recoded groups)
dist_carusecat <- resp_char %>% 
  count(cardays_cat) %>%
  mutate(cardays_cat = fct_reorder(cardays_cat, n, .desc = FALSE)) %>%
  ggplot(aes(x=cardays_cat, y = n/sum(n))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Share of respondents") +
  theme(plot.title = element_text(size = 20, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ggtitle("b. Weekly car use")

ggsave("Distribution weekly car use", device=png, 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 6 )

# combination
subgroups_recoht_p <- ggarrange(dist_sourcehcat, dist_carusecat,
                         ncol = 2, nrow = 1)
subgroups_recoht_p 

ggsave(filename = "Recoded h&t", device = "png", 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 4)

##############################################################################################
# Distribution of group sizes for climate change evaluation, trust and responsibility 
## 

# climate change evaluation
dist_cc <- attitudes %>% 
  drop_na(cceval_cat) %>% 
  count(cceval_cat) %>%
  mutate(cceval_cat = fct_reorder(cceval_cat, n, .desc = FALSE)) %>%
  ggplot(aes(x=cceval_cat, y = n/sum(n))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 20, face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16)) +
  ggtitle("a. Climate change evaluation")

ggsave("Distribution climate change evaluation ", device=png, 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 6 )

# trust in government
dist_trust_gov <- attitudes %>% 
  drop_na(trust_gov_cat) %>% 
  count(trust_gov_cat) %>%
  mutate(trust_gov_cat = fct_reorder(trust_gov_cat, n, .desc = FALSE)) %>%
  ggplot(aes(x=trust_gov_cat, y = n/sum(n))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 20, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16)) +
  ggtitle("b. Trust in government")

dist_trust_gov
ggsave("Distribution trust government", device=png, 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 6 )

# trust in citizens
dist_trust_cit <- attitudes %>% 
  drop_na(trust_cit_cat) %>% 
  count(trust_cit_cat) %>%
  mutate(trust_cit_cat = fct_reorder(trust_cit_cat, n, .desc = FALSE)) %>%
  ggplot(aes(x=trust_cit_cat, y = n/sum(n))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 20, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16)) +
  ggtitle("c. Trust in individuals")
 
dist_trust_cit
ggsave("Distribution trust citizens", device=png, 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 6 )

 # trust in companies
 dist_trust_com <- attitudes %>% 
   drop_na(trust_com_cat) %>% 
   count(trust_com_cat) %>%
   mutate(trust_com_cat = fct_reorder(trust_com_cat, n, .desc = FALSE)) %>%
   ggplot(aes(x=trust_com_cat, y = n/sum(n))) + 
   geom_bar(stat = 'identity', na.rm = TRUE) +
   theme_bw() +
   coord_flip() +
   xlab("") +
   ylab("") +
   theme(plot.title = element_text(size = 20, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16))+
   ggtitle("d. Trust in companies")
 
 dist_trust_com
 ggsave("Distribution trust companies", device=png, 
        path = "build/figures-and-tables/checks/distributions-subgroups",
        width = 12 , height = 6 )
 
# attributed responsibility to government
dist_resp_gov <- attitudes %>% 
  drop_na(resp_gov_cat) %>% 
  count(resp_gov_cat) %>%
  mutate(resp_gov_cat = fct_reorder(resp_gov_cat, n, .desc = FALSE)) %>%
  ggplot(aes(x=resp_gov_cat, y = n/sum(n))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Share of respondents") +
  theme(plot.title = element_text(size = 20, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18,  margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ggtitle("e. Responsibility government")

dist_resp_gov
ggsave("Distribution responsibility government ", device=png, 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 6 )

# attributed responsibility to citizens
dist_resp_cit <- attitudes %>% 
  drop_na(resp_cit_cat) %>% 
  count(resp_cit_cat) %>%
  mutate(resp_cit_cat = fct_reorder(resp_cit_cat, n, .desc = FALSE)) %>%
  ggplot(aes(x=resp_cit_cat, y = n/sum(n))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Share of repondents") +
  theme(plot.title = element_text(size = 20, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18,  margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ggtitle("f. Responsibiliy individuals")

dist_resp_cit
ggsave("Distribution responsibility citizens", device=png, 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 6 )

# attributed responsibility to companies
dist_resp_com <- attitudes %>% 
  drop_na(resp_com_cat) %>% 
  count(resp_com_cat) %>%
  mutate(resp_com_cat = fct_reorder(resp_com_cat, n, .desc = FALSE)) %>%
  ggplot(aes(x=resp_com_cat, y = n/sum(n))) + 
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Share of respondents") +
  theme(plot.title = element_text(size = 20, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18,  margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  ggtitle("g. Responsibility companies")

dist_resp_com
ggsave("Distribution responsibility companies", device=png, 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 12 , height = 6)

# combination
subgroups_p <- ggarrange(dist_cc, NULL, NULL,
                          dist_trust_gov, dist_trust_cit, dist_trust_com,
                          dist_resp_gov, dist_resp_cit, dist_resp_com,
                         ncol = 3, nrow = 3)
subgroups_p 

ggsave(filename = "Subgroups", device = "png", 
       path = "build/figures-and-tables/checks/distributions-subgroups",
       width = 16 , height = 12)

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

##############################################################################################
# Sample statistics
##

# get table with respondent characteristics
resp_char_shares <- apply(resp_char, 2, tabyl)
capture.output(resp_char_shares, 
               file = "build/figures-and-tables/descriptive-statistics/respondent_char shares.csv")

##############################################################################################
# Sample characteristics (heating and transport)
##

# homeownership
ownership_p <- resp_char %>%
  drop_na(ownership)%>%
  count(ownership) %>%
  mutate(building_type = fct_reorder(ownership, n, .desc = FALSE)) %>%
  ggplot(aes(x = ownership, y = n/sum(n))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  coord_flip() +
  theme(legend.position="none")+
  labs(x = "", y = "Share of respondents") +
  theme(plot.title = element_text(size = 24, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18, margin = margin(t =10, r = 0, b = 10, l = 0)))+
  ggtitle("a. Homeownership")
ownership_p
ggsave(filename = "ownership", device = "png", 
       path = "build/figures-and-tables/descriptive-statistics")

# buildingtype
buildingtype_p <- resp_char %>%
  drop_na(building_type)%>%
  count(building_type) %>%
  mutate(building_type = fct_reorder(building_type, n, .desc = FALSE)) %>%
  ggplot(aes(x = building_type, y = n/sum(n))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  coord_flip() +
  theme(legend.position="none")+
  labs(x = "", y = "Share of respondents") +
  theme(plot.title = element_text(size = 24, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18, margin = margin(t =10, r = 0, b = 10, l = 0)))+
  ggtitle("b. Building type")
buildingtype_p
ggsave(filename = "building-type", device = "png", 
       path = "build/figures-and-tables/descriptive-statistics")

# energy source heating
sourceh_p <- resp_char %>%
  count(source_h) %>%
  mutate(source_h = fct_reorder(source_h, n, .desc = FALSE)) %>%
  ggplot(aes(x = source_h, y = n/sum(n))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  coord_flip() +
  theme(legend.position="none")+
  labs(x = "", y = "Share of respondents")+
  theme(plot.title = element_text(size = 24, face = "italic",  margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18, margin = margin(t =10, r = 0, b = 10, l = 0)))+
  ggtitle("c. Energy source heating")
sourceh_p
ggsave(filename = "source heating", device = "png", 
       path = "build/figures-and-tables/descriptive-statistics")

# weekly car use
cardays_p <- resp_char %>%
  count(car_days) %>%
  mutate(car_days = fct_reorder(car_days, n, .desc = FALSE)) %>%
  ggplot(aes(x = car_days, y = n/sum(n))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  coord_flip() +
  theme(legend.position="none") +
  labs(x = "", y = "Share of respondents") +
  theme(plot.title = element_text(size = 24, face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18, margin = margin(t =10, r = 0, b = 10, l = 0)))+
  ggtitle("d. Weekly car use")
cardays_p
ggsave(filename = "cardays", device = "png", 
       path = "build/figures-and-tables/descriptive-statistics")

# car type
cartype_p <- resp_char %>%
  drop_na(car_type)%>%
  count(car_type) %>%
  mutate(car_type = fct_reorder(car_type, n, .desc = FALSE)) %>%
  ggplot(aes(x = car_type, y = n/sum(n))) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  coord_flip() +
  theme(legend.position="none")+
  labs(x = "", y = "Share of respondents") +
  theme(plot.title = element_text(size = 24, face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=16),axis.text.y = element_text(size=16), axis.title.x = element_text(size = 18, margin = margin(t =10, r = 0, b = 10, l = 0)))+
  ggtitle("e. Car type")
cartype_p
ggsave(filename = "cartype", device = "png", 
       path = "build/figures-and-tables/descriptive-statistics")

# combine plots
htvariables_p <- ggarrange(ownership_p, buildingtype_p, sourceh_p,
                            cardays_p, cartype_p, NULL,
                            ncol = 3, nrow = 2)
htvariables_p

ggsave(filename = "heating and transport variables", device = "png", 
       path = "build/figures-and-tables/descriptive-statistics",
       width = 18 , height = 10)

##############################################################################################
# Sample statistics of attitudes towards climate change evaluation, trust and responsibility
##

# labels für likert scale questions (attitudes)
lbs_climate <- c("disagree", "somewhat disagree", "neither  / nor",  "somewhat agree", "agree")
lbs_trust <- c("do not trust", "rather do not trust", "neither  / nor", "rather trust", "trust")
lbs_responsibility <- c("none","low", "neither  / nor", "some", "high")

# prepare attitudes table 
attitudes_climate <- d %>%
  select("climate_change_1", "climate_change_2","climate_change_3", "climate_change_4", "climate_change_5", "climate_change_6") %>%
  rename ("Climate change is ongoing." = "climate_change_1",
          "Climate change results from human activities." = "climate_change_2",
          "The consequences of climate change are already being felt today." = "climate_change_3",
          "Climate change will affect me, or people close to me." = "climate_change_4",
          "Germany must be climate-neutral by 2050 at the latest." = "climate_change_5",
          "Germany must phase out fossil fuels." = "climate_change_6") %>%
  mutate_if(is.numeric, factor, levels = 1:5, labels = lbs_climate) %>%
  drop_na() %>%
  as.data.frame()

# table & graph %-responses climate change attitudes
climate_t <- likert(attitudes_climate)
climate_t
capture.output(climate_t, file = "build/figures-and-tables/attitudes/attitudes_c.csv")
climate_p <- plot(likert(attitudes_climate), 
                  group.order = c("Climate change is ongoing.", "Climate change results from human activities.", "The consequences of climate change are already being felt today.", "Climate change will affect me, or people close to me.", "Germany must be climate-neutral by 2050 at the latest.", "Germany must phase out fossil fuels."), 
                  legend = "Level of agreement", legend.position = "bottom",
                  text.size=0) +
              theme_bw()+
              theme(axis.text.x = element_text(size = 20), axis.text.y =element_text(size=20), axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20)) + 
              theme (legend.text =element_text(size=20), legend.title = element_text(size=16, face ="bold"))
  
climate_p
ggsave(filename = "attitudes_c", device = "png", 
       path = "build/figures-and-tables/attitudes",
       width = 17, height= 7 ) 

# table & graph %-responses trust
attitudes_trust <- d %>%
  select("trust_1", "trust_2", "trust_3") %>%
  rename ("Federal government" = "trust_1",
          "German companies and corporations" = "trust_2",
          "Every individual" = "trust_3") %>%
  mutate_if(is.numeric, factor, levels = 1:5, labels = lbs_trust) %>%
  drop_na() %>%
  as.data.frame()

trust_t <- likert(attitudes_trust)
capture.output(trust_t, file = "build/figures-and-tables/attitudes/trust.csv")
trust_p <- plot(likert(attitudes_trust),
                group.order = c ("Federal government","German companies and corporations","Every individual"),
                legend = "Level of trust", legend.position = "bottom",
                text.size=0) +
           theme_bw()+
           theme(axis.text.x = element_text(size = 20), axis.text.y =element_text(size=20), axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20)) + 
           theme (legend.position = "bottom", legend.text =element_text(size=20), legend.title = element_text(size=16, face ="bold"))
trust_p
ggsave(filename = "attitudes_t", device = "png", 
       path = "build/figures-and-tables/attitudes",
       width = 16, height = 8) 

# table & graph %-responses responsibilities
attitudes_responsibility <- d %>%
  select("responsibility_1", "responsibility_2", "responsibility_3") %>%
  rename ("Federal government" = "responsibility_1",
          "German companies and corporations" = "responsibility_2",
          "Every individual" = "responsibility_3") %>%
  mutate_if(is.numeric, factor, levels = 1:5, labels = lbs_responsibility) %>%
  drop_na() %>%
  as.data.frame()

responsibility_t <- likert(attitudes_responsibility)
capture.output(responsibility_t, file = "build/figures-and-tables/attitudes/responsibility.csv")
responsibility_p <- plot(likert(attitudes_responsibility), 
                         group.order = c ("Federal government","German companies and corporations","Every individual"),
                         legend = "Attributed responsibility", legend.position = "bottom",
                         text.size=0) +
                theme_bw()+
                theme(axis.text.x = element_text(size = 20), axis.text.y =element_text(size=20), axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20)) + 
                theme (legend.position = "bottom", legend.text =element_text(size=20), legend.title = element_text(size=16, face ="bold"))
responsibility_p
ggsave(filename = "attitudes_r", device = "png", 
       path = "build/figures-and-tables/attitudes",
       width = 16, height = 8) 

##############################################################################################
# Frequency of attribute levels
##

# frequencies for heating (choice)
freqh_choice <- cj_freqs( data = choice_h,Y ~ Timing + Purchase + Use + Support, id = ~ ID)
freqh_choice_p <- ggplot(freqh_choice,
                         aes(x = level, y = estimate, fill = feature)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="RdBu") +
  theme_bw()+
  xlab("Levels") + 
  ylab("")+
  ggtitle ("a. Heating (Forced choice)") +
  theme(plot.title = element_text(face = "italic", size = 20, margin = margin(t = 0, r = 0, b = 10, l = 0)),axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 10)), axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15))+
  coord_flip() +
  theme(legend.position="none")
freqh_choice_p

# frequencies for transport (choice)
freqt_choice <- cj_freqs( data = choice_t, Y ~ Timing + Purchase + Use + Support, id = ~ ID)
freqt_choice_p <- ggplot(freqt_choice,
                         aes(x = level, y = estimate, fill = feature)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="RdBu") +
  theme_bw()+
  xlab("Levels") + 
  ylab("Frequency")+
  ggtitle ("c. Transport (Forced choice)") +
  theme(plot.title = element_text(face = "italic", size = 20, margin = margin(t = 0, r = 0, b = 10, l = 0)),axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 10)), axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15))+
  coord_flip() +
  theme(legend.position="none")
freqt_choice_p

# frequencies for heating (rating)
freqh_rating <- cj_freqs( data = rating_h, bin_rate ~ Timing + Purchase + Use + Support, id = ~ ID)
freqh_rating_p <- ggplot(freqh_rating,
                         aes(x = level, y = estimate, fill = feature)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="RdBu") +
  theme_bw()+
  xlab("") + 
  ylab("")+
  ggtitle ("b. Heating (Rating)") +  
  theme(plot.title = element_text(face = "italic", size = 20, margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15))+
  coord_flip() +
  theme(legend.position="none")
freqh_rating_p

# frequencies for transport (rating)
freqt_rating <- cj_freqs( data = rating_t, bin_rate ~ Timing + Purchase + Use + Support, id = ~ ID)
freqt_rating_p<- ggplot(freqt_rating,
                        aes(x = level, y = estimate, fill = feature)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="RdBu") +
  theme_bw()+
  xlab("") + 
  ylab("Frequency")+
  ggtitle ("d. Transport (Rating)") +
  theme(plot.title = element_text(face = "italic", size = 20, margin = margin(t = 0, r = 0, b = 10, l = 0)), axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15))+
  coord_flip() +
  theme(legend.position="none")
freqt_rating_p

# combine all into one plot
freq_attr <- ggarrange(freqh_choice_p, freqh_rating_p, freqt_choice_p, freqt_rating_p,
                        ncol = 2, nrow = 2)

freq_attr
ggsave("Frequencies of attribute levels", device=png, 
       path = "build/figures-and-tables/checks",
       width = 17, height = 20)

##############################################################################################
# Analyse whether framing has a significant effect, and show MM with framng as an interaction effect

# compute F-test for framing interaction 
anova_framingch <- cj_anova(data = choice_h, Y ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~Framing)
capture.output(anova_framingch, file = "build/figures-and-tables/framing-interaction/anova_fch.csv")

anova_framingct <- cj_anova(data = choice_t, Y ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~Framing)
capture.output(anova_framingct, file = "build/figures-and-tables/framing-interaction/anova_fct.csv")

anova_framingrh <- cj_anova(data = rating_h, bin_rate ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~Framing)
capture.output(anova_framingrh, file = "build/figures-and-tables/framing-interaction/anova_frh.csv")

anova_framingrt <- cj_anova(data = rating_t, bin_rate ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~Framing)
capture.output(anova_framingrt, file = "build/figures-and-tables/framing-interaction/anova_frt.csv")

# plot framing interaction heating (choice)
int_framingch <- cj(data = choice_h, Y ~ Timing + Purchase + Use + Support,
              estimate = "mm", id = ~ "ID", by = ~Framing)

level_orderfrch <- factor(int_framingch$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                    "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                    "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                    "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

int_framingch_p <- ggplot(int_framingch, 
                    aes(x = level_orderfrch, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Framing"))

int_framingch_p
ggsave("int_framingch", device=png, 
       path = "build/figures-and-tables/framing-interaction",
       width = 18, height = 16)

# plot framing interaction heating (rating)
int_framingrh <- cj(data = rating_h, rating ~ Timing + Purchase + Use + Support,
              estimate = "mm", id = ~ "ID", by = ~Framing)

level_orderfrrh <- factor(int_framingrh$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                      "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                      "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                      "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

int_framingrh_p <- ggplot(int_framingrh, 
       aes(x = level_orderfrrh, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Framing"))

int_framingrh_p
ggsave("int_framingrh", device=png, 
       path = "build/figures-and-tables/framing-interaction",
       width = 18, height = 16)

# Final plot Interaction heating
int_framing_heating <- rbind(int_framingch, int_framingrh)
int_framing_heating$outcome <- rep(c("Choice", "Rating"), each=nrow(int_framingch))

IntFrH_p <- ggplot(int_framing_heating, 
                aes(x = level, y = estimate, color = BY)) +
  facet_grid(feature~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(data = int_framing_heating %>% filter(outcome == "Choice"),
             aes(yintercept = 0.5), linetype = "dashed")+
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values = c("#D8B365", "#5AB4AC")) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Framing"))

IntFrH_p
ggsave("interaction heating_final", device=png, 
       path = "build/figures-and-tables/framing-interaction",
       width = 16, height = 20)

# plot framing interaction transport (choice)
int_framingct <- cj(data = choice_t, Y ~ Timing + Purchase + Use + Support,
              estimate = "mm", id = ~ "ID", by = ~Framing)

level_orderfrct <- factor(int_framingct$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                      "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                      "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                      "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

plot(int_framingct , group = "Framing", vline = 0.5) + ggtitle("Interaction framing, choice transport")

int_framingct_p <- ggplot(int_framingct, 
                    aes(x = level_orderfrct, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Framing"))

int_framingct_p
ggsave("int_framingct", device=png, 
       path = "build/figures-and-tables/framing-interaction",
       width = 18, height = 16)

# plot framing interaction transport (rating)
int_framingrt <- cj(data = rating_t, rating ~ Timing + Purchase + Use + Support,
              estimate = "mm", id = ~ "ID", by = ~Framing)
plot(int_framingrt , group = "Framing")  + ggtitle("Interaction framing, rating transport")

level_orderfrrt <- factor(int_framingrt$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                      "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                      "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                      "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

int_framingrt_p <- ggplot(int_framingrt, 
                    aes(x = level_orderfrrt, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Framing"))

int_framingrt_p
ggsave("int_framingrt", device=png, 
       path = "build/figures-and-tables/framing-interaction",
       width = 18, height = 16)

# Final plot Interaction Transport
int_framing_transport <- rbind(int_framingct, int_framingrt)
int_framing_transport$outcome <- rep(c("Choice", "Rating"), each=nrow(int_framingct))

IntFrT_p <- ggplot(int_framing_transport, 
                aes(x = level, y = estimate, color = BY)) +
  facet_grid(feature~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(data = int_framing_transport %>% filter(outcome == "Choice"),
             aes(yintercept = 0.5), linetype = "dashed")+
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Framing"))

IntFrT_p
ggsave("interaction transport_final", device=png, 
       path = "build/figures-and-tables/framing-interaction",
       width = 16, height = 20)


##############################################################################################
# Main analysis conjoint survey experiment - marginal means
##

# calculate marginal means
mmh_choice <- cj(data = choice_h, Y ~ Timing + Purchase + Use + Support, 
                 id = ~ ID, estimate = "mm")
level_orderhc <- factor(mmh_choice$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                        "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                        "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                        "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
write.csv(mmh_choice, file = "build/figures-and-tables/marginal-means/mmh_choice.csv")

mmh_rating <- cj(data = rating_h, rating ~ Timing + Purchase + Use + Support, 
                 id = ~ ID, estimate = "mm")
level_orderhr <- factor(mmh_rating$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                        "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                        "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                        "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
write.csv(mmh_rating, file = "build/figures-and-tables/marginal-means/mmh_rating.csv")


mmt_choice <- cj(data = choice_t, Y ~ Timing + Purchase + Use + Support, 
                 id = ~ ID, estimate = "mm")
level_ordertc <- factor(mmt_choice$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                        "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                        "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                        "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
write.csv(mmt_choice, file = "build/figures-and-tables/marginal-means/mmt_choice.csv")

mmt_rating <- cj(data = rating_t, rating ~ Timing + Purchase + Use + Support, 
                 id = ~ ID, estimate = "mm")
level_ordertr <- factor(mmt_rating$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                        "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                        "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                        "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
write.csv(mmt_rating, file = "build/figures-and-tables/marginal-means/mmt_rating.csv")


# plot marginal means heating (choice)
MMHChoice <- ggplot(mmh_choice,
                         aes(x = level_orderhc, y = estimate, color = feature)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none") +
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip()
ggsave("mmh_choice", device=png, 
       path = "build/figures-and-tables/marginal-means",
       width = 16, height = 20)

# plot marginal means heating (rating)
MMHRating <- ggplot(mmh_rating,
                    aes(x = level_orderhr, y = estimate, color = feature)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none") +
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip()
ggsave("mmh_rating", device=png, 
       path = "build/figures-and-tables/marginal-means",
       width = 16, height = 20)

# plot marginal means transport (choice)
MMTChoice <- ggplot(mmt_choice,
                    aes(x = level_ordertc, y = estimate, color = feature)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none") +
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip()
ggsave("mmt_choice", device=png, 
       path = "build/figures-and-tables/marginal-means",
       width = 16, height = 20)

# plot marginal means transport (rating)
MMTRating <- ggplot(mmt_rating,
                    aes(x = level_ordertr, y = estimate, color = feature)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none") +
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip()
ggsave("mmt_rating", device=png, 
       path = "build/figures-and-tables/marginal-means",
       width = 16, height = 20)

##############################################################################################
# Main analysis conjoint survey experiment - AMCE
##

# define baselines
baseline_conjoint <- list(Timing="2030",
                          Purchase = "No purchase instrument",
                          Use = "No use instrument",
                          Support= "No supporting instrument")

# calculate AMCES (cjoint)
amceh_choice <- cjoint::amce(Y ~ Timing + Purchase + Use + Support, data=choice_h, 
                             cluster=TRUE, respondent.id="ID", baselines = baseline_conjoint, na.ignore = TRUE) 

summary(amceh_choice)

amcet_choice <- cjoint::amce(Y ~ Timing + Purchase + Use + Support, data = choice_t, 
                             cluster=TRUE, respondent.id="ID", baselines = baseline_conjoint, na.ignore = TRUE)

summary(amcet_choice)

amceh_rating <- cjoint::amce(rating ~ Timing + Purchase + Use + Support, data = rating_h, 
                             cluster=TRUE, respondent.id="ID", baselines = baseline_conjoint, na.ignore = TRUE)
summary(amceh_rating)

amcet_rating <- cjoint::amce(rating ~ Timing + Purchase + Use + Support, data = rating_t,
                             cluster=TRUE, respondent.id="ID", baselines = baseline_conjoint, na.ignore = TRUE)
summary(amcet_rating)

capture.output(summary(amceh_choice), file ="build/figures-and-tables/AMCE/sumamceh_choice.csv")
capture.output(summary(amcet_choice), file ="build/figures-and-tables/AMCE/sumamcet_choice.csv")
capture.output(summary(amceh_rating), file ="build/figures-and-tables/AMCE/sumamceh_rating.csv")
capture.output(summary(amcet_rating), file ="build/figures-and-tables/AMCE/sumamcet_rating.csv")

# Final plot AMCE Heating Choice
c_timing_amcehc <- c(0, amceh_choice$estimates$Timing["AMCE","Timing2035"], amceh_choice$estimates$Timing["AMCE","Timing2040"], amceh_choice$estimates$Timing["AMCE","Timing2045"], amceh_choice$estimates$Timing["AMCE","Timing2050"])
s_timing_amcehc <- c(0, amceh_choice$estimates$Timing["Std. Error","Timing2035"], amceh_choice$estimates$Timing["Std. Error","Timing2040"], amceh_choice$estimates$Timing["Std. Error","Timing2045"], amceh_choice$estimates$Timing["Std. Error","Timing2050"])

c_purchase_amcehc <- c(0, amceh_choice$estimates$Purchase["AMCE","PurchasePurchasetaxonfossilfuelheating10"], amceh_choice$estimates$Purchase["AMCE","PurchasePurchasetaxonfossilfuelheating20"],amceh_choice$estimates$Purchase["AMCE","PurchasePurchasebanforfossilfuelheating2030"], amceh_choice$estimates$Purchase["AMCE","PurchasePurchasebanforfossilfuelheating2025"])
s_purchase_amcehc <- c(0, amceh_choice$estimates$Purchase["Std. Error","PurchasePurchasetaxonfossilfuelheating10"], amceh_choice$estimates$Purchase["Std. Error","PurchasePurchasetaxonfossilfuelheating20"],amceh_choice$estimates$Purchase["Std. Error","PurchasePurchasebanforfossilfuelheating2030"], amceh_choice$estimates$Purchase["Std. Error","PurchasePurchasebanforfossilfuelheating2025"])

c_use_amcehc <- c(0, amceh_choice$estimates$Use["AMCE","UseTaxonfossilfuels20ctl"], amceh_choice$estimates$Use["AMCE","UseTaxonfossilfuels50ctl"],  amceh_choice$estimates$Use["AMCE","UseReplacementoffossilheating30years"],amceh_choice$estimates$Use["AMCE","UseReplacementoffossilheating15years"])
s_use_amcehc <- c(0, amceh_choice$estimates$Use["Std. Error","UseTaxonfossilfuels20ctl"], amceh_choice$estimates$Use["Std. Error","UseTaxonfossilfuels50ctl"],  amceh_choice$estimates$Use["Std. Error","UseReplacementoffossilheating30years"],amceh_choice$estimates$Use["Std. Error","UseReplacementoffossilheating15years"] )

c_support_amcehc <- c(0, amceh_choice$estimates$Support["AMCE","SupportSubsidiesforclimatefriendlyalternatives"], amceh_choice$estimates$Support["AMCE","SupportTradeinbonus"], amceh_choice$estimates$Support["AMCE","SupportStatesupportedbuildingrenovationmeasures"], amceh_choice$estimates$Support["AMCE","SupportPreferentialloan"])
s_support_amcehc <- c(0, amceh_choice$estimates$Support["Std. Error","SupportSubsidiesforclimatefriendlyalternatives"], amceh_choice$estimates$Support["Std. Error","SupportTradeinbonus"], amceh_choice$estimates$Support["Std. Error","SupportStatesupportedbuildingrenovationmeasures"], amceh_choice$estimates$Support["Std. Error","SupportPreferentialloan"])

c_amceh_c <- c(c_timing_amcehc, c_purchase_amcehc, c_use_amcehc, c_support_amcehc)
s_amceh_c <- c(s_timing_amcehc, s_purchase_amcehc, s_use_amcehc, s_support_amcehc)

dfp_amceh_c <- data.frame(c_amceh_c,s_amceh_c)

dfp_amceh_c$att <- c(amceh_choice$attributes$Timing[1:5], 
                     amceh_choice$attributes$Purchase[5], amceh_choice$attributes$Purchase[1:4],
                     amceh_choice$attributes$Use[3], amceh_choice$attributes$Use[4:5], amceh_choice$attributes$Use[1:2], 
                     amceh_choice$attributes$Support[2], amceh_choice$attributes$Support[4], amceh_choice$attributes$Support[1], amceh_choice$attributes$Support[3], amceh_choice$attributes$Support[5])

dfp_amceh_c$att <- factor(dfp_amceh_c$att, levels = c(rev(c("2030", "2035","2040","2045","2050",
                                                            "Nopurchaseinstrument","Purchasetaxonfossilfuelheating10", "Purchasetaxonfossilfuelheating20", "Purchasebanforfossilfuelheating2030", "Purchasebanforfossilfuelheating2025",
                                                            "Nouseinstrument", "Taxonfossilfuels20ctl", "Taxonfossilfuels50ctl", "Replacementoffossilheating30years","Replacementoffossilheating15years",
                                                            "Nosupportinginstrument", "Subsidiesforclimatefriendlyalternatives", "Tradeinbonus", "Statesupportedbuildingrenovationmeasures", "Preferentialloan"))),
                          labels = c(rev(c("Baseline: 2030", "2035", "2040", "2045", "2050", 
                                           "Baseline: No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                           "Baseline: No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                           "Baseline: No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan"))))

dfp_amceh_c$ptype <- ifelse(dfp_amceh_c$att == "Baseline: 2030" |  dfp_amceh_c$att == "2035" |  dfp_amceh_c$att == "2040" |  dfp_amceh_c$att == "2045" |  dfp_amceh_c$att == "2050", "1", ifelse(
  dfp_amceh_c$att == "Baseline: No purchase instrument" | dfp_amceh_c$att == "Purchase tax on fossil fuel heating (10%)" | dfp_amceh_c$att == "Purchase tax on fossil fuel heating (20%)" | dfp_amceh_c$att == "Purchase ban for fossil fuel heating (2030)"  | dfp_amceh_c$att == "Purchase ban for fossil fuel heating (2025)" , "2", ifelse(
    dfp_amceh_c$att == "Baseline: No use instrument" | dfp_amceh_c$att == "Tax on fossil fuels (20 ct/l)" | dfp_amceh_c$att == "Tax on fossil fuels (50 ct/l)" | dfp_amceh_c$att == "Replacement of fossil heating (> 30 years)" | dfp_amceh_c$att == "Replacement of fossil heating (> 15 years)", "3", ifelse(
      dfp_amceh_c$att == "Baseline: No supporting instrument" | dfp_amceh_c$att == "Subsidies for climate-friendly alternatives" | dfp_amceh_c$att == "Trade in bonus" | dfp_amceh_c$att == "State-supported building renovation measures" | dfp_amceh_c$att == "Preferential loan", "4", NA))))

dfp_amceh_c$ptype = factor(dfp_amceh_c$ptype, levels=c(1:4), labels=c("Timing","Purchase","Use","Support"))

AMCEHChoice <- ggplot(dfp_amceh_c, 
                      aes(x = att, y = c_amceh_c, color = ptype)) +
  facet_grid(ptype~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = c_amceh_c - 1.95 * s_amceh_c, max = c_amceh_c + 1.95 * s_amceh_c), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("\nAverage marginal component effects on respondents' support share") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none")+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20))+
  scale_color_brewer(palette="RdBu") +
  coord_flip()
#  ggtitle("Average marginal component effects for heating, choice")

AMCEHChoice
ggsave("amceh_choice_final", device=png, 
       path = "build/figures-and-tables/AMCE",
       width = 16, height = 20)

# Final plot AMCE Heating Rating
c_timing_amcehr <- c(0, amceh_rating$estimates$Timing["AMCE","Timing2035"], amceh_rating$estimates$Timing["AMCE","Timing2040"], amceh_rating$estimates$Timing["AMCE","Timing2045"], amceh_rating$estimates$Timing["AMCE","Timing2050"])
s_timing_amcehr <- c(0, amceh_rating$estimates$Timing["Std. Error","Timing2035"], amceh_rating$estimates$Timing["Std. Error","Timing2040"], amceh_rating$estimates$Timing["Std. Error","Timing2045"], amceh_rating$estimates$Timing["Std. Error","Timing2050"])

c_purchase_amcehr <- c(0, amceh_rating$estimates$Purchase["AMCE","PurchasePurchasetaxonfossilfuelheating10"], amceh_rating$estimates$Purchase["AMCE","PurchasePurchasetaxonfossilfuelheating20"],amceh_rating$estimates$Purchase["AMCE","PurchasePurchasebanforfossilfuelheating2030"], amceh_rating$estimates$Purchase["AMCE","PurchasePurchasebanforfossilfuelheating2025"])
s_purchase_amcehr <- c(0, amceh_rating$estimates$Purchase["Std. Error","PurchasePurchasetaxonfossilfuelheating10"], amceh_rating$estimates$Purchase["Std. Error","PurchasePurchasetaxonfossilfuelheating20"],amceh_rating$estimates$Purchase["Std. Error","PurchasePurchasebanforfossilfuelheating2030"], amceh_rating$estimates$Purchase["Std. Error","PurchasePurchasebanforfossilfuelheating2025"])

c_use_amcehr <- c(0, amceh_rating$estimates$Use["AMCE","UseTaxonfossilfuels20ctl"], amceh_rating$estimates$Use["AMCE","UseTaxonfossilfuels50ctl"],  amceh_rating$estimates$Use["AMCE","UseReplacementoffossilheating30years"],amceh_rating$estimates$Use["AMCE","UseReplacementoffossilheating15years"])
s_use_amcehr <- c(0, amceh_rating$estimates$Use["Std. Error","UseTaxonfossilfuels20ctl"], amceh_rating$estimates$Use["Std. Error","UseTaxonfossilfuels50ctl"],  amceh_rating$estimates$Use["Std. Error","UseReplacementoffossilheating30years"],amceh_rating$estimates$Use["Std. Error","UseReplacementoffossilheating15years"] )

c_support_amcehr <- c(0, amceh_rating$estimates$Support["AMCE","SupportSubsidiesforclimatefriendlyalternatives"], amceh_rating$estimates$Support["AMCE","SupportTradeinbonus"], amceh_rating$estimates$Support["AMCE","SupportStatesupportedbuildingrenovationmeasures"], amceh_rating$estimates$Support["AMCE","SupportPreferentialloan"])
s_support_amcehr <- c(0, amceh_rating$estimates$Support["Std. Error","SupportSubsidiesforclimatefriendlyalternatives"], amceh_rating$estimates$Support["Std. Error","SupportTradeinbonus"], amceh_rating$estimates$Support["Std. Error","SupportStatesupportedbuildingrenovationmeasures"], amceh_rating$estimates$Support["Std. Error","SupportPreferentialloan"])

c_amceh_r <- c(c_timing_amcehr, c_purchase_amcehr, c_use_amcehr, c_support_amcehr)
s_amceh_r <- c(s_timing_amcehr, s_purchase_amcehr, s_use_amcehr, s_support_amcehr)

dfp_amceh_r <- data.frame(c_amceh_r,s_amceh_r)

dfp_amceh_r$att <- c(amceh_rating$attributes$Timing[1:5], 
                     amceh_rating$attributes$Purchase[5], amceh_rating$attributes$Purchase[1:2], amceh_rating$attributes$Purchase[4], amceh_rating$attributes$Purchase[3],
                     amceh_rating$attributes$Use[3], amceh_rating$attributes$Use[4:5], amceh_rating$attributes$Use[2], amceh_rating$attributes$Use[1],
                     amceh_rating$attributes$Support[2], amceh_rating$attributes$Support[4], amceh_rating$attributes$Support[1], amceh_rating$attributes$Support[3], amceh_rating$attributes$Support[5])

dfp_amceh_r$att <- factor(dfp_amceh_r$att, levels = c(rev(c("2030", "2035","2040","2045","2050",
                                                            "Nopurchaseinstrument","Purchasetaxonfossilfuelheating10", "Purchasetaxonfossilfuelheating20", "Purchasebanforfossilfuelheating2030", "Purchasebanforfossilfuelheating2025",
                                                            "Nouseinstrument", "Taxonfossilfuels20ctl", "Taxonfossilfuels50ctl", "Replacementoffossilheating30years","Replacementoffossilheating15years",
                                                            "Nosupportinginstrument", "Subsidiesforclimatefriendlyalternatives", "Tradeinbonus", "Statesupportedbuildingrenovationmeasures", "Preferentialloan"))),
                          labels = c(rev(c("Baseline: 2030", "2035", "2040", "2045", "2050", 
                                           "Baseline: No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                           "Baseline: No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                           "Baseline: No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan"))))

dfp_amceh_r$ptype <- ifelse(dfp_amceh_r$att == "Baseline: 2030" |  dfp_amceh_r$att == "2035" |  dfp_amceh_r$att == "2040" |  dfp_amceh_r$att == "2045" |  dfp_amceh_r$att == "2050", "1", ifelse(
  dfp_amceh_r$att == "Baseline: No purchase instrument" | dfp_amceh_r$att == "Purchase tax on fossil fuel heating (10%)" | dfp_amceh_r$att == "Purchase tax on fossil fuel heating (20%)" | dfp_amceh_r$att == "Purchase ban for fossil fuel heating (2030)"  | dfp_amceh_r$att == "Purchase ban for fossil fuel heating (2025)" , "2", ifelse(
    dfp_amceh_r$att == "Baseline: No use instrument" | dfp_amceh_r$att == "Tax on fossil fuels (20 ct/l)" | dfp_amceh_r$att == "Tax on fossil fuels (50 ct/l)" | dfp_amceh_r$att == "Replacement of fossil heating (> 30 years)" | dfp_amceh_r$att == "Replacement of fossil heating (> 15 years)", "3", ifelse(
      dfp_amceh_r$att == "Baseline: No supporting instrument" | dfp_amceh_r$att == "Subsidies for climate-friendly alternatives" | dfp_amceh_r$att == "Trade in bonus" | dfp_amceh_r$att == "State-supported building renovation measures" | dfp_amceh_r$att == "Preferential loan", "4", NA))))

dfp_amceh_r$ptype = factor(dfp_amceh_r$ptype, levels=c(1:4), labels=c("Timing","Purchase","Use","Support"))

AMCEHRating <- ggplot(dfp_amceh_r, 
                      aes(x = att, y = c_amceh_r, color = ptype)) +
  facet_grid(ptype~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = c_amceh_r - 1.95 * s_amceh_r, max = c_amceh_r + 1.95 * s_amceh_r), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("\nAverage marginal component effects on respondents' support share") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none") +
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20))+
  coord_flip()
#   ggtitle("Average marginal component effects for heating, rating")

AMCEHRating
ggsave("amceh_rating_final", device=png, 
       path = "build/figures-and-tables/AMCE",
       width = 16, height = 20)

# Final plot AMCE Heating
dfp_amceh_c = dfp_amceh_c  %>%
  rename("c_amceh" = "c_amceh_c", 
         "s_amceh" = "s_amceh_c")

dfp_amceh_r = dfp_amceh_r  %>%
  rename("c_amceh" = "c_amceh_r", 
         "s_amceh" = "s_amceh_r")

dfp_amceh <- rbind(dfp_amceh_c, dfp_amceh_r)
dfp_amceh$outcome <- rep(c("Choice", "Rating"), each=nrow(dfp_amceh_c))

AMCEH <- ggplot(dfp_amceh, 
                aes(x = att, y = c_amceh, color = ptype)) +
  facet_grid(ptype~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = c_amceh - 1.95 * s_amceh, max = c_amceh + 1.95 * s_amceh), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("\nAverage marginal component effects on respondents' support share") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none")+
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), strip.text.x = element_text(size = 20))+
  coord_flip()
#   ggtitle("Average marginal component effects for heating")

AMCEH
ggsave("amceh_final", device=png, 
       path = "build/figures-and-tables/AMCE",
       width = 16, height = 20)

# Final plot AMCE Transport Choice
c_timing_amcetc <- c(0, amcet_choice$estimates$Timing["AMCE","Timing2035"], amcet_choice$estimates$Timing["AMCE","Timing2040"], amcet_choice$estimates$Timing["AMCE","Timing2045"], amcet_choice$estimates$Timing["AMCE","Timing2050"])
s_timing_amcetc <- c(0, amcet_choice$estimates$Timing["Std. Error","Timing2035"], amcet_choice$estimates$Timing["Std. Error","Timing2040"], amcet_choice$estimates$Timing["Std. Error","Timing2045"], amcet_choice$estimates$Timing["Std. Error","Timing2050"])

c_purchase_amcetc <- c(0, amcet_choice$estimates$Purchase["AMCE","PurchasePurchasetaxonICEV10"], amcet_choice$estimates$Purchase["AMCE","PurchasePurchasetaxonICEV20"],amcet_choice$estimates$Purchase["AMCE","PurchasePurchasebanforICEV2030"], amcet_choice$estimates$Purchase["AMCE","PurchasePurchasebanforICEV2025"])
s_purchase_amcetc <- c(0, amcet_choice$estimates$Purchase["Std. Error","PurchasePurchasetaxonICEV10"], amcet_choice$estimates$Purchase["Std. Error","PurchasePurchasetaxonICEV20"],amcet_choice$estimates$Purchase["Std. Error","PurchasePurchasebanforICEV2030"], amcet_choice$estimates$Purchase["Std. Error","PurchasePurchasebanforICEV2025"])

c_use_amcetc <- c(0, amcet_choice$estimates$Use["AMCE","UseTaxonfossilfuels20ctl"], amcet_choice$estimates$Use["AMCE","UseTaxonfossilfuels50ctl"], amcet_choice$estimates$Use["AMCE","UseWeekdaybanonICEVsincitycenters"], amcet_choice$estimates$Use["AMCE","UseDailybanonICEVsincitycenters"])
s_use_amcetc <- c(0, amcet_choice$estimates$Use["Std. Error","UseTaxonfossilfuels20ctl"], amcet_choice$estimates$Use["Std. Error","UseTaxonfossilfuels50ctl"],  amcet_choice$estimates$Use["Std. Error","UseWeekdaybanonICEVsincitycenters"],amcet_choice$estimates$Use["Std. Error","UseDailybanonICEVsincitycenters"] )

c_support_amcetc <- c(0, amcet_choice$estimates$Support["AMCE","SupportSubsidiesforclimatefriendlyalternatives"], amcet_choice$estimates$Support["AMCE","SupportTradeinbonus"], amcet_choice$estimates$Support["AMCE","SupportStatesupportedinfrastructuremeasures"], amcet_choice$estimates$Support["AMCE","SupportPreferentialloan"])
s_support_amcetc <- c(0, amcet_choice$estimates$Support["Std. Error","SupportSubsidiesforclimatefriendlyalternatives"], amcet_choice$estimates$Support["Std. Error","SupportTradeinbonus"], amcet_choice$estimates$Support["Std. Error","SupportStatesupportedinfrastructuremeasures"], amcet_choice$estimates$Support["Std. Error","SupportPreferentialloan"])

c_amcet_c <- c(c_timing_amcetc, c_purchase_amcetc, c_use_amcetc, c_support_amcetc)
s_amcet_c <- c(s_timing_amcetc, s_purchase_amcetc, s_use_amcetc, s_support_amcetc)

dfp_amcet_c <- data.frame(c_amcet_c,s_amcet_c)

dfp_amcet_c$att <- c(amcet_choice$attributes$Timing[1:5], 
                     amcet_choice$attributes$Purchase[5], amcet_choice$attributes$Purchase[1:2], amcet_choice$attributes$Purchase[4], amcet_choice$attributes$Purchase[3],
                     amcet_choice$attributes$Use[2], amcet_choice$attributes$Use[3:4], amcet_choice$attributes$Use[1], amcet_choice$attributes$Use[5],
                     amcet_choice$attributes$Support[2], amcet_choice$attributes$Support[4], amcet_choice$attributes$Support[1], amcet_choice$attributes$Support[3], amcet_choice$attributes$Support[5])

dfp_amcet_c$att <- factor(dfp_amcet_c$att, levels = c(rev(c("2030", "2035","2040","2045","2050",
                                                            "Nopurchaseinstrument","PurchasetaxonICEV10", "PurchasetaxonICEV20", "PurchasebanforICEV2030", "PurchasebanforICEV2025",
                                                            "Nouseinstrument", "Taxonfossilfuels20ctl", "Taxonfossilfuels50ctl", "WeekdaybanonICEVsincitycenters","DailybanonICEVsincitycenters",
                                                            "Nosupportinginstrument", "Subsidiesforclimatefriendlyalternatives", "Tradeinbonus", "Statesupportedinfrastructuremeasures", "Preferentialloan"))),
                          labels = c(rev(c("Baseline: 2030", "2035", "2040", "2045", "2050", 
                                           "Baseline: No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                           "Baseline: No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                           "Baseline: No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan"))))

dfp_amcet_c$ptype <- ifelse(dfp_amcet_c$att == "Baseline: 2030" |  dfp_amcet_c$att == "2035" |  dfp_amcet_c$att == "2040" |  dfp_amcet_c$att == "2045" |  dfp_amcet_c$att == "2050", "1", ifelse(
  dfp_amcet_c$att == "Baseline: No purchase instrument" | dfp_amcet_c$att == "Purchase tax on ICEV (10%)" | dfp_amcet_c$att == "Purchase tax on ICEV (20%)" | dfp_amcet_c$att == "Purchase ban for ICEV (2030)"  | dfp_amcet_c$att == "Purchase ban for ICEV (2025)" , "2", ifelse(
    dfp_amcet_c$att == "Baseline: No use instrument" | dfp_amcet_c$att == "Tax on fossil fuels (20 ct/l)" | dfp_amcet_c$att == "Tax on fossil fuels (50 ct/l)" | dfp_amcet_c$att == "Weekday ban on ICEVs in city centers" | dfp_amcet_c$att == "Daily ban on ICEVs in city centers", "3", ifelse(
      dfp_amcet_c$att == "Baseline: No supporting instrument" | dfp_amcet_c$att == "Subsidies for climate-friendly alternatives" | dfp_amcet_c$att == "Trade in bonus" | dfp_amcet_c$att == "State-supported infrastructure measures" | dfp_amcet_c$att == "Preferential loan", "4", NA))))

dfp_amcet_c$ptype = factor(dfp_amcet_c$ptype, levels=c(1:4), labels=c("Timing","Purchase","Use","Support"))

AMCETChoice <- ggplot(dfp_amcet_c, 
                      aes(x = att, y = c_amcet_c, color = ptype)) +
  facet_grid(ptype~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = c_amcet_c - 1.95 * s_amcet_c, max = c_amcet_c + 1.95 * s_amcet_c), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("\nAverage marginal component effects on respondents' support share") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none")+
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20))+
  coord_flip()
#   ggtitle("Average marginal component effects for transport, choice")

AMCETChoice
ggsave("amcet_choice_final", device=png, 
       path = "build/figures-and-tables/AMCE",
       width = 16, height = 20)

# Final plot AMCE Transport Rating (binary results)
c_timing_amcetr <- c(0, amcet_rating$estimates$Timing["AMCE","Timing2035"], amcet_rating$estimates$Timing["AMCE","Timing2040"], amcet_rating$estimates$Timing["AMCE","Timing2045"], amcet_rating$estimates$Timing["AMCE","Timing2050"])
s_timing_amcetr <- c(0, amcet_rating$estimates$Timing["Std. Error","Timing2035"], amcet_rating$estimates$Timing["Std. Error","Timing2040"], amcet_rating$estimates$Timing["Std. Error","Timing2045"], amcet_rating$estimates$Timing["Std. Error","Timing2050"])

c_purchase_amcetr <- c(0, amcet_rating$estimates$Purchase["AMCE","PurchasePurchasetaxonICEV10"], amcet_rating$estimates$Purchase["AMCE","PurchasePurchasetaxonICEV20"],amcet_rating$estimates$Purchase["AMCE","PurchasePurchasebanforICEV2030"], amcet_rating$estimates$Purchase["AMCE","PurchasePurchasebanforICEV2025"])
s_purchase_amcetr <- c(0, amcet_rating$estimates$Purchase["Std. Error","PurchasePurchasetaxonICEV10"], amcet_rating$estimates$Purchase["Std. Error","PurchasePurchasetaxonICEV20"],amcet_rating$estimates$Purchase["Std. Error","PurchasePurchasebanforICEV2030"], amcet_rating$estimates$Purchase["Std. Error","PurchasePurchasebanforICEV2025"])

c_use_amcetr <- c(0, amcet_rating$estimates$Use["AMCE","UseTaxonfossilfuels20ctl"], amcet_rating$estimates$Use["AMCE","UseTaxonfossilfuels50ctl"], amcet_rating$estimates$Use["AMCE","UseWeekdaybanonICEVsincitycenters"], amcet_rating$estimates$Use["AMCE","UseDailybanonICEVsincitycenters"])
s_use_amcetr <- c(0, amcet_rating$estimates$Use["Std. Error","UseTaxonfossilfuels20ctl"], amcet_rating$estimates$Use["Std. Error","UseTaxonfossilfuels50ctl"],  amcet_rating$estimates$Use["Std. Error","UseWeekdaybanonICEVsincitycenters"],amcet_rating$estimates$Use["Std. Error","UseDailybanonICEVsincitycenters"] )

c_support_amcetr <- c(0, amcet_rating$estimates$Support["AMCE","SupportSubsidiesforclimatefriendlyalternatives"], amcet_rating$estimates$Support["AMCE","SupportTradeinbonus"], amcet_rating$estimates$Support["AMCE","SupportStatesupportedinfrastructuremeasures"], amcet_rating$estimates$Support["AMCE","SupportPreferentialloan"])
s_support_amcetr <- c(0, amcet_rating$estimates$Support["Std. Error","SupportSubsidiesforclimatefriendlyalternatives"], amcet_rating$estimates$Support["Std. Error","SupportTradeinbonus"], amcet_rating$estimates$Support["Std. Error","SupportStatesupportedinfrastructuremeasures"], amcet_rating$estimates$Support["Std. Error","SupportPreferentialloan"])

c_amcet_r <- c(c_timing_amcetr, c_purchase_amcetr, c_use_amcetr, c_support_amcetr)
s_amcet_r <- c(s_timing_amcetr, s_purchase_amcetr, s_use_amcetr, s_support_amcetr)

dfp_amcet_r <- data.frame(c_amcet_r,s_amcet_r)

dfp_amcet_r$att <- c(amcet_rating$attributes$Timing[1:5], 
                     amcet_rating$attributes$Purchase[5], amcet_rating$attributes$Purchase[1:2], amcet_rating$attributes$Purchase[4], amcet_rating$attributes$Purchase[3],
                     amcet_rating$attributes$Use[2], amcet_rating$attributes$Use[3:4], amcet_rating$attributes$Use[1], amcet_rating$attributes$Use[5],
                     amcet_rating$attributes$Support[2], amcet_rating$attributes$Support[4], amcet_rating$attributes$Support[1], amcet_rating$attributes$Support[3], amcet_rating$attributes$Support[5])

dfp_amcet_r$att <- factor(dfp_amcet_r$att, levels = c(rev(c("2030", "2035","2040","2045","2050",
                                                            "Nopurchaseinstrument","PurchasetaxonICEV10", "PurchasetaxonICEV20", "PurchasebanforICEV2030", "PurchasebanforICEV2025",
                                                            "Nouseinstrument", "Taxonfossilfuels20ctl", "Taxonfossilfuels50ctl", "WeekdaybanonICEVsincitycenters","DailybanonICEVsincitycenters",
                                                            "Nosupportinginstrument", "Subsidiesforclimatefriendlyalternatives", "Tradeinbonus", "Statesupportedinfrastructuremeasures", "Preferentialloan"))),
                          labels = c(rev(c("Baseline: 2030", "2035", "2040", "2045", "2050", 
                                           "Baseline: No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                           "Baseline: No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                           "Baseline: No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan"))))

dfp_amcet_r$ptype <- ifelse(dfp_amcet_r$att == "Baseline: 2030" |  dfp_amcet_r$att == "2035" |  dfp_amcet_r$att == "2040" |  dfp_amcet_r$att == "2045" |  dfp_amcet_r$att == "2050", "1", ifelse(
  dfp_amcet_r$att == "Baseline: No purchase instrument" | dfp_amcet_r$att == "Purchase tax on ICEV (10%)" | dfp_amcet_r$att == "Purchase tax on ICEV (20%)" | dfp_amcet_r$att == "Purchase ban for ICEV (2030)"  | dfp_amcet_r$att == "Purchase ban for ICEV (2025)" , "2", ifelse(
    dfp_amcet_r$att == "Baseline: No use instrument" | dfp_amcet_r$att == "Tax on fossil fuels (20 ct/l)" | dfp_amcet_r$att == "Tax on fossil fuels (50 ct/l)" | dfp_amcet_r$att == "Weekday ban on ICEVs in city centers" | dfp_amcet_r$att == "Daily ban on ICEVs in city centers", "3", ifelse(
      dfp_amcet_r$att == "Baseline: No supporting instrument" | dfp_amcet_r$att == "Subsidies for climate-friendly alternatives" | dfp_amcet_r$att == "Trade in bonus" | dfp_amcet_r$att == "State-supported infrastructure measures" | dfp_amcet_r$att == "Preferential loan", "4", NA))))

dfp_amcet_r$ptype = factor(dfp_amcet_r$ptype, levels=c(1:4), labels=c("Timing","Purchase","Use","Support"))

AMCETRating <- ggplot(dfp_amcet_r, 
                      aes(x = att, y = c_amcet_r, color = ptype)) +
  facet_grid(ptype~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = c_amcet_r - 1.95 * s_amcet_r, max = c_amcet_r + 1.95 * s_amcet_r), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("\nAverage marginal component effects on respondents' support share") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none")+
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20))+
  coord_flip()
#   ggtitle("Average marginal component effects for transport, rating")

AMCETRating
ggsave("amcet_rating_final", device=png, 
       path = "build/figures-and-tables/AMCE",
       width = 16, height = 20)

# Final plot AMCE Transport
dfp_amcet_c = dfp_amcet_c  %>%
  rename("c_amcet" = "c_amcet_c", 
         "s_amcet" = "s_amcet_c")

dfp_amcet_r = dfp_amcet_r  %>%
  rename("c_amcet" = "c_amcet_r", 
         "s_amcet" = "s_amcet_r")

dfp_amcet <- rbind(dfp_amcet_c, dfp_amcet_r)
dfp_amcet$outcome <- rep(c("Choice", "Rating"), each=nrow(dfp_amcet_c))

AMCET <- ggplot(dfp_amcet, 
                aes(x = att, y = c_amcet, color = ptype)) +
  facet_grid(ptype~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = c_amcet - 1.95 * s_amcet, max = c_amcet + 1.95 * s_amcet), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("\nAverage marginal component effects on respondents' support share") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="none")+
  scale_color_brewer(palette="RdBu") +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), strip.text.x = element_text(size = 20))+
  coord_flip()
#   ggtitle("Average marginal component effects for transport")

AMCET
ggsave("amcet_final", device=png, 
       path = "build/figures-and-tables/AMCE",
       width = 16, height = 20)

##############################################################################################
# Robustness checks - order of sectors
##

# add first shown sector
choice_h = choice_h %>%
  left_join(first_sector, by= "ID") 

choice_t = choice_t %>%
  left_join(first_sector, by= "ID")

rating_h = rating_h %>%
  left_join(first_sector, by= "ID") 

rating_t = rating_t %>%
  left_join(first_sector, by= "ID")

# analyse influence of which sector was shown first
anova_firstch <- cj_anova(data = choice_h, Y ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~First)
capture.output(anova_firstch, file = "build/figures-and-tables/checks/robustness-checks/anova_firstch.csv")

anova_firstct <- cj_anova(data = choice_t, Y ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~First)
capture.output(anova_firstct, file = "build/figures-and-tables/checks/robustness-checks/anova_firstct.csv")

anova_firstrh <- cj_anova(data = rating_h, bin_rate ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~First)
capture.output(anova_firstrh, file = "build/figures-and-tables/checks/robustness-checks/anova_firstrh.csv")

anova_firstrt <- cj_anova(data = rating_t, bin_rate ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~First)
capture.output(anova_firstrt, file = "build/figures-and-tables/checks/robustness-checks/anova_firstrt.csv")

# plot sector order interaction heating (choice)
int_firstch <- cj(data = choice_h, Y ~ Timing + Purchase + Use + Support,
              estimate = "mm", id = ~ "ID", by = ~First)

level_orderfich <- factor(int_firstch$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                      "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                      "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                      "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

int_firstch_p <- ggplot(int_firstch, 
                    aes(x = level_orderfich, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="First shown sectors"))

int_firstch_p
ggsave("int_firstch", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# plot sector order interaction heating (rating)
int_firstrh <- cj(data = rating_h, rating ~ Timing + Purchase + Use + Support,
              estimate = "mm", id = ~ "ID", by = ~First)

level_orderfirh <- factor(int_firstrh$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                      "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                      "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                      "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

int_firstrh_p <- ggplot(int_firstrh, 
                    aes(x = level_orderfirh, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="First sector shown"))

int_firstrh_p
ggsave("int_firstrh", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# Final plot Interaction heating
int_first_heating <- rbind(int_firstch, int_firstrh)
int_first_heating$outcome <- rep(c("Choice", "Rating"), each=nrow(int_firstch))

IntFiH <- ggplot(int_first_heating, 
               aes(x = level, y = estimate, color = BY)) +
  facet_grid(feature~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(data = int_first_heating %>% filter(outcome == "Choice"),
             aes(yintercept = 0.5), linetype = "dashed")+
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom")+
  scale_color_manual(values = c("#D8B365", "#5AB4AC")) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="First sector shown"))

IntFiH
ggsave("interaction first sector heating_final", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 16, height = 20)

# plot sector order interaction transport (choice)
int_firstct <- cj(data = choice_t, Y ~ Timing + Purchase + Use + Support,
              estimate = "mm", id = ~ "ID", by = ~First)

level_orderfict <- factor(int_firstct$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                      "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                      "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                      "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

# plot(int_firstct , group = "Framing", vline = 0.5) + ggtitle("Interaction framing, choice transport") # FIXME fails

int_firstct_p <- ggplot(int_firstct, 
                    aes(x = level_orderfict, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="First sector shown"))

int_firstct_p
ggsave("int_firstct", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# plot sector order interaction transport (rating)
int_firstrt <- cj(data = rating_t, rating ~ Timing + Purchase + Use + Support,
              estimate = "mm", id = ~ "ID", by = ~First)
# plot(int_firstrt , group = "Framing")  + ggtitle("Interaction framing, rating transport") # FIXME fails

level_orderfirt <- factor(int_firstrt$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                      "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                      "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                      "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

int_firstrt_p <- ggplot(int_firstrt, 
                    aes(x = level_orderfirt, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="First sector shown"))

int_firstrt_p
ggsave("int_firstrt", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# Final plot Interaction Transport
int_first_transport <- rbind(int_firstct, int_firstrt)
int_first_transport$outcome <- rep(c("Choice", "Rating"), each=nrow(int_firstrt))

IntFiTr <- ggplot(int_first_transport, 
                aes(x = level, y = estimate, color = BY)) +
  facet_grid(feature~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(data = int_first_transport %>% filter(outcome == "Choice"),
             aes(yintercept = 0.5), linetype = "dashed")+
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom")+
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="First sector shown"))

IntFiTr
ggsave("interaction first sector transport_final", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 16, height = 20)

##############################################################################################
# Robustness checks - choice round
##

# analyse influence of choice round
anova_choicerch <- cj_anova(data = choice_h, Y ~ Timing + Purchase + Use + Support, 
                          id = ~ "ID", by = ~choiceNum)
capture.output(anova_choicerch, file = "build/figures-and-tables/checks/robustness-checks/anova_choicerch.csv")

anova_choicerct <- cj_anova(data = choice_t, Y ~ Timing + Purchase + Use + Support, 
                          id = ~ "ID", by = ~choiceNum)
capture.output(anova_choicerct, file = "build/figures-and-tables/checks/robustness-checks/anova_firstct.csv")

anova_choicerrh <- cj_anova(data = rating_h, bin_rate ~ Timing + Purchase + Use + Support, 
                          id = ~ "ID", by = ~choiceNum)
capture.output(anova_choicerrh, file = "build/figures-and-tables/checks/robustness-checks/anova_firstrh.csv")

anova_choicerrt <- cj_anova(data = rating_t, bin_rate ~ Timing + Purchase + Use + Support, 
                          id = ~ "ID", by = ~choiceNum)
capture.output(anova_choicerrt, file = "build/figures-and-tables/checks/robustness-checks/anova_firstrt.csv")

# plot sector order interaction heating (choice)
int_choicerch <- cj(data = choice_h, Y ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ "ID", by = ~choiceNum)

level_choicerch <- factor(int_choicerch$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                           "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                           "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                           "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

int_choicerch_p <- ggplot(int_choicerch, 
                        aes(x = level_choicerch, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Choice round"))

int_choicerch_p
ggsave("int_choicerch", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# plot sector order interaction heating (rating)
int_choicerrh <- cj(data = rating_h, rating ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ "ID", by = ~choiceNum)

level_choicerrh <- factor(int_choicerrh$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                           "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                           "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                           "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

int_choicerrh_p <- ggplot(int_choicerrh, 
                        aes(x = level_choicerrh, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Choice round"))

int_choicerrh_p
ggsave("int_choicerrh", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# Final plot Interaction heating
int_choicer_heating <- rbind(int_choicerch, int_choicerrh)
int_choicer_heating$outcome <- rep(c("Choice", "Rating"), each=nrow(int_choicerrh))

IntChH <- ggplot(int_choicer_heating, 
                 aes(x = level, y = estimate, color = BY)) +
  facet_grid(feature~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(data = int_choicer_heating %>% filter(outcome == "Choice"),
             aes(yintercept = 0.5), linetype = "dashed")+
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom")+
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Choice round"))

IntChH
ggsave("interaction choice round heating_final", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 16, height = 20)

# plot sector order interaction transport (choice)
int_choicerct <- cj(data = choice_t, Y ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ "ID", by = ~choiceNum)

level_choicerct <- factor(int_choicerct$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                           "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                           "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                           "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

int_choicerct_p <- ggplot(int_choicerct, 
                        aes(x = level_choicerct, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Choice round"))

int_choicerct_p
ggsave("in_choicerct", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# plot sector order interaction transport (rating)
int_choicerrt <- cj(data = rating_t, rating ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ "ID", by = ~choiceNum)

level_choicerrt <- factor(int_choicerrt$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                           "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                           "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                           "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

int_choicerrt_p <- ggplot(int_choicerrt, 
                        aes(x = level_choicerrt, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Choice round"))

int_choicerrt_p
ggsave("int_choicerrt", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# Final plot Interaction Transport
int_choicer_transport <- rbind(int_choicerct, int_choicerrt)
int_choicer_transport$outcome <- rep(c("Choice", "Rating"), each=nrow(int_choicerrt))

IntChTr <- ggplot(int_choicer_transport, 
                  aes(x = level, y = estimate, color = BY)) +
  facet_grid(feature~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(data = int_choicer_transport %>% filter(outcome == "Choice"),
             aes(yintercept = 0.5), linetype = "dashed")+
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom")+
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Choice round"))

IntChTr
ggsave("interaction choice round transport_final", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 16, height = 20)

##############################################################################################
# Robustness checks - package number (left or right)
##

# plot package number interaction heating (choice)
int_packch <- cj(data = choice_h, Y ~ Timing + Purchase + Use + Support,
                    estimate = "mm", id = ~ "ID", by = ~packNum)

level_packch <- factor(int_packch$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                             "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                             "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                             "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

int_packch_p <- ggplot(int_packch, 
                          aes(x = level_packch, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Package Number"))

int_packch_p
ggsave("int_packch", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# plot package numberr interaction heating (rating)
int_packrh <- cj(data = rating_h, rating ~ Timing + Purchase + Use + Support,
                    estimate = "mm", id = ~ "ID", by = ~packNum)

level_packrh <- factor(int_packrh$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                             "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                             "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                             "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

int_packrh_p <- ggplot(int_packrh, 
                          aes(x = level_packrh, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Package Number"))

int_packrh_p
ggsave("int_packrh", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# Final plot package number Interaction heating
int_pack_heating <- rbind(int_packch, int_packrh)
int_pack_heating$outcome <- rep(c("Choice", "Rating"), each=nrow(int_packch))

IntPH <- ggplot(int_pack_heating, 
                 aes(x = level, y = estimate, color = BY)) +
  facet_grid(feature~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(data = int_pack_heating %>% filter(outcome == "Choice"),
             aes(yintercept = 0.5), linetype = "dashed")+
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom")+
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Package Number"))

IntPH
ggsave("interaction package number heating_final", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 16, height = 20)

# plot package number interaction transport (choice)
int_packct <- cj(data = choice_t, Y ~ Timing + Purchase + Use + Support,
                    estimate = "mm", id = ~ "ID", by = ~packNum)

level_packct <- factor(int_packct$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                             "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                             "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                             "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

int_packct_p <- ggplot(int_packct, 
                          aes(x = level_packct, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Package Number"))

int_packct_p
ggsave("int_packct", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# plot package number interaction transport (rating)
int_packrt <- cj(data = rating_t, rating ~ Timing + Purchase + Use + Support,
                    estimate = "mm", id = ~ "ID", by = ~packNum)

level_packrt <- factor(int_packrt$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                             "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                             "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                             "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

int_packrt_p <- ggplot(int_packrt, 
                          aes(x = level_packrt, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Package Number"))

int_packrt_p
ggsave("int_packrt", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 18, height = 20)

# Final plot package number Interaction Transport
int_pack_transport <- rbind(int_packct, int_packrt)
int_pack_transport$outcome <- rep(c("Choice", "Rating"), each=nrow(int_packct))

IntPTr <- ggplot(int_pack_transport, 
                  aes(x = level, y = estimate, color = BY)) +
  facet_grid(feature~outcome, space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(data = int_pack_transport %>% filter(outcome == "Choice"),
             aes(yintercept = 0.5), linetype = "dashed")+
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom")+
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Package Number"))

IntPTr
ggsave("interaction package number transport_final", device=png, 
       path = "build/figures-and-tables/checks/robustness-checks",
       width = 16, height = 20)

##############################################################################################
# Subgroups - heating and transport related variables
##

# add respondent characteristics
choice_h = choice_h %>%
  left_join(resp_char, by= "ID") 

choice_t = choice_t %>%
  left_join(resp_char, by= "ID")

# analyse influence of source heating on marginal means for heating
anova_sourceh <- cj_anova(data = choice_h, Y ~ Timing + Purchase + Use + Support, id = ~ "ID", by = ~source_h)
capture.output(anova_sourceh, file = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables/anova_sourceh.csv")

sub_sourceh <- cj(data = choice_h, Y ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ ID, by = ~source_h)

level_sourceh <- factor(sub_sourceh$level, 
                         level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                       "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                       "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                       "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

sub_sourceh_p <- ggplot(sub_sourceh, 
                          aes(x = level_sourceh, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#b58177", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Energy source heating"))+
  ggtitle("Energy source heating")

sub_sourceh_p
ggsave("sub_sourceh", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables",
       width = 18, height = 20)

# analyse influence of source heating on marginal means for heating (categories)
anova_sourcehcat <- cj_anova(data = choice_h, Y ~ Timing + Purchase + Use + Support, id = ~ "ID", by = ~source_h_cat)
capture.output(anova_sourcehcat, file = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables/anova_sourcehcat.csv")

sub_sourcehcat <- cj(data = choice_h, Y ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ ID, by = ~source_h_cat)

level_sourcehccat <- factor(sub_sourcehcat$level, 
                         level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                       "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                       "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                       "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

sub_sourcehcat_p <- ggplot(sub_sourcehcat, 
                        aes(x = level_sourcehccat, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Energy source heating"))+
  ggtitle("Energy source heating")

sub_sourcehcat_p
ggsave("sub_sourcehcat", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables",
       width = 18, height = 20)

# analyse influence of building type on marginal means heating
choice_h_buildingtype = choice_h %>%
  drop_na(building_type)

anova_buildingtype <- cj_anova(data = choice_h_buildingtype, Y ~ Timing + Purchase + Use + Support, id = ~ "ID", by = ~building_type)
capture.output(anova_buildingtype, file = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables/anova_buildingtype.csv")

sub_buildingtype <- cj(data = choice_h_buildingtype, Y ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ "ID", by = ~building_type)

level_buildingtype <- factor(sub_buildingtype$level, 
                         level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                       "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                       "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                       "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

sub_buildingtype_p <- ggplot(sub_buildingtype, 
                        aes(x = level_buildingtype, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#b8b6b6", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title= "Building type"))+
  ggtitle("Building type")

sub_buildingtype_p
ggsave("sub_buildingtype", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables",
       width = 18, height = 20)

# analyse influence of ownership of apartment / house on marginal means for heating
choice_h_ownership = choice_h %>%
  drop_na(ownership)

anova_ownership <- cj_anova(data = choice_h_ownership, Y ~ Timing + Purchase + Use + Support, id = ~ "ID", by = ~ownership)
capture.output(anova_ownership, file = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables/anova_ownership.csv")

sub_ownership <- cj(data = choice_h_ownership, Y ~ Timing + Purchase + Use + Support,
                     estimate = "mm", id = ~ "ID", by = ~ownership)

level_ownership <- factor(sub_ownership$level, 
                             level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                           "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                           "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                           "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))

sub_ownership_p <- ggplot(sub_ownership, 
                             aes(x = level_ownership, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Homeownership"))+
  ggtitle("Homeownership")

sub_ownership_p
ggsave("sub_ownership", device = png,
       path = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables",
       width = 18, height = 20)

# analyse influence weekly car use on marginal means for transport
anova_caruse <- cj_anova(data = choice_t, Y ~ Timing + Purchase + Use + Support, 
                         id = ~ "ID", by = ~car_days)
capture.output(anova_caruse, file = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables/anova_caruse.csv")

sub_caruse <- cj(data = choice_t, Y ~ Timing + Purchase + Use + Support,
                     estimate = "mm", id = ~ "ID", by = ~car_days)

level_caruse <- factor(sub_caruse$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                             "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                             "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                             "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

sub_caruse_p <- ggplot(sub_caruse, 
                          aes(x = level_caruse, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Weekly car use"))+
  ggtitle("Weekly car use")

sub_caruse_p
ggsave("sub_caruse", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables",
       width = 18, height = 20)

# analyse influence weekly car use on marginal means for transport (categories)
sub_carusecat <- cj(data = choice_t, Y ~ Timing + Purchase + Use + Support,
                 estimate = "mm", id = ~ "ID", by = ~cardays_cat)

level_carusecat <- factor(sub_carusecat$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                       "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                       "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                       "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
BY_caruse <- factor(sub_carusecat$BY, ordered = TRUE, levels = c("never / exceptionally", "1 - 4 days", "5 - 7 days"))

sub_carusecat_p <- ggplot(sub_carusecat, 
                       aes(x = level_carusecat, y = estimate, color = BY_caruse)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#b8b6b6", "#5AB4AC"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Weekly car use"))+
  ggtitle("Weekly car use")

sub_carusecat_p
ggsave("sub_carusecat", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables",
       width = 18, height = 20)

# analyse influence of car type marginal means for transport
choice_t_cartype = choice_t %>%
  drop_na(car_type)

anova_cartype <- cj_anova(data = choice_t_cartype, Y ~ Timing + Purchase + Use + Support, 
                          id = ~ "ID", by = ~car_type)
capture.output(anova_cartype, file = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables/anova_cartype.csv")

sub_cartype <- cj(data = choice_t_cartype, Y ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ "ID", by = ~car_type)

level_cartype <- factor(sub_cartype$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                       "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                       "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                       "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))

sub_cartype_p <- ggplot(sub_cartype, 
                       aes(x = level_cartype, y = estimate, color = BY)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#A6611A", "#DFC27D", "#c2b48f", "#b8b6b6", "#80CDC1", "#018571"))+
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Car type"))+
  ggtitle("Car type")

sub_cartype_p
ggsave("sub_cartype", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/heating-and-transport-variables",
       width = 18, height = 24)

##############################################################################################
# Analyze inter-item correlation of climate change evaluation questions
##

cor_cceval <- cor(d[,c("climate_change_1",   
                 "climate_change_2",  
                 "climate_change_3",  
                 "climate_change_4",
                 "climate_change_5",   
                 "climate_change_6")],
                 use = "complete.obs")

# calculate Cronbachs alpha and spearman brown for climate change evaluation
cronbach_ccbeliefs <- psych::alpha(d[c("climate_change_1",   
                                               "climate_change_2",  
                                               "climate_change_3",  
                                               "climate_change_4")], check.keys=F)
cronbach_ccbeliefs
capture.output(cronbach_ccbeliefs, file = "build/figures-and-tables/checks/cronbach_ccbeliefs.csv")

# spbr_ccaction <- spearman_brown(d, c("climate_change_5", "climate_change_6")) # FIXME fails
#spbr_ccaction
#capture.output(spbr_ccaction, file = "build/figures-and-tables/checks/spbr_ccaction")

cronbach_cceval <- psych::alpha(d[c("climate_change_1",   
                                "climate_change_2",  
                                "climate_change_3",  
                                "climate_change_4",
                                "climate_change_5",
                                "climate_change_6")], 
                            check.keys=F)
cronbach_cceval

##############################################################################################
# Subgroup analysis climate change evaluation
##

# join attitude data
choice_h = choice_h %>%
  left_join(attitudes, by= "ID") 

choice_t = choice_t %>%
  left_join(attitudes, by= "ID")

# analyse influence of cceval (heating)
ch_cceval = choice_h %>%
  drop_na(cceval_cat)

anova_cceval_h <- cj_anova(data = ch_cceval, Y ~ Timing + Purchase + Use + Support, 
                              id = ~ "ID", by = ~cceval_cat)
capture.output(anova_cceval_h, file = "build/figures-and-tables/subgroup-analysis/climate-change-evaluation/anova_cceval_h.csv")

sub_cceval_h <- cj(data = ch_cceval, Y ~ Timing + Purchase + Use + Support,
                   estimate = "mm", id = ~ID, by = ~cceval_cat)
write.csv(sub_cceval_h, file = "build/figures-and-tables/subgroup-analysis/climate-change-evaluation/sub_cceval_h.csv")

level_orderhcc <- factor(sub_cceval_h$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                           "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                           "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                           "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
BY_orderhcc <- factor(sub_cceval_h$BY, ordered = TRUE, levels = c("do not agree", "neither / nor", "agree"))

MMHCCChoice <- ggplot(sub_cceval_h,
                      aes(x = level_orderhcc, y = estimate, color = BY_orderhcc)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#b8b6b6" , "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Climate change evaluation")) 

MMHCCChoice
ggsave("interaction climate change evaluation (heating)", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/climate-change-evaluation",
       width = 16, height = 20)

# analyse influence of cceval(transport)
ct_cceval = choice_t %>%
  drop_na(cceval_cat)

anova_cceval_t <- cj_anova(data = ct_cceval, Y ~ Timing + Purchase + Use + Support, 
                              id = ~ "ID", by = ~cceval_cat)
capture.output(anova_cceval_t, file = "build/figures-and-tables/subgroup-analysis/climate-change-evaluation/anova_cceval_t.csv")

sub_cceval_t <- cj(data = ct_cceval, Y ~ Timing + Purchase + Use + Support,
                   estimate = "mm", id = ~ ID, by = ~cceval_cat)
write.csv(sub_cceval_t, file = "build/figures-and-tables/subgroup-analysis/climate-change-evaluation/sub_cceval_t.csv")

level_ordertcc <- factor(sub_cceval_t$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                           "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                           "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                           "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
BY_ordertcc <- factor(sub_cceval_t$BY, ordered = TRUE, levels = c("do not agree", "neither / nor", "agree"))

MMTCCChoice <- ggplot(sub_cceval_t,
                      aes(x = level_ordertcc, y = estimate, color = BY_ordertcc)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#b8b6b6" , "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Climate change evaluation"))

MMTCCChoice
ggsave("interaction climate change evaluation(transport) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/climate-change-evaluation",
       width = 16, height = 20)

##############################################################################################
# Subgroup analysis - trust
##

# analyse influence level of trust in government (heating)
ch_trustgov = choice_h %>%
  drop_na(trust_gov_cat)

anova_trustgov_h <- cj_anova(data = ch_trustgov, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~trust_gov_cat)
capture.output(anova_trustgov_h, file = "build/figures-and-tables/subgroup-analysis/trust/anova_trustgov_h.csv")


sub_trustgov_h <- cj(data = ch_trustgov, Y ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ "ID", by = ~trust_gov_cat)

level_orderhtg <- factor(sub_trustgov_h$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                         "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                         "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                         "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
BY_orderhtg <- factor(sub_trustgov_h$BY, ordered = TRUE, levels = c("do not trust", "neither / nor", "trust"))


MMHTrustgovChoice <- ggplot(sub_trustgov_h,
                         aes(x = level_orderhtg, y = estimate, color = BY_orderhtg)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Trust in federal government"))+
  ggtitle ("Trust in federal government (heating)")
MMHTrustgovChoice
ggsave("interaction trust government (heating) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/trust",
       width = 16, height = 20)

# analyse influence level of trust in government (transport)
ct_trustgov = choice_t %>%
  drop_na(trust_gov_cat)

anova_trustgov_t <- cj_anova(data = ct_trustgov, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~trust_gov_cat)
capture.output(anova_trustgov_t, file = "build/figures-and-tables/subgroup-analysis/trust/anova_trustgov_t.csv")

sub_trustgov_t <- cj(data = ct_trustgov, Y ~ Timing + Purchase + Use + Support,
                  estimate = "mm", id = ~ "ID", by = ~trust_gov_cat)

level_orderttg <- factor(sub_trustgov_t$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                         "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                         "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                         "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
BY_orderttg <- factor(sub_trustgov_t$BY, ordered = TRUE, levels = c("do not trust", "neither / nor", "trust"))

MMTTrustgovChoice <- ggplot(sub_trustgov_t,
                         aes(x = level_orderttg, y = estimate, color = BY_orderttg)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Trust in federal government"))+
  ggtitle ("Trust in federal government (transport)")

MMTTrustgovChoice
ggsave("interaction trust government (transport) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/trust",
       width = 16, height = 20)

# analyse influence level of trust in companies (heating)
ch_trustcom = choice_h %>%
  drop_na(trust_com_cat)

anova_trustcom_h <- cj_anova(data = ch_trustcom, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~trust_com_cat)
capture.output(anova_trustcom_h, file = "build/figures-and-tables/subgroup-analysis/trust/anova_trustcom_h.csv")

sub_trustcom_h <- cj(data = ch_trustcom, Y ~ Timing + Purchase + Use + Support,
                     estimate = "mm", id = ~ "ID", by = ~trust_com_cat)

level_orderhtco <- factor(sub_trustcom_h$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                              "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                              "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                              "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
BY_orderhtco <- factor(sub_trustcom_h$BY, ordered = TRUE, levels = c("do not trust", "neither / nor", "trust"))

MMHTrustcomChoice <- ggplot(sub_trustcom_h,
                            aes(x = level_orderhtco, y = estimate, color = BY_orderhtco)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Trust in companies and corporations"))+
  ggtitle ("Trust in companies and corporations (heating)")

MMHTrustcomChoice
ggsave("interaction trust companies (heating) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/trust",
       width = 16, height = 20)

# analyse influence level of trust in companies (transport)
ct_trustcom = choice_t %>%
  drop_na(trust_com_cat)

anova_trustcom_t <- cj_anova(data = ct_trustcom, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~trust_com_cat)
capture.output(anova_trustcom_t, file = "build/figures-and-tables/subgroup-analysis/trust/anova_trustgcom_t.csv")

sub_trustcom_t <- cj(data = ct_trustcom, Y ~ Timing + Purchase + Use + Support,
                     estimate = "mm", id = ~ "ID", by = ~trust_com_cat)

level_orderttco <- factor(sub_trustcom_t$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                            "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                            "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                            "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
BY_orderttco <- factor(sub_trustcom_t$BY, ordered = TRUE, levels = c("do not trust", "neither / nor", "trust"))

MMTTrustcomChoice <- ggplot(sub_trustcom_t,
                            aes(x = level_orderttco, y = estimate, color = BY_orderttco)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Trust in companies and corporations"))+
  ggtitle ("Trust in companies and corporations (transport)")

MMTTrustcomChoice
ggsave("interaction trust companies (transport) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/trust",
       width = 16, height = 20)

# analyse influence level of trust in citizens (heating)
ch_trustcit = choice_h %>%
  drop_na(trust_cit_cat)

anova_trustcit_h <- cj_anova(data = ch_trustcit, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~trust_cit_cat)
capture.output(anova_trustcit_h, file = "build/figures-and-tables/subgroup-analysis/trust/anova_trustcit_h.csv")


sub_trustcit_h <- cj(data = ch_trustcit, Y ~ Timing + Purchase + Use + Support,
                     estimate = "mm", id = ~ "ID", by = ~trust_cit_cat)

level_orderhtci <- factor(sub_trustcit_h$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                             "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                             "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                             "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
BY_orderhtci <- factor(sub_trustcit_h$BY, ordered = TRUE, levels = c("do not trust", "neither / nor", "trust"))

MMHTrustcitChoice <- ggplot(sub_trustcit_h,
                            aes(x = level_orderhtci, y = estimate, color = BY_orderhtci)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Trust in individuals"))+
  ggtitle ("Trust in individuals (heating)")

MMHTrustcitChoice
ggsave("interaction trust citizens (heating) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/trust",
       width = 16, height = 20)

# analyse influence level of trust in citizens (transport)
ct_trustcit = choice_t %>%
  drop_na(trust_cit_cat)

anova_trustcit_t <- cj_anova(data = ct_trustcit, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~trust_cit_cat)
capture.output(anova_trustcit_t, file = "build/figures-and-tables/subgroup-analysis/trust/anova_trustcit_t.csv")

sub_trustcit_t <- cj(data = ct_trustcit, Y ~ Timing + Purchase + Use + Support,
                     estimate = "mm", id = ~ "ID", by = ~trust_cit_cat)

level_orderttcit <- factor(sub_trustcit_t$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                            "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                            "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                            "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
BY_orderttci <- factor(sub_trustcit_t$BY, ordered = TRUE, levels = c("do not trust", "neither / nor", "trust"))

MMTTrustcitChoice <- ggplot(sub_trustcit_t,
                            aes(x = level_orderttcit, y = estimate, color = BY_orderttci)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Trust in individuals"))+
  ggtitle ("Trust in individuals (transport)")


MMTTrustcitChoice
ggsave("interaction trust citizens (transport) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/trust",
       width = 16, height = 20)

##############################################################################################
# Subgroup analysis - responsibility
##

# analyse influence level of attributed responsibility to government (heating)
choice_h_respgov = choice_h %>%
  drop_na(resp_gov_cat)

anova_respgov_h <- cj_anova(data = choice_h_respgov, Y ~ Timing + Purchase + Use + Support, 
                            id = ~ "ID", by = ~resp_gov_cat)
capture.output(anova_respgov_h, file = "build/figures-and-tables/subgroup-analysis//responsibility/anova_respgov_h.csv")

sub_respgov_h <- cj(data = choice_h_respgov, Y ~ Timing + Purchase + Use + Support,
                 estimate = "mm", id = ~ "ID", by = ~resp_gov_cat)

level_orderhrg <- factor(sub_respgov_h$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                        "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                        "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                        "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
BY_orderhrg <- factor(sub_respgov_h$BY, ordered = TRUE, levels = c("not responsible", "neither / nor", "responsible"))

MMHRespgovChoice <- ggplot(sub_respgov_h,
                        aes(x = level_orderhrg, y = estimate, color = BY_orderhrg)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Attributed responsibility to federal government"))+
  ggtitle("Attributed responsibility to the federal government (heating)")

MMHRespgovChoice
ggsave("interaction responsibility government (heating) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/responsibility",
       width = 17, height = 20)

# analyse influence level of attributed responsibility to government (transport)
choice_t_respgov = choice_t %>%
  drop_na(resp_gov_cat)

anova_respgov_t <- cj_anova(data = choice_t_respgov, Y ~ Timing + Purchase + Use + Support, 
                            id = ~ "ID", by = ~resp_gov_cat)
capture.output(anova_respgov_t, file = "build/figures-and-tables/subgroup-analysis//responsibility/anova_respgov_t.csv")

sub_respgov_t <- cj(data = choice_t_respgov, Y ~ Timing + Purchase + Use + Support,
                 estimate = "mm", id = ~ "ID", by = ~resp_gov_cat)

level_ordertrg <- factor(sub_respgov_t$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                        "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                        "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                        "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
BY_ordertrg  <- factor(sub_respgov_t$BY, ordered = TRUE, levels = c("not responsible", "neither / nor", "responsible"))

MMTRespgovChoice <- ggplot(sub_respgov_t,
                       aes(x = level_ordertrg, y = estimate, color = BY_ordertrg)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#b8b6b6","#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Attributed responsibility to federal government"))+
  ggtitle("Attributed responsibility to the federal government (transport)")

MMTRespgovChoice
ggsave("interaction responsibility government (transport) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/responsibility",
       width = 17, height = 20)

# analyse influence level of attributed responsibility to companies (heating)
choice_h_respcom = choice_h %>%
  drop_na(resp_com_cat)

anova_respcom_h <- cj_anova(data = choice_h_respcom, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~resp_com_cat)
capture.output(anova_respcom_h, file = "build/figures-and-tables/subgroup-analysis/responsibility/anova_respcom_h.csv")

sub_respcom_h <- cj(data = choice_h_respcom, Y ~ Timing + Purchase + Use + Support,
                     estimate = "mm", id = ~ "ID", by = ~resp_com_cat)

level_orderhrcom <- factor(sub_respcom_h$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                               "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                               "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                               "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
BY_orderhrcom <- factor(sub_respcom_h$BY, ordered = TRUE, levels = c("not responsible", "neither / nor", "responsible"))

MMHRespcomChoice <- ggplot(sub_respcom_h,
                           aes(x = level_orderhrcom, y = estimate, color = BY_orderhrcom)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Attributed responsibility to companies and corporations"))+
  ggtitle("Attributed responsibility to companies and corporations (heating)")

MMHRespcomChoice
ggsave("interaction company responsibility (heating) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/responsibility",
       width = 18, height = 20)

# analyse influence level of attributed responsibility to companies (transport)
choice_t_respcom = choice_t %>%
  drop_na(resp_com_cat)

anova_respcom_t <- cj_anova(data = choice_t_respcom, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~resp_com_cat)
capture.output(anova_respcom_t, file = "build/figures-and-tables/subgroup-analysis/responsibility/anova_respcom_t.csv")

sub_respcom_t <- cj(data = choice_t_respcom, Y ~ Timing + Purchase + Use + Support,
                    estimate = "mm", id = ~ "ID", by = ~resp_com_cat)

level_ordertrcom <- factor(sub_respcom_t$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                               "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                               "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                               "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
BY_ordertrcom <- factor(sub_respcom_t$BY, ordered = TRUE, levels = c("not responsible", "neither / nor", "responsible"))

MMTRespcomChoice <- ggplot(sub_respcom_t,
                           aes(x = level_ordertrcom, y = estimate, color = BY_ordertrcom)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Attributed responsibility to companies and corporations"))+
  ggtitle("Attributed responsibility to companies and corporations (transport)")

MMTRespcomChoice
ggsave("interaction company responsibility (transport) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/responsibility",
       width = 18, height = 20)

# analyse influence level of attributed responsibility to citizens (heating)
choice_h_respcit = choice_h %>%
  drop_na(resp_cit_cat)

anova_respcit_h <- cj_anova(data = choice_h_respcit, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~resp_cit_cat)
capture.output(anova_respcit_h, file = "build/figures-and-tables/subgroup-analysis/responsibility/anova_respcit_h.csv")

sub_respcit_h <- cj(data = choice_h_respcit, Y ~ Timing + Purchase + Use + Support,
                    estimate = "mm", id = ~ "ID", by = ~resp_cit_cat)

level_orderhrcit <- factor(sub_respcit_h$level, level = rev(c("2030", "2035", "2040", "2045", "2050", 
                                                              "No purchase instrument", "Purchase tax on fossil fuel heating (10%)", "Purchase tax on fossil fuel heating (20%)", "Purchase ban for fossil fuel heating (2030)", "Purchase ban for fossil fuel heating (2025)",
                                                              "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Replacement of fossil heating (> 30 years)", "Replacement of fossil heating (> 15 years)",
                                                              "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported building renovation measures", "Preferential loan")))
BY_orderhrcit <- factor(sub_respcit_h$BY, ordered = TRUE, levels = c("not responsible", "neither / nor", "responsible"))

MMHRespcitChoice <- ggplot(sub_respcit_h,
                           aes(x = level_orderhrcit, y = estimate, color = BY_orderhrcit)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365","#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Attributed responsibility to individuals"))+
  ggtitle("Attributed responsibility to individuals (heating)")

MMHRespcitChoice
ggsave("interaction citizen responsibility (heating) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/responsibility",
       width = 16, height = 20)

# analyse influence level of attributed responsibility to citizens (transport)
choice_t_respcit = choice_t %>%
  drop_na(resp_cit_cat)

anova_respcit_t <- cj_anova(data = choice_t_respcit, Y ~ Timing + Purchase + Use + Support, 
                             id = ~ "ID", by = ~resp_cit_cat)
capture.output(anova_respcit_t, file = "build/figures-and-tables/subgroup-analysis/responsibility/anova_respcit_t.csv")

sub_respcit_t <- cj(data = choice_t_respcit, Y ~ Timing + Purchase + Use + Support,
                    estimate = "mm", id = ~ "ID", by = ~resp_cit_cat)

level_ordertrcit <- factor(sub_respcit_t$level, level = rev(c("2030", "2035", "2040", "2045", "2050",
                                                           "No purchase instrument", "Purchase tax on ICEV (10%)" , "Purchase tax on ICEV (20%)", "Purchase ban for ICEV (2030)", "Purchase ban for ICEV (2025)",
                                                           "No use instrument", "Tax on fossil fuels (20 ct/l)", "Tax on fossil fuels (50 ct/l)", "Weekday ban on ICEVs in city centers" , "Daily ban on ICEVs in city centers",
                                                           "No supporting instrument", "Subsidies for climate-friendly alternatives", "Trade in bonus", "State-supported infrastructure measures", "Preferential loan")))
BY_ordertrcit <- factor(sub_respcit_t$BY, ordered = TRUE, levels = c("not responsible", "neither / nor", "responsible"))

MMTRespcitChoice <- ggplot(sub_respcit_t,
                          aes(x = level_ordertrcit, y = estimate, color = BY_ordertrcit)) + 
  facet_grid(feature~., space = "fixed", scales = "free") +
  geom_pointrange(aes(min = estimate - 1.95 * std.error, max = estimate + 1.95 * std.error), position = position_dodge(width = 1/2), size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylab("\nMarginal Mean") +
  xlab("Attribute Value") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("#D8B365", "#b8b6b6", "#5AB4AC"), na.translate = F) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.text.x = element_text(size=20),axis.text.y = element_text(size=20), axis.title.y = element_text(size = 20, face ="bold"), axis.title.x = element_text(size = 20, face ="bold"), strip.text.y = element_text(size = 20), legend.text=element_text(size=20), legend.title=element_text(size=20))+
  coord_flip() +
  guides(color=guide_legend(title="Attributed responsibility to individuals"))+
  ggtitle("Attributed responsibility to individuals (transport)")


MMTRespcitChoice
ggsave("interaction citizen responsibility (transport) ", device=png, 
       path = "build/figures-and-tables/subgroup-analysis/responsibility",
       width = 16, height = 20)


