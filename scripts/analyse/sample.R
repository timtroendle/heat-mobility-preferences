library(cjoint)
library(cregg)
library(forcats)
library(ggpubr)
library(janitor)
library(likert)
library(tidyverse)
library(arrow)

##############################################################################################
# Load data
##

d <- read_feather(snakemake@input[["d"]])
resp_char <- read_feather(snakemake@input[["resp_char"]])
attitudes <- read_feather(snakemake@input[["attitudes"]])

rating_t <- read_feather(snakemake@input[["rating_t"]])
rating_h <- read_feather(snakemake@input[["rating_h"]])
choice_t <- read_feather(snakemake@input[["choice_t"]])
choice_h <- read_feather(snakemake@input[["choice_h"]])

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
ggsave(snakemake@output[["frequency_framing"]], device=png,
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
