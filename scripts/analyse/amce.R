library(cjoint)
library(cregg)
library(tidyverse)
library(arrow)

##############################################################################################
# Load data
##

rating_t <- read_feather(snakemake@input[["rating_t"]])
rating_h <- read_feather(snakemake@input[["rating_h"]])
choice_t <- read_feather(snakemake@input[["choice_t"]])
choice_h <- read_feather(snakemake@input[["choice_h"]])

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
ggsave(snakemake@output[["amcet_final"]], device=png,
       width = 16, height = 20)
