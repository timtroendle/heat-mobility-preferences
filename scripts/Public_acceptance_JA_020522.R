library(cjoint)
library(cregg)
library(forcats)
library(ggpubr)
library(janitor)
library(likert)
library(psych)
library(tidyverse)
devtools::install_github("LukasWallrich/rNuggets@5dc76f1")
library(arrow)

##############################################################################################
# Load data
##

d <- read_feather(snakemake@input[["d"]])
first_sector <- read_feather(snakemake@input[["first_sector"]])
framing <- read_feather(snakemake@input[["framing"]])
resp_char <- read_feather(snakemake@input[["resp_char"]])
attitudes <- read_feather(snakemake@input[["attitudes"]])

rating_t <- read_feather(snakemake@input[["rating_t"]])
rating_h <- read_feather(snakemake@input[["rating_h"]])
choice_t <- read_feather(snakemake@input[["choice_t"]])
choice_h <- read_feather(snakemake@input[["choice_h"]])

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
ggsave(snakemake@output[["robustness_final"]], device=png,
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


