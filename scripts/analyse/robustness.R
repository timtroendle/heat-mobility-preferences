library(cjoint)
library(cregg)
library(tidyverse)
library(arrow)

##############################################################################################
# Load data
##

first_sector <- read_feather(snakemake@input[["first_sector"]])

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
ggsave("int_firstch.png",
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
ggsave("int_firstrh.png",
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
ggsave("interaction first sector heating_final.png",
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
ggsave("int_firstct.png",
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
ggsave("int_firstrt.png",
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
ggsave("interaction first sector transport_final.png",
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
ggsave("int_choicerch.png",
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
ggsave("int_choicerrh.png",
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
ggsave("interaction choice round heating_final.png",
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
ggsave("in_choicerct.png",
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
ggsave("int_choicerrt.png",
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
ggsave("interaction choice round transport_final.png",
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
ggsave("int_packch.png",
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
ggsave("int_packrh.png",
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
ggsave("interaction package number heating_final.png",
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
ggsave("int_packct.png",
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
ggsave("int_packrt.png",
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
ggsave(snakemake@output[["robustness_final"]],
       width = 16, height = 20)
