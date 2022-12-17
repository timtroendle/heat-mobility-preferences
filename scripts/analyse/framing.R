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
# Analyse whether framing has a significant effect, and show MM with framng as an interaction effect

# compute F-test for framing interaction 
anova_framingch <- cj_anova(data = choice_h, choice ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~Framing)
capture.output(anova_framingch, file = "build/figures-and-tables/framing-interaction/anova_fch.csv")

anova_framingct <- cj_anova(data = choice_t, choice ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~Framing)
capture.output(anova_framingct, file = "build/figures-and-tables/framing-interaction/anova_fct.csv")

anova_framingrh <- cj_anova(data = rating_h, bin_rate ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~Framing)
capture.output(anova_framingrh, file = "build/figures-and-tables/framing-interaction/anova_frh.csv")

anova_framingrt <- cj_anova(data = rating_t, bin_rate ~ Timing + Purchase + Use + Support, 
                      id = ~ "ID", by = ~Framing)
capture.output(anova_framingrt, file = "build/figures-and-tables/framing-interaction/anova_frt.csv")

# plot framing interaction heating (choice)
int_framingch <- cj(data = choice_h, choice ~ Timing + Purchase + Use + Support,
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
ggsave("int_framingch.png",
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
ggsave("int_framingrh.png",
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
ggsave("interaction heating_final.png",
       path = "build/figures-and-tables/framing-interaction",
       width = 16, height = 20)

# plot framing interaction transport (choice)
int_framingct <- cj(data = choice_t, choice ~ Timing + Purchase + Use + Support,
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
ggsave("int_framingct.png",
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
ggsave("int_framingrt.png",
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
ggsave(snakemake@output[["transport_interaction"]],
       width = 16, height = 20)
