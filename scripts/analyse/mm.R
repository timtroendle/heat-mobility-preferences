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
ggsave(snakemake@output[["mmt_rating"]], device=png,
       width = 16, height = 20)
