# This script analyses more subgroups. I've created it since the existing script is already quite long.
library(cjoint)
library(cregg)
library(tidyverse)
library(arrow)


d <- read_feather(snakemake@input[["d"]])
resp_char <- read_feather(snakemake@input[["resp_char"]])
attitudes <- read_feather(snakemake@input[["attitudes"]])

choice_t <- read_feather(snakemake@input[["choice_t"]])
choice_h <- read_feather(snakemake@input[["choice_h"]])

rating_t <- read_feather(snakemake@input[["rating_t"]])
rating_h <- read_feather(snakemake@input[["rating_h"]])


# add respondent characteristics
choice_h <- choice_h %>% left_join(resp_char, by = "ID")
choice_t <- choice_t %>% left_join(resp_char, by = "ID")
rating_t <- rating_t %>% left_join(resp_char, by = "ID")
rating_t <- rating_t %>% left_join(resp_char, by = "ID")


# add attitude data
choice_h <- choice_h %>% left_join(attitudes, by = "ID")
choice_t <- choice_t %>% left_join(attitudes, by = "ID")
rating_h <- rating_h %>% left_join(attitudes, by = "ID")
rating_t <- rating_t %>% left_join(attitudes, by = "ID")


# climate concern
ch_cceval <- choice_h %>% drop_na(cceval_cat) # TODO assess NAs
ct_cceval <- choice_t %>% drop_na(cceval_cat)
rh_cceval <- rating_h %>% drop_na(cceval_cat)
rt_cceval <- rating_t %>% drop_na(cceval_cat)

sub_cceval_hr <- cj(
    data = rh_cceval,
    rating ~ Timing + Purchase + Use + Support,
    estimate = "mm",
    id = ~ID,
    by = ~cceval_cat
)
write.csv(sub_cceval_hr, file = snakemake@output[["cceval_hr"]])

sub_cceval_tr <- cj(
    data = rt_cceval,
    rating ~ Timing + Purchase + Use + Support,
    estimate = "mm",
    id = ~ID,
    by = ~cceval_cat
)
write.csv(sub_cceval_tr, file = snakemake@output[["cceval_tr"]])

sub_cceval_hc <- cj(
    data = ch_cceval,
    Y ~ Timing + Purchase + Use + Support,
    estimate = "mm",
    id = ~ID,
    by = ~cceval_cat
)
write.csv(sub_cceval_hc, file = snakemake@output[["cceval_hc"]])

sub_cceval_tc <- cj(
    data = ct_cceval,
    Y ~ Timing + Purchase + Use + Support,
    estimate = "mm",
    id = ~ID,
    by = ~cceval_cat
)
write.csv(sub_cceval_tc, file = snakemake@output[["cceval_tc"]])
