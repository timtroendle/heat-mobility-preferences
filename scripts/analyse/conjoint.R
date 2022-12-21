# This script analyses main effects and subgroups.
library(cregg)
library(tidyverse)
library(arrow)


analyse_main_effects <- function(data, estimate, measure, path_to_output) {
    measure_sym <- as.symbol(measure)
    # eval and bquote necessary because of dynamic var "measure"
    fit <- eval(bquote(cj(
        data = data,
        .(measure_sym) ~ Timing + Purchase + Use + Support,
        estimate = estimate,
        id = ~ ID,
    )))
    write_csv(fit, path_to_output)
}


analyse_subgroups <- function(data, attitudes, resp_char, estimate, measure, by, path_to_output) {
    # add respondent characteristics
    data <- data %>%
        left_join(resp_char, by = "ID") %>%
        left_join(attitudes, by = "ID") %>%
        drop_na(by) # TODO assess NAs

    by_sym <- as.symbol(by)
    measure_sym <- as.symbol(measure)
    # eval and bquote necessary because of dynamic var "by" and "measure"
    fit <- eval(bquote(cj(
        data = data,
        .(measure_sym) ~ Timing + Purchase + Use + Support,
        estimate = estimate,
        id = ~ ID,
        by = ~.(by_sym)
    )))
    write_csv(fit, path_to_output)
}

if (is.null(snakemake@wildcards[["subgroup"]])) {
    analyse_main_effects(
        data = read_feather(snakemake@input[["data"]]),
        measure = snakemake@wildcards[["measure"]],
        estimate = snakemake@wildcards[["estimate"]],
        path_to_output = snakemake@output[[1]]
    )
} else {
    analyse_subgroups(
        data = read_feather(snakemake@input[["data"]]),
        attitudes = read_feather(snakemake@input[["attitudes"]]),
        resp_char = read_feather(snakemake@input[["resp_char"]]),
        measure = snakemake@wildcards[["measure"]],
        by = snakemake@wildcards[["subgroup"]],
        estimate = snakemake@wildcards[["estimate"]],
        path_to_output = snakemake@output[[1]]
    )
}
