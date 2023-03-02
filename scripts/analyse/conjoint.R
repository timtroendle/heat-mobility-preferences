# This script analyses main effects and subgroups.
library(cregg)
library(tidyverse)
library(arrow)


analyse_main_effects <- function(data, estimate, measure, alpha, path_to_output) {
    measure_sym <- as.symbol(measure)
    # eval and bquote necessary because of dynamic var "measure"
    fit <- eval(bquote(cj(
        data = data,
        .(measure_sym) ~ Timing + Purchase + Use + Support,
        estimate = estimate,
        alpha = alpha,
        id = ~ ID,
    )))
    write_csv(fit, path_to_output)
}


analyse_subgroups <- function(data, respondents, estimate, measure, by, alpha, path_to_output) {
    # add respondent characteristics
    data <- data %>%
        left_join(respondents, by = "ID") %>%
        drop_na(by) # TODO assess NAs

    by_sym <- as.symbol(by)
    measure_sym <- as.symbol(measure)
    # eval and bquote necessary because of dynamic var "by" and "measure"
    fit <- eval(bquote(cj(
        data = data,
        .(measure_sym) ~ Timing + Purchase + Use + Support,
        estimate = estimate,
        id = ~ ID,
        alpha = alpha,
        by = ~.(by_sym)
    )))
    write_csv(fit, path_to_output)
}

if (is.null(snakemake@wildcards[["subgroup"]])) {
    analyse_main_effects(
        data = read_feather(snakemake@input[["data"]]),
        measure = snakemake@wildcards[["measure"]],
        estimate = snakemake@wildcards[["estimate"]],
        alpha = snakemake@params[["alpha"]],
        path_to_output = snakemake@output[[1]]
    )
} else {
    analyse_subgroups(
        data = read_feather(snakemake@input[["data"]]),
        respondents = read_feather(snakemake@input[["respondents"]]),
        measure = snakemake@wildcards[["measure"]],
        by = snakemake@wildcards[["subgroup"]],
        estimate = snakemake@wildcards[["estimate"]],
        alpha = snakemake@params[["alpha"]],
        path_to_output = snakemake@output[[1]]
    )
}
