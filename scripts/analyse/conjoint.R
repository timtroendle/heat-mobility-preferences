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


analyse_subgroups <- function(data, estimate, measure, by, alpha, path_to_output) {
    data <- data %>%
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


handle_speeder_special_case <- function(data) {
    all_respondents <- data
    all_respondents$speeders <- "All"
    no_speeder <- data_without_speeder
    no_speeder$speeders <- "Without speeders"
    data <- rbind(all_respondents, no_speeder)
    data$speeders <- factor(data$speeders)
    data
}

data <- read_feather(snakemake@input[["data"]]) %>%
    left_join(read_feather(snakemake@input[["respondents"]]), by = "ID")
data_without_speeder <- data %>% filter(speeders == "No speeders")

if (is.null(snakemake@wildcards[["subgroup"]])) {
    analyse_main_effects(
        data = data_without_speeder,
        measure = snakemake@wildcards[["measure"]],
        estimate = snakemake@wildcards[["estimate"]],
        alpha = snakemake@params[["alpha"]],
        path_to_output = snakemake@output[[1]]
    )
} else {
    subgroup <- snakemake@wildcards[["subgroup"]]
    if (subgroup == "speeders") {
        data <- handle_speeder_special_case(data)
    } else {
        data <- data_without_speeder
    }
    analyse_subgroups(
        data = data,
        measure = snakemake@wildcards[["measure"]],
        by = snakemake@wildcards[["subgroup"]],
        estimate = snakemake@wildcards[["estimate"]],
        alpha = snakemake@params[["alpha"]],
        path_to_output = snakemake@output[[1]]
    )
}
