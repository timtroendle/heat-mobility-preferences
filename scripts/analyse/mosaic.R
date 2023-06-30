library(arrow)
library(ggmosaic)
library(dplyr)
library(tidyr)


mosaic_plot <- function(data, sector, path_to_plot) {
    if (sector == "heat") {
        plot <- mosaic_plot_heat(data)
    } else {
        plot <- mosaic_plot_transport(data)
    }
    ggsave(file = path_to_plot, width = 10, height = 10, dpi = 300)
}

mosaic_plot_heat <- function(data) {
    g <- ggplot(data = data) +
        geom_mosaic(aes(x = product(cceval_cat), fill = relevance_h_cat)
        ) +
        theme_mosaic() +
        labs(x = "Climate concern", y = "Emission attribution (Building)") +
        scale_fill_manual(
            name = "Emission attribution (Building)",
            labels = c("Too low", "Correct", "Too high"),
            values = c("#FC8F66", "grey", "#8DA0CB"))
}

mosaic_plot_transport <- function(data) {
    g <- ggplot(data = data) +
        geom_mosaic(aes(x = product(cceval_cat), fill = relevance_t_cat)
        ) +
        theme_mosaic() +
        labs(x = "Climate concern", y = "Emission attribution (Transport)") +
        scale_fill_manual(
            name = "Emission attribution (Transport)",
            labels = c("Too low", "Correct", "Too high"),
            values = c("#FC8F66", "grey", "#8DA0CB"))
}


mosaic_plot(
    data = read_feather(snakemake@input[[1]]),
    sector = snakemake@wildcards[["sector"]],
    path_to_plot = snakemake@output[[1]]
)
