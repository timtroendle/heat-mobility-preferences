import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator
import seaborn as sns


COLOR_PALETTE = [
    "#FABC3C",
    "#4F6DB8",
    "#424242",
]
sns.set_palette(COLOR_PALETTE)

REFERENCE_YEAR = 1990
FINAL_YEAR = 2019 # avoid Corona effect


def time_series_plot(path_to_emissions: str, path_to_plot: str):
    emissions = pd.read_csv(path_to_emissions, index_col=0).loc[REFERENCE_YEAR:FINAL_YEAR, :]
    rel_emissions = emissions / emissions.loc[REFERENCE_YEAR, :]

    fig = plt.figure(figsize=(8, 2))
    ax = fig.add_subplot(111)

    ax.set_prop_cycle(color=COLOR_PALETTE[:6], linestyle=['-', '--', ':'])
    ax.plot(rel_emissions["Energiewirtschaft"], label="Energy industry", marker="o", markersize=2)
    ax.plot(rel_emissions["Haushalte"], label="Households", marker="o", markersize=2)
    ax.plot(rel_emissions["Straßenverkehr"], label="Road transport", marker="o", markersize=2)
    ax.legend()

    ax.set_ylim(0, )
    ax.set_ylabel(f"Change since {REFERENCE_YEAR}")
    ax.get_xaxis().set_major_locator(MultipleLocator(10))
    ax.get_xaxis().set_minor_locator(MultipleLocator(1))
    ax.set_title(f"CO2 emissions in selected sectors in Germany")
    sns.despine(fig)
    fig.tight_layout()
    fig.savefig(path_to_plot)


if __name__ == "__main__":
    time_series_plot(
        path_to_emissions=snakemake.input.data,
        path_to_plot=snakemake.output[0]
    )
