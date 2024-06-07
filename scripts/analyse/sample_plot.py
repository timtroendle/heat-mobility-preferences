import altair as alt
import pandas as pd

domain = ["population", "sample"]
range_ = ["#9EC00B", "#F5A314"]
DARK_GREY = "#424242"


def plot_sample(data: pd.DataFrame) -> alt.Chart:
    gender = plot_single_variable(data, "Gender", width=100)
    age = plot_single_variable(data, "Age", width=300)
    muni = plot_single_variable(data, "Municipality size", 470)
    return (
        ((gender & age) | muni)
        .configure(font="Lato", background="none")
        .configure_axis(titleColor=DARK_GREY, labelColor=DARK_GREY)
        .configure_header(titleColor=DARK_GREY, labelColor=DARK_GREY)
        .configure_legend(titleColor=DARK_GREY, labelColor=DARK_GREY, orient="right")
    )


def plot_single_variable(
    data: pd.DataFrame, variable: str, width: float, height: float = 200
) -> alt.Chart:
    data = data[data.variable == variable]
    return (
        alt.Chart(data, width=height, height=width, title=variable)
        .encode(
            y=alt.Y("level").title(None),
            x=alt.X("value").title("Share (%)"),
            color=alt.Color("level_2").title(None).scale(domain=domain, range=range_),
            yOffset="level_2",
        )
        .mark_bar()
    )


def read_data(path_to_data: str) -> pd.DataFrame:
    return (
        pd.read_csv(
            path_to_data,
            names=["variable", "level", "sample", "population"],
            skiprows=1,
        )
        .assign(variable=lambda df: df.variable.fillna(method="ffill"))
        .set_index(["variable", "level"])
        .stack()
        .str.rstrip("%")
        .astype("float")
        .rename("value")
        .reset_index()
    )


if __name__ == "__main__":
    chart = plot_sample(read_data(snakemake.input.data))
    chart.save(snakemake.output[0])
