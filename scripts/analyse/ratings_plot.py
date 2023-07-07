import altair as alt
import pandas as pd


DARK_GREY = "#424242"
WIDTH_SINGLE = 413
STEP_SIZE = 25


def plot_ratings(path_to_heat_data: str, path_to_transport_data: str, path_to_plot: str):
    averages = pd.concat([
        read_average_ratings(path_to_heat_data, "Building"),
        read_average_ratings(path_to_transport_data, "Transport")
    ])

    chart = (
        alt
        .Chart(averages)
        .encode(
            x=alt.X(
                'rating:Q',
                title="Average rating",
                sort=["High", "Medium", "Low"],
                scale=alt.Scale(domain=[1, 5])
            ),
            y=alt.Y(
                'Climate change concern:O',
                sort=["High", "Medium", "Low"],
                title="Climate change concern"
            ),
            detail=alt.Detail('Climate change concern:O')
        ).properties(
            width=WIDTH_SINGLE,
        )
    )
    lines = chart.mark_line(color=DARK_GREY)
    points = (
        chart
        .mark_point(
            size=100,
            opacity=1,
            filled=True
        ).encode(
            color=alt.Color(
                'Emission attribution:O',
                scale=alt.Scale(
                    domain=['Too low', 'Correct', 'Too high'],
                )
            )
        )
    )
    layered = (lines + points)
    facetted = layered.facet(row='Sector:N')

    (
        facetted
        .configure(font="Lato")
        .configure_view(step=STEP_SIZE)
        .configure_axis(titleColor=DARK_GREY, labelColor=DARK_GREY)
        .configure_header(titleColor=DARK_GREY, labelColor=DARK_GREY)
        .configure_legend(titleColor=DARK_GREY, labelColor=DARK_GREY, orient="right")
        .save(path_to_plot)
    )


def read_average_ratings(path_to_data: str, sector_name: str) -> pd.DataFrame:
    df = pd.read_csv(path_to_data)
    df["Climate change concern"] = df.BY.str.split("/").map(lambda row: row[0])
    df["Emission attribution"] = df.BY.str.split("/").map(lambda row: row[1]).str.capitalize()
    return (
        df
        .groupby(["Climate change concern", "Emission attribution"])
        .estimate
        .mean()
        .rename("rating")
        .reset_index()
        .assign(Sector=sector_name)
    )


if __name__ == "__main__":
    plot_ratings(
        path_to_heat_data=snakemake.input.heat,
        path_to_transport_data=snakemake.input.transport,
        path_to_plot=snakemake.output[0]
    )
