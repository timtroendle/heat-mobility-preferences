import altair as alt
import pandas as pd


DARK_GREY = "#424242"
LABEL_LIMIT = 300
WIDTH_SINGLE = 200
HEIGHT_SINGLE = 200


def design_plot(path_to_heat_data: str, path_to_transport_data: str, level_order: dict[str: list[str]],
                path_to_plot: str):
    df_heat = pd.read_csv(path_to_heat_data)
    df_transport = pd.read_csv(path_to_transport_data)

    chart_heating = visualise_single_sector(
        df_heat,
        title="Heat",
        level_order=level_order["heat"],
        y_orientation="left",
    )
    chart_transport = visualise_single_sector(
        df_transport,
        title="Transport",
        level_order=level_order["transport"],
        y_orientation="right",
    )
    combined_chart = chart_heating | chart_transport
    (
        combined_chart
        .configure(font="Lato")
        .configure_axis(titleColor=DARK_GREY, labelColor=DARK_GREY)
        .configure_header(titleColor=DARK_GREY, labelColor=DARK_GREY)
        .configure_legend(titleColor=DARK_GREY, labelColor=DARK_GREY, orient="bottom")
        .save(path_to_plot)
    )


def visualise_single_sector(data: pd.DataFrame, title: str, level_order: list[str], y_orientation: str):
    return (
        alt
        .Chart(data, title=title)
        .properties(
            width=WIDTH_SINGLE,
            height=HEIGHT_SINGLE
        )
        .mark_rect()
        .encode(
            x=alt.X('level_0:O', sort=level_order, title="Level"),
            y=alt.Y('level_1:O', sort=level_order, title="Level",
                    axis=alt.Axis(orient=y_orientation, labelLimit=LABEL_LIMIT)),
            color=alt.Color('frequency:Q', title="Frequency", scale=alt.Scale(scheme="greys"))
        )
    )


if __name__ == "__main__":
    design_plot(
        path_to_heat_data=snakemake.input.heat,
        path_to_transport_data=snakemake.input.transport,
        level_order=snakemake.params.level_order,
        path_to_plot=snakemake.output[0]
    )
