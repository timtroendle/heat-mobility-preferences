import altair as alt
import pandas as pd

DARK_GREY = "#424242"
COLORS = {
    "Timing": "#65C2A5",
    "Purchase": "#FC8D63",
    "Use": "#8DA0CB",
    "Support": "#E78AC3",
}
LABEL_LIMIT = 300
WIDTH_SINGLE = 163
MIN_OPACITY_FOR_TWO = 0.4
MIN_OPACITY_FOR_MORE_THAN_TWO = 0.2
STEP_SIZE = 14


def visualise_both_sectors(
    path_to_heat_data: str,
    path_to_transport_data: str,
    path_to_plot,
    measure: str,
    estimate: str,
    level_order: dict[str : list[str]],
    attribute_order: list[str],
    attribute: str = None,
    by: str = None,
    by_order: list[str] = None,
):
    match (estimate, measure, by):
        case ("amce", "choice", any_by):
            zero = 0
            domain = (-0.25, 0.25)
            estimate_name = ["Average marginal", "component effects"]
        case ("amce", "rating", any_by):
            zero = 0
            domain = (-0.45, 0.45)
            estimate_name = ["Average marginal", "component effects"]
        case ("mm", "choice", any_by):
            zero = 0.5
            domain = (0.3, 0.7)
            estimate_name = "Marginal means"
        case ("mm", "rating", "Climate change concern"):
            zero = 3
            domain = (1.5, 4.5)
            estimate_name = "Marginal means"
        case ("mm", "rating", any_by):
            zero = 3
            domain = (2, 4)
            estimate_name = "Marginal means"
        case _:
            raise ValueError(
                f"Unknown combination of estimate {estimate} and measure {measure}."
            )
    df_heat = pd.read_csv(path_to_heat_data).assign(zero=zero)
    df_transport = pd.read_csv(path_to_transport_data).assign(zero=zero)
    if attribute:
        df_heat = df_heat[df_heat.feature == attribute]
        df_transport = df_transport[df_transport.feature == attribute]

    chart_heating = visualise_single_sector(
        df_heat,
        title="Buildings sector",
        estimate=estimate_name,
        domain=domain,
        attribute_order=attribute_order,
        level_order=level_order["heat"],
        y_orientation="left",
        by=by,
        by_order=by_order,
        attribute=attribute,
    )
    chart_transport = visualise_single_sector(
        df_transport,
        title="Transport sector",
        estimate=estimate_name,
        domain=domain,
        attribute_order=attribute_order,
        level_order=level_order["transport"],
        y_orientation="right",
        by=by,
        by_order=by_order,
        attribute=attribute,
    )
    combined_chart = chart_heating | chart_transport
    n_legend_cols = 0 if by != "Age" else 2
    (
        combined_chart.configure(font="Lato", background="none")
        .configure_view(step=STEP_SIZE if not by else STEP_SIZE / len(by_order))
        .configure_axis(titleColor=DARK_GREY, labelColor=DARK_GREY)
        .configure_header(titleColor=DARK_GREY, labelColor=DARK_GREY)
        .configure_legend(
            titleColor=DARK_GREY,
            labelColor=DARK_GREY,
            orient="bottom",
            columns=n_legend_cols,
        )
        .save(path_to_plot)
    )


def visualise_single_sector(
    data: pd.DataFrame,
    title: str,
    estimate: str,
    attribute_order: list[str],
    level_order: list[str],
    y_orientation: str,
    domain: tuple[float, float],
    attribute: str = None,
    by: str = None,
    by_order: list[str] = None,
):
    if by:
        data.BY = data.BY.astype("category").cat.reorder_categories(
            by_order, ordered=False
        )
        data = data.sort_values("BY").reset_index(drop=True)
    if attribute:
        color = alt.value(COLORS[attribute])
    else:
        color = alt.Color(
            "feature",
            sort=attribute_order,
            legend=alt.Legend(title="Attribute"),
            scale=alt.Scale(scheme="set2"),
        )
    base = (
        alt.Chart(data, title=title)
        .encode(
            y=alt.Y(
                "level",
                sort=level_order,
                title=None,
                axis=alt.Axis(orient=y_orientation, labelLimit=LABEL_LIMIT),
            ),
            x=alt.X(
                "estimate", title=estimate, scale=alt.Scale(domain=domain, zero=False)
            ),
            color=color,
        )
        .properties(
            width=WIDTH_SINGLE,
        )
    )
    if by:
        min_opacity = (
            MIN_OPACITY_FOR_MORE_THAN_TWO if len(by_order) > 2 else MIN_OPACITY_FOR_TWO
        )
        base = base.encode(
            yOffset=alt.YOffset("BY", sort=by_order),
            opacity=alt.Opacity(
                "BY",
                sort=by_order,
                title=by,
                scale=alt.Scale(rangeMin=min_opacity, rangeMax=1),
                legend=None,
            ),
            color=color,
        )

    interval = base.mark_rule(strokeWidth=1.5, opacity=1).encode(x="lower", x2="upper")

    point_base = (
        base.encode(shape=alt.Shape("BY", sort=by_order, title=by)) if by else base
    )
    point = point_base.encode().mark_point(opacity=1, filled=True)

    base_line = (
        alt.Chart(data)
        .mark_rule(color=DARK_GREY, strokeDash=[4], opacity=0.8)
        .encode(x="zero:Q")
    )

    left = 0 if y_orientation == "left" else WIDTH_SINGLE - 3
    right = 4 if y_orientation == "left" else WIDTH_SINGLE
    area = base.mark_area(opacity=1, filled=True, fillOpacity=1, line=False).encode(
        x=alt.value(left), x2=alt.value(right), opacity=alt.FillOpacityValue(1)
    )

    entire_chart = area + base_line + interval + point
    return entire_chart


if __name__ == "__main__":
    try:
        attribute = snakemake.wildcards.attribute
    except AttributeError:
        attribute = None
    visualise_both_sectors(
        path_to_heat_data=snakemake.input.heat,
        path_to_transport_data=snakemake.input.transport,
        path_to_plot=snakemake.output.plot,
        by=snakemake.params.by,
        by_order=snakemake.params.by_order,
        level_order=snakemake.params.level_order,
        attribute_order=snakemake.params.attribute_order,
        measure=snakemake.wildcards.measure,
        estimate=snakemake.wildcards.estimate,
        attribute=attribute,
    )
