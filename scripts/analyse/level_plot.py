import pandas as pd
import altair as alt


DARK_GREY = "#424242"
LABEL_LIMIT = 300
WIDTH_SINGLE = 250
HEIGHT_SINGLE = 250

HEAT_LEVEL_ORDER = [
    '2030', '2035', '2040', '2045', '2050',
    'No purchase instrument',
    'Purchase tax on fossil fuel heating (10%)',
    'Purchase tax on fossil fuel heating (20%)',
    'Purchase ban for fossil fuel heating (2025)',
    'Purchase ban for fossil fuel heating (2030)',
    'No use instrument',
    'Tax on fossil fuels (20 ct/l)',
    'Tax on fossil fuels (50 ct/l)',
    'Replacement of fossil heating (> 15 years)',
    'Replacement of fossil heating (> 30 years)',
    'No supporting instrument',
    'Subsidies for climate-friendly alternatives',
    'Trade in bonus',
    'State-supported building renovation measures',
    'Preferential loan'
]

TRANSPORT_LEVEL_ORDER = [
    '2030', '2035', '2040', '2045', '2050',
    'No purchase instrument',
    'Purchase tax on ICEV (10%)',
    'Purchase tax on ICEV (20%)',
    'Purchase ban for ICEV (2025)',
    'Purchase ban for ICEV (2030)',
    'No use instrument',
    'Tax on fossil fuels (20 ct/l)',
    'Tax on fossil fuels (50 ct/l)',
    'Weekday ban on ICEVs in city centers',
    'Daily ban on ICEVs in city centers',
    'No supporting instrument',
    'Subsidies for climate-friendly alternatives',
    'Trade in bonus',
    'State-supported infrastructure measures',
    'Preferential loan'
]


def visualise_both_sectors(path_to_heat_data: str, path_to_transport_data: str, path_to_plot,
                           measure: str, estimate: str, by: str = None, by_order: list[str] = None):
    match (estimate, measure):
        case ("amce", str()):
            zero = 0
            domain = (-0.5, 0.5)
            estimate_name = "Average marginal component effects"
        case ("mm", "choice"):
            zero = 0.5
            domain = (0.3, 0.7)
            estimate_name = "Marginal means"
        case ("mm", "rating"):
            zero = 3
            domain = (1, 5)
            estimate_name = "Marginal means"
        case _:
            raise ValueError(f"Unknown combination of estimate {estimate} and measure {measure}.")
    df_heat = pd.read_csv(path_to_heat_data).assign(zero=zero)
    df_transport = pd.read_csv(path_to_transport_data).assign(zero=zero)

    chart_heating = visualise_single_sector(
        df_heat,
        title="Heat",
        estimate=estimate_name,
        domain=domain,
        level_order=HEAT_LEVEL_ORDER,
        y_orientation="left",
        by=by,
        by_order=by_order
    )
    chart_transport = visualise_single_sector(
        df_transport,
        title="Transport",
        estimate=estimate_name,
        domain=domain,
        level_order=TRANSPORT_LEVEL_ORDER,
        y_orientation="right",
        by=by,
        by_order=by_order
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


def visualise_single_sector(data: pd.DataFrame, title: str, estimate: str, level_order: list[str], y_orientation: str,
                            domain: tuple[float, float], by: str = None, by_order: list[str] = None):
    base = alt.Chart(
        data,
        title=title
    ).encode(
        y=alt.Y("level", sort=level_order, title="Level", axis=alt.Axis(orient=y_orientation, labelLimit=LABEL_LIMIT)),
        x=alt.X("estimate", title=estimate, scale=alt.Scale(domain=domain, zero=False)),
        color=alt.Color("feature", sort=data.feature.unique(), legend=alt.Legend(title="Attribute")),
    ).properties(
        width=WIDTH_SINGLE,
        height=HEIGHT_SINGLE
    )
    if by:
        base = base.encode(
            yOffset=alt.YOffset("BY", sort=by_order),
            opacity=alt.Opacity("BY", sort=by_order, title=by, scale=alt.Scale(rangeMin=0.2, rangeMax=1))
        )

    interval = base.mark_rule(strokeWidth=1.5, opacity=0.6).encode(
        x="lower",
        x2="upper"
    )

    point = base.mark_circle(opacity=1)

    base_line = alt.Chart(data).mark_rule(color=DARK_GREY, strokeDash=[4], opacity=0.8).encode(
        x='zero:Q'
    )

    left = 0 if y_orientation == "left" else WIDTH_SINGLE - 4
    right = 4 if y_orientation == "left" else WIDTH_SINGLE
    area = base.mark_area(opacity=1, filled=True, fillOpacity=0.7, line=False).encode(
        x=alt.value(left),
        x2=alt.value(right),
    )

    entire_chart = (area + base_line + interval + point)
    return entire_chart


if __name__ == "__main__":
    visualise_both_sectors(
        path_to_heat_data=snakemake.input.heat,
        path_to_transport_data=snakemake.input.transport,
        path_to_plot=snakemake.output.plot,
        by=snakemake.params.by,
        by_order=snakemake.params.by_order,
        measure=snakemake.wildcards.measure,
        estimate=snakemake.wildcards.estimate
    )
