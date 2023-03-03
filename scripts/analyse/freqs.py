import pandas as pd


def frequencies_of_levels(path_to_data: str, path_to_respondents: str, attributes: list[str], path_to_output: str):
    data = pd.read_feather(path_to_data)
    respondents = pd.read_feather(path_to_respondents)
    data = data.join(respondents, how="left", on="ID", rsuffix="r")
    data = data[data.speeders == "No speeders"]
    freqs = pd.concat(
        [build_row(data, attribute1, attributes) for attribute1 in attributes],
        axis=0)
    (
        freqs
        .stack()
        .rename("frequency")
        .reset_index()
        .rename_axis()
        .to_csv(path_to_output)
    )


def build_row(data: pd.DataFrame, attribute1: str, all_attributes: list[str]):
    return pd.concat(
        [pd.crosstab(data[attribute1], data[attribute2]) for attribute2 in all_attributes],
        axis=1
    )


if __name__ == "__main__":
    frequencies_of_levels(
        path_to_data=snakemake.input.data,
        path_to_respondents=snakemake.input.respondents,
        attributes=snakemake.params.attributes,
        path_to_output=snakemake.output[0]
    )
