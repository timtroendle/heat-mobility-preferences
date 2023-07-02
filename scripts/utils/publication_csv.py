from typing import Union
from pathlib import Path

import pandas as pd


def to_csv(path_to_data: Path, columns: Union[slice, list[str]],
           float_format_per_column: dict[str: str], path_to_csv: str):
    (
        read_data(path_to_data)
        .loc[:, columns]
        .assign(**{
            col_name: format_float_col(col_name, fmt_str)
            for col_name, fmt_str in float_format_per_column.items()
        })
        .to_csv(path_to_csv, header=True, index=False)
    )


def read_data(path_to_data: Path):
    match path_to_data.suffix:
        case ".feather":
            return pd.read_feather(path_to_data)
        case ".csv":
            return pd.read_csv(path_to_data)
        case _:
            raise ValueError(f"Unsupported data type {path_to_data.suffix}")


def format_float_col(col: str, fmt_str: str):
    def format_col(df: pd.DataFrame):
        return df[col].map(lambda cell: f"{cell:{fmt_str}}")
    return format_col


def optional_param(name: str, default):
    return snakemake.params[name] if name in snakemake.params.keys() else default


if __name__ == "__main__":
    to_csv(
        path_to_data=Path(snakemake.input.data),
        columns=optional_param(name="columns", default=slice(None)),
        float_format_per_column=optional_param(name="float_format_per_column", default={}),
        path_to_csv=snakemake.output[0]
    )
