rule preprocess:
    message: "Preprocess the survey data."
    input:
        data = config["data-sources"]["survey"]
    params:
        co2_share_heat = config["co2-share"]["heat"],
        co2_share_transport = config["co2-share"]["transport"],
        co2_share_tolerance = config["co2-share"]["tolerance"]
    output:
        d = "build/data/d.feather",
        respondents = "build/data/respondents.feather",
        rating_t = "build/data/rating-transport.feather",
        rating_h = "build/data/rating-heat.feather",
        choice_t = "build/data/choice-transport.feather",
        choice_h = "build/data/choice-heat.feather",
        duration = "build/results/checks/Histogram duration.png",
    conda: "../envs/r.yaml"
    script: "../scripts/preprocess/preprocess.R"


rule census_data:
    message: "Download zensus data to compare our sample against."
    params:
        url = config["data-sources"]["census"]
    output:
        zip = temp("data/automatic/raw-census.zip"),
        csv = protected("data/automatic/Zensus11_Datensatz_Bevoelkerung.csv")
    shadow:
        "minimal"
    shell: """
        curl -sLo {output.zip} '{params.url}'
        unzip -o {output.zip} -d data/automatic
        """
