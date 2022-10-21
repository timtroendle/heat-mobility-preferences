rule plot_emissions:
    message: "Create plot of historical emissions."
    input:
        data = "data/historic-co2/co2-in-mt.csv"
    output: "build/emissions.png"
    conda: "../envs/default.yaml"
    script: "../scripts/analyse/emissions.py"


rule entire_analysis:
    message: "Run the entire analysis."
    input:
        data = "data/surveydata-2022-03-19.xlsx"
    output:
        duration = "build/figures-and-tables/checks/Histogram duration.png",
    conda: "../envs/r.yaml"
    script: "../scripts/Public_acceptance_JA_020522.R"
