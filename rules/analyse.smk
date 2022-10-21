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
        d = rules.preprocess.output.d,
        first_sector = rules.preprocess.output.first_sector,
        framing = rules.preprocess.output.framing,
        resp_char = rules.preprocess.output.resp_char,
        attitudes = rules.preprocess.output.attitudes,
        rating_t = rules.preprocess.output.rating_t,
        rating_h = rules.preprocess.output.rating_h,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
        duration = rules.preprocess.output.duration,
    output:
        frequency_framing = "build/figures-and-tables/checks/Frequency Framing & Sector.png",
    conda: "../envs/r.yaml"
    script: "../scripts/Public_acceptance_JA_020522.R"
