rule plot_emissions:
    message: "Create plot of historical emissions."
    input:
        data = "data/historic-co2/co2-in-mt.csv"
    output: "build/emissions.png"
    conda: "../envs/default.yaml"
    script: "../scripts/analyse/emissions.py"


rule sample:
    message: "Analyse the sample."
    input:
        d = rules.preprocess.output.d,
        resp_char = rules.preprocess.output.resp_char,
        attitudes = rules.preprocess.output.attitudes,
        rating_t = rules.preprocess.output.rating_t,
        rating_h = rules.preprocess.output.rating_h,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
    output:
        frequency_framing = "build/figures-and-tables/checks/Frequency Framing & Sector.png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/sample.R"


rule framing:
    message: "Analyse the framing experiment."
    input:
        rating_t = rules.preprocess.output.rating_t,
        rating_h = rules.preprocess.output.rating_h,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
    output:
        transport_interaction = "build/figures-and-tables/framing-interaction/interaction transport_final.png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/framing.R"


rule marginal_means:
    message: "Analyse conjoint using marginal means."
    input:
        rating_t = rules.preprocess.output.rating_t,
        rating_h = rules.preprocess.output.rating_h,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
    output:
        mmt_rating = "build/figures-and-tables/marginal-means/mmt_rating.png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/mm.R"


rule amce:
    message: "Analyse conjoint using AMCEs."
    input:
        rating_t = rules.preprocess.output.rating_t,
        rating_h = rules.preprocess.output.rating_h,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
    output:
        amcet_final = "build/figures-and-tables/AMCE/amcet_final.png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/amce.R"


rule robustness:
    message: "Check robustness of experiment."
    input:
        first_sector = rules.preprocess.output.first_sector,
        rating_t = rules.preprocess.output.rating_t,
        rating_h = rules.preprocess.output.rating_h,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
    output:
        robustness_final = "build/figures-and-tables/checks/robustness-checks/interaction package number transport_final.png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/robustness.R"


rule subgroups:
    message: "Analyse subgroups."
    input:
        d = rules.preprocess.output.d,
        resp_char = rules.preprocess.output.resp_char,
        attitudes = rules.preprocess.output.attitudes,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
    output:
        subgroups_final = "build/figures-and-tables/subgroup-analysis/responsibility/interaction citizen responsibility (transport).png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/subgroups.R"


rule subgroups2:
    message: "Analyse more subgroups."
    input:
        d = rules.preprocess.output.d,
        resp_char = rules.preprocess.output.resp_char,
        attitudes = rules.preprocess.output.attitudes,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
        rating_t = rules.preprocess.output.rating_t,
        rating_h = rules.preprocess.output.rating_h,
    output:
        cceval_hr = "build/paper/mm-by-climate-change-heat-rating.csv",
        cceval_tr = "build/paper/mm-by-climate-change-transport-rating.csv",
        cceval_hc = "build/paper/mm-by-climate-change-heat-choice.csv",
        cceval_tc = "build/paper/mm-by-climate-change-transport-choice.csv"
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/subgroups2.R"


rule visualise_levels:
    message: "Visualise main results for measure {wildcards.measure}."
    input:
        heat = "build/figures-and-tables/marginal-means/mmh_{measure}.csv",
        transport = "build/figures-and-tables/marginal-means/mmt_{measure}.csv"
    params:
        by = None,
        by_order = None
    output:
        plot = "build/paper/main-{measure}.pdf"
    conda: "../envs/default.yaml"
    script: "../scripts/analyse/level_plot.py"


rule visualise_climate_concern:
    message: "Visualise climate concern for measure {wildcards.measure}."
    input:
        heat = "build/paper/mm-by-climate-change-heat-{measure}.csv",
        transport = "build/paper/mm-by-climate-change-transport-{measure}.csv"
    params:
        by = "Climate concern",
        by_order = ['Low', 'Medium', 'High']
    output:
        plot = "build/paper/climate-concern-{measure}.html"
    conda: "../envs/altair-dev.yaml"
    script: "../scripts/analyse/level_plot.py"
