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


rule analyse_main_effect:
    message: "Analyse {wildcards.estimate} in sector {wildcards.sector} based on {wildcards.measure}."
    input:
        data = "build/data/{measure}-{sector}.feather"
    output:
        "build/paper/fit/{estimate}-{measure}-{sector}.csv"
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/conjoint.R"



rule analyse_subgroup:
    message: "Analyse {wildcards.estimate} in sector {wildcards.sector} based on {wildcards.measure} " +
             "by subgroup {wildcards.subgroup}."
    input:
        data = "build/data/{measure}-{sector}.feather",
        resp_char = rules.preprocess.output.resp_char,
        attitudes = rules.preprocess.output.attitudes
    output:
        "build/paper/fit/{estimate}-{measure}-{sector}-by-{subgroup}.csv"
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/conjoint.R"


rule visualise_main_effects:
    message: "Visualise main results for measure {wildcards.measure} and estimate {wildcards.estimate}."
    input:
        heat = "build/paper/fit/{estimate}-{measure}-heat.csv",
        transport = "build/paper/fit/{estimate}-{measure}-transport.csv"
    params:
        by = None,
        by_order = None
    output:
        plot = temporary("build/paper/{estimate}-{measure}.json")
    conda: "../envs/default.yaml"
    script: "../scripts/analyse/level_plot.py"


rule visualise_subgroup:
    message: "Visualise {wildcards.subgroup} for measure {wildcards.measure} and estimate {wildcards.estimate}."
    input:
        heat = "build/paper/fit/{estimate}-{measure}-heat-by-{subgroup}.csv",
        transport = "build/paper/fit/{estimate}-{measure}-transport-by-{subgroup}.csv",
    params:
        by = lambda wildcards, output: config["subgroups"][wildcards.subgroup]["name"],
        by_order = lambda wildcards, output: config["subgroups"][wildcards.subgroup]["level-order"],
    output:
        plot = temporary("build/paper/{estimate}-{measure}-by-{subgroup}.json")
    conda: "../envs/altair-dev.yaml"
    script: "../scripts/analyse/level_plot.py"


rule render_vega_lite:
    message: "Render Vega Lite spec {wildcards.filename}.json to pdf."
    input:
        json = "build/paper/{filename}.json"
    output:
        pdf = "build/paper/{filename}.pdf"
    conda: "../envs/vega.yaml"
    # vl2pdf not usable because of https://github.com/queryverse/VegaLite.jl/issues/383
    shell: "vl2vg {input.json} | vg2pdf > {output.pdf}"
