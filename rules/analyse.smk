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
        respondents = rules.preprocess.output.respondents,
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
        framing = rules.preprocess.output.framing,
    params:
        level_order_heat = config["level-order"]["heat"],
        level_order_transport = config["level-order"]["transport"]
    output:
        transport_interaction = "build/figures-and-tables/framing-interaction/interaction transport_final.png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/framing.R"


rule robustness:
    message: "Check robustness of experiment."
    input:
        first_sector = rules.preprocess.output.first_sector,
        rating_t = rules.preprocess.output.rating_t,
        rating_h = rules.preprocess.output.rating_h,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
    params:
        level_order_heat = config["level-order"]["heat"],
        level_order_transport = config["level-order"]["transport"]
    output:
        robustness_final = "build/figures-and-tables/checks/robustness-checks/interaction package number transport_final.png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/robustness.R"


rule subgroups:
    message: "Analyse subgroups."
    input:
        d = rules.preprocess.output.d,
        respondents = rules.preprocess.output.respondents,
        choice_t = rules.preprocess.output.choice_t,
        choice_h = rules.preprocess.output.choice_h,
    params:
        level_order_heat = config["level-order"]["heat"],
        level_order_transport = config["level-order"]["transport"]
    output:
        subgroups_final = "build/figures-and-tables/subgroup-analysis/responsibility/interaction citizen responsibility (transport).png",
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/subgroups.R"


rule analyse_main_effect:
    message: "Analyse {wildcards.estimate} in sector {wildcards.sector} based on {wildcards.measure}."
    input:
        data = "build/data/{measure}-{sector}.feather"
    params:
        alpha = 1 - config["confidence-level"]
    output:
        "build/paper/fit/{estimate}-{measure}-{sector}.csv"
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/conjoint.R"



rule analyse_subgroup:
    message: "Analyse {wildcards.estimate} in sector {wildcards.sector} based on {wildcards.measure} " +
             "by subgroup {wildcards.subgroup}."
    input:
        data = "build/data/{measure}-{sector}.feather",
        respondents = rules.preprocess.output.respondents,
    params:
        alpha = 1 - config["confidence-level"]
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
        by_order = None,
        attribute_order = config["attribute-order"],
        level_order = config["level-order"]
    output:
        plot = "build/paper/vega/{estimate}-{measure}.json"
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
        attribute_order = config["attribute-order"],
        level_order = config["level-order"]
    output:
        plot = "build/paper/vega/{estimate}-{measure}-by-{subgroup}.json"
    conda: "../envs/altair-dev.yaml"
    script: "../scripts/analyse/level_plot.py"


rule render_vega_lite_to_pdf:
    message: "Render Vega Lite spec {wildcards.filename}.json to pdf."
    input:
        json = "build/paper/vega/{filename}.json"
    output:
        pdf = "build/paper/{filename}.pdf"
    conda: "../envs/vega.yaml"
    # vl2pdf not usable because of https://github.com/queryverse/VegaLite.jl/issues/383
    shell: "vl2vg {input.json} | vg2pdf > {output.pdf}"


rule render_vega_lite_to_png:
    message: "Render Vega Lite spec {wildcards.filename}.json to png."
    input:
        json = "build/paper/vega/{filename}.json"
    output:
        png = "build/paper/{filename}.png"
    conda: "../envs/vega.yaml"
    # vl2png not usable because of https://github.com/queryverse/VegaLite.jl/issues/383
    shell: "vl2vg {input.json} | vg2png --scale 4.167 > {output.png}" # scale to ~300dpi
