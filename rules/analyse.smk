rule plot_emissions:
    message: "Create plot of historical emissions."
    input:
        data = "data/historic-co2/co2-in-mt.csv"
    output: "build/emissions.png"
    conda: "../envs/default.yaml"
    script: "../scripts/analyse/emissions.py"


rule compare_to_population:
    message: "Compare sample to population."
    input:
        sample = rules.preprocess.output.respondents,
        population = rules.census_data.output.csv
    output:
        "build/results/sample-vs-population.csv"
    conda:
        "../envs/default.yaml"
    script:
        "../scripts/analyse/sample.py"


rule analyse_main_effect:
    message: "Analyse {wildcards.estimate} in sector {wildcards.sector} based on {wildcards.measure}."
    input:
        data = "build/data/{measure}-{sector}.feather",
        respondents = rules.preprocess.output.respondents
    params:
        alpha = 1 - config["confidence-level"]
    output:
        "build/results/fit/{estimate}-{measure}-{sector}.csv"
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
        "build/results/fit/{estimate}-{measure}-{sector}-by-{subgroup}.csv"
    conda: "../envs/r.yaml"
    script: "../scripts/analyse/conjoint.R"


rule analyse_experimental_design:
    message: "Analyse experimental design in sector {wildcards.sector} based on {wildcards.measure}."
    input:
        data = "build/data/{measure}-{sector}.feather",
        respondents = rules.preprocess.output.respondents
    params:
        attributes = config["attribute-order"]
    output:
        freqs = "build/design/{measure}-{sector}.csv"
    conda:
        "../envs/default.yaml"
    script:
        "../scripts/analyse/freqs.py"


rule visualise_main_effects:
    message: "Visualise main results for measure {wildcards.measure} and estimate {wildcards.estimate}."
    input:
        heat = "build/results/fit/{estimate}-{measure}-heat.csv",
        transport = "build/results/fit/{estimate}-{measure}-transport.csv"
    params:
        by = None,
        by_order = None,
        attribute_order = config["attribute-order"],
        level_order = config["level-order"]
    output:
        plot = "build/results/vega/{estimate}-{measure}.json"
    conda: "../envs/default.yaml"
    script: "../scripts/analyse/level_plot.py"


rule visualise_subgroup:
    message: "Visualise {wildcards.subgroup} for measure {wildcards.measure} and estimate {wildcards.estimate}."
    input:
        heat = "build/results/fit/{estimate}-{measure}-heat-by-{subgroup}.csv",
        transport = "build/results/fit/{estimate}-{measure}-transport-by-{subgroup}.csv",
    params:
        by = lambda wildcards, output: config["subgroups"][wildcards.subgroup]["name"],
        by_order = lambda wildcards, output: config["subgroups"][wildcards.subgroup]["level-order"],
        attribute_order = config["attribute-order"],
        level_order = config["level-order"]
    output:
        plot = "build/results/vega/{estimate}-{measure}-by-{subgroup}.json"
    conda: "../envs/default.yaml"
    script: "../scripts/analyse/level_plot.py"


rule visualise_experimental_design:
    message: "Visual experimental design for measure {wildcards.measure}."
    input:
        heat = "build/design/{measure}-heat.csv",
        transport = "build/design/{measure}-transport.csv",
    params:
        level_order = config["level-order"]
    output:
        plot = "build/results/vega/{measure}-experimental-design.json"
    conda:
        "../envs/default.yaml"
    script:
        "../scripts/analyse/design_plot.py"


rule visualise_ratings:
    message: "Visualise average ratings by climate concern and attributed emissions."
    input:
        heat = "build/results/fit/mm-rating-heat-by-concern_and_understanding.csv",
        transport = "build/results/fit/mm-rating-transport-by-concern_and_understanding.csv"
    output:
        plot = "build/results/vega/ratings-by-concern_and_understanding.json"
    conda:
        "../envs/default.yaml"
    script:
        "../scripts/analyse/ratings_plot.py"


rule visualise_concern_and_understanding:
    message: "Visualise the shares of climate concern and attributed emissions for sector {wildcards.sector}."
    input:
        respondents = rules.preprocess.output.respondents
    output:
        plot = "build/results/concern-and-understanding-shares-{sector}.png"
    conda:
        "../envs/mosaic.yaml"
    script:
        "../scripts/analyse/mosaic.R"


rule render_vega_lite_to_pdf:
    message: "Render Vega Lite spec {wildcards.filename}.json to pdf."
    input:
        json = "build/results/vega/{filename}.json"
    output:
        pdf = "build/results/{filename}.pdf"
    conda: "../envs/vega.yaml"
    # vl2pdf not usable because of https://github.com/queryverse/VegaLite.jl/issues/383
    shell: "vl2vg {input.json} | vg2pdf > {output.pdf}"


rule render_vega_lite_to_png:
    message: "Render Vega Lite spec {wildcards.filename}.json to png."
    input:
        json = "build/results/vega/{filename}.json"
    output:
        png = "build/results/{filename}.png"
    conda: "../envs/vega.yaml"
    # vl2png not usable because of https://github.com/queryverse/VegaLite.jl/issues/383
    shell: "vl2vg {input.json} | vg2png --scale 4.167 > {output.png}" # scale to ~300dpi
