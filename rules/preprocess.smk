rule preprocess:
    message: "Preprocess the survey data."
    input:
        data = "data/surveydata-2022-03-19.xlsx"
    output:
        d = "build/data/d.feather",
        first_sector = "build/data/first_sector.feather",
        framing = "build/data/framing.feather",
        resp_char = "build/data/resp_char.feather",
        attitudes = "build/data/attitudes.feather",
        rating_t = "build/data/rating-transport.feather",
        rating_h = "build/data/rating-heat.feather",
        choice_t = "build/data/choice-transport.feather",
        choice_h = "build/data/choice-heat.feather",
        duration = "build/figures-and-tables/checks/Histogram duration.png",
    conda: "../envs/r.yaml"
    script: "../scripts/preprocess/preprocess.R"
