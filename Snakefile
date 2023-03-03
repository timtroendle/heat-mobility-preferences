from snakemake.utils import min_version
PANDOC = "pandoc --filter pantable --filter pandoc-fignos --filter pandoc-tablenos --citeproc"

configfile: "config/default.yaml"

include: "rules/preprocess.smk"
include: "rules/analyse.smk"
min_version("7.8")
wildcard_constraints:
    measure = "choice|rating",
    sector = "heat|transport",
    estimate = "mm|amce"


onstart:
    shell("mkdir -p build/figures-and-tables/checks/distributions-subgroups")
    shell("mkdir -p build/figures-and-tables/descriptive-statistics")
    shell("mkdir -p build/figures-and-tables/attitudes")
    shell("mkdir -p build/figures-and-tables/framing-interaction")
    shell("mkdir -p build/figures-and-tables/marginal-means")
    shell("mkdir -p build/figures-and-tables/AMCE")
    shell("mkdir -p build/figures-and-tables/checks/robustness-checks")
    shell("mkdir -p build/figures-and-tables/subgroup-analysis")
    shell("mkdir -p build/figures-and-tables/subgroup-analysis/heating-and-transport-variables")
    shell("mkdir -p build/figures-and-tables/subgroup-analysis/climate-change-evaluation")
    shell("mkdir -p build/figures-and-tables/subgroup-analysis/trust")
    shell("mkdir -p build/figures-and-tables/subgroup-analysis/responsibility")
onsuccess:
    if "email" in config.keys():
        shell("echo "" | mail -s 'heat-mobility-preferences succeeded' {config[email]}")
onerror:
    if "email" in config.keys():
        shell("echo "" | mail -s 'heat-mobility-preferences failed' {config[email]}")


rule all:
    message: "Run entire analysis and compile report."
    input:
        "build/supplementary.pdf",
        "build/test-report.html",
        "build/emissions.png",
        "build/paper/mm-choice.pdf",
        "build/paper/mm-rating.pdf",
        "build/paper/amce-choice.pdf",
        "build/paper/amce-rating.pdf",
        "build/paper/mm-choice-by-cceval_cat.pdf",
        "build/paper/mm-choice-by-relevance_cat.pdf",
        "build/paper/mm-rating-by-cceval_cat.pdf",
        "build/paper/mm-rating-by-relevance_cat.pdf",
        "build/paper/amce-rating-by-cceval_cat.pdf",
        rules.sample.output[0],
        rules.framing.output[0],
        rules.robustness.output[0],
        rules.subgroups.output[0],


def pandoc_options(wildcards):
    suffix = wildcards["suffix"]
    if suffix == "html":
        return "--embed-resources --standalone --to html5"
    elif suffix == "pdf":
        return "--pdf-engine weasyprint"
    elif suffix == "docx":
        return []
    else:
        raise ValueError(f"Cannot create report with suffix {suffix}.")


rule supplementary:
    message: "Compile supplementary.{wildcards.suffix}."
    input:
        "report/literature.yaml",
        "report/supplementary.md",
        "report/pandoc-metadata.yaml",
        "report/apa.csl",
        "report/reset.css",
        "report/supplementary.css",
        "report/fonts/KlinicSlabBook.otf",
        "report/fonts/KlinicSlabBookIt.otf",
        "report/fonts/KlinicSlabMedium.otf",
        "report/fonts/KlinicSlabMediumIt.otf",
        "build/paper/amce-choice-by-First.png",
        "build/paper/amce-choice-by-choiceNum.png",
        "build/paper/amce-choice-by-packNum.png",
        "build/paper/amce-choice-by-speeders.png",
        "build/paper/choice-experimental-design.png"
    params: options = pandoc_options
    output: "build/supplementary.{suffix}"
    wildcard_constraints:
        suffix = "((html)|(pdf)|(docx))"
    conda: "envs/report.yaml"
    shadow: "minimal"
    shell:
        """
        cd report
        ln -s ../build .
        {PANDOC} supplementary.md  --metadata-file=pandoc-metadata.yaml {params.options} \
        -f markdown-implicit_figures \
        -o ../build/supplementary.{wildcards.suffix}
        """


rule dag:
     message: "Plot dependency graph of the workflow."
     output:
         dot = "build/dag.dot",
         pdf = "build/dag.pdf"
     conda: "envs/dag.yaml"
     shell:
         """
         snakemake --rulegraph > {output.dot}
         dot -Tpdf -o {output.pdf} {output.dot}
         """


rule clean: # removes all generated results
    message: "Remove all build results but keep downloaded data."
    run:
         import shutil

         shutil.rmtree("build")
         print("Data downloaded to data/ has not been cleaned.")


rule test:
    conda: "envs/test.yaml"
    output: "build/test-report.html"
    shell:
        "py.test --html={output} --self-contained-html"
