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
        "build/report.html",
        "build/test-report.html",
        "build/emissions.png",
        "build/paper/mm-choice.pdf",
        "build/paper/mm-rating-by-cceval_cat.pdf",
        rules.sample.output[0],
        rules.framing.output[0],
        rules.marginal_means.output[0],
        rules.amce.output[0],
        rules.robustness.output[0],
        rules.subgroups.output[0],


def pandoc_options(wildcards):
    suffix = wildcards["suffix"]
    if suffix == "html":
        return "--self-contained --to html5"
    elif suffix == "pdf":
        return "--pdf-engine weasyprint"
    elif suffix == "docx":
        return []
    else:
        raise ValueError(f"Cannot create report with suffix {suffix}.")


rule report:
    message: "Compile report.{wildcards.suffix}."
    input:
        "report/literature.yaml",
        "report/report.md",
        "report/pandoc-metadata.yaml",
        "report/apa.csl",
        "report/reset.css",
        "report/report.css"
    params: options = pandoc_options
    output: "build/report.{suffix}"
    wildcard_constraints:
        suffix = "((html)|(pdf)|(docx))"
    conda: "envs/report.yaml"
    shadow: "minimal"
    shell:
        """
        cd report
        ln -s ../build .
        {PANDOC} report.md  --metadata-file=pandoc-metadata.yaml {params.options} \
        -o ../build/report.{wildcards.suffix}
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
