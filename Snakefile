from snakemake.utils import min_version
PANDOC = "pandoc --filter pantable --filter pandoc-crossref --citeproc"

configfile: "config/default.yaml"

include: "rules/preprocess.smk"
include: "rules/analyse.smk"
min_version("7.8")
wildcard_constraints:
    measure = "choice|rating",
    sector = "heat|transport",
    estimate = "mm|amce"


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
        "build/results/amce-choice.png",
        "build/results/mm-choice-by-cceval_cat.png",
        "build/results/mm-rating-by-relevance_cat.png",
        "build/results/ratings-by-concern_and_understanding.png",
        "build/results/fit/amce-choice-heat-by-cceval_cat.csv",
        "build/results/fit/amce-choice-transport-by-cceval_cat.csv",


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
        "build/results/mm-choice-by-First.png",
        "build/results/mm-choice-by-choiceNum.png",
        "build/results/mm-choice-by-packNum.png",
        "build/results/amce-rating.png",
        "build/results/mm-choice-by-speeders.png",
        "build/results/choice-experimental-design.png",
        "build/results/mm-choice-by-relevance_cat.png",
        "build/results/mm-rating-by-cceval_cat.png",
        "build/results/sample-vs-population.csv",
        expand("build/results/concern-and-understanding-shares-{sector}.png", sector=["heat", "transport"]),
        "build/results/mm-choice-by-age.png",
        "build/results/mm-choice-by-ownership.png",
        "build/results/mm-choice-by-cardays_cat.png",
        "build/results/mm-rating-by-trust_gov_cat.png",
        "build/results/fit/amce-choice-heat-publication.csv",
        "build/results/fit/amce-choice-transport-publication.csv",
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
        -o ../build/supplementary.{wildcards.suffix}
        """


rule push:
    message: "Package, zip, and move entire build."
    params: push_directory = config["push-directory"]
    shell:
        """
        zip -r {params.push_directory}/$(date -Idate).zip build
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
