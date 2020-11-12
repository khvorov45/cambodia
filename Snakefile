rule install_deps:
    input:
        "renv.lock"
    output:
        touch(".deps-installed")
    shell:
        """Rscript -e 'renv::restore()'"""

rule data:
    input:
        ".deps-installed",
        "data/data.R",
        "data-raw/responses-2015.dta",
        "data-raw/responses-2017.dta",
        "data-raw/serology.xlsx",
    output:
        "data/subject.csv",
        "data/titre.csv",
        "data/animal-possession.csv",
    shell:
        "Rscript data/data.R"

rule data_plot:
    input:
        ".deps-installed",
        "data-plot/data-plot.R",
        "data/subject.csv",
        "data/read_data.R",
    output:
        "data-plot/age-hist.pdf",
        "data-plot/titre.pdf",
        "data-plot/titre-multiple-years.pdf",
    shell:
        "Rscript data-plot/data-plot.R"

rule data_table:
    input:
        ".deps-installed",
        "data-table/data-table.R",
        "data/subject.csv",
        "data/read_data.R",
    output:
        "data-table/subject.csv",
        "data-table/titre.csv",
    shell:
        "Rscript data-table/data-table.R"

rule zip:
    input:
        rules.data.output,
        rules.data_plot.output,
        rules.data_table.output,
    output:
        "cambodia.zip"
    shell:
        "zip -r cambodia.zip . -x 'renv/library*' '.snakemake*' '.deps-installed'"

rule all:
    input:
        rules.zip.output
