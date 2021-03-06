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
        "data/virus.csv",
        "data/animal-possession.csv",
        "data/animal-sale.csv",
    shell:
        "Rscript data/data.R"

rule data_plot:
    input:
        ".deps-installed",
        "data-plot/data-plot.R",
        "data/subject.csv",
        "data/titre.csv",
        "data/animal-possession.csv",
        "data/animal-sale.csv",
        "data/read_data.R",
    output:
        "data-plot/age-gender-hist.pdf",
        "data-plot/titre.pdf",
        "data-plot/titre-multiple-years.pdf",
        "data-plot/animal-possession.pdf",
        "data-plot/animal-sale.pdf",
        "data-plot/titre-summary.pdf",
        "data-plot/titre-summary-by-haem.pdf",
        "data-plot/slaughter.pdf",
    shell:
        "Rscript data-plot/data-plot.R"

rule data_table:
    input:
        ".deps-installed",
        "data-table/data-table.R",
        "data/subject.csv",
        "data/titre.csv",
        "data/animal-possession.csv",
        "data/animal-sale.csv",
        "data/read_data.R",
    output:
        "data-table/subject.csv",
        "data-table/titre.csv",
        "data-table/animal-possession.csv",
        "data-table/animal-sale.csv",
    shell:
        "Rscript data-table/data-table.R"

rule cluster:
    input:
        ".deps-installed",
        "cluster/cluster.R",
        "data/titre.csv",
        "data/virus.csv",
    output:
        "cluster/clusters.csv",
        "cluster/diag.csv",
        "cluster/parameters.csv",
    shell:
        "Rscript cluster/cluster.R"

rule cluster_summary:
    input:
        ".deps-installed",
        "cluster/summary.R",
        "cluster/parameters.csv",
    output:
        directory("cluster/trace-plots")
    shell:
        "Rscript cluster/summary.R"

rule zip:
    input:
        rules.data.output,
        rules.data_plot.output,
        rules.data_table.output,
        rules.cluster.output,
        rules.cluster_summary.output,
    output:
        "cambodia.zip"
    shell:
        "zip -r cambodia.zip . -x 'renv/library*' '.snakemake*' '.deps-installed'"

rule all:
    input:
        rules.zip.output
