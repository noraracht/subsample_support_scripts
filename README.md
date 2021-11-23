# Access to the data used for becnhmarking subsample support

This repository contains raw data and output reports we used for the study.

## Raw data
* Cherry tree simulations:
    - [d=0.01](https://tera-trees.com/data/consult/v1.0.0/d0.01_100sims.tar.gz)
    - [d=0.02](https://tera-trees.com/data/consult/v1.0.0/d0.02_100sims.tar.gz)
    - [d=0.05](https://tera-trees.com/data/consult/v1.0.0/d0.05_100sims.tar.gz)
    - [d=0.10](https://tera-trees.com/data/consult/v1.0.0/d0.10_100sims.tar.gz)
    - [d=0.15](https://tera-trees.com/data/consult/v1.0.0/d0.15_100sims.tar.gz)
    - [d=0.25](https://tera-trees.com/data/consult/v1.0.0/d0.25_100sims.tar.gz)
    
* Felsenstein-zone quartet trees simulations (upload in progress):
    - [1-500 replicates]()
    - [501-1000 replicates]()
    

* Eight taxa simulations:
    - [balanced topologies](https://tera-trees.com/data/consult/v1.0.0/balanced_unif_0.00001.tar.gz)
    - [unbalanced (caterpillar) topologies](https://tera-trees.com/data/consult/v1.0.0/caterpillar_unif_0.00001.tar.gz)

* Real data analysis:
    * Whale:
        - [Assemblies](https://tera-trees.com/data/consult/v1.0.0/whales_asm.tar.gz)
        - [Sequencing data](https://tera-trees.com/data/consult/v1.0.0/whales_new_analysis.tar.gz)
    * Bee (upload in progress):
        - [Assemblies]()
        - [1x sequencing data]()
        - [2x sequencing data]()
        - [4x sequencing data]()
        - [8x sequencing data]()
    * Drosophila (upload in progress):
        - [Assemblies]()
        - [Sequencing data]()
    * Lice (upload in progress):
        - [Sequencing data]()




## Summary data

<!---This section contains summary data tables and scripts we used to processes them.--->


* Controlled distances
  - [Tol_all_kmers_bin_comp_lsh_kraken_clark_clarkS_0914_customTax.xlsx](https://github.com/noraracht/lsh_scripts/blob/main/Tol_all_kmers_bin_comp_lsh_kraken_clark_clarkS_0914_customTax.xlsx) contains recall and false positive information computed per bin for all CONSULT, Kraken-II, CLARK, CLARK-S and Bowtie. It is an input for [Tol_allkmers_bin_cmp_030821.R script](https://github.com/noraracht/lsh_scripts/blob/main/Tol_allkmers_bin_cmp_030721.R) to generate curves depicted in Fig. 2a. 
  
   - [tools_runtime_1030_in_paper.xlsx](https://github.com/noraracht/lsh_scripts/blob/main/tools_runtime_1030_in_paper.xlsx) includes running time and memory data for CONSULT, Kraken-II, CLARK, CLARK-S and Bowtie used for performance comparison of the tools. The table serves as an input into [tools_runtime_1014.R](https://github.com/noraracht/lsh_scripts/blob/main/tools_runtime_1014.R) to generate bar plots shown in Fig. 2b. 
    
    
* Novel genomes
   - [roc_p3c0.csv](https://github.com/noraracht/lsh_scripts/blob/main/roc_p3c0.csv), [roc_p4c0.csv](https://github.com/noraracht/lsh_scripts/blob/main/roc_p4c0.csv) and [roc_p4c1.csv](https://github.com/noraracht/lsh_scripts/blob/main/roc_p4c1.csv) hold mean recall and false positive values for GORG dataset queried against  GTDB, TOL, Bac/Arch Kraken databases at variable settings. These files serves as an input into [combo_ROC.R](https://github.com/noraracht/lsh_scripts/blob/main/combo_ROC.R) to generate ROC curves demonstrated in Fig. 3a.
     
   - [combined_gorg_v1_results_custtax_lineplot.csv](https://github.com/noraracht/lsh_scripts/blob/main/combined_gorg_v1_results_custtax_lineplot.csv) contains percentages of matched reads for every GORG sample searched with default parameters against GTDB, TOL and Bac/Arch Kraken databases for Kraken and CONSULT tools. It is an input for [gorg.R](https://github.com/noraracht/lsh_scripts/blob/main/gorg.R) to generate ECDF curves on Fig. 3b.
    
