# Access to the data used for becnhmarking subsample support

This repository contains raw data and output reports we used for the study.

## Raw data
* Cherry tree simulations:
    <!--- - [d=0.01](https://tera-trees.com/data/consult/v1.0.0/d0.01_100sims.tar.gz)--->
    - [d=0.01](https://drive.google.com/file/d/1LlZXDjqWcwoqcYxaa_Ywf8eNJzL9XQH0/view?usp=sharing)
    - [d=0.02](https://tera-trees.com/data/consult/v1.0.0/d0.02_100sims.tar.gz)
    - [d=0.05](https://tera-trees.com/data/consult/v1.0.0/d0.05_100sims.tar.gz)
    - [d=0.10](https://tera-trees.com/data/consult/v1.0.0/d0.10_100sims.tar.gz)
    - [d=0.15](https://tera-trees.com/data/consult/v1.0.0/d0.15_100sims.tar.gz)
    - [d=0.25](https://tera-trees.com/data/consult/v1.0.0/d0.25_100sims.tar.gz)
    
* Felsenstein-zone quartet trees simulations:
    - [1-500 replicates](https://tera-trees.com/data/consult/v1.0.0/variable_a_b.tar.gz)
    - [501-1000 replicates](https://tera-trees.com/data/consult/v1.0.0/variable_a_b_2.tar.gz)
    

* Eight taxa simulations:
    <!--- - [balanced topologies](https://tera-trees.com/data/consult/v1.0.0/balanced_unif_0.00001.tar.gz)--->
    <!--- - [unbalanced (caterpillar) topologies](https://tera-trees.com/data/consult/v1.0.0/caterpillar_unif_0.00001.tar.gz)--->
    - [balanced topologies](https://drive.google.com/file/d/1mZmYwBrYyK9mckxvWJo1dcasTQNZuuS1/view?usp=sharing)
    - [unbalanced (caterpillar) topologies](https://drive.google.com/file/d/1gaQqiW8WFKN-pgDQQ5sjtgIsOQoXqoXl/view?usp=sharing)

* Real data phylogenetic analysis:
    * Whale:
        - [Assemblies](https://tera-trees.com/data/consult/v1.0.0/whales_asm.tar.gz)
        - [Sequencing data](https://tera-trees.com/data/consult/v1.0.0/whales_new_analysis.tar.gz)
    * Bee:
        - [Assemblies](https://tera-trees.com/data/consult/v1.0.0/asm.tar.gz)
        - [1x sequencing data](https://tera-trees.com/data/consult/v1.0.0/1xArt_from_asm_subsample_reads_9t10_finalver.tar.gz)
        - [2x sequencing data](https://tera-trees.com/data/consult/v1.0.0/2xArt_from_asm_subsample_reads_9t10.tar.gz)
        - [4x sequencing data](https://tera-trees.com/data/consult/v1.0.0/4xArt_from_asm_subsample_reads_9t10.tar.gz)
        - [8x sequencing data](https://tera-trees.com/data/consult/v1.0.0/8xArt_from_asm_subsample_reads_9t10.tar.gz)
    * Drosophila:
        - [Assemblies](https://tera-trees.com/data/consult/v1.0.0/assembledGenomes.tar.gz)
        - [Sequencing data](https://tera-trees.com/data/consult/v1.0.0/consult_gtdb_p3c1.tar.gz)
    * Lice:
        - [Sequencing data](https://tera-trees.com/data/consult/v1.0.0/after_merge.tar.gz)
        
* Resample experiment:
    - [Drosophila sequencing data](https://tera-trees.com/data/consult/v1.0.0/resample_reads.tar.gz)




## Summary data

<!---This section contains summary data tables and scripts we used to processes them.--->


* Cherry trees simulations
  - [combined_all_dist_removed_outliers_threeD.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/combined_all_dist_removed_outliers_threeD.csv) and  [combined_all_dist_removed_outliers_threeD_16x.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/combined_all_dist_removed_outliers_threeD_16x.csv) contains distance estimates for variable subsample coverage when original sample is either at 1x or 16x correspondingly. It is an input for [cherry_var_subsample_sz.R](https://github.com/noraracht/subsample_support_scripts/blob/main/cherry_var_subsample_sz.R) to generate plots depicted in Fig. 3c and S3. 
  
   - [combined_all_dist_new.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/combined_all_dist_new.csv) includes distance estimates at variable true distance. The table serves as an input into [cherry_var_distances.R](https://github.com/noraracht/subsample_support_scripts/blob/main/cherry_var_distances.R) to generate bar and line plots shown in Fig. 3ab. 
    
    
* Felsenstein-zone simulations
   - [roc_p3c0.csv](https://github.com/noraracht/lsh_scripts/blob/main/roc_p3c0.csv), [roc_p4c0.csv](https://github.com/noraracht/lsh_scripts/blob/main/roc_p4c0.csv) and [roc_p4c1.csv](https://github.com/noraracht/lsh_scripts/blob/main/roc_p4c1.csv) hold mean recall and false positive values for GORG dataset queried against  GTDB, TOL, Bac/Arch Kraken databases at variable settings. These files serves as an input into [combo_ROC.R](https://github.com/noraracht/lsh_scripts/blob/main/combo_ROC.R) to generate ROC curves demonstrated in Fig. 3a.
     
   - [combined_gorg_v1_results_custtax_lineplot.csv](https://github.com/noraracht/lsh_scripts/blob/main/combined_gorg_v1_results_custtax_lineplot.csv) contains percentages of matched reads for every GORG sample searched with default parameters against GTDB, TOL and Bac/Arch Kraken databases for Kraken and CONSULT tools. It is an input for [gorg.R](https://github.com/noraracht/lsh_scripts/blob/main/gorg.R) to generate ECDF curves on Fig. 3b.
    
