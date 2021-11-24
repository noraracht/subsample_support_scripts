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
  - [combined_all_dist_removed_outliers_threeD.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/combined_all_dist_removed_outliers_threeD.csv) and [combined_all_dist_removed_outliers_threeD_16x.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/combined_all_dist_removed_outliers_threeD_16x.csv) contains distance estimates for variable subsample coverage when original sample is either at 1x or 16x correspondingly. It is an input for [cherry_var_subsample_sz.R](https://github.com/noraracht/subsample_support_scripts/blob/main/cherry_var_subsample_sz.R) to generate plots depicted in Fig. 3c and S3. 
  
   - [combined_all_dist_new.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/combined_all_dist_new.csv) includes distance estimates at variable true distance. The table serves as an input into [cherry_var_distances.R](https://github.com/noraracht/subsample_support_scripts/blob/main/cherry_var_distances.R) to generate bar and line plots shown in Fig. 3ab. 
    
    
* Felsenstein-zone simulations
   - [constensus6all.txt](https://github.com/noraracht/subsample_support_scripts/blob/main/constensus6all.txt), [500reps_try6all](https://github.com/noraracht/subsample_support_scripts/blob/main/500reps_try6all) and [brlen_support.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/brlen_support.csv) hold support and branch length information for Felsentein tree experiment. These files serves as an input into [fel_trees_summary_v2.R](https://github.com/noraracht/subsample_support_scripts/blob/main/fel_trees_summary_v2.R) to generate curves demonstrated in Fig. 4abc, S1 and S6a.


* Eight taxa simulations
   - [8leaves.txt](https://github.com/noraracht/subsample_support_scripts/blob/main/8leaves.txt), [8leaves_CONS.txt](https://github.com/noraracht/subsample_support_scripts/blob/main/8leaves_CONS.txt), [original_branch_length_bal.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/original_branch_length_bal.csv) and [original_branch_length_cat.csv](https://github.com/noraracht/subsample_support_scripts/blob/main/original_branch_length_cat.csv) contain support and branch length data for 8 taxa tree simulations. These files serves as an input into [fel_trees_summary_v2_8taxa.R](https://github.com/noraracht/subsample_support_scripts/blob/main/fel_trees_summary_v2_8taxa.R) to generate curves demonstrated in Fig. 4abc, S2, S6b, and S7.
     

* Phylogenetic inference on real data
   - [script_dros_heatmap_v1.R](https://github.com/noraracht/lsh_scripts/blob/main/script_dros_heatmap_v1.R) takes an as input [dros_distances_updatedGTDB4.csv](https://github.com/noraracht/lsh_scripts/blob/main/dros_distances_updatedGTDB4.csv) that contains distance values and relative error information (ground truth, before and after filtering) for all combinations of Drosophila species and [lsh_gtdb_results_dros_queries.csv](https://github.com/noraracht/lsh_scripts/blob/main/lsh_gtdb_results_dros_queries.csv) that lists percentage of reads that have been filtered out with GTDB. This script generates plots on Fig. S8.
   
   - [script_dros_heatmap_arrows_v2.R](https://github.com/noraracht/lsh_scripts/blob/main/script_dros_heatmap_arrows_v2.R) takes an as input [dros_distances_updatedGTDB4.csv](https://github.com/noraracht/lsh_scripts/blob/main/dros_distances_updatedGTDB4.csv) to generate arrow plot.
   - [combined_gorg_v1_results_custtax_lineplot.csv](https://github.com/noraracht/lsh_scripts/blob/main/combined_gorg_v1_results_custtax_lineplot.csv) contains percentages of matched reads for every GORG sample searched with default parameters against GTDB, TOL and Bac/Arch Kraken databases for Kraken and CONSULT tools. It is an input for [gorg.R](https://github.com/noraracht/lsh_scripts/blob/main/gorg.R) to generate ECDF curves on Fig. 3b.


* Workflow diagrams
   <!--- Scirpt to generate theoretical model in Fig. 1b. and Fig. S1.-->
   - [workflow_v1.pptx](https://github.com/noraracht/subsample_support_scripts/blob/main/workflow_v1.pptx) is the sketch of the general subsample workflow displayed on Fig. 2a.
   - [sketch_pic_v2_vertical_larger.pptx](https://github.com/noraracht/subsample_support_scripts/blob/main/sketch_pic_v2_vertical_larger.pptx) represents stepd of Felsenstein-zone expriment shown in Fig. 2b.


    
