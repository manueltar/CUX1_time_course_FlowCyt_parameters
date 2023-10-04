#!/bin/bash

eval "$(conda shell.bash hook)"



Rscripts_path=$(echo "/home/manuel.tardaguila/Scripts/R/")

MASTER_ROUTE=$1
analysis=$2

Master_path_analysis=$(echo "$MASTER_ROUTE""$analysis""/")

rm -rf $Master_path_analysis
mkdir -p $Master_path_analysis



Log_files=$(echo "$Master_path_analysis""/""Log_files/")

rm -rf $Log_files
mkdir -p $Log_files



#### data_wrangling ####

module load R/4.1.0

type=$(echo "data_wrangling""_""$analysis")
outfile_data_wrangling=$(echo "$Log_files""outfile_""$type"".log")
touch $outfile_data_wrangling
echo -n "" > $outfile_data_wrangling
name_data_wrangling=$(echo "$type""_job")


Rscript_data_wrangling=$(echo "$Rscripts_path""16_FlowCyt_parameters_v2.R")


FlowCyt_results=$(echo "/group/soranzo/manuel.tardaguila/FlowCyt_parameters/""FlowCyt_global.csv")
selection_samples=$(echo "WT_A,WT_B,WT_C,clone_13,clone_27,clone_29,del_233,del_235,del_287")
selection_treatment=$(echo "5nM_PMA")
selection_time_point=$(echo "Basal,16_hrs,24_hrs,48_hrs,72_hrs")



myjobid_data_wrangling=$(sbatch --job-name=$name_data_wrangling --output=$outfile_data_wrangling --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=512M --parsable --wrap="Rscript $Rscript_data_wrangling --FlowCyt_results $FlowCyt_results --Master_path_analysis $Master_path_analysis --selection_samples $selection_samples --selection_treatment $selection_treatment --selection_time_point $selection_time_point --type $type --out $Master_path_analysis")
myjobid_seff_data_wrangling=$(sbatch --dependency=afterany:$myjobid_data_wrangling --open-mode=append --output=$outfile_data_wrangling --job-name="seff" --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_data_wrangling >> $outfile_data_wrangling")

#### graphs ####

module load R/4.1.0

type=$(echo "graphs""_""$analysis")
outfile_graphs=$(echo "$Log_files""outfile_""$type"".log")
touch $outfile_graphs
echo -n "" > $outfile_graphs
name_graphs=$(echo "$type""_job")


Rscript_graphs=$(echo "$Rscripts_path""17_FlowCyt_graphs_v2.R")


FlowCyt_results=$(echo "$Master_path_analysis""FlowCyt_global_adapted.rds")
FlowCyt_subpopulation_selected=$(echo "Double_pos,CD41_single")
#time_point_selected=$(echo "48_hrs,72_hrs")
time_point_selected=$(echo "72_hrs")
FlowCyt_results_selected=$(echo "$Master_path_analysis""Mean_FSC__Mean_SSC_data_frame.rds")

myjobid_graphs=$(sbatch --dependency=afterany:$myjobid_data_wrangling --job-name=$name_graphs --output=$outfile_graphs --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_graphs --FlowCyt_subpopulation_selected $FlowCyt_subpopulation_selected --time_point_selected $time_point_selected --FlowCyt_results $FlowCyt_results --FlowCyt_results_selected $FlowCyt_results_selected --type $type --out $Master_path_analysis")
myjobid_seff_graphs=$(sbatch --dependency=afterany:$myjobid_graphs --open-mode=append --output=$outfile_graphs --job-name="seff" --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_graphs >> $outfile_graphs")

#myjobid_graphs=$(sbatch --job-name=$name_graphs --output=$outfile_graphs --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_graphs --FlowCyt_subpopulation_selected $FlowCyt_subpopulation_selected --time_point_selected $time_point_selected --FlowCyt_results $FlowCyt_results --FlowCyt_results_selected $FlowCyt_results_selected --type $type --out $Master_path_analysis")
#myjobid_seff_graphs=$(sbatch --dependency=afterany:$myjobid_graphs --open-mode=append --output=$outfile_graphs --job-name="seff" --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_graphs >> $outfile_graphs")


#### model ####

module load R/4.1.0

type=$(echo "FSC_SSC_model""_""$analysis")
outfile_FSC_SSC_model=$(echo "$Log_files""outfile_""$type"".log")
touch $outfile_FSC_SSC_model
echo -n "" > $outfile_FSC_SSC_model
name_FSC_SSC_model=$(echo "$type""_job")


Rscript_FSC_SSC_model=$(echo "$Rscripts_path""18_FSC_SSC_model.R")


FlowCyt_results_selected=$(echo "$Master_path_analysis""Double_pos__CD41_single_72_hrs_data_frame.rds")


myjobid_FSC_SSC_model=$(sbatch --dependency=afterany:$myjobid_graphs --job-name=$name_FSC_SSC_model --output=$outfile_FSC_SSC_model --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_FSC_SSC_model --FlowCyt_results_selected $FlowCyt_results_selected --type $type --out $Master_path_analysis")
myjobid_seff_FSC_SSC_model=$(sbatch --dependency=afterany:$myjobid_FSC_SSC_model --open-mode=append --output=$outfile_FSC_SSC_model --job-name="seff" --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_FSC_SSC_model >> $outfile_FSC_SSC_model")

#myjobid_FSC_SSC_model=$(sbatch --job-name=$name_FSC_SSC_model --output=$outfile_FSC_SSC_model --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_FSC_SSC_model --FlowCyt_results_selected $FlowCyt_results_selected --type $type --out $Master_path_analysis")
#myjobid_seff_FSC_SSC_model=$(sbatch --dependency=afterany:$myjobid_FSC_SSC_model --open-mode=append --output=$outfile_FSC_SSC_model --job-name="seff" --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_FSC_SSC_model >> $outfile_FSC_SSC_model")

