
suppressMessages(library("plyr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("data.table", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("crayon", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggplot2", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("farver", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("labeling", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("optparse", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("dplyr", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("backports", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("broom", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rstudioapi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cli", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tzdb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggeasy", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("sandwich", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("digest", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tidyverse", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("RColorBrewer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("org.Hs.eg.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("ggforce", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)


data_wrangling = function(option_list)
{

  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### Master_path_analysis----
  
  Master_path_analysis = opt$Master_path_analysis
  
  cat("OUT_\n")
  cat(sprintf(as.character(Master_path_analysis)))
  cat("\n")
  
  #### READ and transform selection_samples ----
  
  selection_samples = unlist(strsplit(opt$selection_samples, split=','))
  
  cat("selection_samples_\n")
  cat(sprintf(as.character(selection_samples)))
  cat("\n")
  
  #### READ and transform selection_treatment ----
  
  selection_treatment = unlist(strsplit(opt$selection_treatment, split=','))
  
  cat("selection_treatment_\n")
  cat(sprintf(as.character(selection_treatment)))
  cat("\n")
  
  #### READ and transform selection_time_point ----
  
  selection_time_point = unlist(strsplit(opt$selection_time_point, split=','))
  
  cat("selection_time_point_\n")
  cat(sprintf(as.character(selection_time_point)))
  cat("\n")
  
  #### Read FlowCyt_results----
  
  FlowCyt_results<-as.data.frame(fread(file=opt$FlowCyt_results, sep=",", header=T), stringsAsFactors=F)
  
  cat("FlowCyt_results_0\n")
  cat(str(FlowCyt_results))
  cat("\n")
  
  
  
  FlowCyt_results$sample<-factor(FlowCyt_results$sample,
                           levels=c('WT_A','WT_B','WT_C','clone_13','clone_27','clone_29','del_233','del_235','del_287'),
                           ordered=T)
  
  

  FlowCyt_results$Genotype<-'NA'
  
  indx.wt<-grep(paste(c("WT_A","WT_B","WT_C"),collapse="|"),FlowCyt_results$sample)
  FlowCyt_results$Genotype[indx.wt]<-'wt'
  
  indx.homALT<-grep(paste(c("clone_13","clone_27","clone_29"),collapse="|"),FlowCyt_results$sample)
  FlowCyt_results$Genotype[indx.homALT]<-'homALT'
  
  indx.Del80<-grep(paste(c("del_233","del_235","del_287"),collapse="|"),FlowCyt_results$sample)
  FlowCyt_results$Genotype[indx.Del80]<-'Del80'
 
  
  FlowCyt_results$Genotype<-factor(FlowCyt_results$Genotype,
                                   levels=c('wt','homALT','Del80'),
                                   ordered=T)
  
  
  cat("FlowCyt_results_1\n")
  cat(str(FlowCyt_results))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(FlowCyt_results$treatment))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(FlowCyt_results$treatment)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results$sample)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results$sample))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(FlowCyt_results$time_point))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(FlowCyt_results$time_point)))))
  cat("\n")
  
  #### Assign untreated + 16h to the Basal time point ----
  
 
  FlowCyt_results$time_point[which(FlowCyt_results$time_point == '16_hrs' &
                                     FlowCyt_results$treatment == 'untreated')]<-'Basal'
  
  FlowCyt_results$treatment[which(FlowCyt_results$time_point == 'Basal')]<-'5nM_PMA'
  
  FlowCyt_results$treatment<-factor(FlowCyt_results$treatment,
                                    levels=c('untreated','5nM_PMA'),
                                    ordered=T)
  
  FlowCyt_results$time_point<-factor(FlowCyt_results$time_point,
                               levels=c('Basal','16_hrs','24_hrs','48_hrs','72_hrs'),
                               ordered=T)
  
  cat("FlowCyt_results_2\n")
  cat(str(FlowCyt_results))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results$treatment)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results$treatment))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results$sample)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results$sample))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results$time_point)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results$time_point))))
  cat("\n")
  
  ##### Select specific conditions----
  
  FlowCyt_results_sel<-droplevels(FlowCyt_results[which(FlowCyt_results$treatment%in%selection_treatment &
                                                        FlowCyt_results$time_point%in%selection_time_point),])
  

  selection_sample_labels<-unique(FlowCyt_results_sel$FlowCyt_name)
  
  cat("FlowCyt_results_sel_0\n")
  cat(str(FlowCyt_results_sel))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_sel$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_sel$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_sel$treatment)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_sel$treatment))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_sel$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_sel$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_sel$sample)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_sel$sample))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_sel$time_point)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_sel$time_point))))
  cat("\n")
  cat("\n")
  cat("\n")
  cat(sprintf(as.character(selection_sample_labels)))
  cat("\n")
  
  
  design_df<-unique(FlowCyt_results_sel[,c(which(colnames(FlowCyt_results_sel) == 'sample'),
                                          which(colnames(FlowCyt_results_sel) == 'Genotype'),
                                          which(colnames(FlowCyt_results_sel) == 'treatment'),
                                          which(colnames(FlowCyt_results_sel) == 'time_point'),
                                          which(colnames(FlowCyt_results_sel) == 'FlowCyt_name'))])
  
  cat("design_df_0\n")
  cat(str(design_df))
  cat("\n")
  
  
  
  design_df<-design_df[order(design_df$Genotype,design_df$time_point),]
  
  
  cat("design_df_1\n")
  cat(str(design_df))
  cat("\n")
  
  ################    SAVE   #######################
  
  setwd(out)
  
  saveRDS(design_df, file='design.rds')
  write.table(design_df, file='design.tsv', sep="\t", quote=F, row.names = F)
  write.table(FlowCyt_results_sel, file='FlowCyt_global_adapted.csv', sep=",", quote=F, row.names = F)
  saveRDS(FlowCyt_results_sel, file='FlowCyt_global_adapted.rds')
  
}

printList = function(l, prefix = "    ") {
  list.df = data.frame(val_name = names(l), value = as.character(l))
  list_strs = apply(list.df, MARGIN = 1, FUN = function(x) { paste(x, collapse = " = ")})
  cat(paste(paste(paste0(prefix, list_strs), collapse = "\n"), "\n"))
}


#### main script ----

main = function() {
  cmd_line = commandArgs()
  cat("Command line:\n")
  cat(paste(gsub("--file=", "", cmd_line[4], fixed=T),
            paste(cmd_line[6:length(cmd_line)], collapse = " "),
            "\n\n"))
  option_list <- list(
    make_option(c("--FlowCyt_results"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--selection_samples"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--selection_treatment"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--selection_time_point"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Master_path_analysis"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--type"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--out"), type="character", default=NULL, 
                metavar="filename", 
                help="Path to tab-separated input file listing regions to analyze. Required.")
  )
  parser = OptionParser(usage = "140__Rscript_v106.R
                        --subset type
                        --TranscriptEXP FILE.txt
                        --cadd FILE.txt
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  data_wrangling(opt)
  
  
  
}


###########################################################################

system.time( main() )