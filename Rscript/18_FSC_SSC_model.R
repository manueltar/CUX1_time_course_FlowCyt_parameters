
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
suppressMessages(library("tidyr", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)


model = function(option_list)
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
  
 
  #### Read FlowCyt_results_selected----
  
  FlowCyt_results_selected<-readRDS(file=opt$FlowCyt_results_selected)
  
  FlowCyt_results_selected$sample_label<-paste(FlowCyt_results_selected$FlowCyt_subpopulation, FlowCyt_results_selected$Genotype, FlowCyt_results_selected$sample, FlowCyt_results_selected$time_point, FlowCyt_results_selected$treatment, sep ="__")
  
  
  cat("FlowCyt_results_selected_0\n")
  cat(str(FlowCyt_results_selected))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected$FlowCyt_subpopulation)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected$FlowCyt_subpopulation))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected$time_point)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected$time_point))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected$sample)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected$sample))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected$treatment)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected$treatment))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(FlowCyt_results_selected$sample_label))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(FlowCyt_results_selected$sample_label)))))
  cat("\n")
  
 
 
  
  #### DEBUG ----
  
  DEBUG <-1
  
  
  #### Calculate principal components ----
  
  Matrix_1<-as.matrix(FlowCyt_results_selected[,c(which(colnames(FlowCyt_results_selected) == 'Mean_FSC'),
                                                  which(colnames(FlowCyt_results_selected) == 'Mean_SSC'))])
  
  row.names(Matrix_1)<-FlowCyt_results_selected$sample_label
  
  if(DEBUG ==1)
  {
    cat("Matrix_1_0\n")
    cat(str(Matrix_1))
    cat("\n")
  }
  
  
  PC_list<-princomp(Matrix_1)
  
  if(DEBUG ==1)
  {
    cat("PC_list_0\n")
    cat(str(PC_list))
    cat("\n")
  }
  
  PC_scores<-PC_list$scores
  
  if(DEBUG ==1)
  {
    cat("PC_scores_0\n")
    cat(str(PC_scores))
    cat("\n")
  }
  
  df_COMP<-as.data.frame(PC_scores, stringsAsFactors=F)
  row.names(df_COMP)<-NULL
  df_COMP$sample_label<-row.names(PC_scores)
  
  if(DEBUG ==1)
  {
    cat("df_COMP_0\n")
    cat(str(df_COMP))
    cat("\n")
  }
  
  #### Merge PC components with the initial dataframe ----
  
  FlowCyt_results_selected<-merge(FlowCyt_results_selected, df_COMP, by="sample_label")
  
  if(DEBUG ==1)
  {
    cat("FlowCyt_results_selected_1\n")
    cat(str(FlowCyt_results_selected))
    cat("\n")
  }
  
  #### LOOP to select specific genotype comparisons----
  
  array_comparisons<-c("wt","homALT","wt","Del80")
  
  
  START<-1
  
  result_DEF<-data.frame()
  
  for(i in seq(from=START, to=length(array_comparisons), by=2))
  {
    array_comparisons_sel<-c(array_comparisons[i],array_comparisons[i+1])
    
    
    cat("----------------------------->\t")
    cat(sprintf(as.character(i)))
    cat("\t")
    cat(sprintf(as.character(array_comparisons_sel)))
    cat("\n")
    
    FlowCyt_results_selected_subset<-droplevels(FlowCyt_results_selected[which(FlowCyt_results_selected$Genotype%in%array_comparisons_sel),])
    
    if(DEBUG ==1)
    {
      cat("FlowCyt_results_selected_subset_0\n")
      cat(str(FlowCyt_results_selected_subset))
      cat("\n")
    }
    
    #### linear model of Comp.1 against genotype and cell type ----
    
    model_1<-lm(FlowCyt_results_selected_subset$Comp.1 ~ FlowCyt_results_selected_subset$FlowCyt_subpopulation + FlowCyt_results_selected_subset$Genotype)
    
    # if(DEBUG ==1)
    # {
    #   cat("model_1_0\n")
    #   cat(str(model_1))
    #   cat("\n")
    # }
    
    summary_model_1<-summary(model_1)
    
    # if(DEBUG ==1)
    # {
    #   cat("summary_model_1_0\n")
    #   cat(str(summary_model_1))
    #   cat("\n")
    # }
    
    pval_model<-summary_model_1$coefficients[,4]
    
    
    if(DEBUG ==1)
    {
      cat("pval_model_0\n")
      cat(str(pval_model))
      cat("\n")
    }
    
    result_df<-as.data.frame(cbind(as.numeric(pval_model), names(pval_model)), stringsAsFactors=F)
    
    colnames(result_df)<-c("pval","coefficient")
    result_df$pval<-as.numeric(result_df$pval)
    result_df$comparison<-paste(array_comparisons_sel, collapse="__")
    
    if(DEBUG ==1)
    {
      cat("result_df_0\n")
      cat(str(result_df))
      cat("\n")
    }
    
    result_df$coefficient<-gsub("FlowCyt_results_selected_subset\\$","",result_df$coefficient)
    
    if(DEBUG ==1)
    {
      cat("result_df_1\n")
      cat(str(result_df))
      cat("\n")
    }
    
    result_DEF<-rbind(result_df,result_DEF)
    
  }# i in seq(from=START, to=length(array_comparisons), by=2
  
  
  if(dim(result_DEF)[1] >0)
  {
    if(DEBUG ==1)
    {
      cat("result_DEF_0\n")
      cat(str(result_DEF))
      cat("\n")
    }
    
    setwd(out)
    
    write.table(result_DEF, file="FSC_SSC_model_result.tsv", sep="\t", quote=F, row.names = F)
    
    
    
    # #### path_model ----
    # 
    # path_model<-paste(out,'model','/',sep='')
    # 
    # if (file.exists(path_model)){
    #   
    # }else{
    #   
    #   dir.create(file.path(path_model))
    #   
    # }#path_model
    
  }# dim(result_DEF)[1] >0
  
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
    make_option(c("--FlowCyt_results_selected"), type="character", default=NULL, 
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
  
 
  
  model(opt)


  
}


###########################################################################

system.time( main() )