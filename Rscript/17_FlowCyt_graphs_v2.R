
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

Graph_panoramic = function(option_list)
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
  
  #### Read FlowCyt_results----
  
  FlowCyt_results<-readRDS(file=opt$FlowCyt_results)
  
  cat("FlowCyt_results_0\n")
  cat(str(FlowCyt_results))
  cat("\n")
  
  
  
  #### LOOP TO PRINT ----
  
  path_graphs<-paste(out,'graphs','/',sep='')
  
  if (file.exists(path_graphs)){
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  path_graphs<-paste(out,'graphs','/','Feature_graphs','/',sep='')
  
  if (file.exists(path_graphs)){
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  feature_array<-unique(FlowCyt_results$Feature)
  
  cat("feature_array_0\n")
  cat(str(feature_array))
  cat("\n")
  
  
  test_index<-which(feature_array == 'Mean_FSC')
  
  
  scatter_plot_combination<-c("Mean_FSC","Mean_SSC","Geo_mean_FSC","Geo_mean_SSC")
  
  feature_array<-scatter_plot_combination
  
  
  START<-test_index
  START<-1
  
  DEBUG<-1
  
  list_DEF<-list()
  
  for(i in seq(from=START, to=length(feature_array), by=2))
  {
    feature_array_sel<-c(feature_array[i],feature_array[i+1])
    
   
    cat("----------------------------->\t")
    cat(sprintf(as.character(i)))
    cat("\t")
    cat(sprintf(as.character(feature_array_sel)))
    cat("\n")
      
   
    
    FlowCyt_results_sel<-unique(FlowCyt_results[which(FlowCyt_results$Feature %in% feature_array_sel),])
    
    if(DEBUG == 1)
    {
      cat("FlowCyt_results_sel_0\n")
      cat(str(FlowCyt_results_sel))
      cat("\n")
    }
    
    FlowCyt_results_sel_subset<-unique(FlowCyt_results_sel[,-which(colnames(FlowCyt_results_sel) == 'FlowCyt_name')])
    
    if(DEBUG == 1)
    {
      cat("FlowCyt_results_sel_subset_0\n")
      cat(str(FlowCyt_results_sel_subset))
      cat("\n")
    }
   
    
                                                               
    
    FlowCyt_results_sel_subset.m<-melt(FlowCyt_results_sel_subset, id.vars=c('Feature','time_point','sample','treatment','Genotype'), variable.name='FlowCyt_subpopulation', value.name='feature_magnitude')
    FlowCyt_results_sel_subset.m$feature_magnitude<-as.numeric(FlowCyt_results_sel_subset.m$feature_magnitude)
    FlowCyt_results_sel_subset.m$FlowCyt_subpopulation<-as.character(FlowCyt_results_sel_subset.m$FlowCyt_subpopulation)
    
    if(DEBUG == 1)
    {
      cat("FlowCyt_results_sel_subset.m_0\n")
      cat(str(FlowCyt_results_sel_subset.m))
      cat("\n")
    }
   
    
    REP_wide<-as.data.frame(pivot_wider(FlowCyt_results_sel_subset.m,
                                                               id_cols=c('FlowCyt_subpopulation','time_point','sample','treatment','Genotype'),
                                                               names_from="Feature",
                                                               values_from='feature_magnitude'),stringsAsFactors=F)
    
    REP_wide$FlowCyt_subpopulation<-factor(REP_wide$FlowCyt_subpopulation,
                                           levels=c("Double_neg","CD235_single","Double_pos","CD41_single"),
                                           ordered=T)
    
    if(DEBUG == 1)
    {
      cat("REP_wide_0\n")
      cat(str(REP_wide))
      cat("\n")
    }
    
    check<-REP_wide[which(REP_wide$time_point == 'Basal'),]
    
    if(DEBUG == 1)
    {
      cat("check_0\n")
      cat(str(check))
      cat("\n")
    }
   
    
    ### feature_magnitude 1 ----
    
    
    ind.feature_1<-which(colnames(REP_wide) == feature_array[i])

    A<-round(summary(REP_wide[,ind.feature_1][!is.na(REP_wide[,ind.feature_1])]),2)

    step<-abs(A[6]-A[1])/10

    if(step == 0)
    {

      step<-1
    }

    if(DEBUG ==1)
    {
      cat("Summary_feature_magnitude\n")
      cat(sprintf(as.character(names(A))))
      cat("\n")
      cat(sprintf(as.character(A)))
      cat("\n")
      cat(sprintf(as.character(step)))
      cat("\n")
    }



    breaks.feature_1<-unique(sort(c(seq(from= A[1], to=A[6]+step,by=step))))
    labels.feature_1<-as.character(round(breaks.feature_1,1))


    if(DEBUG == 1)
    {
      cat("breaks.feature_1:\t")
      cat(sprintf(as.character(breaks.feature_1)))
      cat("\n")

      cat("labels.feature_1:\t")
      cat(sprintf(as.character(labels.feature_1)))
      cat("\n")

    }
    
    ### feature_magnitude 2 ----
    
    
    ind.feature_2<-which(colnames(REP_wide) == feature_array[i+1])
    
    A<-round(summary(REP_wide[,ind.feature_2][!is.na(REP_wide[,ind.feature_2])]),2)
    
    step<-abs(A[6]-A[1])/10
    
    if(step == 0)
    {
      
      step<-1
    }
    
    if(DEBUG ==1)
    {
      cat("Summary_feature_magnitude\n")
      cat(sprintf(as.character(names(A))))
      cat("\n")
      cat(sprintf(as.character(A)))
      cat("\n")
      cat(sprintf(as.character(step)))
      cat("\n")
    }
    
    
    
    breaks.feature_2<-unique(sort(c(seq(from= A[1], to=A[6]+step,by=step))))
    labels.feature_2<-as.character(round(breaks.feature_2,1))
    
    
    if(DEBUG == 1)
    {
      cat("breaks.feature_2:\t")
      cat(sprintf(as.character(breaks.feature_2)))
      cat("\n")
      
      cat("labels.feature_2:\t")
      cat(sprintf(as.character(labels.feature_2)))
      cat("\n")
      
    }
    
    jitter_pos <- position_jitter(width=1, height = 1, seed = 1)


    graph_feature_magnitude<-ggplot(data=REP_wide,
                                    aes(x=REP_wide[,ind.feature_2], 
                                        y=REP_wide[,ind.feature_1], 
                                        color=sample,
                                        shape=FlowCyt_subpopulation)) +
      geom_point(position=jitter_pos,size=4)+
      theme_bw()+
      scale_y_continuous(name=feature_array[i],breaks=breaks.feature_1,labels=labels.feature_1, limits=c(breaks.feature_1[1],breaks.feature_1[length(breaks.feature_1)]))+
      scale_x_continuous(name=feature_array[i+1],breaks=breaks.feature_2,labels=labels.feature_2, limits=c(breaks.feature_2[1],breaks.feature_2[length(breaks.feature_2)]))+
      scale_color_brewer(palette = "Set3", drop=F)+
      scale_shape_manual(values=c(15,17:19))
    
    if(DEBUG == 1)
    {
      cat("Part_I:\t")

    }
    

    graph_feature_magnitude<-graph_feature_magnitude+
      facet_grid(REP_wide$time_point ~ REP_wide$Genotype, scales='free_x', space='free_x') +
      theme_cowplot(font_size = 14)+
      theme( strip.background = element_blank(),
             strip.placement = "inside",
             strip.text = element_text(size=14),
             panel.spacing = unit(0.2, "lines"),
             panel.background=element_rect(fill="white"),
             panel.border=element_rect(colour="black",size=1),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())+
      theme(axis.title.y=element_text(size=18, family="sans"),
            axis.title.x=element_text(size=18, family="sans"),
            axis.text.y=element_text(angle=0,size=14, color="black", family="sans"),
            axis.text.x=element_text(angle=45,size=14,vjust=1,hjust=1, color="black", family="sans"),
            legend.title=element_text(size=16,color="black", family="sans"),
            legend.text=element_text(size=12,color="black", family="sans"))+
      theme(legend.key.size = unit(1.5, 'cm'), #change legend key size
            legend.key.height = unit(1.5, 'cm'), #change legend key height
            legend.key.width = unit(1, 'cm'), #change legend key width
            legend.title = element_blank(), #change legend title font size
            legend.text = element_text(size=14))+ #change legend text font size
      theme(legend.position = "bottom")+
      guides(colour = guide_legend(nrow = 3))+
      ggeasy::easy_center_title()
    
    if(DEBUG == 1)
    {
      cat("Part_II:\t")
      
    }

   

    setwd(path_graphs)

    svgname<-paste(paste(paste(feature_array_sel, collapse="__"),"Model_plot", sep='_'),".svg",sep='')
    makesvg = TRUE

    if (makesvg == TRUE)
    {
      ggsave(svgname, plot= graph_feature_magnitude,
             device="svg",
             height=10, width=12)
    }

    if(DEBUG == 1)
    {
      cat("THE_END:\n")

    }
    
    
    setwd(out)
    
    rdsname<-paste(paste(paste(feature_array_sel, collapse="__"),"data_frame", sep='_'),".rds",sep='')
    saveRDS(REP_wide,file=rdsname)
   
  }#i in 1:length(feature_array)
}

Graph_selected = function(option_list)
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
  
  #### READ and transform FlowCyt_subpopulation_selected ----
  
  FlowCyt_subpopulation_selected = unlist(strsplit(opt$FlowCyt_subpopulation_selected, split=","))
  
  cat("FlowCyt_subpopulation_selected_\n")
  cat(sprintf(as.character(FlowCyt_subpopulation_selected)))
  cat("\n")
  
  #### READ and transform time_point_selected ----
  
  time_point_selected = unlist(strsplit(opt$time_point_selected, split=","))
  
  cat("time_point_selected_\n")
  cat(sprintf(as.character(time_point_selected)))
  cat("\n")
  
  #### Read FlowCyt_results_selected----
  
  FlowCyt_results_selected<-readRDS(file=opt$FlowCyt_results_selected)
  
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
  
  #### select FlowCyt_subpopulation_selected and time_point_selected ----
  
  FlowCyt_results_selected_subset<-droplevels(unique(FlowCyt_results_selected[which(FlowCyt_results_selected$FlowCyt_subpopulation%in%FlowCyt_subpopulation_selected &
                                                                    FlowCyt_results_selected$time_point%in%time_point_selected),]))
  
  cat("FlowCyt_results_selected_subset_0\n")
  cat(str(FlowCyt_results_selected_subset))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected_subset$FlowCyt_subpopulation)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected_subset$FlowCyt_subpopulation))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected_subset$time_point)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected_subset$time_point))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected_subset$sample)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected_subset$sample))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected_subset$treatment)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected_subset$treatment))))
  cat("\n")
  cat(sprintf(as.character(names(summary(FlowCyt_results_selected_subset$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(FlowCyt_results_selected_subset$Genotype))))
  cat("\n")
  
  #### path_graphs ----
  
  path_graphs<-paste(out,'graphs','/',sep='')
  
  if (file.exists(path_graphs)){
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  path_graphs<-paste(out,'graphs','/','Feature_graphs','/',sep='')
  
  if (file.exists(path_graphs)){
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  #### DEBUG ----
  
  DEBUG <-1
  
  ### feature_magnitude 1 ----
  
  
  ind.feature_1<-which(colnames(FlowCyt_results_selected_subset) == 'Mean_FSC')
  
  A<-round(summary(FlowCyt_results_selected_subset[,ind.feature_1][!is.na(FlowCyt_results_selected_subset[,ind.feature_1])]),2)
  
  step<-abs(A[6]-A[1])/10
  
  if(step == 0)
  {
    
    step<-1
  }
  
  if(DEBUG ==1)
  {
    cat("Summary_feature_magnitude\n")
    cat(sprintf(as.character(names(A))))
    cat("\n")
    cat(sprintf(as.character(A)))
    cat("\n")
    cat(sprintf(as.character(step)))
    cat("\n")
  }
  
  
  
  breaks.feature_1<-unique(sort(c(seq(from= A[1], to=A[6]+step,by=step))))
  labels.feature_1<-as.character(round(breaks.feature_1,1))
  
  
  if(DEBUG == 1)
  {
    cat("breaks.feature_1:\t")
    cat(sprintf(as.character(breaks.feature_1)))
    cat("\n")
    
    cat("labels.feature_1:\t")
    cat(sprintf(as.character(labels.feature_1)))
    cat("\n")
    
  }
  
  ### feature_magnitude 2 ----
  
  
  ind.feature_2<-which(colnames(FlowCyt_results_selected_subset) == 'Mean_SSC')
  
  A<-round(summary(FlowCyt_results_selected_subset[,ind.feature_2][!is.na(FlowCyt_results_selected_subset[,ind.feature_2])]),2)
  
  step<-abs(A[6]-A[1])/10
  
  if(step == 0)
  {
    
    step<-1
  }
  
  if(DEBUG ==1)
  {
    cat("Summary_feature_magnitude\n")
    cat(sprintf(as.character(names(A))))
    cat("\n")
    cat(sprintf(as.character(A)))
    cat("\n")
    cat(sprintf(as.character(step)))
    cat("\n")
  }
  
  
  
  breaks.feature_2<-unique(sort(c(seq(from= A[1], to=A[6]+step,by=step))))
  labels.feature_2<-as.character(round(breaks.feature_2,1))
  
  
  if(DEBUG == 1)
  {
    cat("breaks.feature_2:\t")
    cat(sprintf(as.character(breaks.feature_2)))
    cat("\n")
    
    cat("labels.feature_2:\t")
    cat(sprintf(as.character(labels.feature_2)))
    cat("\n")
    
  }
  
  jitter_pos <- position_jitter(width=0.4, height = 0.4, seed = 1)
  
  
  graph_feature_magnitude<-ggplot(data=FlowCyt_results_selected_subset,
                                  aes(x=Mean_SSC, 
                                      y=Mean_FSC, 
                                      shape=Genotype)) +
    geom_point(position=jitter_pos,size=5, color="black")+
    theme_bw()+
    scale_y_continuous(name='Mean_FSC',breaks=breaks.feature_1,labels=labels.feature_1, limits=c(breaks.feature_1[1]-100,breaks.feature_1[length(breaks.feature_1)]+100))+
    scale_x_continuous(name='Mean_SSC',breaks=breaks.feature_2,labels=labels.feature_2, limits=c(breaks.feature_2[1]-100,breaks.feature_2[length(breaks.feature_2)]+100))+
    scale_color_brewer(palette = "Set3", drop=F)+
    scale_shape_manual(values=c(16,17,15))
  
  if(DEBUG == 1)
  {
    cat("Part_I:\t")
    
  }
  
  
  graph_feature_magnitude<-graph_feature_magnitude+
    facet_grid(FlowCyt_results_selected_subset$time_point ~ FlowCyt_results_selected_subset$FlowCyt_subpopulation, scales='free_x', space='free_x') +
    theme_cowplot(font_size = 14)+
    theme( strip.background = element_blank(),
           strip.placement = "inside",
           strip.text = element_text(size=14),
           panel.spacing = unit(0.2, "lines"),
           panel.background=element_rect(fill="white"),
           panel.border=element_rect(colour="black",size=1),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+
    theme(axis.title.y=element_text(size=18, family="sans"),
          axis.title.x=element_text(size=18, family="sans"),
          axis.text.y=element_text(angle=0,size=14, color="black", family="sans"),
          axis.text.x=element_text(angle=45,size=14,vjust=1,hjust=1, color="black", family="sans"),
          legend.title=element_text(size=16,color="black", family="sans"),
          legend.text=element_text(size=12,color="black", family="sans"))+
    theme(legend.key.size = unit(1.5, 'cm'), #change legend key size
          legend.key.height = unit(1.5, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=18, family="sans"), #change legend title font size
          legend.text = element_text(size=14, family="sans"))+ #change legend text font size
    theme(legend.position = "bottom")+
    guides(colour = guide_legend(nrow = 3))+
    ggeasy::easy_center_title()
  
  if(DEBUG == 1)
  {
    cat("Part_II:\t")
    
  }
  
  
  
  setwd(path_graphs)
  
  svgname<-paste(paste("Selected_plot",paste(FlowCyt_subpopulation_selected, collapse="__"),paste(time_point_selected, collapse="__"), sep='_'),".svg",sep='')
  makesvg = TRUE
  
  if (makesvg == TRUE)
  {
    ggsave(svgname, plot= graph_feature_magnitude,
           device="svg",
           height=10, width=12)
  }
  
  if(DEBUG == 1)
  {
    cat("THE_END:\n")
    
  }
  
  
  setwd(out)
  
  rdsname<-paste(paste(paste(FlowCyt_subpopulation_selected, collapse="__"),paste(time_point_selected, collapse="__"),"data_frame", sep='_'),".rds",sep='')
  saveRDS(FlowCyt_results_selected_subset,file=rdsname)
  
  
  
  
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
    make_option(c("--FlowCyt_results_selected"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--FlowCyt_subpopulation_selected"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--time_point_selected"), type="character", default=NULL, 
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
  
 
  Graph_panoramic(opt)
  Graph_selected(opt)


  
}


###########################################################################

system.time( main() )