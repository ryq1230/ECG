library(MeanShift)

AOI_data_frame <- read.csv('C:\\Users\\lenovo\\Desktop\\ecg\\baseline\\P29F.csv')
AOI_data_frame$ClusterLabel<-as.integer(0)
AOI_data_frame$FixationCount<-as.integer(0)
AOI_data_frame$Predict <- as.integer(0)
AOI_data_frame$Intersd <- as.integer(0)

ECGGroup <- c('Normal Sinus Rhythm.JPG','SVT.JPG','Ventricular Fibrillation.jpg','Myocarditis - sinus tachy non specific ST changes.JPG','VT.JPG','Anterior STEMI.jpg','AF, LBBB.JPG','WPW.jpg','Hyperkalaemia.jpg','Atrial Fluter.jpg','Bivent Pacer.JPG')

difference_vector <- NULL

AOI_data_frame$Predict<-as.integer(0)
training <- sample(1:11,6)
testing <- setdiff(1:11,training)

for (ECG_index in 1:11) {
  
  interpretation_sd  <- NULL
  interpretation_ave  <- NULL
  training_result <- NULL
  cluster_ave <- NULL
  count_AOI_count <- NULL
  
  AOI_subset <- AOI_data_frame[AOI_data_frame$Medianame == ECGGroup[ECG_index],]
  
  start <- AOI_subset[1,]$Index+2
  end <- AOI_subset[1,]$Index+nrow(AOI_subset)-1
  
  #average,sdiance duration
  for (predit_index in start:end) {
    interpretation_subset <- AOI_data_frame[c(predit_index-2,predit_index-1,predit_index),]
    sd_duration <- sd(interpretation_subset$FixationDuration)
    ave_duration <- mean(interpretation_subset$FixationDuration)
    interpretation_sd <- c(interpretation_sd,sd_duration)
    interpretation_ave <- c(interpretation_ave,ave_duration)
  }
  
  #intepretation AOI
  for (predit_index in start:end) {
    interpetation_point <- AOI_subset[AOI_subset$Label==1,]
    x <- interpetation_point$Index
    interpretation_subset <- AOI_data_frame[c(x-2,x-1,x),]
    interpretation_AOI <- interpretation_subset$AOI
  }
  
  #clustering
  ivd <- dist(interpretation_sd)
  clustering <- bmsClustering(ivd)
  initclusterlabel <- clustering$labels[1]
  initintersd <- interpretation_sd[1]
  AOI_subset$ClusterLabel <- c(initclusterlabel, initclusterlabel, clustering$labels)
  AOI_subset$Intersd <- c(initintersd, initintersd,interpretation_sd)
  
  summary_cluster <- names(rev(sort(table(AOI_subset$ClusterLabel))))
  baseline <- summary_cluster[1]
  

  
  for (y in 1:length(summary_cluster)) {
    baseCluster <- AOI_subset[AOI_subset$ClusterLabel==summary_cluster[y],]
    baseCluster_sd <- mean(baseCluster$Intersd)
    cluster_ave <- c(cluster_ave,baseCluster_sd)
  }
  
  cluster_ave <- data.frame(cluster=summary_cluster, duration=cluster_ave)
  target_cluster <- AOI_subset[AOI_subset$Label==1,]$ClusterLabel
  difference <- cluster_ave[cluster_ave$cluster==target_cluster,]$duration/cluster_ave[1,]$duration
  difference_vector <- c(difference_vector,difference)
  
  #Fixation Count
  FixationCount_AOI <- AOI_subset[!duplicated(AOI_subset$AOI),]
  FixationCount_AOI$FixationCount<-as.integer(0)
  
  for (FixationCount_Index in 1:nrow(AOI_subset)-1) {
    FC_AOI <- AOI_subset[FixationCount_Index,]$AOI
    FC_AOI_Next <-AOI_subset[FixationCount_Index+1,]$AOI
    if(toString(FC_AOI_Next) != toString(FC_AOI)){
      FixationCount_AOI[FixationCount_AOI$AOI==FC_AOI,]$FixationCount <- FixationCount_AOI[FixationCount_AOI$AOI==FC_AOI,]$FixationCount+1
    }
  }
  
  Count_AOI <-FixationCount_AOI[rev(order(FixationCount_AOI$FixationCount)),][1:3,]
  Useful_AOI <- names(rev(sort(summary(AOI_subset$AOI))))[1:3]
  blabla_AOI <- c("M","N","O")
  
  
  #Fixation Count AOI distribute
  for (predit_index in start:end) {
    interpretation_subset <- AOI_data_frame[c(predit_index-2,predit_index-1,predit_index),]
    interpretation_subset <- interpretation_subset[!duplicated(interpretation_subset$AOI),]
    AOI_fixation_count <- 0
    hehe <- nrow(interpretation_subset)
    for (x in 1:hehe) {
      fixation_AOI <- interpretation_subset[x,]$AOI
      if(fixation_AOI %in% Count_AOI$AOI){
        AOI_fixation_count <- AOI_fixation_count +1
      }
    }
    count_AOI_count <- c(count_AOI_count,AOI_fixation_count)
  }
  
  initcAc <- count_AOI_count[1]
  AOI_subset$FixationCount <- c(initcAc, initcAc,count_AOI_count)
  
  for (baseline_index in AOI_subset[1,]$Index:end) {
    cluster_label <- AOI_subset[AOI_subset$Index==baseline_index,]$ClusterLabel
    if(cluster_label==baseline){
      AOI_data_frame[AOI_data_frame$Index==baseline_index,]$Baseline =1
      AOI_data_frame[AOI_data_frame$Index==baseline_index,]$ClusterLabel=AOI_subset[AOI_subset$Index==baseline_index,]$ClusterLabel
      AOI_data_frame[AOI_data_frame$Index==baseline_index,]$FixationCount =AOI_subset[AOI_subset$Index==baseline_index,]$FixationCount
      AOI_data_frame[AOI_data_frame$Index==baseline_index,]$Intersd =AOI_subset[AOI_subset$Index==baseline_index,]$Intersd
    }else{
      AOI_data_frame[AOI_data_frame$Index==baseline_index,]$Baseline =0
      AOI_data_frame[AOI_data_frame$Index==baseline_index,]$ClusterLabel=AOI_subset[AOI_subset$Index==baseline_index,]$ClusterLabel
      AOI_data_frame[AOI_data_frame$Index==baseline_index,]$FixationCount =AOI_subset[AOI_subset$Index==baseline_index,]$FixationCount
      AOI_data_frame[AOI_data_frame$Index==baseline_index,]$Intersd =AOI_subset[AOI_subset$Index==baseline_index,]$Intersd
    }
  }
}

#write.csv(AOI_data_frame,file = "C:\\Users\\lenovo\\Desktop\\ecg\\baseline\\P42F.csv",row.names = FALSE,col.names = FALSE,quote = TRUE)
