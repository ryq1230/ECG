AOI_data_frame <- read.csv('C:\\Users\\lenovo\\Desktop\\ecg\\baseline\\P36F.csv')

ECGGroup <- c('Normal Sinus Rhythm.JPG','SVT.JPG','Ventricular Fibrillation.jpg','Myocarditis - sinus tachy non specific ST changes.JPG','VT.JPG','Anterior STEMI.jpg','AF, LBBB.JPG','WPW.jpg','Hyperkalaemia.jpg','Atrial Fluter.jpg','Bivent Pacer.JPG')

difference_vector <- NULL
baseline_vector <-NULL

AOI_data_frame$Predict<-as.integer(0)
training <- sample(1:11,6)
testing <- setdiff(1:11,training)

for (ECG_index in testing) {
  
  AOI_subset <- AOI_data_frame[AOI_data_frame$Medianame == ECGGroup[ECG_index],]
  cluster_ave <- NULL
  
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
  baseline_vector <- c(baseline_vector,cluster_ave[1,]$duration)
  
  for (test_cluster in 1:length(summary_cluster)) {
    
    criterion <- cluster_ave[cluster_ave$cluster==test_cluster,]$duration/cluster_ave[1,]$duration
    if(2.2<criterion && criterion<3.6){
      AOI_subset[AOI_subset$ClusterLabel==test_cluster,]$Predict <- 1
    }
  }
  
  for (index in 1:nrow(AOI_subset)) {
    
    if(AOI_subset[index,]$Predict == 1){
      AOI_data_frame[AOI_data_frame$Index==AOI_subset[index,]$Index,]$Predict <- 1
    }else{
      AOI_data_frame[AOI_data_frame$Index==AOI_subset[index,]$Index,]$Predict <- 0
    }
  }
}


#write.csv(AOI_data_frame,file = "C:\\Users\\lenovo\\Desktop\\ecg\\baseline\\PF.csv",row.names = FALSE,col.names = FALSE,quote = TRUE)