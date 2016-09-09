AOI_data_frame <- read.csv('C:\\Users\\lenovo\\Desktop\\ecg\\baseline\\P42F.csv')

AOI_data_frame <- AOI_data_frame[,-6]
AOI_data_frame <- AOI_data_frame[,-6]

#return if insufficient AOI data
if(nrow(AOI_data_frame)<1) return(FALSE)

#declare vectors

Medianame_vector <-NULL
AOIHit_vector <-NULL
Participants_vector <- NULL
Fixations_vector <-NULL
AOI_vector <-NULL
Label_vector <-NULL

ECGGroup <- c('Normal Sinus Rhythm.JPG','SVT.JPG','Ventricular Fibrillation.jpg','Myocarditis - sinus tachy non specific ST changes.JPG','VT.JPG','Anterior STEMI.jpg','AF, LBBB.JPG','WPW.jpg','Hyperkalaemia.jpg','Atrial Fluter.jpg','Bivent Pacer.JPG')

number_of_rows <- nrow(AOI_data_frame)

for(row_index in 1:number_of_rows){
  current_row <- AOI_data_frame[row_index,c(1:ncol(AOI_data_frame))]
  ECG_name <- toString(current_row$Medianame)
  AOI_name <- toString(current_row$AOI)

  
  
  switch(ECG_name,
         'Normal Sinus Rhythm.JPG'={
           if(AOI_name=='O'||AOI_name=='B'||AOI_name=='M'||AOI_name=='N'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'SVT.JPG'={
           if(AOI_name=='O'||AOI_name=='L'||AOI_name=='G'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'Ventricular Fibrillation.jpg'={
           if(AOI_name=='E'||AOI_name=='H'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'Myocarditis - sinus tachy non specific ST changes.JPG'={
           if(AOI_name=='M'||AOI_name=='O'||AOI_name=='H'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'VT.JPG'={
           if(AOI_name=='O'||AOI_name=='G'||AOI_name=='N'||AOI_name=='A'||AOI_name=='D'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'Anterior STEMI.jpg'={
           if(AOI_name=='H'||AOI_name=='A'||AOI_name=='J'||AOI_name=='I'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'AF, LBBB.JPG'={
           if(AOI_name=='G'||AOI_name=='N'||AOI_name=='M'||AOI_name=='O'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'WPW.jpg'={
           if(AOI_name=='M'||AOI_name=='G'||AOI_name=='H'||AOI_name=='J'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'Hyperkalaemia.jpg'={
           if(AOI_name=='E'||AOI_name=='F'||AOI_name=='H'||AOI_name=='I'||AOI_name=='J'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'Atrial Fluter.jpg'={
           if(AOI_name=='G'||AOI_name=='H'||AOI_name=='M'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         'Bivent Pacer.JPG'={
           if(AOI_name=='N'||AOI_name=='O'||AOI_name=='B'||AOI_name=='H'){
             AOIHit_vector <-c(AOIHit_vector,'1')
           }else{
             AOIHit_vector <-c(AOIHit_vector,'0')
           }
         },
         {
           AOIHit_vector <-c(AOIHit_vector,'0')
         }
  )
}
  AOI_data_frame$AOIHit<-AOIHit_vector
  write.csv(AOI_data_frame,file = "C:\\Users\\lenovo\\Desktop\\ecg\\baseline\\P42F.csv",row.names = FALSE,col.names = FALSE,quote = TRUE)






