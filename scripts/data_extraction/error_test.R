## distinguishing head from tail ##
###################################
## thanks for your help with this ##

'%!in%'<-Negate('%in%') ## helper function

## install any needed dependencies
if("reticulate" %!in% installed.packages()){install.packages("reticulate")}
if("data.table" %!in% installed.packages()){install.packages("data.table")}
if("parallel" %!in% installed.packages()){install.packages("parallel")}
if("foreach" %!in% installed.packages()){install.packages("foreach")}
if("doParallel" %!in% installed.packages()){install.packages("doParallel")}
if("pcds" %!in% installed.packages()){install.packages("pcds")}
if("magick" %!in% installed.packages()){install.packages("magick")}

## load dependencies
require(reticulate)
require(data.table)
require(parallel)
require(foreach)
require(doParallel)
require(pcds)
require(magick)
require(png)

np<-import("numpy") ## make sure you have numpy in your python install! if not, run "pip install numpy" in a shell with your favored python activated
## edit line below with your home directory (probably where this file is located)
drctry<-"/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/error-test/"
initials<-"EH" ## edit this line with observer initials

#read in transition file
transitions = read.csv("/Users/ellag/Desktop/PhD/academic_projects/eel_diel/data/transitions/updated/transitions_D2_23_05_25_complete.csv", header=FALSE)
transitions <- transitions[complete.cases(transitions),]

first_seg = "GH089724.MP4"
beg <- substr(first_seg, start = 1, stop = 2)
end <- substr(first_seg, start = 5, stop = 8)
first_seg_ID <- as.numeric(substr(first_seg, start = 3,stop=4))


#sample 10% 
chosen_cols <- sample(2:ncol(transitions), floor((ncol(transitions)-1)*.01))

#initialise a dataframe
error_df <- data.frame(matrix(ncol=7,nrow=length(chosen_cols)))
colnames(error_df) <- c("ncol","nrow","image_folder","image","eel_ID","state","error")
error_df$ncol <- chosen_cols

for (i in 1:length(chosen_cols)) {
  col_ID <- chosen_cols[i]
  row_ID <- sample(1:nrow(transitions), 1)
  eel_ID <- transitions[row_ID,1]
  state <- transitions[row_ID, col_ID]
  seg_ID <- floor(col_ID/512)
  real_seg_ID <- first_seg_ID + seg_ID
  real_seg_ID_str <- as.character(real_seg_ID)
  
  if (nchar(real_seg_ID_str) < 2) {
    real_seg_ID_str <- paste("0",real_seg_ID_str, sep="")
  }
  
  final_seg_ID <- paste("frames_", beg, real_seg_ID_str, end, sep="")
  frame_ID <- ((col_ID/512) %% 1)*512
  
  frame_ID <- sprintf("%05d", frame_ID)
  frame_ID <- paste(frame_ID, ".png", sep="")
  

  
  error_df$nrow[i] <- row_ID
  error_df$eel_ID[i] <- eel_ID
  error_df$state[i] <- state
  error_df$image_folder[i] <- final_seg_ID
  error_df$image[i] <- frame_ID
}

for (i in 1:nrow(error_df)) {
  drctry_folder <-paste0(drctry,error_df$image_folder[i])
  setwd(drctry_folder)
  
  flnme <- error_df$image[i]
  
  jj <- readPNG(flnme)
  plot.new()
  rasterImage(jj,0,0,1,1)
  
  choicen1<-menu(c("correct","incorrect"))
  if(choicen1==1){
  
  error_df$error[i] <- 0
  
  } else {
    error_df$error[i] <- 0
  }
}








flst<-list.files(pattern=".png")
dev.new()
for(zoom in 1:length(flst)){
  flnme<-flst[zoom]

  jj <- readPNG(flnme)
  plot.new()
  rasterImage(jj,0,0,1,1)
  
  choicen1<-menu(c("correct","incorrect"))
  if(choicen1==1){
    
    <- 1

  # as.matrix(m)
  plot(m)
  mtext(flnme,side=3)
  dL<-list()
  for(i in 1:length(lnoutL)){
    segments(y0=dim(mskL[[i]])[1]-(lnoutL[[i]][[2]][1,row]),#/dim(mskL[[i]])[1]),
             y1=dim(mskL[[i]])[1]-(lnoutL[[i]][[2]][2,row]),#/dim(mskL[[i]])[1]),
             x0=(lnoutL[[i]][[2]][1,col]),#/dim(mskL[[i]])[2]),
             x1=(lnoutL[[i]][[2]][2,col]),#/dim(mskL[[i]])[2]),
             col="red",lwd=2)
    d<-locator(1)
    dd<-as.data.table(d)
    dd[,y:=(dim(mskL[[i]])[1]-y)]
    dd[,mask:=i]
    dL[[length(dL)+1]]<-dd
    segments(y0=dim(mskL[[i]])[1]-(lnoutL[[i]][[2]][1,row]),#/dim(mskL[[i]])[1]),
             y1=dim(mskL[[i]])[1]-(lnoutL[[i]][[2]][2,row]),#/dim(mskL[[i]])[1]),
             x0=(lnoutL[[i]][[2]][1,col]),#/dim(mskL[[i]])[2]),
             x1=(lnoutL[[i]][[2]][2,col]),#/dim(mskL[[i]])[2]),
             col="darkgreen",lwd=2)
  }
  
  ddd<-rbindlist(dL)
  
  newloutL<-list()
  for(i in 1:ddd[,.N]){
    tmp<-lnoutL[[ddd[i,mask]]][[2]]
    ddstL<-list()
    for(oo in 1:tmp[,.N]){
      myxd<-ddd[i,x]-tmp[oo,col]
      myyd<-ddd[i,y]-tmp[oo,row]
      ddstL[[length(ddstL)+1]]<-sqrt((abs(myxd)^2+abs(myyd)^2))
    }
    ## if the first point in the major axis is the closest to the head, 
    ## switch order or points (we want tail, then head)
    if(which.min(unlist(ddstL))==1){ 
      tmps<-copy(rbindlist(list(tmp[2,],tmp[1,])))
    } else {
      tmps<-copy(tmp)
    }
    newloutL[[length(newloutL)+1]]<-tmps
  }
  
  nL<-newloutL
  mystem<-unlist(strsplit(flnme,split="_combined.npz"))
  if(!dir.exists(paste0(drctry,"headout/"))){
    dir.create(paste0(drctry,"headout/"))
  }
  saveRDS(nL,paste0("headout/",mystem,"_headings.rds"))
  fwrite(ddd,paste0("headout/",mystem,"_headlocs.csv"))
  fwrite(data.table(flnme=mystem,
                    observer=initials),
         paste0("headout/",mystem,"_records.csv"))
  
  
  alignL<-list()
  checkL<-list()
  for(x in nL){ ## run a (slow) loop to avoid inverse comparisons
    # alignL<-mclapply(nL,function(x){
    a<-copy(x)
    myi<-which(unlist(lapply(nL,function(hey){all.equal(hey,x)}))=="TRUE")
    tmpL<-list()
    for(i in 1:length(nL)){
      a<-copy(x)
      if(all.equal(a,nL[[i]])==T){next}
      if(paste0(myi,"_",i) %in% checkL | paste0(i,"_",myi) %in% checkL ){next}
      checkL[[length(checkL)+1]]<-paste0(myi,"_",i)
      
      b<-copy(nL[[i]])
      a[,row:=row-a[1,row]]
      
      a[2,col:=(col-a[1,col])]
      a[1,col:=0]
      
      b[,row:=row-b[1,row]]
      b[2,col:=(col-b[1,col])]
      b[1,col:=0]
      
      if(all.equal(b[2,],a[2,])==T){
        ang<-0
      } else {
        ang<-angle3pnts(a=a[2,c(row,col)],
                        b=c(0,0),
                        c=b[2,c(row,col)],
                        radian = F)
      }
      
      
      
      tmpL[[length(tmpL)+1]]<-ang
      names(tmpL[[length(tmpL)]])<-checkL[[length(checkL)]]
    }
    if(length(tmpL)>0){
      alignL[[length(alignL)+1]]<-unlist(tmpL)
      names(alignL[[length(alignL)]])<-unlist(lapply(tmpL,names))
    }
  }
  saveRDS(alignL,paste0("headout/",mystem,"_alignments.rds"))
  
  
  
}




