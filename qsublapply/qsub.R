

qsubLapply<-function(x,fun,...,ncpus=10,mem=4,jobName="qjob",verbose=0,walltime=36000, waitTime=0, cluster="computerome", group_list=NULL, user=NULL){
  
  #' lapply function using parallelization over pbs pro
  #' @param works as the standard R lapply function but runs in multicore with pbs pro
  #' @param x list for inputting to lapply
  #' @param fun function that is applied to each of the elements in x
  #' @param ... extra arguments to pass to the functions. Currently only character entries supported. 
  #' @param ncpus number of cpus to use
  #' @param mem memory to assign per processor (in GB)
  #' @param jobName name of pbs job
  #' @param verbose if set to >0, then the actual command (including qsub call to	pbs) will be printed to screen
  #' @param waitTime if set to >0, then a wait will be introduced before submitting next job (in seconds)
  #' @return a list which is the result of function fun being applied to each element of x 
  #' @author lwf
  #' @export
  
  
  
  #	startTime1<-Sys.time()
  
  #initial checks to see that the input is ok
  if(class(x) != "list")stop(paste("x must be of class list, not",class(x)))
  if(length(x) < 2)stop(paste("x must be of a length 2 or longer, otherwise multicore would not be necessary"))
  if(is.null(names(x))){
    stop("x must be a named list")	
  }
  if(any(duplicated(names(x))))stop("x had duplicated list entry names")
  
  if(class(fun) != "function")stop(paste("fun must be of class function, not",class(fun)))
  
  if(class(ncpus) == "numeric")ncpus<-as.integer(ncpus)
  if(class(ncpus) != "integer")stop(paste("ncpus must be of class integer, not",class(ncpus)))
  if(length(ncpus) != 1)stop(paste("memncpus must be of a length 1, not",length(ncpus)))
  if(ncpus > 500 | ncpus < 2)stop(paste("ncpus must be between 2 and 500"))
  if(length(x) < ncpus){
    ncpus <- length(x)
    if(verbose>0){print(paste("ncpus was adjusted down to",ncpus,"because no more were needed for the length of x"))}
  }
  
  if(class(mem) == "numeric")mem<-as.integer(mem)
  if(class(mem) != "integer")stop(paste("mem must be of class integer, not",class(mem)))
  if(length(mem) != 1)stop(paste("mem must be of a length 1, not",length(mem)))
  if(mem > 100 | mem < 1)stop(paste("mem must be between 1 and 100 gb"))

  
  
  
  # 	if(class(retrieveLag) == "numeric")retrieveLag<-as.integer(retrieveLag)
  # 	if(class(retrieveLag) != "integer")stop(paste("retrieveLag must be of class integer, not",class(retrieveLag)))
  # 	if(length(retrieveLag) != 1)stop(paste("retrieveLag must be of a length 1, not",length(retrieveLag)))
  
  # 	if(class(queue) != "character")stop(paste("queue must be of class character, not",class(queue)))
  # 	if(length(queue) != 1)stop(paste("queue must be of a length 1, not",length(queue)))
  # 	if(!queue%in%c("low","medium"))stop(paste("queue must be either 'low' or 'medium', not",queue))
  
  if(class(jobName) != "character")stop(paste("jobName must be of class character, not",class(jobName)))
  if(length(jobName) != 1)stop(paste("jobName must be of a length 1, not",length(jobName)))
  if(length(grep(" ",jobName)) > 0)stop(paste("jobName must not contain spaces"))
  if(length(grep("[^a-zA-Z0-9]", jobName)) > 0)stop(paste("jobName must not contain special characters"))
  
  
  if(class(walltime) == "numeric")walltime<-as.integer(walltime)
  if(class(walltime) != "integer")stop(paste("walltime must be of class integer, not",class(walltime)))
  if(length(walltime) != 1)stop(paste("walltime must be of a length 1, not",length(walltime)))
  if(walltime < 60)stop(paste("walltime must be at least length 60"))
  
  if(class(cluster) != "character")stop(paste("cluster must be of class character, not",class(cluster)))
  if(length(cluster) != 1)stop(paste("cluster must be of a length 1, not",length(cluster)))
  allowedclusters<-c("computerome","jhpce")
  if(!cluster%in%allowedclusters)stop(paste("cluster must be on of:",paste(allowedclusters,collapse=", ")))
  if(cluster=="computerome"){
    system("module load moab torque")	
    
  }
  
  
  if(cluster=="computerome"){
    if(is.null(user))stop("Must give user")
    if(class(user)!="character")stop("Must give user as character")
    if(length(user)!=1)stop("Must give user as a single character")
    
    if(is.null(group_list))stop("Must give group_list")
    if(class(group_list)!="character")stop("Must give group_list as character")
    if(length(group_list)!=1)stop("Must give group_list as a single character")
  }
  
  
  #splitting the list x into sensible entries depending on length(x) and ncpus
  smallStepSize <- floor(length(x) / ncpus)
  bigStepSizeNumber <- length(x) %% ncpus
  xNames<-names(x)
  namesToProcess <- list()
  for(i in 1:ncpus){
    if(i > bigStepSizeNumber){
      namesToProcess [[as.character(i)]] <- sample(xNames,smallStepSize)
    }else{
      namesToProcess [[as.character(i)]] <-sample(xNames,smallStepSize+1)
    }
    xNames<-xNames[!xNames%in%namesToProcess [[i]]]
  }
  if(!all(unlist(namesToProcess) %in% names(x)) | !all(names(x) %in%unlist(namesToProcess))){stop("Something went wrong in distributing jobs to CPUs")}
  
  #creating a temporary working dir
  tempDir <- "temp_pbssubmit_0"
  i<-1
  while(file.exists(tempDir)){
    tempDir <- paste("temp_pbssubmit",i,sep="_")
    i<-i+1
  }
  dir.create(tempDir)
  s<-.Platform[["file.sep"]]
  fullPathTempDir <- paste(getwd(), tempDir, sep=s)
  
  #saving arguments
  if(length(list(...))>0){
    if(is.null(names(list(...))))stop("When providing extra arguments as ... they must be named, i.e. 'argumentName=argument'")
  }
  for(entry in names(list(...))){
    if(nchar(entry) == 0)stop("When providing extra arguments as ... they must all be named, i.e. 'argumentName=argument'")
    assign(entry,list(...)[[entry]])
  }
  if(verbose>0 & length(list(...))>0)print(paste("Saving",length(list(...)),"arguments for distribution:",paste(names(list(...)),collapse=", ")))
  save(list=names(list(...)),file=paste(tempDir,s,"arguments.rdata",sep=""))
  
  #for loop that writes temporary data and scripts and submits to qsub
  if(verbose>0)print(paste("Creating temporary script files in folder",tempDir,"and submitting",length(namesToProcess),"jobs each containing this amount of names:",paste(unique(unlist(lapply(namesToProcess,length))),collapse=" or ")))
  
  
  jobIds<-vector()
  for(process in names(namesToProcess)){
    
    
    entriesToProcessThisCPU<-x[namesToProcess[[process]]]
    save(list=c("fun","entriesToProcessThisCPU"),file=paste(tempDir,s,"tempNames_",process,".rdata",sep=""))
    
    runFileR<-file(paste(tempDir,s,"runFileR_",process,sep=""),"w")
    writeLines(paste("load('",fullPathTempDir,s,"tempNames_",process,".rdata')",sep=""),runFileR)
    writeLines(paste("load('",fullPathTempDir,s,"arguments.rdata')",sep=""),runFileR)
    writeLines("results<-list()",runFileR)
    writeLines("for(i in 1:length(entriesToProcessThisCPU)){",runFileR)
    if(length(list(...))==0){
      writeLines(paste("results[[names(entriesToProcessThisCPU)[i]]]<-try(fun(entriesToProcessThisCPU[[i]]))",sep=""),runFileR)
    }else{
      writeLines(paste("results[[names(entriesToProcessThisCPU)[i]]]<-try(fun(entriesToProcessThisCPU[[i]],",paste(paste(names(list(...)),"=",names(list(...)),sep=""),collapse=", "),"))",sep=""),runFileR)
    }
    writeLines("}",runFileR)
    writeLines(paste("save(results,file='",fullPathTempDir,s,"output_",process,".rdata')",sep=""),runFileR)
    close(runFileR)
    
    
    if(cluster=="jhpce"){
      runFileBash<-file(paste(tempDir,s,"runFileBash_",process,sep=""),"w")
      writeLines(paste("#$ -N ",jobName,"_",process,sep=""),runFileBash)
      writeLines(paste("#$ -l mem_free=",mem,"G,h_vmem=",mem,"G",sep=""),runFileBash)
      writeLines(paste("#$ -o ",tempDir,s,jobName,"_stdout_",process,sep=""),runFileBash)
      writeLines(paste("#$ -e ",tempDir,s,jobName,"_stderr_",process,sep=""),runFileBash)
      writeLines(paste("R CMD BATCH ",fullPathTempDir,s,"runFileR_",process, " ",fullPathTempDir,s,"runFileR_out_",process,sep=""),runFileBash)
      close(runFileBash)
      
      jobId<-system(paste("qsub ",tempDir,s,"runFileBash_",process,sep=""),intern=TRUE)
      jobIds<-c(jobIds,sub(" .+$","",sub("Your job ","",jobId)))
      
    }else if(cluster=="computerome"){
      runFileBash<-file(paste(tempDir,s,"runFileBash_",process,sep=""),"w")
      
      writeLines(paste("#PBS -W group_list=",group_list,sep=""),runFileBash)
      writeLines(paste("#PBS -N ",jobName,"_",process,sep=""),runFileBash)
      writeLines(paste("#PBS -o ",tempDir,s,jobName,"_stdout_",process,sep=""),runFileBash)
      writeLines(paste("#PBS -e ",tempDir,s,jobName,"_stderr_",process,sep=""),runFileBash)
      writeLines(paste("#PBS -l nodes=1:ppn=1,mem=",mem,"gb",sep=""),runFileBash)
      writeLines(paste("#PBS -l walltime=",walltime,sep=""),runFileBash)
      writeLines(paste("module load ngs",sep=""),runFileBash)
      writeLines(paste("module load R",sep=""),runFileBash)
      # print("WARNING- submitting with R-dev. Use another myfunctions if this is not ok.")
      writeLines(paste("R CMD BATCH ",fullPathTempDir,s,"runFileR_",process, " ",fullPathTempDir,s,"runFileR_out_",process,sep=""),runFileBash)
      close(runFileBash)
      jobId<-system(paste("qsub ",tempDir,s,"runFileBash_",process,sep=""),intern=TRUE)
      jobIds<-c(jobIds,sub("\\.risoe.+$","",jobId))
      
    }else{stop("Very odd")}
    if(waitTime>0){
      if(verbose>3)print(paste("Waiting",waitTime,"seconds before submitting next job"))
      Sys.sleep(waitTime)
    }
  }
  
  
  #checking when the runs are finished
  notReady=TRUE		
  if(cluster=="jhpce"){		 
    while(notReady){
      qstat <- suppressWarnings(structure(as.data.frame(do.call(rbind,strsplit(system("qstat",intern=TRUE)[-(1:2)]," +",perl=TRUE))),names=c("jobId","jobName","user","timeUse","S","queue","time","somename","somenumber")))
      if(!any(jobIds %in% qstat[,"jobId"])){
        notReady<-FALSE
      }else{
        Sys.sleep(20)
        if(verbose>1)print(paste("Still have",sum(jobIds %in% qstat[,"jobId"]),"running jobs"))
      }
    }
  }else if(cluster=="computerome"){
    while(notReady){
      qstat <- try(system(paste0("qstat -u ",user),intern=TRUE),silent=T)
      if(class(qstat)=="try-error" |  length(qstat)==0){
        print("Communication with log-in node failed. Re-trying in one minute")
        Sys.sleep(60)
        next
      }
      qstat<-qstat[(grep("^------------",qstat)+1):length(qstat)]
      qstat<-as.data.frame(do.call(rbind,strsplit(qstat," +",perl=TRUE)))
      colnames(qstat)<-c("jobId","user","type","jobName","SessID","NDS","TSK","memory","required time","status","elapsed time")	
      qstat<-qstat[!qstat[,"status"]%in%"C",]
      qstat[,"jobId"] <- sub("\\.risoe.+$","",qstat[,"jobId"])
      
      if(verbose>5){
        print(paste("jobIds:",paste(jobIds,collapse=", ")))
        print("Tail of qstat")
        print(tail(qstat))
        print("")
      }
      
      if(!any(jobIds %in% as.character(qstat[,"jobId"]))){
        notReady<-FALSE
      }else{
        Sys.sleep(20)
        if(verbose>1)print(paste("Still have",sum(jobIds %in% qstat[,"jobId"]),"running jobs"))
      }
    }
  }else{stop("Very odd")}
  
  
  #catching saved data files - or in case of error, the stderr and stdout
  output <- list()
  cleanExit<-TRUE
  for(process in names(namesToProcess)){
    outFile<-paste(tempDir,s,"output_",process,".rdata",sep="")
    #checking if outfile exists, otherwise loading stderr to present
    
    # 		#If we are debugging If it's the first process and the file is not detected, we wait a little longer
    # 		if(verbose>5){
    # 			if(which(names(namesToProcess)%in%process) == 1 & !file.exists(outFile)){
    # 				if(verbose>0)print(paste("Waiting 50 seconds extra to make sure this is not a computerome-lag related thing that output_XXX.rdata is not found"))
    # 				Sys.sleep(50)
    # 			}
    # 		}
    
    if(file.exists(outFile)){
      load(outFile)
      output <- c(output,results)
    }else{
      rFile<-file(paste(tempDir,s,"runFileR_out_",process,sep=""),"r")
      rout<-readLines(rFile)
      close(rFile)
      stdoutFile<-file(paste(tempDir,s,jobName,"_stdout_",process,sep=""),"r")
      stdout<-readLines(stdoutFile)
      close(stdoutFile)
      stderrFile<-file(paste(tempDir,s,jobName,"_stderr_",process,sep=""),"r")
      stderr<-readLines(stderrFile)
      close(stderrFile)
      output[[process]]<-list(stdout=stdout,stderr=stderr,rout=rout)
      cleanExit<-FALSE
      
      
      # if(output[["rout"]][length(output[["rout"]])-1]=="Error: error reading from connection"){
      # print("One could possible put in a fix regarding the computerome instability here")	
      # }
    }
  }
  
  #2015-03-31 addition after try-argument. Also check if there are try errors in the output.
  if("try-error"%in%sapply(output,class)){
    cleanExit<-FALSE
  }
  
  if(cleanExit){
    if(verbose>0)print("Script ran to completion without errors - removing temporary data folder and returning output")
    unlink(tempDir, recursive = TRUE)
  }else{
    print(paste("ERROR: not all R-scripts finished correctedly. Preserving temporary folder for inspection:",tempDir))
  }
  return(output[names(x)])
  
}




qsubKill<-function(user=NULL){
  if(is.null(user))stop("Must give user")
  if(class(user)!="character")stop("Must give user as character")
  if(length(user)!=1)stop("Must give user as a single character")
  qstat <- system(paste0("qstat -u ",user),intern=TRUE)
  qstat<-qstat[(grep("^------------",qstat)+1):length(qstat)]
  qstat<-as.data.frame(do.call(rbind,strsplit(qstat," +",perl=TRUE)))
  colnames(qstat)<-c("jobId","user","type","jobName","SessID","NDS","TSK","memory","required time","status","elapsed time")	
  qstat<-qstat[!qstat[,"status"]%in%"C",]
  qstat[,"jobId"] <- sub("\\.risoe.+$","",qstat[,"jobId"])
  for(job in qstat[,"jobId"]){
    system(paste("canceljob",job)	)
  }
  
}

