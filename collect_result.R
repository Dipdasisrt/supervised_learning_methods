rm(list = ls())
setwd("/home/das70453/thesis/simulation")
n.sim=c(1000, 2000, 5000, 10000)



result=c()
for (n in n.sim){
  for (i in 1:10) {
    filename <- paste("./data/n", n, "_Seed", i, ".txt", sep="")
    
		if (file.exists(filename)) {
		  tryCatch({
			  is = c(is, i)
			  df = read.table(filename, sep = " ")
        df$n=n
			  result=rbind(result,df)
		  }, error = function(e) {
      })
	  }
    }
}
rownames(result)=NULL

result


	# write.table(re,savefile,col.names = TRUE,row.names = F)\n
	#write.table(Ghat.re,Ghat.savefile,col.names = c(1:6),row.names = F)\n

#rownames(output)=NULL
#write.table(output,"dynamic_probit_output.txt",col.names = TRUE) 
#write.xlsx(output,"wrong_model_output.xlsx") 


