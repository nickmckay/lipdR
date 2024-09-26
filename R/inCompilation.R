# checkfun <- function(cn,cv,compName,compVers){
#   if(is.list(cv)){
#     anycvm <- purrr::map_lgl(cv,function(x){any(x == compVers)})
#   }else{
#     cvm <- t(as.matrix(cv))
#     anycvm <- apply(cvm,1,function(x){any(x == compVers)})
#   }
#
#   # old code here:
#   # bothMatch <- (cn==compName & purrr::map_lgl(cv,function(x){any(x == compVers)}))
#   bothMatch <- (cn==compName & anycvm)
#
#   #put NAs back in for compName
#   incn <- which(is.na(cn))
#   bothMatch[incn] <- NA
#   out <- data.frame(bothMatch)
#   out <- setNames(out, createTSid())
#   return(out)
# }
#
#
# inThisCompilation <- function(TS,compName,compVers){
#   TS <- as.lipdTs(TS)
#   allNames <- sort(unique(unlist(sapply(TS,names))))#get all names in TS
#   #get all the names of the compilations
#   allComps <- allNames[grepl(pattern = "inCompilationBeta[0-9]+_compilationName",allNames)]
#   allVers <- allNames[grepl(pattern = "inCompilationBeta[0-9]+_compilationVersion",allNames)]
#
#   if(length(allComps) == 0){
#     return(matrix(NA,nrow = length(TS)))
#
#   }
#   allCompNames <- vector(mode = "list",length=length(allComps))
#   allCompVersions <- vector(mode = "list",length=length(allComps))
#
#   #get all the data
#   for(i in 1:length(allComps)){
#     allCompNames[[i]] <- pullTsVariable(TS,allComps[i])
#     allCompVersions[[i]] <- pullTsVariable(TS,allVers[i])
#   }
#
#   #check for each compilation
#   compCheck <- purrr::map2_dfc(allCompNames,allCompVersions,checkfun,compName,compVers)
#
#   #check across rows
#   unify <- as.matrix(apply(compCheck,1,any))
#
#   return(unify)
#
# }
