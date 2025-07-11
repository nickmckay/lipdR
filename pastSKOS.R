#This file doesn't belong here.

# library("rdflib")
#
# PaST <- rdf_parse("C:/Users/dce25/Downloads/past-thesaurus.rdf")
#
# sparql <-
#   'PREFIX dc: <http://www.w3.org/2004/02/skos/core#>
#   SELECT ?a ?c
# WHERE { ?a dc:definition ?c .}'
# allDefs <- rdf_query(PaST, sparql)
# head(allDefs)
#
# sparql <-
#   'PREFIX dc: <http://www.w3.org/2004/02/skos/core#>
#   SELECT ?a ?c
# WHERE { ?a dc:prefLabel ?c .}'
# allPastNames <- rdf_query(PaST, sparql)
# head(allPastNames)
#
# variableName <- read.csv("C:/Users/dce25/Downloads/paleoData_variableName - vn.csv")
#
# nameRow <- rep(NA, length(variableName$pastId))
# countA <- 0
# for (i in variableName$pastId){
#   countA <- countA + 1
#   if (!is.na(as.numeric(i))){
#     nameRow[countA] <- which(i == unlist(lapply(list(allPastNames$a), function(x) sub('.+=(.+)', '\\1', x))))
#   }
# }
#
# variableName[1,]
# allPastNames[nameRow[1],]

