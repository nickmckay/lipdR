LiPD standard for neotoma conversion

Hierarchy synonyms (Lipd - Neotoma)
Dataset - Site
Paleodata - Collunit
Paleodata Measurement Table - Dataset
Paleodata Measurement Table individual rows - Sample/Datum

Dataset (Site) synonyms
geo$siteName - sitename
geo$elevation - altitude
geo$longitude + geo$latitude - geography
geo$description - description
archiveType - depositionalenvironment from first collunit
geo$location - geopolitical

Explicit object relationships
linkedChronData - {points to} - chronDataId
linkedPaleoData - paleoDataId
linkedMeasurementTable - measurementTableId
linkedModel - modelId
linkedSummaryTable - summaryTableId
linkedDistributionTable - distributionTableId
linkedEnsembleTable - ensembleTableId

Paleodata object: paleoDataId, linkedChronData
Paleodata measurement table: measurementTableId, linkedMeasurementTable, linkedModel, linkedSummaryTable, linkedDistributionTable, linkedEnsembleTable
Chrondata object: chronDataId, linkedPaleoData
Chrondata measurement table: measurementTableId, linkedMeasurementTable, linkedModel, linkedSummaryTable, linkedDistributionTable, linkedEnsembleTable


Paleodata

Paleodata (collunit) synonyms
Possible keys include (note: “datasets” and “chronologies” are Paleodata measurement tables and Chrondata): 


Neotoma key
Lipd key



1 - collectionunitid
2 - notes
3 - handle 
4 - colldate
5 - location               
6 - waterdepth
7 - gpslocation
8 - collunittype
9 - collectiondevice
10 - collectionunitname
11 - depositionalenvironment
12 - defaultchronology

1 - neotomaCollectionUnitId
2 - notes
3 - neotomaHandle
4 - collectionDate
5 - location
6 - waterDepth
7 - gpsLocation
8 - neotomaCollectionUnitType
9 - collectionDevice
10 - paleoDataName
11 - depositionalEnvironment
12 - linkedChronData





Paleodata Measurement Tables (dataset)
Possible keys include (note: “samples” are the individual rows of Paleodata measurement tables):



Neotoma key
Lipd key



1 - datasetid
2 - database
3 - datasettype
4 - datasetname 
5 - age_range_young
6 - notes
7 - doi
8 - age_range_old
9 - specimens
10 - pi_list
1 - measurementTableId
2 - neotomaDatabase
3 - neotomaDatasetType
4 - measurementTableName
5 - ageRangeYoung
6 - notes
7 - measurementTableDOI
8 - ageRangeOld
9 - specimens
10 - measurementTablePIList








Paleodata Measurement Table columns (sample)
Each row of the measurement table represents a neomtoma “sample”
“age” and “depth” are controlled terms in lipid, please use them in this exact spelling with all lower-case letters. Both can be used as column names in the measurement table, and at least one must be present
The sample metadata keys are used as column names with the corresponding values input as table values:


Neotoma key
Lipd key




1 - ages
2 - igsn
3 - sampleid
4 - thickness
5 - samplename
6 - sampleanalyst
7 - analysisunitid
8 - analysisunitname
9 - depth
1 - age
2 - igsn
3 - neotomaSampleId
4 - thickness
5 - neotomaSampleName
6 - neotomaSampleAnalyst
7 - neotomaAnalysisUnitId
8 - neotomaAnalysisUnitName
9 - depth



Sample datum data frames are restructured and housed as column names, column metadata, and row values


The datum “variablename” and “value”
Each “variablename” becomes a column name
The corresponding “value” is the value in the table

Lastly, the other datum keys have values which can vary across “variablename” but are invariant for a given “variablename”. These are given as metadata associated with the column header (eg. L2$paleoData[[1]]$measurementTable[[1]]$`Hypoestes-type aristata`$taxongroup = “vascular plants”)



Neotoma key
Lipd key




1 - taxonid
2 - symmetry
3 - taxongroup
4 - element
1 - neotomaTaxonId
2 - neotomaSymmetry
3 - neotomaTaxonGroup
4 - neotomaElement

5 - ecologicalgroup
6 - value
7 - context
8 - variablename
9 - units
5 - neotomaEcologicalGroup
6 - values
7 - neotomaContext
8 - variableName
9 - units




Chrondata
Chrondata synonyms
Lipid age-model metadata goes into the neotoma chronology table
chronData[[1]]$model[[1]]$methods (this is the methods associated with the sample ages)


Neotoma key
Lipd key

agemodel
modelagetype
method
units


chronData[[1]]$model[[1]]$summaryTable[[1]] (this is the sample ages)


Neotoma key
Lipd key

depth
age
NA
depth
Age
summaryTableId

Chrondata measurement tables
metadata (chronology)


Neotoma key
Lipd key



notes
chronologyId
contact
isdefault
dateprepared
chronologyname


notes
measurementTableId
contact
NA (auto-detected via linkages)
neotomaDatePrepared
neotomaChronologyname


Table (chroncontrols)
Metadata associated with these lipd keys will be ignored when converted to neotoma, only the values will be stored

Neotoma key
Lipd key




depth
age
error
thickness
labid

depth
age
error
thickness
labid



