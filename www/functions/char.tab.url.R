####################################
# author: Hcliu
# name: char.tab.url.R
# created date: 2026/03/24
# revise date: 
# desciption: Converts raw database IDs in the characteristic table (char.tab) into clickable HTML anchor tags for external databases including LION, LIPID MAPS, SwissLipids, HMDB, ChEBI, KEGG, PubChem, MetaNetX, and PlantFA.
# input: char.tab (data frame containing lipid characteristic annotations with database ID columns)
# output: char.tab with database ID columns replaced by HTML hyperlink strings

####################################

char.tab.url <- function(char.tab){
  .url <- function(ID, type){
    if(is.na(ID)){
      return(NA)
    }else{
      url <- switch(type,
                    LION='https://bioportal.bioontology.org/ontologies/LION/?p=classes&conceptid=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2F',
                    LIPIDMAPS='https://www.lipidmaps.org/databases/lmsd/',
                    SwissLipids='https://www.swisslipids.org/#/entity/',
                    HMDB='https://hmdb.ca/metabolites/',
                    ChEBI='https://www.ebi.ac.uk/chebi/searchId.do?chebiId=',
                    KEGG='https://www.kegg.jp/entry/',
                    PubChem='https://pubchem.ncbi.nlm.nih.gov/compound/',
                    MetaNetX='https://www.metanetx.org/chem_info/',
                    PlantFA='https://plantfadb.org/fatty_acids/')
      if(grepl('\\|', ID)){ ID <- unlist(stringr::str_split(ID,'\\|')) }
      herf <- paste0(paste0('<a href="', url, ifelse(type == 'LION', gsub(':','_',ID),ID), 
                            '" target="_blank" style="color: blue;text-decoration: underline;">',ID,'</a>'), collapse='|')
      return(herf)
    }
  }
  for(i in 1:nrow(char.tab)){
    char.tab$LION.ID[i] <- .url(char.tab$LION.ID[i],type='LION')
    char.tab$LIPID.MAPS.ID[i] <- .url(char.tab$LIPID.MAPS.ID[i],type='LIPIDMAPS')
    char.tab$SwissLipids.ID[i] <- .url(char.tab$SwissLipids.ID[i],type='SwissLipids')
    char.tab$HMDB.ID[i] <- .url(char.tab$HMDB.ID[i],type='HMDB')
    char.tab$ChEBI.ID[i] <- .url(char.tab$ChEBI.ID[i],type='ChEBI')
    char.tab$KEGG.ID[i] <- .url(char.tab$KEGG.ID[i],type='KEGG')
    char.tab$PubChem.CID[i] <- .url(char.tab$PubChem.CID[i],type='PubChem')
    char.tab$MetaNetX.ID[i] <- .url(char.tab$MetaNetX.ID[i],type='MetaNetX')
    char.tab$PlantFA.ID[i] <- .url(char.tab$PlantFA.ID[i],type='PlantFA')
  }
  return(char.tab)
}