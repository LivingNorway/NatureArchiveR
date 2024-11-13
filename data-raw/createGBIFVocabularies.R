library(xml2)
source(file.path(usethis::proj_get(), "R", "term.R"))

### 1 **** INITIALISE INTERNAL DATA STORAGE ####

# Setup a temporary environment to hold the internal data elements
outputEnv <- new.env()
# Load the existing internal data into the temporary environment
outputFile <- file.path(usethis::proj_get(), "R", "sysdata.rda")
if(file.exists(outputFile)) {
  load(outputFile, envir = outputEnv, verbose = TRUE)
}

### 2 **** RETRIEVE LIST OF GBIF REGISTERED EXTENSIONS ****
### 2.1 ==== Retrieve the locations where the registered extensions are defined ====
# Lookup where the registered extensions information is kept and then retrieve
# the list of URLs where the vocabulary is defined
urlRegExtensions <- "https://rs.gbif.org/extensions.html"
urlsRegExtensionDefs <- gsub(
  "^\\s*href\\=\\\"", "",
  gsub(
    "\\\"$", "",
    as.character(xml_find_all(xml2::read_html(urlRegExtensions), "//div[@class='definition']/div[@class='title']/a/@href")),
    perl = TRUE),
  perl = TRUE)
### 2.2 ==== Function to create controlled vocabulary from GBIF XML definition ====
createControlledVocabularyFromGBIFXML <- function(xmlSpec) {
  # Read the high-level descriptive information about the registered extension
  inSpec <- xml2::read_xml(xmlSpec)
  inTitle <- xml2::xml_attr(inSpec, "title")
  inDescription <- xml2::xml_attr(inSpec, "description")
  # Retrieve the terms associated with that vocabulary
  inTermList <- lapply(X = xml2::xml_children(inSpec), FUN = function(curTerm) {
    createControlledTerm(
      tname = xml2::xml_attr(curTerm, "name"),
      tIRI = xml2::xml_attr(curTerm, "qualName"),
      tdescription = xml2::xml_attr(curTerm, "description"),
      texamples = strsplit(xml2::xml_attr(curTerm, "examples"), "\\s*,\\s*", perl = TRUE)[[1]]
    )
  })
  termNames <- sapply(X = inTermList, FUN = as.character)
  # Sometimes there may be duplicated terms in the definition.  In that case
  # affix a suffix to the relevant terms so that they can be uniquely mapped to
  # and also add a 'default' unadorned term that refers to the first defined
  # term in the list
  correctedDupTerms <- do.call(c, lapply(X = unique(termNames[duplicated(termNames)]), FUN = function(curDupName, termNames, inTermList) {
    dupTermSubset <- inTermList[termNames == curDupName]
    lapply(X = 1:length(dupTermSubset), FUN = function(curIndex, dupTermSubset) {
      newTerm <- paste(as.character(dupTermSubset[[curIndex]]), curIndex, sep = "_")
      attributes(newTerm) <- attributes(dupTermSubset[[curIndex]])
      class(newTerm) <- class(dupTermSubset[[curIndex]])
      newTerm
    }, dupTermSubset = dupTermSubset)
  }, termNames = termNames, inTermList = inTermList))
  inTermList <- c(inTermList[!duplicated(termNames)], correctedDupTerms)
  # Create the controlled vocabulary object from the list of terms
  createControlledVocabulary(vocabname = inTitle, termlist = inTermList, vocabdef = xmlSpec, vocabdesc = inDescription)
}
### 2.3 ==== Apply vocabulary function to generate internal object ====
outputEnv$gbifVocabularies <- stats::setNames(
  lapply(X = urlsRegExtensionDefs, FUN = createControlledVocabularyFromGBIFXML),
  sapply(X = urlsRegExtensionDefs, FUN = function(xmlSpec) {
    xml2::xml_attr(xml2::read_xml(xmlSpec), "name")
  })
)

### 3 **** SAVE THE INTERNAL DATA OBJECTS ####

# Save the internal data objects
eval(parse(text = paste0("usethis::use_data(", paste(names(outputEnv), collapse = ", "), ", internal = TRUE, overwrite = TRUE)")), envir = outputEnv)