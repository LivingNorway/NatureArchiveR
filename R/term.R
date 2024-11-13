### 1 **** CONTROLLED TERM METHODS ####

### 1.1 ==== Define a Controlled Term ====
#' @title Define a Controlled Term
#'
#' @description Function to help with the specification of controlled terms for
#' use as part of standardised vocabularies
#'
#' @param tname A character scalar containing the name of the controlled term.
#' @param tlabel An optional character scalar label to use for the term in
#' printing and descriptions. The default is to not use a label.
#' @param tIRI A character vector containing the Internationalized Resource
#' Identifier (IRI) or series of IRIs where the controlled term is defined. The
#' default is that the term does not have an IRI.
#' @param tdescription A character scalar containing a description of the term
#' and its usage. The default is that the term does not have a description.
#' @param texamples A character vector containing examples that can be used as
#' values associated with the term. The default is that the term does not have
#' any examples.
#' @param tdate An object of class \code{Date} (or an object that can be
#' converted to a \code{Date} object) that represents the date when the term was
#' defined.
#'
#' @return A \code{controlledTerm} object (derived from the base
#' \code{character} class) that additionally has the following attributes:
#' \describe{
#'  \item{\code{name}}{A character scalar containing the name of controlled
#'  term}
#'  \item{\code{label}}{A character scalar containing a label to use in
#'  printing and descriptions}
#'  \item{\code{IRI}}{A character containing the Internationalized Resource
#' Identifier (IRI) or series of IRIs where the controlled term is defined}
#'  \item{\code{description}}{A character scalar containing a description of the
#'  term and its usage}
#'  \item{\code{examples}}{A character vector containing examples that can be
#'  used as values associated with the term}
#'  \item{\code{date}}{An object of class \code{Date} that represents the date
#'  when the term was defined}
#' }
#'
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link{as.Date}}
#' @export
createControlledTerm <- function(
    tname,
    tlabel = as.character(NA),
    tIRI = as.character(NA),
    tdescription = as.character(NA),
    texamples = character(),
    tdate = NULL
) {
  outTerm <- tryCatch(as.character(tname), error = function(err) {
    stop("unable to produce controlled term: ", err)
  })
  if(length(outTerm) <= 0) {
    stop("unable to produce controlled term: zero-length object type")
  } else if(length(outTerm) > 1) {
    warning("input term has length greater than zero: only the first element will be used")
    outTerm <- outTerm[1]
  }
  if(is.na(outTerm) || outTerm == "") {
    stop("unable to produce controlled term: NA or empty string value given")
  }
  attr(outTerm, "label") <- tryCatch(as.character(tlabel), error = function(err) {
    stop("unable to produce controlled term: ", err)
  })
  attr(outTerm, "IRI") <- tryCatch(as.character(tIRI), error = function(err) {
    stop("unable to produce controlled term: ", err)
  })
  attr(outTerm, "description") <- tryCatch(as.character(tdescription), error = function(err) {
    stop("unable to produce controlled term: ", err)
  })
  attr(outTerm, "examples") <- tryCatch(as.character(texamples), error = function(err) {
    stop("unable to produce controlled term: ", err)
  })
  if(length(tdate) > 0) {
    attr(outTerm, "date") <- tryCatch(as.Date(tdate), error = function(err) {
      stop("unable to produce controlled term: ", err)
    })
  }
  class(outTerm) <- c("controlledTerm", class(outTerm))
  validate.controlledTerm(outTerm)
}

### 1.2 ==== Validate a Controlled Term ====
#' @title Validate a Controlled Term
#'
#' @description Function to check to see whether a controlled term object is
#' correctly defined and returns a version of the object that is properly
#' defined. This is usually used as an internal method.
#'
#' @param inTerm A \code{controlledTerm} object to be validated.
#'
#' @return A \code{controlledTerm} object (derived from the base
#' \code{character} class) that additionally has the following attributes:
#' \describe{
#'  \item{\code{name}}{A character scalar containing the name of controlled
#'  term}
#'  \item{\code{label}}{A character scalar containing a label to use in
#'  printing and descriptions}
#'  \item{\code{IRI}}{A character containing the Internationalized Resource
#' Identifier (IRI) or series of IRIs where the controlled term is defined}
#'  \item{\code{description}}{A character scalar containing a description of the
#'  term and its usage}
#'  \item{\code{examples}}{A character vector containing examples that can be
#'  used as values associated with the term}
#'  \item{\code{date}}{An object of class \code{Date} that represents the date
#'  when the term was defined}
#' }
#'
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link{createControlledTerm}}
#' @keywords internal
#' @export
validate.controlledTerm <- function(inTerm) {
  ### 1.2.1 ---- Sanity check the term itself ----
  outTerm <- inTerm
  if(!inherits(outTerm, "character")) {
    stop("invalid input object type")
  }
  outAttr <- attributes(outTerm)
  outTerm <- tryCatch(as.character(outTerm), error = function(err) {
    stop("invalid input object type: ", err)
  })
  if(length(outTerm) <= 0) {
    stop("term has zero length")
  } else if(length(outTerm) > 1) {
    warning("term has length greater than one: only the first element will be used")
    outTerm <- outTerm[1]
  }
  if(any(grepl("\\s+", outTerm, perl = TRUE))) {
    warning("term contains whitespace characters, this is not recommended")
  }
  ### 1.2.2 ---- Sanity check the term label ----
  if("label" %in% names(outAttr)) {
    outLabel <- tryCatch(as.character(outAttr[["label"]]), error = function(err) {
      stop("invalid input object type: ", err)
    })
    outLabel <- outLabel[!is.na(outLabel) & outLabel != ""]
    if(length(outLabel) <= 0) {
      outLabel <- as.character(NA)
    } else if(length(outLabel) > 1) {
      warning("term has a label with length greater than one: only the first element will be used")
      outLabel <- outLabel[1]
    }
    outAttr[["label"]] <- outLabel
  } else {
    outAttr <- append(outAttr, list(label = as.character(NA)))
  }
  ### 1.2.3 ---- Sanity check the term IRI ----
  if("IRI" %in% names(outAttr)) {
    outIRI <- tryCatch(as.character(outAttr[["IRI"]]), error = function(err) {
      stop("invalid input object type: ", err)
    })
    outIRI <- outIRI[!is.na(outIRI) & outIRI != ""]
    if(length(outIRI) <= 0) {
      outLabel <- as.character(NA)
    }
    outAttr[["IRI"]] <- outIRI
  } else {
    outAttr <- append(outAttr, list(IRI = as.character(NA)))
  }
  ### 1.2.4 ---- Sanity check the term description ----
  if("description" %in% names(outAttr)) {
    outDescription <- tryCatch(as.character(outAttr[["description"]]), error = function(err) {
      stop("invalid input object type: ", err)
    })
    outDescription <- outDescription[!is.na(outDescription) & outDescription != ""]
    if(length(outDescription) <= 0) {
      outDescription <- as.character(NA)
    } else if(length(outDescription) > 1) {
      warning("description entry has length greater than one: elements will be collapsed into one string")
      outDescription <- paste(outDescription, collapse = "\n")
    }
    outAttr[["desciption"]] <- outIRI
  } else {
    outAttr <- append(outAttr, list(description = as.character(NA)))
  }
  ### 1.2.5 ---- Sanity check the term examples ----
  if("examples" %in% names(outAttr)) {
    outExamples <- tryCatch(as.character(outAttr[["examples"]]), error = function(err) {
      stop("invalid input object type: ", err)
    })
    outExamples <- outExamples[!is.na(outExamples) & outExamples != ""]
    if(length(outExamples) <= 0) {
      outExamples <- character()
    }
    outAttr[["examples"]] <- outExamples
  } else {
    outAttr <- append(outAttr, list(examples = character()))
  }
  ### 1.2.6 ---- Sanity check the term implementation date ----
  if("date" %in% names(outAttr)) {
    outDate <- tryCatch(as.Date(outAttr[["date"]]), error = function(err) {
      stop("invalid input object type: ", err)
    })
    outDate <- outDate[!is.na(outDate)]
    if(length(outDate) <= 0) {
      outDate <- as.Date(c())
    } else if(length(outDate) > 1) {
      warning("implementation date has a length greater than one: only the first element will be used")
      outDate <- outDate[1]
    }
    outAttr[["date"]] <- outDate
  } else {
    outAttr <- append(outAttr, list(date = as.Date(c())))
  }
  ### 1.2.7 ---- Final setting of output attributes ----
  attributes(outTerm) <- outAttr
  if(!inherits(outTerm, "controlledTerm")) {
    class(outTerm) <- c("controlledTerm", class(outTerm))
  }
  outTerm
}

### 1.3 ==== Overloaded print method ====
#' @title Print a Controlled Term
#' 
#' @description Print a controlled term object to the console
#' @param x A \code{controlledTerm} object to print.
#' @param ... Ignored.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link{createControlledTerm}}
#' @export
print.controlledTerm <- function(x, ...) {
  valterm <- validate.controlledTerm(x)
  cat(as.character(valterm), 
    ifelse(is.na(attr(valterm, "label")), "", paste0(" (", attr(valterm, "label"), ")")), "\n",
    ifelse(is.na(attr(valterm, "description")), "", paste0(attr(valterm, "description"), "\n")),
    ifelse(is.na(attr(valterm, "IRI")), "", paste0("Defined in: ", attr(valterm, "IRI"), "\n")),
    sep = "")
  if(length(attr(valterm, "examples")) > 0) {
    cat("Examples: ", paste(attr(valterm, "examples"), collapse = "; "), "\n", sep = "")
  }
  invisible(valterm)
}

### 2 **** CONTROLLED VOCABULARY METHODS ####

### 2.1 ==== Define a Controlled Vocabulary ====
#' @title Define a Controlled Vocabulary
#'
#' @description Function to help with the specification of controlled
#' vocabularies for use to ensure data standardisation
#'
#' @param vocabname A character scalar containing the name of the controlled
#' vocabulary
#' @param termlist A list of \code{controlledTerm} objects (as created by the
#' \code{createControlledTerm} function) that represent the list of terms that
#' can be used in the controlled vocabulary. Alternatively this entry can be a
#' character vector and, in this situation, each element of the character vector
#' will be converted into a \code{controlledTerm} object.
#' @param vocabdef An optional character scalar containing the location
#' (possibly a URL or ISI) where the controlled vocabulary is defined. The
#' default is that is definition location is NA.
#' @param vocabdesc An optional character scalar containing a description of the
#' vocabulary and its intended purpose.
#'
#' @return A \code{controlledVocabulary} object that is a list of
#' \code{controlledTerm} objects that can be used and additionally has the
#' following attributes:
#' \describe{
#'  \item{\code{name}}{A character scalar containing the name of the controlled
#'  vocabulary}
#'  \item{\code{def}}{A character scalar containing the definition}
#'  \item{\code{description}}{A character scalar containing a description of the
#'  vocabulary and its intended usage}
#' }
#'
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link{createControlledTerm}}
#' @export
createControlledVocabulary <- function(vocabname, termlist, vocabdef = as.character(NA), vocabdesc = as.character(NA)) {
  ### 2.1.1 ---- Process the term list ----
  outList <- lapply(X = termlist, FUN = function(curElement) {
    outElement <- curElement
    if(!inherits(outElement, "controlledTerm")) {
      outElement <- tryCatch(createControlledTerm(as.character(outElement)), error = function(err) {
        stop("error encountered processing controlled term: ", err)
      })
      outElement
    }
    outElement
  })
  ### 2.1.2 ---- Validate and return the output ----
  attr(outList, "name") <- vocabname
  attr(outList, "def") <- vocabdef
  attr(outList, "description") <- vocabdesc
  class(outList) <- c("controlledVocabulary", class(outList))
  validate.controlledVocabulary(outList)
}

### 2.2 ==== Validate a Controlled Vocabulary ====
#' @title Validate a Controlled Vocabulary
#'
#' @description Function to check to see whether a controlled vocabulary object
#' is correctly defined and returns a version of the object that is properly
#' defined. This is usually used as an internal method.
#'
#' @param inVocab A \code{controlledVocabulary} object to be validated
#'
#' @return A \code{controlledVocabulary} object that is a list of
#' \code{controlledTerm} objects that can be used and additionally has the
#' following attributes:
#' \describe{
#'  \item{\code{name}}{A character scalar containing the name of the controlled
#'  vocabulary}
#'  \item{\code{def}}{A character scalar containing the definition}
#'  \item{\code{description}}{A character scalar containing a description of the
#'  vocabulary and its intended usage}
#' }
#'
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link{createControlledVocabulary}}
#' @keywords internal
#' @export
validate.controlledVocabulary <- function(inVocab) {
  ### 2.2.1 ---- Sanity check the term list ----
  outList <- tryCatch(as.list(inVocab), error = function(err) {
    stop("invalid vocabulary object: ", err)
  })
  if(length(outList) <= 0) {
    warning("vocabulary object is empty")
  }
  # Validate each of the elements
  outList <- lapply(X = outList, FUN = validate.controlledTerm)
  ### 2.2.2 ---- Sanity check the name attribute ----
  nameAttr <- tryCatch(as.character(attr(inVocab, "name")), error = function(err) {
    stop("invalid vocabulary name object type: ", err)
  })
  if(length(nameAttr) <= 0) {
    stop("invalid vocabulary name object type: vector has zero length")
  } else if(length(nameAttr) > 1) {
    warning("vocabulary name object has a length greater than one: only the first element will be used")
    nameAttr <- nameAttr[1]
  }
  if(is.na(nameAttr) || nameAttr == "") {
    stop("invalid vocabulary name object type: entry is NA or empty string")
  }
  attr(outList, "name") <- nameAttr
  ### 2.2.3 ---- Sanity check the def attribute ----
  defAttr <- tryCatch(as.character(attr(inVocab, "def")), error = function(err) {
    stop("invalid vocabulary definition object: ", err)
  })
  if(length(defAttr) <= 0) {
    defAttr <- as.character(NA)
  } else if(length(defAttr) > 1) {
    warning("vocabulary definition object has a length greater than one: only the first element will be used")
    defAttr <- defAttr[1]
  }
  if(is.na(defAttr) || defAttr == "") {
    defAttr <- as.character(NA)
  }
  attr(outList, "def") <- defAttr
  ### 2.2.4 ---- Sanity check the description attribute ----
  desAttr <- tryCatch(as.character(attr(inVocab, "description")), error = function(err) {
    stop("invalid vocabulary description object: ", err)
  })
  if(length(desAttr) <= 0) {
    desAttr <- as.character(NA)
  } else if(length(desAttr) > 1) {
    warning("description entry has length greater than one: elements will be collapsed into one string")
    desAttr <- paste(desAttr, collapse = "\n")
  }
  if(is.na(desAttr) || desAttr == "") {
    desAttr <- as.character(NA)
  }
  attr(outList, "description") <- desAttr
  ### 2.2.5 ---- Package the validated output ----
  if(!inherits(outList, "controlledVocabulary")) {
    class(outList) <- c("controlledVocabulary", class(outList))
  }
  names(outList) <- sapply(X = outList, FUN = function(curElement) {
    as.character(curElement)
  })
  outList
}

### 2.3 ==== Overloaded print method ====
#' @title Print a Controlled Vocabulary
#' 
#' @description Print a controlled vocabulary object to the console
#' @param x A \code{controlledVocabulary} object to print
#' @param ... Ignored
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link{createControlledVocabulary}}
#' @export
print.controlledVocabulary <- function(x, ...) {
  # Print the vocabulary information
  valvocab <- validate.controlledVocabulary(x)
  cat("VOCABULARY: ", toupper(attr(valvocab, "name")), "\n", sep = "")
  if(!is.na(attr(valvocab, "def"))) {
    cat("DEFINITION: ", attr(valvocab, "def"), "\n", sep = "")
  }
  if(!is.na(attr(valvocab, "description"))) {
    cat("DESCRIPTION: ", attr(valvocab, "description"), "\n", sep = "")
  }
  cat("\n")
  # Print the list of terms
  print(names(valvocab))
  invisible(valvocab)
}

### 2.4 ==== Retrieve a GBIF vocabulary ====
#' @title Retrieve a GBIF vocabulary
#'
#' @description Retrieve a \code{controlledVocabulary} object (or list of
#' objects) corresponding to a vocabulary established as a registered extension
#' used by the Global Biodiversity Information Facility (GBIF)
#' 
#' @param exname A character scalar giving the name of the registered extension
#' for which you wish to retrieve the \code{controlledVocabulary} object for. A
#' value of \code{NA} (the default) indicates that the user wishes to get a list
#' containing each \code{controlledVocabulary} object for every supported
#' registered extension.
#' 
#' @return Either a \code{controlledVocabulary} object corresponding to the
#' requested vocabulary or, if \code{exname} is \code{NA}, a list of
#' \code{controlledVocabulary} objects corresponding to each supported
#' registered extension.
#' 
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#' @seealso \code{\link{createControlledVocabulary}}
#' @export
retrieveGBIFVocabulary <- function(exname = as.character(NA)) {
  ### 2.4.1 ---- Sanity check the input parameter ----
  inexname <- tryCatch(as.character(exname), error = function(err) {
    stop("vocabulary specification is not a valid object type: ", err)
  })
  if(length(inexname) <= 0) {
    inexname <- as.character(NA)
  } else if(length(inexname) > 1) {
    warning("vocabulary specification has a length greater than one: only the first element will be used")
    inexname <- inexname[1]
  }
  if(is.na(inexname) || inexname == "") {
    inexname <- as.character(NA)
  }
  ### 2.4.2 ---- Retrieve the requested vocabulary ----
  outVal <- gbifVocabularies
  if(!is.na(inexname)) {
    if(inexname %in% names(outVal)) {
      outVal <- outVal[[inexname]]
    } else {
      stop(inexname, " is not a valid GBIF registered extension vocabulary")
    }
  }
  outVal
}