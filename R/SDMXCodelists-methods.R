#' @name SDMXCodelists
#' @rdname SDMXCodelists
#' @aliases SDMXCodelists,SDMXCodelists-method
#' 
#' @usage
#' SDMXCodelists(xmlObj, namespaces)
#' 
#' @param xmlObj object of class "XMLInternalDocument derived from XML package
#' @param namespaces object of class "data.frame" given the list of namespace URIs
#' @return an object of class "SDMXCodelists"
#' 
#' @seealso \link{readSDMX}
#' @export
#' 
SDMXCodelists <- function(xmlObj, namespaces){
  new("SDMXCodelists",
      SDMX(xmlObj, namespaces),
      codelists = codelists.SDMXCodelists(xmlObj, namespaces)
  )
}

#get list of SDMXCodelist
#=======================
codelists.SDMXCodelists <- function(xmlObj, namespaces){
  
  codelists <- NULL
  
  messageNsString <- "message"
  if(isRegistryInterfaceEnvelope(xmlObj, FALSE)) messageNsString <- "registry"
  messageNs <- findNamespace(namespaces, messageNsString)
  strNs <- findNamespace(namespaces, "structure")
  
  sdmxVersion <- version.SDMXSchema(xmlObj, namespaces)
  VERSION.21 <- sdmxVersion == "2.1"
  
  codelistsXML <- NULL
  if(VERSION.21){
    codelistsXML <- getNodeSet(xmlObj,
                              "//mes:Structures/str:Codelists/str:Codelist",
                              namespaces = c(mes = as.character(messageNs),
                                             str = as.character(strNs)))
  }else{
    codelistsXML <- getNodeSet(xmlObj,
                              "//mes:CodeLists/str:CodeList",
                              namespaces = c(mes = as.character(messageNs),
                                             str = as.character(strNs)))
  }
  if(!is.null(codelistsXML)){
    codelists <- lapply(codelistsXML, SDMXCodelist, namespaces)
  }
  return(codelists)
}


#as.data.frame
#=============
#'@export
as.data.frame.SDMXCodelists <- function(x, ...,
                                       codelistId = NULL,
                                       ignore.empty.slots = TRUE){
  codelist <- NULL
  
  if(length(x@codelists) == 0){
    warning("No codelist found in SDMXCodelists object.")
    return(NULL)
  } else if (length(x@codelists) == 1){
    codelist <- x@codelists[[1]]
  } else if (length(x@codelists) > 1){
    if(is.null(codelistId)){
      warning("Using first codelist referenced in SDMXCodelists object: \n
               Specify 'codelistId' argument for a specific codelist")
      codelist <- x@codelists[[1]]
    }else{
      #TODO: codelist is only selected by id, codelist agency/version not looked at.
      #      This can lead to multiple matching codelists, putting a warning if multiple 
      #      codelists are found.
      counfcl_count = 0
      for(cl in x@codelists){
        
        if(cl@id == codelistId){
          codelist <- cl
          counfcl_count += 1
        }
      }
      if(counfcl_count > 1){
        warning("Multiple codelists with id '", codelistId, "' found in SDMXCodelists object. \n
                 Using last codelist referenced in SDMXCodelists object.")
      } elseif(counfcl_count == 0){
        warning("No codelist with id '", codelistId, "' found in SDMXCodelists object.")
        return(NULL)
      }
    }
  } else {
    stop("Unexpected number of codelists in SDMXCodelists object")
  }
 
  return(as.data.frame(codeList, ignore.empty.slots =ignore.empty.slots))
}

setAs("SDMXCodelists", "data.frame",
      function(from) as.data.frame.SDMXCodelists(from));
