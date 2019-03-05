# Part of the SMITIDstruct R package.
# Copyright (C) 2018 Jean-François Rey <jean-francois.rey@inra.fr>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,i
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

## TODO add to documentation, set as global variable in global env pkg
CODE_MOVE <- "000001"
CODE_STATES <- "000010"
CODE_SOURCES <- "000100"
CODE_OFFSPRINGS <- "001000"
CODE_OBS <- "010000"
CODE_COVAR <- "100000"

#' createIndex
#' @description create an index of time id_host and event code
#' @param hostlist a Hostset 
#' @return a data.frame with TIME, ID_HOST and EVENTCODE as columns
#' @include Class-Host.R Class-ViralPop.R
#' @export
createIndex <- function(hostlist) {
  
  df <- data.frame(matrix(ncol=3,nrow=0))
  colnames(df) <- c("TIME","ID_HOST", "EVENTCODE")
  
  for(hid in 1:length(hostlist)) {
    #print(hostlist[[hid]]@coordinates)
    apply(st_coordinates(hostlist[[hid]]@coordinates),1, function(x){df <<- addIndex(df,hid,x["M"],CODE_MOVE)})
    lapply(hostlist[[hid]]@states$time, function(x){ df <<- addIndex(df,hid,x,CODE_STATES)})
    lapply(hostlist[[hid]]@sources$time, function(x){ df <<- addIndex(df,hid,x,CODE_SOURCES)})
    lapply(hostlist[[hid]]@offsprings$time, function(x){ df <<- addIndex(df,hid,x,CODE_OFFSPRINGS)})
    lapply(hostlist[[hid]]@ID_V_POP$time, function(x){ df <<- addIndex(df,hid,x,CODE_OBS)})
    lapply(hostlist[[hid]]@covariates$time, function(x){ df <<- addIndex(df,hid,x,CODE_COVAR)})
  }
  return(df)
}

#' addIndex 
#' @description add to an index a new eventcode
#' @param index an index
#' @param id_host an host index in HostSet
#' @param time a time
#' @param code an event code
#' @return the index updated (add a row or update one)
addIndex <- function(index, id_host, time, code) {
  
  ev <- index[which(index$TIME == time & index$ID_HOST == id_host),]
  
  if(!is.null(ev) & nrow(ev) != 0) {
    newcode <- addcode(code,ev$EVENTCODE)
    index[ which(index$TIME == time & index$ID_HOST == id_host),]$EVENTCODE <- newcode 
  }
  else {
    index[nrow(index) + 1,] = c(time,id_host,code)
  }
  
  return(index)
  
  # if( sum(index$TIME == time) ) {
  #   indexT <- index[index$TIME == time]
  #   if( indexT$ID_HOST == id_host & index$ == id_pop )
  #   } 
  # 
}

#' addcode 
#' @description add a code event to an another
#' @param code an existing code
#' @param code.add the code to add
#' @return merge of the two code
addcode <- function(code,code.add) {
  pos <- regexpr("1",code.add)[1]
  return( paste(substr(code,1,pos-1), "1", substr(code,pos+1,nchar(code)), sep="") )
}

#' mergeCode
#' @description merge a list of event code
#' @param listcode a list of event code*
#' @return a code
#' @export
mergeCode <- function(listcode) {
  newcode <- "000000"
  for(i in 1:length(listcode)) {
    newcode <- addcode(newcode,listcode[i])
  }
  return(newcode)
}
