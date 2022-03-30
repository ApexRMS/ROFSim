farNorthLandcover<-function(lTab,omitRare=NULL){
  library(RColorBrewer)
  lTab$type=NA
  
  unique(lTab$Label)
  #evergreens - 
  evergreens = subset(lTab,grepl("Coniferous",lTab$Label)|grepl("Treed Fen",lTab$Label)|grepl("Treed Bog",lTab$Label)|grepl("Treed Swamp",lTab$Label))
  evergreens = evergreens[order(-evergreens$ID),]
  evergreenCols = brewer.pal(nrow(evergreens),name="Greens")
  evergreenCols = col2rgb(evergreenCols)
  #evergreenCols= evergreenCols[,-1]
  evergreens$RGB1=evergreenCols[1,]
  evergreens$RGB2=evergreenCols[2,]
  evergreens$RGB3=evergreenCols[3,]
  evergreens$type="Conifer Treed"
  evergreens$type[grepl("Swamp",evergreens$Label)|grepl("Fen",evergreens$Label)|grepl("Bog",evergreens$Label)]="conifer sparse wet"

  lTab
  #mixed
  mix = subset(lTab,grepl("Mixed",lTab$Label)|grepl("Sparse Treed",lTab$Label,fixed=T))
  mix = mix[order(-mix$ID),]
  mixCols = brewer.pal(nrow(mix)+1,name="Oranges")
  mixCols = col2rgb(mixCols)
  mixCols=mixCols[,-1]
  mix$RGB1=mixCols[1,]
  mix$RGB2=mixCols[2,]
  mix$RGB3=mixCols[3,]
  mix$type="Mixed Treed"
  mix$type[grepl("Sparse Treed",mix$Label,fixed=T)]="mixed sparse"
  
  #decid
  decid = subset(lTab,grepl("Deciduous",lTab$Label))
  decid = decid[order(decid$ID),]
  decidCols = brewer.pal(nrow(decid)+1,name="Purples")
  decidCols = col2rgb(decidCols)
  decidCols=decidCols[,-1]
  decid$RGB1=decidCols[1,]
  decid$RGB2=decidCols[2,]
  decid$RGB3=decidCols[3,]
  decid$type="Deciduous Treed"
  decid$type[grepl("Swamp",decid$Label)]="deciduous sparse wet"
  
  if(is.null(omitRare)){
    rare = subset(lTab,is.element(ID,"fred"))
  }else{
    #rare
    rare = subset(lTab,is.element(ID,omitRare$ID))
    if(nrow(rare)>0){
      rare$RGB1 = 35
      rare$RGB2 = 35
      rare$RGB3 = 35
      rare$type="rare"
    }
  }
  
  #wet
  wet = subset(lTab,grepl("Water",lTab$Label)|grepl("tidal",lTab$Label)|grepl("water",lTab$Label)|grepl("Thicket Swamp",lTab$Label)|grepl("Open",lTab$Label))
  wet = wet[order(wet$ID),]
  wetCols = brewer.pal(nrow(wet),name="Blues")
  wetCols = col2rgb(wetCols)
  wet$RGB1=wetCols[1,]
  wet$RGB2=wetCols[2,]
  wet$RGB3=wetCols[3,]
  wet$type="wet"
  
  crops = subset(lTab,grepl("Heath",lTab$Label,fixed=T)|grepl("Agriculture",lTab$Label,fixed=T)|grepl("Sand",lTab$Label,fixed=T)|grepl("Infrastructure",lTab$Label,fixed=T)|grepl("Bedrock",lTab$Label,fixed=T))
  crops = crops[order(crops$ID),]
  cropCols = brewer.pal(nrow(crops)+2,name="Greys")
  cropCols = col2rgb(cropCols)
  cropCols=cropCols[,-1]
  cropCols=cropCols[,-1]
  crops$RGB1=cropCols[1,]
  crops$RGB2=cropCols[2,]
  crops$RGB3=cropCols[3,]
  crops$type="human/rock"
  
  combo = rbind(evergreens,mix,decid,rare,wet,crops)
  remainder = subset(lTab,!is.element(ID,combo$ID))
  remainder
  
  combo = rbind(evergreens,mix,decid,rare,wet,crops)
  
  remainder = subset(lTab,!is.element(ID,combo$ID))
  remainder
  remainder$RGB1[grepl("Disturbance",remainder$Label)] = 197
  remainder$RGB2[grepl("Disturbance",remainder$Label)] = 27
  remainder$RGB3[grepl("Disturbance",remainder$Label)] = 138
  remainder$type[grepl("Disturbance",remainder$Label)] = "young"
  
  remainder$type[remainder$Label=="No Data"]=NA
  remainder$type[remainder$Label=="Cloud/Shadow"]=NA
  remainder$type[remainder$Label=="Other"]=NA
  
  combo = rbind(remainder,evergreens,mix,decid,wet,crops,rare)
  return(combo)
  
}

provincialLandcover<-function(lTab,omitRare=NULL){
  #evergreens -
  evergreens = subset(lTab,is.element(Label,c("Coniferous Forest","Coniferous Swamp")))
  evergreens = evergreens[order(-evergreens$ID),]
  evergreenCols = brewer.pal(nrow(evergreens)+1,name="Greens")
  evergreenCols = col2rgb(evergreenCols)
  evergreenCols= evergreenCols[,-1]
  evergreens$RGB1=evergreenCols[1,]
  evergreens$RGB2=evergreenCols[2,]
  evergreens$RGB3=evergreenCols[3,]
  #evergreens$RGB1=0
  #evergreens$RGB2=109
  #evergreens$RGB3=44
  
  lTab
  #mixed
  mix = subset(lTab,is.element(Label,c("Mixed Forest","Sparse Forest","Treed Fen","Tree Bog")))
  mix = mix[order(-mix$ID),]
  mixCols = brewer.pal(nrow(mix)+1,name="Oranges")
  mixCols = col2rgb(mixCols)
  mixCols=mixCols[,-1]
  mix$RGB1=mixCols[1,]
  mix$RGB2=mixCols[2,]
  mix$RGB3=mixCols[3,]
  
  #decid
  decid = subset(lTab,is.element(Label,c("Deciduous Forest","Deciduous Swamp")))
  decid = decid[order(-decid$ID),]
  decidCols = brewer.pal(nrow(decid)+1,name="Purples")
  decidCols = col2rgb(decidCols)
  decidCols=decidCols[,-1]
  decid$RGB1=decidCols[1,]
  decid$RGB2=decidCols[2,]
  decid$RGB3=decidCols[3,]
  
  combo = rbind(evergreens,mix,decid)
  
  lTab
  #wet
  wet = subset(lTab,is.element(Label,c("Water - Deep or Clear","Water - Shallow or Sedimented","Mudflats"))|grepl("Marsh",Label)|grepl("Swamp",Label)|grepl("Fen",Label)|grepl("Bog",Label))
  wet=subset(wet,!is.element(ID,combo$ID))
  wet = wet[order(-wet$ID),]
  wetCols = brewer.pal(nrow(wet)+1,name="Blues")
  wetCols = col2rgb(wetCols)
  wetCols=wetCols[,-1]
  wet$RGB1=wetCols[1,]
  wet$RGB2=wetCols[2,]
  wet$RGB3=wetCols[3,]
  
  
  combo = rbind(evergreens,mix,decid,wet)
  
  remainder = subset(lTab,!is.element(ID,combo$ID))
  remainder
  
  #dryLow = subset(remainder,!grepl("No Data",remainder$Label,fixed=T)&!grepl("Burns",remainder$Label,fixed=T)&!grepl("Regenerating Depletion",remainder$Label,fixed=T)&!grepl("Cloud and Shadow",remainder$Label,fixed=T)&!grepl("Other",remainder$Label,fixed=T)&!grepl("Tundra Heath",remainder$Label,fixed=T))
  #dryLow = dryLow[order(dryLow$ID),]
  #dryCols = brewer.pal(nrow(dryLow)+1,name="Greys")
  #dryCols = col2rgb(dryCols)
  #dryCols=dryCols[,-1]
  #dryLow$RGB1=dryCols[1,]
  #dryLow$RGB2=dryCols[2,]
  #dryLow$RGB3=dryCols[3,]
  
  combo = rbind(evergreens,mix,decid,wet)
  
  remainder = subset(lTab,!is.element(ID,combo$ID))
  remainder
  remainder$RGB1 = 0
  remainder$RGB2 = 0
  remainder$RGB3 = 0
  
  remainder$RGB1[grepl("Burns",remainder$Label)] = 197
  remainder$RGB2[grepl("Burns",remainder$Label)] = 27
  remainder$RGB3[grepl("Burns",remainder$Label)] = 138
  
  remainder$RGB1[grepl("Regenerating Depletion",remainder$Label)] = 250
  remainder$RGB2[grepl("Regenerating Depletion",remainder$Label)] = 159
  remainder$RGB3[grepl("Regenerating Depletion",remainder$Label)] = 181
  
  
  combo = rbind(evergreens,mix,decid,wet,remainder)
  return(combo)
}

nationalLandcover<-function(lTab,omitRare=NULL){
  library(RColorBrewer)
  lTab$type=NA
  #evergreens - 
  evergreens = subset(lTab,(grepl("evergreen",lTab$Label)|grepl("Evergreen",lTab$Label))&!grepl("Mixed",lTab$Label))
  evergreens = evergreens[order(-evergreens$ID),]
  evergreenCols = brewer.pal(nrow(evergreens),name="Greens")
  evergreenCols = col2rgb(evergreenCols)
  #evergreenCols= evergreenCols[,-1]
  evergreens$RGB1=evergreenCols[1,]
  evergreens$RGB2=evergreenCols[2,]
  evergreens$RGB3=evergreenCols[3,]
  evergreens$RGB1[grepl("Sparse NL",evergreens$Label,fixed=T)]=255
  evergreens$RGB2[grepl("Sparse NL",evergreens$Label,fixed=T)]=255
  evergreens$RGB3[grepl("Sparse NL",evergreens$Label,fixed=T)]=255
  evergreens$type[grepl("closed",evergreens$Label)]="conifer dense"
  evergreens$type[grepl("medium",evergreens$Label)]="conifer open"
  evergreens$type[grepl("Sparse",evergreens$Label)|grepl("low",evergreens$Label)]="conifer sparse"
  
  lTab
  #mixed
  mix = subset(lTab,grepl("Mixed",lTab$Label))
  mix = mix[order(-mix$ID),]
  mixCols = brewer.pal(nrow(mix)+1,name="Oranges")
  mixCols = col2rgb(mixCols)
  mixCols=mixCols[,-1]
  mix$RGB1=mixCols[1,]
  mix$RGB2=mixCols[2,]
  mix$RGB3=mixCols[3,]
  mix$type="mixed sparse"
  mix$type[grepl("closed",mix$Label)]="mixed dense"

  #decid
  decid = subset(lTab,grepl("deciduous",lTab$Label)&!(grepl("mixed",lTab$Label)|grepl("Mixed",lTab$Label)))
  decid = decid[order(decid$ID),]
  decidCols = brewer.pal(nrow(decid)+1,name="Purples")
  decidCols = col2rgb(decidCols)
  decidCols=decidCols[,-1]
  decid$RGB1=decidCols[1,]
  decid$RGB2=decidCols[2,]
  decid$RGB3=decidCols[3,]
  decid$type="deciduous"
  decid$type="decid sparse"
  decid$type[grepl("closed",decid$Label)]="decid dense"
  
  if(is.null(omitRare)){
    rare = subset(lTab,is.element(ID,"fred"))
  }else{
    #rare
    rare = subset(lTab,is.element(ID,omitRare$ID))
    if(nrow(rare)>0){
      rare$RGB1 = 35
      rare$RGB2 = 35
      rare$RGB3 = 35
      rare$type="rare"
    }
  }
  
  #wet
  wet = subset(lTab,grepl("water",lTab$Label)|grepl("Water",lTab$Label)|grepl("wet",lTab$Label)|grepl("Wet",lTab$Label)|grepl("bog",lTab$Label))
  wet = wet[order(wet$ID),]
  wetCols = brewer.pal(nrow(wet)+1,name="Blues")
  wetCols = col2rgb(wetCols)
  wetCols=wetCols[,-1]
  wet$RGB1=wetCols[1,]
  wet$RGB2=wetCols[2,]
  wet$RGB3=wetCols[3,]
  wet$type="wet"
  
  crops = subset(lTab,grepl("Built",lTab$Label,fixed=T)|grepl("Grassland",lTab$Label,fixed=T)|grepl("cropland",lTab$Label,fixed=T)|grepl("Cropland",lTab$Label,fixed=T))
  cropCols = brewer.pal(5,name="Greys")
  cropCols = col2rgb(cropCols)
  crops$RGB1=cropCols[1,1]
  crops$RGB2=cropCols[2,1]
  crops$RGB3=cropCols[3,1]
  crops$type="human"

  rocks = subset(lTab,grepl("Rock",lTab$Label,fixed=T)|grepl("ice",lTab$Label,fixed=T))
  rocks$RGB1=cropCols[1,2]
  rocks$RGB2=cropCols[2,2]
  rocks$RGB3=cropCols[3,2]
  rocks$type="rock/ice"
  
  combo = rbind(evergreens,mix,decid,rare,wet,crops,rocks)
  remainder = subset(lTab,!is.element(ID,combo$ID))
  remainder
  
  dryLow = subset(remainder,!grepl("young",remainder$Label,fixed=T)&!grepl("burns",remainder$Label,fixed=T)&!grepl("No Data",remainder$Label,fixed=T))
  dryCols = cropCols
  dryLow$RGB1=dryCols[1,3]
  dryLow$RGB2=dryCols[2,3]
  dryLow$RGB3=dryCols[3,3]
  dryLow$type="lowVeg"  

  combo = rbind(evergreens,mix,decid,rare,wet,crops,rocks,dryLow)
  
  remainder = subset(lTab,!is.element(ID,combo$ID))
  remainder
  remainder$RGB1[grepl("burns",remainder$Label)] = 197
  remainder$RGB2[grepl("burns",remainder$Label)] = 27
  remainder$RGB3[grepl("burns",remainder$Label)] = 138
  
  remainder$RGB1[grepl("young",remainder$Label)] = 250
  remainder$RGB2[grepl("young",remainder$Label)] = 159
  remainder$RGB3[grepl("young",remainder$Label)] = 181
  remainder$type="young"
  remainder$type[remainder$Label=="No Data"]=NA
  
  combo = rbind(remainder,evergreens,mix,decid,wet,dryLow,crops,rocks,rare)
  return(combo)
  
}