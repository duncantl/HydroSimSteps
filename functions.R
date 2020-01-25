mround <- function(x,base){ #this function ensures matching between stages
  base*round(x/base) 
}

Vdiscretizations=function(Vc,Vw){
  i=j=1
  z=0
  Vcoptions=length(Vc)
  Vwoptions=length(Vw)
  Voptions=matrix(0, nrow=3, ncol=Vcoptions*Vwoptions)
  for(i in 1:Vcoptions){
    for (j in 1:Vwoptions){
      VH=Vc[i]+Vw[j]
      z=z+1
      Voptions[1,z]=Vw[j]
      Voptions[2,z]=Vc[i]
      Voptions[3,z]=VH
    }
  }
  rownames(Voptions)=c("Vw","Vc", "V")
  return(Voptions)
}


MonthDays = c("January" = 31, "February" = 28, "March" = 31, "April" = 30,
              "May" = 31, "June" = 30, "July" = 31, 
              "August" = 31, "September" = 30, "October" = 31, 
              "November" = 30, "December" = 31)

finalinflow = finalinflowprep =
function(month, Q)    
    MonthDays[ as.character(month) ] * 1.98 * Q


TaLookup = TaLookupprep=function(month,p){
  Lookupy[which(Lookupy[,1]==month & Lookupy[,2]==p),4] #the max is just in case there are multiples 
}
#TaLookup=Vectorize(TaLookupprep)

QLookup = QLookupprep=function(month,p){ #this could be VC, Vc or Vcstates
  Qprep=Lookupy[which(Lookupy[,1]==month & Lookupy[,2]==p),3]
  as.numeric(Qprep)
}    
#QLookup=Vectorize(QLookupprep)

monthcounter=function(Stage){ #gives month per stage number
    monthlocation = rep(12, length(Stage))
    w = Stage%%12 != 0
    monthlocation[w] = Stage[w] - floor(Stage[w]/12)*12
    month.name[monthlocation] #starts with january
}
#gets climate season based on month
seasonbin=function(month){
  ifelse(month== "December" || month== "January", "winter", 
                ifelse(month== "February" || month== "March", "earlyspring", 
                       ifelse(month== "April", "spring", 
                              ifelse(month== "May" || month=="June" || month== "July", "summer", 
                                     ifelse(month== "August" || month== "September" || month=="October", "latesummer",
                                            ifelse(month== "November", "fall", 
                                                   "ERROR"))))))
}
#determines if lake is stratified or not based on month
lakeseasonbin=function(month){
  ifelse(month== "December" || month== "January", "winter", 
                    ifelse(month== "February" || month== "March", "earlyspring", 
                           ifelse(month== "April" || month== "May" || month== "June" || month== "July" ||month== "August" || month== "September" || month=="October", "stratified",
                                  ifelse(month== "November", "overturn", 
                                         "ERROR"))))
}



WinterDeltaVc = WinterDeltaVcprep=function(month,p){ 
  mround(wintercoeff[1]+wintercoeff[2]*as.numeric(TaLookup(month,p))+wintercoeff[3]*as.numeric(QLookup(month,p))+wintercoeff[4]*1.6, bin) 
  #round((0.855*Qin+0.264*TcLookup(VC,VW)+(0.253*0.12)*Qin+0.887)/2, digits=-6) #check eq and units
  
}
#WinterDeltaVc=Vectorize(WinterDeltaVcprep)


SpringDeltaVc = SpringDeltaVcprep=function(RcstarWinter,month, RC,p){
    tmp = springcoeff[1]+springcoeff[2]*RC+springcoeff[3]*as.numeric(TaLookup(month,p))+springcoeff[4]*as.numeric(QLookup(month,p))
    DeltaVc=ifelse(tmp < 0,0, tmp)
                 
  mround(DeltaVc#-1.5*10^6
                             ,bin)
}
#SpringDeltaVc=Vectorize(SpringDeltaVcprep)

ColdDeltaprep=function(month, RcstarWinter,RC,p){ 
  ifelse(lakeseasonbin(month)=="winter",WinterDeltaVc(month,p),
                 SpringDeltaVc(RcstarWinter,month,RC,p)) 
}
ColdDelta=Vectorize(ColdDeltaprep)

#####################
###including spill/transition states in model state and action spaces
#####################

#get discretizations with spill (remember to line up the index)
VRdiscretizations=function(VC,VW,RC,RW){
  i=j=k=l=1
  z=0
  allVRoptions=matrix(0, nrow=length(VC)*length(VW)*length(RC)*length(RW), ncol=4)
  for(i in 1:length(RW)){
    for (j in 1:length(RC)){
      for(k in 1:length(VW)){
        for(l in 1:length(VC)){
          z=z+1
          allVRoptions[z,1]=VC[l]
          allVRoptions[z,2]=VW[k]
          allVRoptions[z,3]=RC[j]
          allVRoptions[z,4]=RW[i]
        }
      }
    }
  }
  colnames(allVRoptions)=c("Vc","Vw", "Rc","Rw")
  return(allVRoptions)
}


MakingBins=function(ObservedLookupTable,observedVc,observedVw,Vc,Vw,L){
  Vcintervalbin=cut(as.numeric(observedVc),breaks=L, labels=Vc)
  Vwintervalbin=cut(as.numeric(observedVw),breaks=L, labels=Vw)
  allbinned=cbind(as.numeric(as.character(Vcintervalbin)),as.numeric(as.character(Vwintervalbin)),ObservedLookupTable)
  colnames(allbinned)[[1]]=c("Vcbin") 
  colnames(allbinned)[[2]]=c("Vwbin") 
  return(allbinned)
}


ReleaseTemp = ReleaseTempprep=function(VC,VW, RC, RW){
 # Tc=LookupVRTprep[(LookupVRTprep[,1]==VC) & (LookupVRTprep[,2]==VW) & (LookupVRTprep[,3]==RC) & (LookupVRTprep[,4]==RW),5]
#  Tw=LookupVRTprep[(LookupVRTprep[,1]==VC) & (LookupVRTprep[,2]==VW) & (LookupVRTprep[,3]==RC) & (LookupVRTprep[,4]==RW),6] 
  Tc=ifelse(VC>max(Vc), greaterTc,
            ifelse(VW>max(Vw), LookupTable[(LookupTable[,1]==VC) & (LookupTable[,2]==max(Vw)),3],
            LookupTable[(LookupTable[,1]==VC) & (LookupTable[,2]==VW),3]))
  Tw=ifelse(VW>max(Vw), greaterTw,
            ifelse(VC>max(Vc), LookupTable[(LookupTable[,1]==max(Vc)) & (LookupTable[,2]==VW),4],
            LookupTable[(LookupTable[,1]==VC) & (LookupTable[,2]==VW),4]))
  ifelse(Tc==0, Tw,
           ifelse(Tw==0, Tc,
                  (Tw*RW+Tc*RC)/(RC+RW)))
}
#ReleaseTemp=Vectorize(ReleaseTempprep)


ClrCk=function(RT){
    ans =  2.67491 + 0.95678*RT
    ans[ans < 30] = 0
    ans
}
Balls=function(RT){
    ans = 11.26688 + 0.81206*RT
    ans[ans < 30] = 0
    ans
}
Jelly=function(RT){
    ans = 7.74007 + 0.88836*RT
    ans[ans < 30] = 0
    ans
}
#Bend=ifelse(31.69574 + 0.41228*LookupVRT$ReleaseT <33, 0, #additional Bend influences?
 #     31.69574 + 0.41228*LookupVRT$ReleaseT)
RBDD=function(RT){
    ans = 15.8956 + 0.7529*RT
    ans[ans < 30] =  0
}


ConstantTempThreshold=56
ReturningAdults=64
EmbryoIncubation=55
Emergents=68
OutmigratingJuveniles=66

FishtempLookup = numeric()
FishtempLookup[ c("January", "February", "September", "October", "November", "December")] = OutmigratingJuveniles 
FishtempLookup[ c("March", "April") ] = ReturningAdults
FishtempLookup[ c("May", "June", "July") ] = EmbryoIncubation
FishtempLookup[ c("August") ] = Emergents

fishtemp = function(month)
  FishtempLookup[month]




stagepolicy=function(VSpace,NoofStages){
  holdingmatrix=matrix(0, nrow=length(Vstates), #ncol=length(ystates)*NoofStages) 
                       ncol=NoofStages)
  dim(holdingmatrix)=c(length(Vstates),#length(ystates), 
                       NoofStages)
  rownames(holdingmatrix)=Vstates
  #colnames(holdingmatrix)=ystates
  return(holdingmatrix)
}

OutgoingVcprep=function(S,VC, RcstarWinter,VW,RC,RW,p){ #put month in quotations, add O #all matrices
  month=monthcounter(S)
  sb = lakeseasonbin(month)
  ifelse(sb =="winter", VC+WinterDeltaVc(month,p)-RC,
                     ifelse(sb == "earlyspring", VC+SpringDeltaVc(RcstarWinter,month, RC,p)-RC, 
                            ifelse(sb == "stratified", VC-RC, 
                                   ifelse(sb == "overturn", VC+QLookup(month,p)-RC+VW-RW,
                                          "ERROR"))))
}

OutgoingVc=Vectorize(OutgoingVcprep)

OutgoingVwprep=function(S,VW,RW,p){ #put month in quotations
  month=monthcounter(S)
  sb = lakeseasonbin(month)
  ifelse(sb == "winter" || sb == "earlyspring" || sb == "overturn",0,
                ifelse(sb == "stratified", VW+QLookup(month,p)-RW,#+E 
                       #ifelse(lakeseasonbin(month)=="overturn", Vw-Rw, 
                       "ERROR"))
}
OutgoingVw=Vectorize(OutgoingVwprep)

benefit = benefitprep=function(Vc,Vw,Rc,Rw,month){
  RT=ReleaseTemp(Vc,Vw,Rc,Rw)
  #xrow=LookupVRTx[which(LookupVRTx[,1]==Vc & LookupVRTx[,2]==Vw & LookupVRTx[,3]==Rc & LookupVRTx[,4]==Rw),]
  tempthreshold=fishtemp(month)
  ifelse(RT==0, 0, #if release temp is 0
                       #ifelse(xrow$RBDD<tempthreshold,distance[6], 
                             # ifelse(xrow$Bend<tempthreshold,distance[6],
                                     ifelse(Jelly(RT)<tempthreshold,distance[5],   
                                            ifelse(Balls(RT)<tempthreshold, distance[4],
                                                   ifelse(ClrCk(RT)<tempthreshold, distance[3],
                                                          0))))#)#)
}

#benefit=Vectorize(benefitprep)

mixedsolve = mixedsolveprep=function(VW,VC,RC, RW, R, V, month, RcstarWinter, K, DP,p){ #when lake is mixed #matrices
  deltaVC=#ifelse(month=="February" || month=="March", deltaVC-springcoeff[2]*RC, WinterDeltaVc(month,p))
         ColdDelta(month, RcstarWinter,RC,p)
  AvailableVc=ifelse(VC+deltaVC<0,0,VC+deltaVC)
  ifelse(VW>0 || RW>0, -9999, #no warmpool
           ifelse(VC +deltaVC < RC, -9999, #consv of mass (not enough VC) 
                  ifelse(V + deltaVC- R < DP || 
                           V + deltaVC - R > K, -9999, #infrastructure limitations
                         benefit(AvailableVc,VW,RC,RW,month) #ifelse(Nmax< x, Nmax, x)
                  )))
}
#mixedsolve=Vectorize(mixedsolveprep)

springsolve = springsolveprep=function(VW,VC,RC, RW, R, V,K, DP,month,p){ #initial conditions are no warm 
  deltaVw=QLookup(month,p)
  AvailableVw = pmax(VW+deltaVw , 0)   #!!!  Vw rather than VW??

  ans = rep(-9999, length(VW))
  w = !( VW <= 0 | (V + deltaVw - R < DP | V + deltaVw - R > K) | (VC < RC | VW + deltaVw < RW, -9999) )
  ans[w] =  benefit(VC, AvailableVw, RC, RW, month)[w]
}

#springsolve=Vectorize(springsolveprep)

summersolve = summersolveprep=function(VW,VC,RC, RW, R, V,K, DP,month,p){ #initial conditions are no warm (i dont like this, want VW to organically come online)
  deltaVw=QLookup(month,p)
  tmp = VW + deltaVw
  AvailableVW=ifelse(tmp < 0, 0, tmp)
  ifelse(V + deltaVw - R < DP | V + deltaVw- R > K, -9999, #infrastructure limitations
            ifelse(VC < RC, -9999, #consv mass
                   ifelse(VW + deltaVw < RW, -9999,
                          benefit(VC,AvailableVW,RC,RW,month)  #ifelse(Nmax< x, Nmax, x)
                   )))
}

#summersolve=Vectorize(summersolveprep)

fallsolve = fallsolveprep=function(VW,VC,RC,RW,K,DP,month,p){ #initial conditions are no warm (i dont like this, want VW to organically come online)
    deltaVc=QLookup(month,p)+VW-RW
    tmp = VC+deltaVc
  AvailableVC=ifelse(tmp < 0, 0, tmp)
  ifelse(#RW > 0, -9999, #all cold at the end
         tmp - RC < DP || tmp - RC > K, -9999, #infrastructure limitations, no spill allowed
    ifelse(tmp < RC, -9999, #all water becomes cold #|| VW < RW, -9999, #consv mass
           ifelse(VW<RW,-9999,
                  benefit(AvailableVC,VW,RC,RW,month)  #ifelse(Nmax< x, Nmax, x)
           )))
}
# fallsolve=Vectorize(fallsolveprep)

choosesolveprep=function(month,VW,VC,RC, RW, R, V,RcstarWinter,K, DP,p){ #matrix
    sb = seasonbin(month)
    ifelse(sb =="winter" || sb =="earlyspring", mixedsolve(VW,VC,RC, RW, R, V, month, RcstarWinter,K, DP,p),
           ifelse( sb == "spring", springsolve(VW,VC,RC, RW, R, V,K, DP,month,p),
                  ifelse(sb =="summer" || sb =="latesummer" , summersolve(VW,VC,RC, RW, R, V,K, DP,month,p),
                         #ifelse(seasonbin(month)=="october", octobersolve(VW,VC,RC, RW, R, V,K, DP,month,p),
                           ifelse(sb == "fall", fallsolve(VW,VC,RC, RW, K, DP,month,p),
                                "ERROR"))))#)
} 

choosesolve=Vectorize(choosesolveprep)

###############
####b. accumulative obj function
#######################
accumulate=function(month,S,Vcstates,Vwstates,VcSpace,RcstarWinter,VwSpace,Rcspace,Rwspace,VSpace,Rspace,p){  #lookup f*t+1 
  fs = fstarvalue = matrix(0,nrow=length(Vstates),ncol=length(Rdecs))
  for(j in 1:length(Rdecs)){ 
    for(i in 1:length(Vstates)){ 
      fs[i,j]=ifelse(choosesolve(month,VwSpace[i,j],VcSpace[i,j],Rcspace[i,j], Rwspace[i,j], Rspace[i,j],VSpace[i,j],RcstarWinter,K, DP,p)<0,-9999, #remove infeasibles
                     which(Vcstates==OutgoingVc(S,VcSpace[i,j],RcstarWinter,VwSpace[i,j],Rcspace[i,j],Rwspace[i,j],p) & #,Vcstates)[i,j] & #this gives the location of Vcstates
                             Vwstates==OutgoingVw(S, VwSpace[i,j], Rwspace[i,j],p))) #match Vw and Vc in LookupV table #location of Vw states
      fstarvalue[i,j]=ifelse(is.na(fs[i,j]), -9999, ifelse(fs[i,j]<0, -9999, #remove infeasibles
                                                           fstar[fs[i,j],(S+1)])) #get the fstar from t+1 with the matching Vw and Vc states
    } 
  } 
  return(fstarvalue) #produces a matrix of fstartt+1 values to accumulate in the benefit function, looking backwards
}

firststageaccumulate=function(month, S, Vcstates,Vwstates,Vcinitial,RcstarWinter,Vwinitial,Rcdecs,Rwdecs,Rdecs, Vinitial,p){
    fs= fstarvalue = vector(length=length(Rdecs))

    for(j in 1:length(Rdecs)){#calculates f*t+1 from next stage
    fs[j]=ifelse(choosesolve(month,Vwinitial,Vcinitial,Rcdecs[j], Rwdecs[j], Rdecs[j],Vinitial,RcstarWinter,K, DP,p)<0,-9999, #remove infeasibles
                which(Vcstates==OutgoingVc(S,Vcinitial,RcstarWinter,Vwinitial,Rcdecs[j],Rwdecs[j],p) & #,Vcstates)[i,j] & 
                        Vwstates==OutgoingVw(S, Vwinitial, Rwdecs[j],p))) 
                #which(Vcstates==OutgoingVc(S,Vcinitial,RcstarWinter,Vwinitial,Rcdecs[j],Rwdecs[j],p) & #,Vcstates)[i,j] & 
                 #        Vwstates==OutgoingVw(S, Vwinitial, Rwdecs[j],p))) #match Vw and Vc in LookupV table
 
# }
    fstarvalue[j]=ifelse(is.na(fs[j]), -9999, ifelse(fs[j]<0, -9999, fstar[fs[j],(S+1)])) #get the fstar from t+1 with the matching Vw and Vc states
 #   print(test)
    }
  return(fstarvalue) #produces a matrix of fstartt+1 values to accumulate in the benefit function, looking backwards
}



