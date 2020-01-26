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


QLookup = function(month, p) 
    LookupyQ[ cbind(as.character(month), as.character(p)) ]

TaLookup = function(month, p) 
    LookupyTa[ cbind(as.character(month), as.character(p)) ]



monthcounter=function(Stage){ #gives month per stage number
    monthlocation = rep(12, length(Stage))
    w = Stage%%12 != 0
    monthlocation[w] = Stage[w] - floor(Stage[w]/12)*12
    month.name[monthlocation] #starts with january
}

#gets climate season based on month
SeasonbinLookup = character()
SeasonbinLookup[ c("December", "January")] = "winter"
SeasonbinLookup[ c("February", "March")] = "earlyspring"
SeasonbinLookup[ c("April")] = "spring"
SeasonbinLookup[ c("May", "June", "July")] = "summer"
SeasonbinLookup[ c("August", "September", "October")] = "latesummer"
SeasonbinLookup[ c("November")] = "fall"

seasonbin=
function(month)
  SeasonbinLookup[month]


#determines if lake is stratified or not based on month
LakeseasonbinLookup = character()
LakeseasonbinLookup[ c("December", "January")] = "winter"
LakeseasonbinLookup[ c("February", "March")] = "earlyspring"
LakeseasonbinLookup[ c("April", "May", "June", "July", "August", "September", "October")] = "stratified"
#LakeseasonbinLookup[ c()] = "latesummer"
LakeseasonbinLookup[ c("November")] = "overturn"

lakeseasonbin =
function(month)
  LakeseasonbinLookup[month]



WinterDeltaVc = WinterDeltaVcprep=function(month,p){ 
  mround(wintercoeff[1]+wintercoeff[2]*as.numeric(TaLookup(month,p))+wintercoeff[3]*as.numeric(QLookup(month,p))+wintercoeff[4]*1.6, bin) 
  #round((0.855*Qin+0.264*TcLookup(VC,VW)+(0.253*0.12)*Qin+0.887)/2, digits=-6) #check eq and units
  
}
#WinterDeltaVc=Vectorize(WinterDeltaVcprep)


SpringDeltaVc = SpringDeltaVcprep=function(RcstarWinter,month, RC,p){
    tmp = springcoeff[1]+springcoeff[2]*RC+springcoeff[3]*as.numeric(TaLookup(month,p))+springcoeff[4]*as.numeric(QLookup(month,p))
    DeltaVc = pmax(tmp, 0)
                 
    mround(DeltaVc#-1.5*10^6
                             ,bin)
}
#SpringDeltaVc=Vectorize(SpringDeltaVcprep)


# Vectorized version.  month and p are always scalars.
ColdDelta = ColdDeltaprep =
function(month, RcstarWinter, RC, p)
{ 
    if(lakeseasonbin(month)=="winter")
        rep(WinterDeltaVc(month, p), length(RC))
    else
        SpringDeltaVc(RcstarWinter, month, RC, p) 
}



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


ReleaseTemp =
  # Vectorized version using 2-D lookup table
  #  All arguments are vectors. 
  # RC and RW can be longer than VC and VW which can be scalars.
  # xx = unique(with(as.data.frame(argInfo$choosesolveNums), paste(month, VW, VC, RC, RW, R, V, RcstarWinter, K, DP, p, sep = ",")))
  # VC & VW have same length.
  # 
function(VC, VW, RC, RW)
{
    Tc = rep(greaterTc, length(VC))
    # VW is a scalar at least in some calls.
    if(length(VW) == 1)
        VW = rep(VW, length(VC))
    
    w = VC <= max(Vc)
    if(any(w)) {
        w2 = VW > max(Vw) # should we do this on VW[w] and do the subsetting differently.
        w3 = w & w2
        Tc[w3] = LookupTableTc[ cbind(as.character(VC[w3]), rep(as.character(max(Vw)), sum(w3))) ]
        w3 = w & !w2
        Tc[ w3 ] = LookupTableTc[ cbind(as.character(VC[w3]), as.character(VW[w3])) ]
    }

    Tw = rep(greaterTw, length(VW))
    w = VW <= max(Vw)
    if(any(w)) {
        w2 = VC > max(Vc)
        w3 = w & w2
        Tw[w3] = LookupTableTw[ cbind(rep(as.character(max(Vc)), sum(w3)), as.character(VW[w3])) ]
        w3 = w & !w2
        Tw[ w3 ] = LookupTableTw[ cbind(as.character(VC[w3]), as.character(VW[w3])) ]
    }    

    
    ans = Tc
    w = !is.na(Tc) & (Tc == 0)
    ans[w] = Tw[w]
    w = !w & !is.na(Tw) & Tw != 0 # Don't need the !is.na(Tw) here in our original solution.
    ans[w] = (Tw[w] * RW[w] + Tc[w] * RC[w])/(RC[w] + RW[w])
    ans   
}

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


# Need the prep version as it is called directly in the TOY2.R script.
OutgoingVc = OutgoingVcprep =
function(S,VC, RcstarWinter,VW,RC,RW,p)
{ 
    month = monthcounter(S)
    val = lakeseasonbin(month)
    
as.numeric(switch(val,
           "winter" = VC + WinterDeltaVc(month, p) - RC,
           "earlyspring" = VC + SpringDeltaVc(RcstarWinter, month, RC, p) - RC, 
           "stratified" = VC - RC, 
           "overturn" = VC + QLookup(month, p) - RC + VW - RW,
           { stop("problem in OutgoingVc"); rep(NA, length(RW))}))
}

OutgoingVw = OutgoingVwprep =
function(S, VW, RW, p)
{ 
    month = monthcounter(S)
    val = lakeseasonbin(month)
    if(val %in% c("winter", "earlyspring", "overturn"))
        rep(0, length(RW))
    else if(val  == "stratified")
        as.numeric(VW + QLookup(month, p) - RW)
    else {
        warning("OutgoingVw returning NAs")
        rep(NA, length(RW))
    }
}


# Vectorized.
benefit =
function(Vc,Vw,Rc,Rw,month)
{
  RT = ReleaseTemp(Vc, Vw, Rc, Rw)
  tempthreshold = fishtemp(month)

  ans = rep(0, length(Vc))
  w.na = is.na(RT) | RT == 0
  ans[w.na] = RT[w.na]
  if(any(w.na <- !w.na)) {
     idx = which(w.na)
     w1 = Jelly(RT[idx]) < tempthreshold
     ans[idx[w1]] = distance[5]
     idx = idx[!w1]
     w2 = Balls(RT[idx]) < tempthreshold
     ans[idx[w2]] = distance[4]
     idx = idx[!w2]
     w3 = ClrCk(RT[idx]) < tempthreshold
     ans[idx[w3]] = distance[3]
  }
  
  ans
}


mixedsolve = mixedsolveprep=function(VW,VC,RC, RW, R, V, month, RcstarWinter, K, DP,p){ #when lake is mixed #matrices
  deltaVC=#ifelse(month=="February" || month=="March", deltaVC-springcoeff[2]*RC, WinterDeltaVc(month,p))
         ColdDelta(month, RcstarWinter,RC,p)
  AvailableVc= pmax(VC+deltaVC, 0)

  # R = as.numeric(R)
  ans = rep(-9999, length(RC)) # RC  
  w = ! ( (VW>0 | as.numeric(RW) > 0) | (VC +deltaVC < as.numeric(RC)) | (V + deltaVC - R < DP | V + deltaVC - R > K))
  ans[w] = benefit(AvailableVc, VW, RC, RW, month)[w]
  ans
}
#mixedsolve=Vectorize(mixedsolveprep)

springsolve = springsolveprep=function(VW,VC,RC, RW, R, V,K, DP,month,p){ #initial conditions are no warm 
  deltaVw=QLookup(month,p)
  AvailableVw = pmax(VW+deltaVw , 0)   #!!!  Vw rather than VW??
  # Old version with bug:
  #   AvailableVw = ifelse(VW+deltaVw < 0, 0, Vw + deltaVw)   #!!!  Vw rather than VW??

  ans = rep(-9999, length(VW))
  w = !( VW > 0 | (V + deltaVw - R < DP | V + deltaVw - R > K) | (VC < RC | VW + deltaVw < RW ) )
  ans[w] =  benefit(VC, AvailableVw, RC, RW, month)[w]
  ans
}


summersolve = summersolveprep=function(VW,VC,RC, RW, R, V,K, DP,month,p){ #initial conditions are no warm (i dont like this, want VW to organically come online)
  deltaVw = QLookup(month,p)
  AvailableVW= pmax(VW + deltaVw, 0)

  ans = rep(-9999, length(VW))
  w = !(  (V + deltaVw - R < DP | V + deltaVw- R > K) | (VC < RC) | (VW + deltaVw < RW))
  ans[w] = benefit(VC, AvailableVW, RC, RW, month)[w]  #ifelse(Nmax< x, Nmax, x)
  ans
}


fallsolve = fallsolveprep=function(VW,VC,RC,RW,K,DP,month,p){ #initial conditions are no warm (i dont like this, want VW to organically come online)

    deltaVc = QLookup(month,p)+VW-RW
    tmp = VC + deltaVc
    AvailableVC = pmax(tmp, 0)

    ans = rep(-9999, length(VC))
    w = !( (tmp - RC < DP | tmp - RC > K) | (tmp < RC) | (VW < RW) )
    ans[w] = benefit(AvailableVC,VW,RC,RW,month)[w]
    ans
}
# fallsolve=Vectorize(fallsolveprep)

choosesolveprep = function(month,VW,VC,RC, RW, R, V,RcstarWinter,K, DP,p){ #matrix

    switch(seasonbin(month),
           winter = ,
           earlyspring = mixedsolve(VW,VC,RC, RW, R, V, month, RcstarWinter,K, DP,p),
           spring = springsolve(VW,VC,RC, RW, R, V,K, DP,month,p),
           summer = ,
           latesummer = summersolve(VW,VC,RC, RW, R, V,K, DP,month,p),
           fall = fallsolve(VW,VC,RC, RW, K, DP,month,p),
           "ERROR")
} 
#choosesolve=Vectorize(choosesolveprep)

#choosesolve=
#function(month, VW, VC, RC, RW, R, V, RcstarWinter, K, DP,p)
#   mapply(choosesolveprep, month, VW, VC, RC, RW, R, V, RcstarWinter, K, DP, p)

choosesolve = choosesolveprep

###############
####b. accumulative obj function
#######################
accumulate=function(month,S,Vcstates,Vwstates,VcSpace,RcstarWinter,VwSpace,Rcspace,Rwspace,VSpace,Rspace,p){  #lookup f*t+1 
  fs = fstarvalue = matrix(0,nrow=length(Vstates),ncol=length(Rdecs))
  for(j in 1:length(Rdecs)){ 
      for(i in 1:length(Vstates)){
          tmp = choosesolve(month,VwSpace[i,j],VcSpace[i,j],Rcspace[i,j], Rwspace[i,j], Rspace[i,j],VSpace[i,j],RcstarWinter,K, DP,p)
          fs[i,j] = if(is.na(tmp) || tmp < 0)
                       -9999 #remove infeasibles
                    else
                       which(Vcstates==OutgoingVc(S,VcSpace[i,j],RcstarWinter,VwSpace[i,j],Rcspace[i,j],Rwspace[i,j],p) & #,Vcstates)[i,j] & #this gives the location of Vcstates
                             Vwstates==OutgoingVw(S, VwSpace[i,j], Rwspace[i,j],p)) #match Vw and Vc in LookupV table #location of Vw states

        fstarvalue[i,j] = if(is.na(fs[i,j]) | fs[i,j] < 0)
                            -9999 #remove infeasibles
                          else
                            fstar[fs[i,j],(S+1)] #get the fstar from t+1 with the matching Vw and Vc states
    } 
  } 
  return(fstarvalue) #produces a matrix of fstartt+1 values to accumulate in the benefit function, looking backwards
}

firststageaccumulate =
function(month, S, Vcstates,Vwstates,Vcinitial,RcstarWinter,Vwinitial,Rcdecs,Rwdecs,Rdecs, Vinitial,p)
{
    fstarvalue = rep(-9999, length(Rdecs))

    tmp = choosesolve(month, Vwinitial, Vcinitial, Rcdecs, Rwdecs, Rdecs, Vinitial, RcstarWinter, K, DP, p)        
    for(j in 1:length(Rdecs)){#calculates f*t+1 from next stage
        if(!is.na(tmp[j]) & tmp[j] >= 0) {
          idx = which(Vcstates == OutgoingVc(S,Vcinitial,RcstarWinter,Vwinitial,Rcdecs[j],Rwdecs[j],p) & 
                        Vwstates==OutgoingVw(S, Vwinitial, Rwdecs[j],p))

          fstarvalue[j] = fstar[idx,(S+1)] #get the fstar from t+1 with the matching Vw and Vc states
       }
    }

    fstarvalue #produces a matrix of fstartt+1 values to accumulate in the benefit function, looking backwards
}



