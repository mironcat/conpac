#calibrate to MA

calibrateToMA <- function(dat, dmarkers) {

  #  ------------ input format ---------------
  # minimal format columns input dat: EVENT, FAD, LAD
  # $ EVENT: chr [1:31] "s_Bia04_Smorodinyi-0" "s_Bia04_Smorodinyi-65" "s_Bia04_Smorodinyi-75" "s_Bia04_Smorodinyi-115" ...
  # $ FAD  : num [1:31] 1440 1498 1507 1543 1084 ...
  # $ LAD  : num [1:31] 1440 1498 1507 1543 1084 ...

  #dmarkers or e.g. dated events
  # $ ID    : chr [1:14] "UE00137" "UE00135" "UE00136" "UE00138" ...
  # $ EVENT : chr [1:14] "Assel" "Sakm" "Artin" "Kungur" ...
  # $ DATLEV: num [1:14] 1006 1178 1207 1257 1284 ...
  # $ AGE   : num [1:14] 299 294 290 282 277 ...
  # --------------------------------------

  calibratedColumns<-c('max_ma', 'min_ma') %in% colnames(dat)
  if ( calibratedColumns[1] | calibratedColumns[2]  ) {
    print('Error. Calibrated column max_ma and min_ma already exist! ')
    View(dat)
    browser()
  }


  if (nrow(dmarkers)<2) {
    print('Error. Markers is less than two.')
    View(dmarkers)
    browser()
  }

  #create tempID column for dat tibble
  dat<-dat%>%mutate(tempID=1:nrow(dat))

  #function for calculating the angular coefficient - SLOPE
  getSLOPE <- function(AGE1,AGE2, LEV1,LEV2) {
    return( (AGE1-AGE2)/(LEV2-LEV1) )
  }

  # fpaired - formatted paired events
  # convert FAD and LAD to LEVEL and set the event type ETYPE=1 for FAD, ETYPE=2 for LAD
  # $ LEVEL: num [1:21] 1021 1024 1024 1024 1024 ...
  # $ ID   : chr [1:21] "S00436" "S00404" "S00636" "S00404" ...
  # $ EVENT: chr [1:21] "s_Prk06_538_well-4.5" "s_Neu70_TomComp-0" "s_PTpct_BabKam2020-0" "s_Neu70_TomComp-0" ...
  # $ ETYPE: num [1:21] 2 1 1 2 2 1 2 1 2 1 ...

  fpaired <- select(dat, FAD, tempID) %>%rename(LEVEL=FAD)%>%mutate(ETYPE=1)%>%
    bind_rows( select(dat, LAD,tempID)%>%rename(LEVEL=LAD)%>%mutate(ETYPE=2) )%>%
    mutate(LEVEL = round(LEVEL,3) )%>%
    arrange(LEVEL)


  # maxLevel and minLevel used for prepare intervals table below
  maxLevel=max(fpaired$LEVEL)
  minLevel=min(fpaired$LEVEL)

  # marker intervals table
    # $ INT   : num [1:14] 1 2 3 4 5 6 7 8 9 10 ...
    # $ EVENT : chr [1:14] "Assel" "Sakm" "Artin" "Kungur" ...
    # $ DATLEV: num [1:14] 1006 1178 1207 1257 1284 ...
    # $ L1    : num [1:14] 1006 1178 1207 1257 1284 ...
    # $ L2    : num [1:14] 1178 1207 1257 1284 1330 ...
    # $ AGE   : num [1:14] 299 294 290 282 277 ...
    # $ AGE1  : num [1:14] 299 294 290 282 277 ...
    # $ AGE2  : num [1:14] 294 290 282 277 271 ...
    # $ SLOPE : num [1:14] 0.0302 0.118 0.1621 0.1848 0.1297 ...
  intervals <- dmarkers %>%
    mutate(L1=round(DATLEV, 3)) %>%
    mutate(L2=lead(L1)) %>%
    mutate(L2=if_else( is.na(L2), maxLevel , L2 )) %>%
    mutate(AGE1=AGE) %>%
    mutate(AGE2=lead(AGE)) %>%
    mutate(SLOPE= getSLOPE ( AGE1, AGE2, L1, L2 ) ) %>%
    mutate(SLOPE=if_else( is.na(SLOPE), lag( SLOPE ), SLOPE )) %>%
    mutate(L1=if_else( is.na( lag(L1) & minLevel<L1 ), minLevel, L1 )) %>%
    rownames_to_column(var = 'INT') %>%
    mutate(INT=as.numeric(INT)) %>%
    select(INT, EVENT,DATLEV, L1,L2,AGE, AGE1,AGE2,SLOPE)


  # function for getting the interval number for a specific level and apply to fpaired tibble
  f <- function(p) {
    LEVEL=as.numeric(p['LEVEL'])
    condition <-(LEVEL>=intervals$L1 & LEVEL<=intervals$L2 )
    p['INT']=intervals[condition,]$INT
    return(p)
  }

  cpaired <-as.tibble(t(apply(fpaired,1,f)))


  calibrated <- cpaired %>%
    left_join(select(intervals,INT,SLOPE,AGE, DATLEV) ,by = 'INT') %>%
    mutate(CALIBAGE=AGE-SLOPE*(LEVEL-DATLEV))

  calibrated.fad <- calibrated %>%
    filter(ETYPE==1) %>%
    rename(FAD=LEVEL, max_ma=CALIBAGE) %>%
    select(tempID, FAD, max_ma)%>%
    arrange(tempID)

  calibrated.lad <- calibrated %>%
    filter(ETYPE==2) %>%
    rename(LAD=LEVEL, min_ma=CALIBAGE) %>%
    arrange(tempID)%>%
    select(LAD, min_ma)

  #calibration results
  calibres<- bind_cols(calibrated.fad,calibrated.lad)%>%select(tempID, max_ma,min_ma)
  #results to export
  dat<-dat%>%inner_join(calibres, by="tempID")%>%select(-tempID)

  return( dat )


}

