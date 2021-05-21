write.UAoutput <-function(dat, patch) {
  # dat - list with
  # dat$ranges
  # dat$taxa
  # dat$sections_levels

  ua_taxa<-dat$ranges%>%distinct(EVENT)%>%
    dplyr::left_join(dat$taxa%>%dplyr::select(name, ABBR),by = c('EVENT'='name'))%>%
    dplyr::arrange(ABBR)%>%
    dplyr::select(ABBR, EVENT)

  # write taxon dictionary
  write_delim(ua_taxa, paste0(patch,"output.dct"),col_names = F,delim = '\t',quote_escape =  "double")

  sections_info<-dat$sections_levels%>%dplyr::group_by(SECTION)%>%
    dplyr::summarise(bottom= 1,top=max(UALEVEL))

  ua_ranges<-dat$ranges%>%
    dplyr::inner_join( select(ua_taxa, EVENT, ABBR) , by='EVENT')%>%
    dplyr::inner_join(sections_levels,by = c('SECTION'='SECTION', 'FAD'='LEVEL'))%>%
    dplyr::rename(UAFAD=UALEVEL)%>%
    dplyr::inner_join(sections_levels,by = c('SECTION'='SECTION', 'LAD'='LEVEL'))%>%
    dplyr::rename(UALAD=UALEVEL)%>%
    dplyr::select(SECTION, ABBR, UAFAD, UALAD)

 # write dat file
  output.dat = paste0(patch,"output.dat")
  fileConn <-file( output.dat )
  fileHeader<-'DATUM\n\nTITLE:\n"RANGE CHART"\n\n'
  writeLines(fileHeader, fileConn)
  close(fileConn)
  for (snum in 1:nrow(sections_info) ) {
    si<-sections_info[snum, ]
    fileConn <-file(output.dat,open = 'a')
    writeLines( paste0('\nSECTION ',si$SECTION,'-' ) , fileConn)
    writeLines( paste0('bottom ',si$bottom,' - top ', si$top ) , fileConn)
    close(fileConn)
    taxdat<-dplyr::filter(ua_ranges, SECTION == si$SECTION)%>%
            dplyr::select(ABBR, UAFAD, UALAD)
    write_delim(taxdat, output.dat ,col_names = F,delim = '\t',quote_escape =  "none",append = T)
  }
  print("done")
}
