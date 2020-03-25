
for (drug in drug1$drugs.D30.all) {
  drug <- tolower(drug)
  print (drug)
  
  drug <- gsub(' hcl', '', drug)
  #drug <- gsub(' ', '-', drug)
  drug <- strsplit(drug, ' ')[[1]][1]
  
  url <- paste0('https://www.drugs.com/', drug, '.html')
  fl <- paste0('data/IQVIA/Dispensing/Drugs.com/', drug, '.html')
  
  if (! file.exists(fl) & ! file.exists(gsub('.html', '', fl))) {
    main <- system(paste0('wget ', url, ' -P ', 'data/IQVIA/Dispensing/Drugs.com/'))
    
    fl <- paste0('data/IQVIA/Dispensing/Drugs.com/mtm/', drug, '.html')
  
    if (main != 0 & ! file.exists(fl)) {
      url <- paste0('https://www.drugs.com/mtm/', drug, '.html')
      
      mtm <- system(paste0('wget ', url, ' -P ', 'data/IQVIA/Dispensing/Drugs.com/mtm/'))
      
      fl <- paste0('data/IQVIA/Dispensing/Drugs.com/cdi/', drug, '.html')
      
      if (mtm != 0 & ! file.exists(fl)) {
        url <- paste0('https://www.drugs.com/cdi/', drug, '.html')
        
        cdi <- system(paste0('wget ', url, ' -P ', 'data/IQVIA/Dispensing/Drugs.com/cdi/'))
        
        fl <- paste0('data/IQVIA/Dispensing/Drugs.com/international/', drug, '.html')
        
        if (cdi != 0 & ! file.exists(fl)) {
          url <- paste0('https://www.drugs.com/international/', drug, '.html')
          
          international <- system(paste0('wget ', url, ' -P ', 'data/IQVIA/Dispensing/Drugs.com/international/'))
          
        }
      }
    }
  }
}
