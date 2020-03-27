
rvest & xml2: https://towardsdatascience.com/tidy-web-scraping-in-r-tutorial-and-resources-ac9f72b4fe47

### Table based
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


### Drugs.com based
drugs.az <- c()
for (c1 in LETTERS) {
  for (c2 in c(LETTERS, '0-9')) {

    az <- tolower(paste0(c1, c2))
    fl <- paste0('data/IQVIA/Dispensing/Drugs.com/AZlist/', az, '.html')
    
    doc.html = htmlTreeParse(fl, useInternal = TRUE)
    
    plain.text <- xpathSApply(doc.html, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    
    if (c2=='0-9') {
      idx <- grep(paste0('^',c1, '.*\\d+'), plain.text, ignore.case = TRUE)
    } else {
      idx <- grep(paste0('^',az), plain.text, ignore.case = TRUE)
    }

    plain.text <- plain.text[idx]
    filter <- which(duplicated(plain.text))
    
    plain.text <- plain.text[-filter]
    drugs.az <- c(drugs.az, plain.text)
    
    
  }
}

fl <- paste0('data/IQVIA/Dispensing/Drugs.com/AZlist/0-9.html')

doc.html = htmlTreeParse(fl, useInternal = TRUE)

plain.text <- xpathSApply(doc.html, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
View(plain.text)

idx <- grep('^\\d+', plain.text)
idx

plain.text <- plain.text[idx]
plain.text

filter <- which(duplicated(plain.text))

plain.text <- plain.text[-filter]
drugs.az <- c(drugs.az, plain.text)

site <- gsub("\\(|\\)|\\'", '', drugs.az)

site <- gsub('\\W+\\s*', '-', site)
site

site <- tolower(site)
site

drug.info <- data.frame(drugs.az=drugs.az, site=site, stringsAsFactors = F)

write.table(drug.info, file='data/IQVIA/Dispensing/Drugs.com/drugs.az.txt',
            quote = F, sep = '\t', row.names = F)
