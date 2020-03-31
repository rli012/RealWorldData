
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




library(rvest)
library(xml2)
drug.info <- c()

for (i in 1:length(drugs.az)) {
  az.name <- drugs.az[i]
  drug <- site[i]
  
  fl <- paste0('data/IQVIA/Dispensing/Drugs.com/main/', drug, '.html')
  
  if (file.exists(fl)) {
    
    mtm.html <- fl
    mtm.doc <- read_html(mtm.html)
    
    mtm.list <- mtm.doc %>% 
      html_nodes("p") %>% 
      html_text()
    
    generic.brand.name <- mtm.list[1]
    generic.brand.name <- gsub('\n.*', '', generic.brand.name)

    #description <- mtm.doc %>% 
    #  rvest::html_nodes('body') %>% 
    #  xml2::xml_find_all("//div[contains(@id, 'WhatIs')]") %>% 
    #  rvest::html_text()
    
    description <- mtm.list[3]  ## TODO: More description
    description <- gsub('\\n\\s+', ' ', description)
    
    mtm.list <- mtm.doc %>% 
      html_nodes("li") %>% 
      html_text()
    
    drug.class <- mtm.list[grep('Drug class', mtm.list)]
    
    generic.brand.name <- ifelse(length(generic.brand.name)!=0, generic.brand.name, NA)
    drug.class <- ifelse(length(drug.class)!=0, drug.class, NA)
    description <- ifelse(length(description)!=0, description, NA)
    
    drug.info <- rbind(drug.info, c(az.name, drug, 'main', generic.brand.name, drug.class, description, NA, NA))
    
    next
  }
  
  fl <- paste0('data/IQVIA/Dispensing/Drugs.com/mtm/', drug, '.html')
  
  if (file.exists(fl)) {
    
    mtm.html <- fl
    mtm.doc <- read_html(mtm.html)
    
    mtm.list <- mtm.doc %>% 
      html_nodes("p") %>% 
      html_text()
    
    generic.brand.name <- mtm.list[1]
    generic.brand.name <- gsub('\n.*', '', generic.brand.name)
    
    #description <- mtm.doc %>% 
    #  rvest::html_nodes('body') %>% 
    #  xml2::xml_find_all("//div[contains(@id, 'WhatIs')]") %>% 
    #  rvest::html_text()
    
    description <- mtm.list[3]  ## TODO: More description
    description <- gsub('\\n\\s+', ' ', description)
    
    mtm.list <- mtm.doc %>% 
      html_nodes("li") %>% 
      html_text()
    
    drug.class <- mtm.list[grep('Drug class', mtm.list)]
    
    generic.brand.name <- ifelse(length(generic.brand.name)!=0, generic.brand.name, NA)
    drug.class <- ifelse(length(drug.class)!=0, drug.class, NA)
    description <- ifelse(length(description)!=0, description, NA)
    
    drug.info <- rbind(drug.info, c(az.name, drug, 'mtm', generic.brand.name, drug.class, description, NA, NA))
    
    next
  }
  
  fl <- paste0('data/IQVIA/Dispensing/Drugs.com/cdi/', drug, '.html')
  
  if (file.exists(fl)) {
    
    cdi.html <- fl
    cdi.doc <- read_html(cdi.html)
    
    cdi.list <- cdi.doc %>% 
      html_nodes("p") %>% 
      html_text()
    
    generic.brand.name <- cdi.list[1]
    generic.brand.name <- gsub('\n.*', '', generic.brand.name)

    cdi.list <- cdi.doc %>% 
      html_nodes("li") %>% 
      html_text()

    drug.class <- cdi.list[grep('Drug class', cdi.list)]
    
    cdi.list <- cdi.doc %>% 
      html_nodes("ul") %>% 
      html_text()
    
    
    vmig <- cdi.doc %>% 
      rvest::html_nodes('body') %>% 
      xml2::xml_find_all("//ul[contains(@class, 'nav-tabs nav-tabs-collapse vmig')]") %>% 
      rvest::html_text()
    
    
    warn <- cdi.doc %>% 
      rvest::html_nodes('body') %>% 
      xml2::xml_find_all("//div[contains(@class, 'blackboxWarning')]") %>% 
      rvest::html_text()
    
    idx <- 3 + length(vmig) + length(warn)
    
    description <- cdi.list[idx]
    description <- gsub('\\n\\s+', ' ', description)
    
    #drug.class <- cdi.doc %>% 
    #  rvest::html_nodes('body') %>% 
    #  xml2::xml_find_all("//ul[contains(@class, 'more-resources-list more-resources-list-general')]") %>% 
    #  rvest::html_text()
    
    generic.brand.name <- ifelse(length(generic.brand.name)!=0, generic.brand.name, NA)
    drug.class <- ifelse(length(drug.class)!=0, drug.class, NA)
    description <- ifelse(length(description)!=0, description, NA)
    
    drug.info <- rbind(drug.info, c(az.name, drug, 'cdi', generic.brand.name, drug.class, description, NA, NA))
    
    next
  }
  
  
  fl <- paste0('data/IQVIA/Dispensing/Drugs.com/international/', drug, '.html')
  
  if (file.exists(fl)) {
    
    intl.html <- fl
    intl.doc <- read_html(intl.html)
    
    intl.list <- intl.doc %>% 
      html_nodes("p") %>% 
      html_text()
    
    note <- intl.list[1]
    
    drug.us <- gsub('known as (\\w+) in the US', '\\1', str_extract(note, 'known as \\w+ in the US'))
    
    note <- ifelse(length(note)!=0, note, NA)
    drug.us <- ifelse(length(drug.us)!=0, drug.us, NA)

    drug.info <- rbind(drug.info, c(az.name, drug, 'international', NA, NA, NA, note, drug.us))

  } else {
    
    drug.info <- rbind(drug.info, c(az.name, drug, NA, NA, NA, NA, NA, NA))
    
  }
  
}

View(drug.info)

drug.info <- data.frame(drug.info, stringsAsFactors = F)

colnames(drug.info) <- c('drug.az', 'site', 'source', 'generic.brand.name', 'drug.class', 'description', 'note', 'drug.us')

write.table(drug.info, file='data/IQVIA/Dispensing/Drugs.com/drugs.az.info.txt',
            quote = F, sep = '\t', row.names = F)



idx <- which(is.na(drug.info$source))
idx

for (drug in drug.info$site[idx]) {

  url <- paste0('https://www.drugs.com/pro/', drug, '.html')
  pro <- system(paste0('wget ', url, ' -P ', 'data/IQVIA/Dispensing/Drugs.com/pro/'))
    
}


#################
query <- "SELECT UPPER(MEDS_NM), COUNT(*) AS COUNTS FROM IMS_EMR_VW.DISPENSING GROUP BY UPPER(MEDS_NM) ORDER BY COUNTS DESC"
