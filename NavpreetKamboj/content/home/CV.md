---
hero-title: Navpreet Kamboj RN, MScN, PhD Student
hero-subtitle: |
  PhD Student, University of Toronto
output: 
  myrmdtemplate::bloomberg:
  toc: true
toc_float: true
params:
  orcid.id: "https://orcid.org/0000-0001-6500-0456"
  years.since: 2020
  spacer: ', '
  journal.only: "Yes"
  order: "dyear"
  max.authors: 3
  style: "APA"
  bullets: 'numbers'
  extra.order: 'None'
  flag.OA: FALSE
  bold.author: FALSE
---
  
```{r setup, include=FALSE}
library(tidyverse)
library(shiny)
library(scholar)
library(kableExtra)
library(cvR)
library(plotly)
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, results = 'asis')
library(rorcid)
x  <- "dc7ea3ae-38fd-4188-8bd1-1db4215ea139"
Sys.setenv(ORCID_TOKEN=x, orcid_email="navpreet.kamboj@mail.utoronto.ca")
```

# <i class="fas fa-user-graduate fa-fw"></i> Education

```{r  results='asis'}
edu <- rorcid::orcid_educations(params$orcid.id)
edu <- edu$`0000-0002-9583-8636`$`affiliation-group`$summaries

edu <- tibble(
  organization = purrr::map_chr(edu, "education-summary.organization.name"),
  title = purrr::map_chr(edu, "education-summary.role-title"),
  start = purrr::map_chr(edu, "education-summary.start-date.year.value"),
  end = purrr::map_chr(edu, "education-summary.end-date.year.value"),
  city = purrr::map_chr(edu, "education-summary.organization.address.city"),
  country = purrr::map_chr(edu, "education-summary.organization.address.country"))

edu$time <- glue::glue("{edu$start} - {edu$end}")
edu$location <- glue::glue("{edu$city}, {edu$country}") 
glue::glue('<br>
<ul style="list-style-type:none;">
           <li>
           <em>{edu$title}</em><br>{edu$organization}<br>{edu$location} <span style="float:right;">{edu$time}</span>
           </li>
           </ul>')
```

# <i class="fas fa-briefcase fa-fw"></i> Employment

```{r}
employ <- rorcid::orcid_employments(params$orcid.id)

currentFunction <- function(x){
  if(!is.null(employ$`0000-0002-9583-8636`$`affiliation-group`$summaries[[x]]$`employment-summary.end-date`)){
    "Current"
  } else {employ$`0000-0002-9583-8636`$`affiliation-group`$summaries[[x]]$`employment-summary.end-date.year.value`}
}

end_year <- vapply(seq(1,length(employ$`0000-0002-9583-8636`$`affiliation-group`$summaries), by = 1), currentFunction, "")

employ <- employ$`0000-0002-9583-8636`$`affiliation-group`$summaries

employ <- tibble(
  organization = purrr::map_chr(employ, "employment-summary.organization.name"),
  title = purrr::map_chr(employ, "employment-summary.role-title"),
  start = purrr::map_chr(employ, "employment-summary.start-date.year.value"),
  city = purrr::map_chr(employ, "employment-summary.organization.address.city"),
  country = purrr::map_chr(employ, "employment-summary.organization.address.country"))

employ$time <- glue::glue("{employ$start} - {end_year}")
employ$location <- glue::glue("{employ$city}, {employ$country}") 

glue::glue('<br>
<ul style="list-style-type:none;">
           <li>
           <em>{employ$title}</em><br>{employ$organization}<br>{employ$location} <span style="float:right;">{employ$time}</span>
           </li>
           </ul>')

```



# <i class="fas fa-file-certificate fa-fw"></i> Awards

```{r}
distinctions  <- orcid_distinctions(params$orcid.id)
distinctions <- distinctions$`0000-0002-9583-8636`$`affiliation-group`$summaries
distinctions <- tibble(
  organization = purrr::map_chr(distinctions, "distinction-summary.organization.name"),
  title = purrr::map_chr(distinctions, "distinction-summary.role-title"),
  time = purrr::map_chr(distinctions, "distinction-summary.start-date.year.value"))

glue::glue('<br>
<ul style="list-style-type:none;">
           <li>
           <em>{distinctions$title}</em><br>{distinctions$organization} <span style="float:right;">{distinctions$time}</span>
           </li>
           </ul>')

```
# <i class="fas fa-users-class fa-fw"></i> Service

```{r}
service  <- orcid_services(params$orcid.id)

currentFunction <- function(x){
  if(!is.null(service$`0000-0002-9583-8636`$`affiliation-group`$summaries[[x]]$`service-summary.end-date`)){
    "Current"
  } else {service$`0000-0002-9583-8636`$`affiliation-group`$summaries[[x]]$`service-summary.end-date.year.value`}
}

end_year <- vapply(seq(1, length(service$`0000-0002-9583-8636`$`affiliation-group`$summaries), by = 1), currentFunction, "")


service  <- service$`0000-0002-9583-8636`$`affiliation-group`$summaries
service <- tibble(
  organization = purrr::map_chr(service, "service-summary.organization.name"),
  title = purrr::map_chr(service, "service-summary.role-title"),
  start = purrr::map_chr(service, "service-summary.start-date.year.value"))

service$time <- glue::glue("{service$start} - {end_year}") 

service$time <- glue::glue("{service$start} - {end_year}") 
glue::glue('<br>
<ul style="list-style-type:none;">
           <li>
           <em>{service$title}</em><br>{service$organization} <span style="float:right;">{service$time}</span>
           </li>
           </ul>')
```


# <i class="fas fa-users fa-fw"></i> Memberships

```{r}
membership  <- orcid_memberships(params$orcid.id)

currentFunction <- function(x){
  if(!is.null(membership$`0000-0002-9583-8636`$`affiliation-group`$summaries[[x]]$`membership-summary.end-date`)){
    "Current"
  } else {membership$`0000-0002-9583-8636`$`affiliation-group`$summaries[[x]]$`membership-summary.end-date.year.value`}
}

end_year <- vapply(seq(1, length(membership$`0000-0002-9583-8636`$`affiliation-group`$summaries), by = 1), currentFunction, "")

membership <- membership$`0000-0002-9583-8636`$`affiliation-group`$summaries 

membership <- tibble(
  organization = purrr::map_chr(membership, "membership-summary.organization.name"),
  title = purrr::map_chr(membership, "membership-summary.role-title"),
  start = purrr::map_chr(membership, "membership-summary.start-date.year.value"))

membership$time <- glue::glue("{membership$start} - {end_year}")
glue::glue('<br>
<ul style="list-style-type:none;">
           <li>
           <em>{membership$title}</em><br>{membership$organization} <span style="float:right;">{membership$time}</span>
           </li>
           </ul>')
```

# <i class="fas fa-money-check-alt fa-fw"></i> Funding


```{r}
funding <- rorcid::orcid_fundings(params$orcid.id)
pcodes <- vapply(funding[[1]]$group$`funding-summary`, "[[", 1, "put-code")
out <- lapply(pcodes, function(z) orcid_fundings(params$orcid.id, put_code=z))
amount <- vapply(out, function(w) w[[1]]$amount$value, "")
amount  <- paste("$", amount, sep = "")
funding <- funding$`0000-0002-9583-8636`$group$`funding-summary` 
funding <- tibble(
  type = purrr::map_chr(funding, "type"),
  funder = purrr::map_chr(funding, "organization.name"),
  title = purrr::map_chr(funding, "title.title.value"),
  start = purrr::map_chr(funding, "start-date.year.value"),
  end = purrr::map_chr(funding, "end-date.year.value")) %>% 
  mutate(timeframe = glue::glue("{start} - {end}")) %>% 
  select(type, funder, title, timeframe) 
funding$type <- stringr::str_replace_all(funding$type, "SALARY_AWARD", "SALARY AWARD")
funding <- funding %>% 
  mutate(Amount = amount)

glue::glue('<br>
<ul style="list-style-type:none;">
           <li>
           <em>{funding$title}</em><br>{funding$funder}<br>{funding$Amount} <span style="float:right;">{funding$timeframe}</span>
           </li>
           </ul>')
```

# <i class="fas fa-book fa-fw"></i> Publications
```{r}
id <- "2hpmnr8AAAAJ"

## Get his profile
l <- get_profile(id)
```

```{r}
scholar <- scholar::get_publications(id) 
scholar$journal <- recode(scholar$journal, "The Cochrane Library" = "Cochrane Database of Systematic Reviews")
```

```{r}
papers <- get_num_articles(id)
journals  <- get_num_distinct_journals(id)
```

<br>
  <div class="row">
  <div class="col-md-4">
  <div class = "box">
  <span style="font-size: 32px;">
  `r papers`<br>
  </span>
  articles
</div>
  </div>
  <div class="col-md-4">
  <div class = "box">
  <span style="font-size: 32px;">
  `r l$total_cites`<br>
  </span>
  citations
</div>
  </div>
  <div class="col-md-4">
  <div class = "box">
  <span style="font-size: 32px;">
  `r l$h_index`<br>
  </span>
  h-index
</div>
  </div>
  </div>
  
  <br>
  <div class="row">
  <div class="col-md-6">
  ```{r pubs, out.width="90%"}
pubs_plot <- scholar %>% 
  group_by(year) %>% 
  summarise(pubsperyear = n()) %>% 
  mutate(total = cumsum(pubsperyear)) %>% 
  ggplot(aes(year, total)) +
  geom_line(size=1, color='darkgrey') +
  geom_point(size=3, color="#284260") +
  theme_classic() +
  labs(x= "Year", y = "Publications") +
  scale_x_continuous(breaks = seq(2010, max(scholar$year, na.rm = TRUE), by=2))+
  theme(axis.title  =element_text(face = "bold", size=16))
ggplotly(pubs_plot)
```
</div>
  <div class="col-md-6">
  ```{r cites, out.width="90%"}
ct <- get_citation_history(id)

## Plot citation trend
citations_plot <- ggplot(ct, aes(year, cites)) +
  geom_segment(aes(xend = year, yend = 0), size=1, color='darkgrey') +
  geom_point(size=3, color="DarkSlateGray") +
  theme_classic() +
  labs(x= "Year", y = "Citations")+
  theme(axis.title  =element_text(face = "bold", size=16))
ggplotly(citations_plot)

```
</div>
  </div>
  
  ```{r }
results <- my.orcid(params$orcid.id) # single
```


```{r }
papers  <-  my.filter(results, max.authors=params$max.authors, order=params$order,
                      journal.only=params$journal.only, years.since=params$years.since,
                      spacer=params$spacer, bold.author = params$bold.author)
```

## Journal articles
```{r list.papers, results='asis'}
print_papers(papers)
```


# <i class="fas fa-computer-classic fa-fw"></i> Software

```{r}
software <- tribble(~package, ~description, ~year,
                    "spiritR", "A workflow to enable direct upload of a clinical trial protocol to clinicaltrials.gov
", 2019)

glue::glue('<br>
  <ul style="list-style-type:none;">
  <li>
  <a href="https://spiritr.netlify.com"><em>{software$package}</em></a><br>{software$description} <span style="float:right;">{software$year}</span>
  </li>
  </ul>')
```

# <i class="fas fa-comments fa-fw"></i> Peer reviews
```{r}
reviews <- orcid_peer_reviews(params$orcid.id)
reviews_issn <- reviews$`0000-0002-9583-8636`$group$`external-ids.external-id`

get_title_from_issn <- function(issn) {
  tryCatch(issn_title[[issn]], error = function(e) {rcrossref::cr_journals(issn)$data$title})
}

issnfunction <- function(x){
  issn <- stringr::str_remove(reviews_issn[[x]]$`external-id-value`, "issn:")
  if(issn == "1532-8473") {
    "Journal of Peri-Anesthesia Nursing"
  } else if(issn == "1651-2219"){
    "Annals of Medicine"
  }  else if(issn == "1873-491X"){
    "International Journal of Nursing Studies"
  }  else if(issn == "publons:Wiley"){
    "Wiley journal not otherwise classified"
  }   else if(issn == "1469-493X"){
    "Cochrane Database of Systematic Reviews"
  }   else if(issn == "1552-3799"){
    "Clinical Nursing Research"
  } else if(issn == "publons:Elsevier"){
    "Elsevier journal not otherwise classified"
  } else if(issn == "1557-3117"){
    "Journal of Heart & Lung Transplantation"
  }
  else(get_title_from_issn(issn))
}

journalsReviewed <- vapply(seq(1,length(reviews_issn), by = 1), issnfunction, "")  

reviews_per_journal  <- reviews$`0000-0002-9583-8636`$group$`peer-review-group`



numberReviewsFunction <- function(x){
  nrow(reviews_per_journal[[x]])
}

numberReviewed <- purrr::map_dbl(seq(1,length(reviews_issn), by = 1), numberReviewsFunction)  
reviewdf <- tibble(journalsReviewed, numberReviewed)
reviewdf$journalsReviewed <- recode(reviewdf$journalsReviewed, "Collegian Journal of the Royal College of Nursing Australia" = "Collegian")
totalreviews <- sum(reviewdf$numberReviewed)

reviewdf <- tibble(journalsReviewed, numberReviewed)
reviewdf$journalsReviewed <- recode(reviewdf$journalsReviewed, "Collegian Journal of the Royal College of Nursing Australia" = "Collegian")

totalreviews <- sum(reviewdf$numberReviewed)
```
<br>
  
 

<br>
  &nbsp;
<hr />
  <p style="text-align: center;">Created by <a href="https://www.aaronconway.info">Aaron Conway</a></p>
  <p style="text-align: center;"><span style="color: #808080;"><em>Updated on `r format(Sys.time(), "%a %b %d %X %Y")`</em></span></p>
  
  &nbsp;