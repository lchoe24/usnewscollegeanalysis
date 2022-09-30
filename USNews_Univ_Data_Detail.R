options(stringsAsFactors = FALSE)
library(httr)
library(jsonlite)

extr_regexpr <- function(pat, src, global = FALSE){
  if(global){
    expr <- gregexpr(pat, src)
  }  else {
    expr <- regexpr(pat,src)
  }
  
  pos <- expr[[1]]
  len <- attr(pos, 'match.length')
  
  if(length(pos) == 0) return(NULL)
  
  result <- NULL
  
  for(i in 1:length(pos)){
    result <- c(result, substr(src, pos[i], pos[i] + len[i] - 1))  
  }
  result
}

extr_trim <- function(str, head_trim = NULL, tail_trim = NULL){
  if(!is.null(head_trim)){
    pos <- regexpr(head_trim,str)
    str <- substring(str, pos[[1]] + attr(pos,'match.length'))
  }
  if(!is.null(tail_trim)){
    pos <- regexpr(tail_trim,str)
    str <- substr(str, 1, pos[[1]]-1)
  }
  str
}

extr_expr <- function(beg_pat, end_pat, src, beg_fixed = FALSE, end_fixed = FALSE, keep = 200,
                      head_trim = NULL, tail_trim = NULL, strip_wspace = FALSE){
  result <- NULL
  ss <- ''
  
  beg_match <- gregexpr(beg_pat,src,beg_fixed)
  beg <- beg_match[[1]]
  beg_len <- attr(beg,'match.length')
  
  for(i in 1:length(beg)){
    src1 <- substr(src, beg[i] + beg_len, beg[i]+keep+beg_len[i])
    end_len <-regexpr(end_pat, src1,end_fixed)
    if(end_len > 0){
      ss <- substr(src1,1,end_len[1]-1)
      result <- c(result, extr_trim(ss,head_trim, tail_trim))
    }
  }
  if(strip_wspace) result <- gsub('\\s','',result)
  
  if(is.null(result)) result <- ''
  return(result)
}

extr_expr_rev <- function(beg_pat, end_pat, src, beg_fixed = FALSE, end_fixed = FALSE, keep = 200,
                          head_trim = NULL, tail_trim = NULL, strip_wspace =FALSE){
  result <- NULL
  ss <- ''
  
  beg_match <- gregexpr(end_pat,src,end_fixed)
  beg <- beg_match[[1]]
  beg_len <- attr(beg,'match.length')
  
  for(i in 1:length(beg)){
    src1 <- substr(src, beg[i] -keep , beg[i])
    end_len <-regexpr(beg_pat, src1,beg_fixed)
    if(end_len > 0){
      ss <- substring(src1,end_len[[1]]+1)
      result <- c(result, extr_trim(ss, head_trim, tail_trim))
    }
  }
  if(strip_wspace) result <- gsub('\\s','',result)
  return(result)
}
univ <- read.csv('USNews_Univ_ranking.csv')
head(univ$url)

# download

sub_pages <- c('','/overall-rankings','/applying','/paying','/academics','/campus-safety','/student-life')
sub_pages <- c('/overall-rankings')
for(i in 1:nrow(univ)){
  
  for(subp in sub_pages){
    url_base <- paste0('https://',univ$url[i],subp)
    
    fn <- gsub('/','__',substring(url_base,9))
    fn <- paste0('D:/Work/Univ/',fn)
    
    if(file.exists(fn)){
      cat(url_base,' Skip..\n')
      next
    } else {
      cat(url_base,' Reading...\n')
    }
    
    pg <- readLines(file(url_base))
    length(pg)
    
    if(!file.exists(fn)) file.create(fn)
    writeLines(pg, con=file(fn))
    
    Sys.sleep(sample(1:3,1))
  }
  Sys.sleep(sample(1:10,1))
}


# overview ----------------------------------------------------------------


univ.overview <- NULL

for(i in 1:nrow(univ)){
  
  if(i%%10 == 0) cat(i,'\n')
  
  subp <- ''
  url_base <- paste0('https://',univ$url[i],subp)
  
  fn <- gsub('/','__',substring(url_base,9))
  fn <- paste0('D:/Work/Univ/',fn)
  
  #f <- readLines(paste0('D:/Work/Univ/www.usnews.com__best-colleges__adelphi-university-2666'))
  f <- paste(readLines(fn),collapse = '\n')
  f <- stringi::stri_trans_general(f, "latin-ascii")
  
  tuition <- extr_expr('Tuition &amp; Fees</span> <strong>','</strong>',f,TRUE,TRUE)
  roomboard <- extr_expr('Room and Board</span> <strong>','</strong>',f,TRUE,TRUE)
  app_deadline <- extr_expr('Application Deadline</span> <strong>','</strong>',f,TRUE,TRUE)
  school_type <- extr_expr_rev('>','school type',f, tail_trim = '<')
  founded <- extr_expr_rev('>','year founded',f, tail_trim = '<')
  religion <- extr_expr_rev('>','religious affiliation',f, tail_trim = '<')
  salary <- extr_expr_rev('data-test-id','title="Median starting salary for alumni with 0-5',f,
                          head_trim='\\$', tail_trim = '<') 
  
  if(is.null(tuition)) tuition <- NA
  if(is.null(roomboard)) roomboard <- NA
  if(is.null(app_deadline)) app_deadline <- NA
  if(is.null(school_type)) school_type <- NA
  if(is.null(founded)) founded <- NA
  if(is.null(religion)) religion <- NA
  if(is.null(salary)) salary <- NA
  
  univ.overview <- rbind(univ.overview,
                         data.frame(
                           name = univ$name[i], rank = univ$rank[i],
                           tuition = tuition, roomboard = roomboard, app_deadline = app_deadline,
                           school_type = school_type, founded = founded, religion = religion, salary = salary
                         )
  )
}

univ.overview$founded <- as.numeric(univ.overview$founded)

tuition.cost <- as.numeric(gsub(',','',gsub('\\$([0-9,]+) .*','\\1',univ.overview$tuition)))
tuition.year <- gsub('.* (\\S+)','\\1',univ.overview$tuition)
rmb.cost <- as.numeric(gsub(',','',gsub('\\$([0-9,]+) .*','\\1',univ.overview$roomboard)))
rmb.year <- gsub('.* (\\S+)','\\1',univ.overview$roomboard)

univ.overview$tuition <- tuition.cost
univ.overview$tuition.year <- tuition.year

univ.overview$rmb <- rmb.cost
univ.overview$rmb.year <- rmb.year

univ.overview$salary <- as.numeric(gsub(',','',univ.overview$salary))
write.csv(file='USNews_Univ_ranking_overview.csv', univ.overview)
save(file = 'USNews_Univ.RData', univ, univ.overview)


# academics  -----------------------------------------------------------------

univ.academics <- univ.academics.majors <- NULL

for(i in 1:nrow(univ)){
  subp <- '/academics'
  url_base <- paste0('https://',univ$url[i],subp)
  
  fn <- gsub('/','__',substring(url_base,9))
  fn <- paste0('D:/Work/Univ/',fn)
  
  f <- paste(readLines(fn),collapse = '\n')
  f <- stringi::stri_trans_general(f, "latin-ascii")
  
  ret_rate <- extr_expr('freshman retention rate','percent',f,TRUE,TRUE, head_trim = 'is ')
  stfac_ratio <- extr_expr('v_student_faculty_ratio">','</span>',f,TRUE,TRUE, strip_wspace = TRUE)[1]
  class20 <- extr_expr('fewer than 20 students','%',f,TRUE,TRUE, head_trim = '">')
  class49 <- extr_expr('20-49','%',f,TRUE,TRUE, head_trim = '">')
  class50 <- extr_expr('50 or more','%',f,TRUE,TRUE, head_trim = '">')
  z <- extr_expr('\\s+data-for="topMajors"','"displayValue"',f,TRUE,TRUE, head_trim = '"rawValue":', keep=1500)
  mdat <- data.frame(fromJSON(substr(z,1,nchar(z)-6)))
  if(nrow(mdat) > 0) {
    names(mdat) <- c('major','percent')
    mdat$percent <- as.numeric(mdat$percent)
    
    mdat$name <- univ$name[i]
    mdat$major_rank <- 1:nrow(mdat)
    mdat <- mdat[,c('name','major_rank','major','percent')]
    univ.academics.majors <- rbind(univ.academics.majors, mdat)
  }
  
  result <- data.frame(
    name = univ$name[i],
    st_to_fac = as.numeric(gsub(':1','',gsub(':1','',stfac_ratio)),fixed=T),
    retention = as.numeric(ret_rate),
    class20 = as.numeric(class20),
    class49 = as.numeric(class49),
    class50 = as.numeric(class50)
  )
  
  univ.academics <- rbind(univ.academics, result)
  
}

write.csv(file='univ.academics.csv', univ.academics, row.names = FALSE)
write.csv(file='univ.academics.majors.csv', univ.academics.majors, row.names = FALSE)
save(file = 'USNews_Univ.RData', univ, univ.overview, univ.academics, univ.academics.majors)

# applying ----------------------------------------------------------------

univ.applying <- NULL
subp <- '/applying'

for(i in 1:nrow(univ)){
  url_base <- paste0('https://',univ$url[i],subp)
  
  fn <- gsub('/','__',substring(url_base,9))
  fn <- paste0('D:/Work/Univ/',fn)
  
  f <- paste(readLines(fn),collapse = '\n')
  f <- stringi::stri_trans_general(f, "latin-ascii")
  
  deadline <- extr_expr_rev('"rawValue":','APPLICATION_DEADLINE',f,TRUE,TRUE,head_trim=' "',tail_trim='",')
  ed_close <- extr_expr_rev('"rawValue":','EARLY_DEC_PLAN_CLOSE',f,TRUE,TRUE,head_trim=' "',tail_trim='",')
  app_fee <- extr_expr_rev('"rawValue":','APPLICATION_FEE',f,TRUE,TRUE,head_trim = ': ', tail_trim = ',')
  common_app <- extr_expr_rev('"rawValue":','COMMON_APP_YN',f,TRUE,TRUE,head_trim=' "',tail_trim='",')
  interview_required <- extr_expr_rev('"rawValue":','SPE_REQ_ADM_INT_REQ',f,TRUE,TRUE,head_trim=' "',tail_trim='",')
  sat_act <- extr_expr_rev('"rawValue":','V_SAT_ACT_POLICY',f,TRUE,TRUE,head_trim=' "',tail_trim='",')
  sat_act_date <- extr_expr_rev('"rawValue":','ACT_SATI_LATEST_DATE',f,TRUE,TRUE,head_trim=' "',tail_trim='",')
  selective_class <- extr_expr_rev('"rawValue":','C_SELECT_CLASS',f,TRUE,TRUE,head_trim=' "',tail_trim='",')
  accept_rate <- extr_expr_rev('"rawValue":','R_C_ACCEPT_RATE',f,TRUE,TRUE,head_trim = ': ', tail_trim = ',')
  accept_rate_ed <- extr_expr_rev('"rawValue":','EARLY_DEC_ACCEPT_RATE',f,TRUE,TRUE,head_trim = ': ', tail_trim = ',')
  accept_rate_ea <- extr_expr_rev('"rawValue":','EARLY_ACT_ACCEPT_RATE',f,TRUE,TRUE,head_trim = ': ', tail_trim = ',')
  accept_rate_reg <- extr_expr_rev('"rawValue":','NON_EARLY_ACCEPT_RATE',f,TRUE,TRUE,head_trim = ': ', tail_trim = ',')
  
  result <- data.frame(
    name = univ$name[i],
    deadline = deadline,
    ed_close = ed_close,
    app_fee = as.numeric(app_fee),
    common_app = common_app,
    interview_required = interview_required,
    sat_act = sat_act,
    sat_act_date = sat_act_date,
    selectiveness = selective_class,
    accept_rate = as.numeric(accept_rate),
    accept_rate_ed = as.numeric(accept_rate_ed),
    accept_rate_ea = as.numeric(accept_rate_ea),
    accept_rate_reg = as.numeric(accept_rate_reg)
    
  )
  
  univ.applying <- rbind(univ.applying, result)
  
}

write.csv(file='univ.applying.csv', univ.applying, row.names = FALSE)
save(file = 'USNews_Univ.RData', univ, univ.overview, univ.academics, univ.academics.majors, univ.applying)


# campus-safety -----------------------------------------------------------

univ.safety <- NULL
subp <- '/campus-safety'

for(i in 1:nrow(univ)){
  url_base <- paste0('https://',univ$url[i],subp)
  fn <- gsub('/','__',substring(url_base,9))
  fn <- paste0('D:/Work/Univ/',fn)
  
  f <- paste(readLines(fn),collapse = '\n')
  f <- stringi::stri_trans_general(f, "latin-ascii")
  
  data_id <- extr_regexpr('[A-Z_]+PY[1-3]_[A-Z_]+',f, global=TRUE)
  data_value <- rep(0, length(data_id))
  for(j in 1:length(data_value))
    data_value[j] <- as.numeric(extr_expr(data_id[j],'</div>',f,head_trim = '>'))
  
  result <- data.frame(
    name = univ$name[i],
    data_id = data_id,
    year = gsub('(.*)_PY.*','\\1',data_id),
    item = gsub('.*_(PY[1-3])_.*','\\1',data_id),
    location = gsub('.*_PY[1-3]_(.*)','\\1',data_id),
    value = data_value
  )
  
  univ.safety <- rbind(univ.safety, result)  
}

write.csv(file='univ.safety.csv', univ.safety, row.names = FALSE)
save(file = 'USNews_Univ.RData', univ, univ.overview, univ.academics, univ.academics.majors, univ.applying,
     univ.safety)


# paying ------------------------------------------------------------------

univ.paying <- NULL
subp <- '/paying'

for(i in 1:nrow(univ)){
  url_base <- paste0('https://',univ$url[i],subp)
  fn <- gsub('/','__',substring(url_base,9))
  fn <- paste0('D:/Work/Univ/',fn)
  
  f <- paste(readLines(fn),collapse = '\n')
  f <- stringi::stri_trans_general(f, "latin-ascii")
  
  data_id <- gsub('"','',gsub('\"dataQaId\": ','',extr_regexpr('"dataQaId": "[A-Z_]+"',f, global=TRUE)))
  data_value <- rep(0, length(data_id))
  for(j in 1:length(data_value)){
    data_value[j] <- (extr_expr_rev('"rawValue":',data_id[j],f,head_trim = ':',tail_trim=','))
  }
  
  applied_need_based <- as.numeric(extr_expr('"Applied for need-based aid":',',',f))
  v <- rep('',5)
  
  v[1] <- as.numeric(extr_expr('"Applied for need-based aid":',',',f))
  v[2] <- as.numeric(extr_expr('"Need was fully met":',',',f))
  v[3] <- as.numeric(extr_expr('"Received need-based financial aid":',',',f))
  v[4] <- as.numeric(extr_expr('"Received need-based self-help aid":',',',f))
  v[5] <- as.numeric(extr_expr('"Average percent of need met":',',',f))
  
  result <- data.frame(name = univ$name[i], data_id = data_id, value = data_value)
  result$value <- as.numeric(result$value)
  result <- rbind(result,
                  data.frame(
                    name = rep(univ$name[i],5),
                    data_id = c('applied_need_based','need_fully_met','need_based_fin_aid',
                                'need_based_self_aid','average_pct_need_met'),
                    value = as.numeric(v)))
  univ.paying <- rbind(univ.paying, result)
}

write.csv(file='univ.paying.csv', univ.paying, row.names = FALSE)
save(file = 'USNews_Univ.RData', univ, univ.overview, univ.academics, univ.academics.majors, univ.applying,
     univ.safety,univ.paying)

# ranking -----------------------------------------------------------------

univ.ranking <- NULL
subp <- '/overall-rankings'

for(i in 1:nrow(univ)){
  
  url_base <- paste0('https://',univ$url[i],subp)
  fn <- gsub('/','__',substring(url_base,9))
  fn <- paste0('D:/Work/Univ/',fn)
  
  f <- paste(readLines(fn),collapse = '\n')
  f <- stringi::stri_trans_general(f, "latin-ascii")
  
  gex <-  extr_regexpr('>#[0-9]+</span>\n\\s+in\n\\s+<a[^<]+</a>',f,global = TRUE)
  
  ranks <- unlist(lapply(gex, function(x) gsub('>#([0-9]+)<.*','\\1',x[1])))
  subjects <-unlist(
    lapply(gex,
           function(x) 
             gsub('^\\s+','',gsub('\\s+</a>$','',gsub('.*>\n','',x[1])))
    ))
  
  ranks <- as.numeric(ranks)
  result <- data.frame(
    name=rep(univ$name[i],length(ranks)),
    rank = ranks, subject = subjects)
  
  univ.ranking <- rbind(univ.ranking, result)
}

write.csv(file='univ.ranking.csv', univ.paying, row.names = FALSE)
save(file = 'USNews_Univ.RData', univ, univ.overview, univ.academics, univ.academics.majors, univ.applying,
     univ.safety,univ.paying, univ.ranking)


# student life ------------------------------------------------------------

univ.student <- NULL
subp <- '/student-life'

for(i in 1:nrow(univ)){
  
  url_base <- paste0('https://',univ$url[i],subp)
  fn <- gsub('/','__',substring(url_base,9))
  fn <- paste0('D:/Work/Univ/',fn)
  
  f <- paste(readLines(fn),collapse = '\n')
  f <- stringi::stri_trans_general(f, "latin-ascii")
  
  data_id <- gsub('"','',gsub('\"dataQaId\": ','',extr_regexpr('"dataQaId": "[A-Z_]+"',f, global=TRUE)))
  data_id <- setdiff(data_id,'V_HOU_OFF')
  data_value <- rep(0, length(data_id))
  for(j in 1:length(data_value)){
    data_value[j] <- (extr_expr_rev('"rawValue":',data_id[j],f,head_trim = ':',tail_trim=','))
  }
  
  v <- rep('',9)
  ff <- substr(f,regexpr('V_HOU_OFF',f)-300,regexpr('V_HOU_OFF',f))
  v[1] <- extr_expr('coed dorms \\(','%',ff,TRUE,TRUE)
  v[2] <- extr_expr("women's dorms \\(",'%',ff,TRUE,TRUE)
  v[3] <- extr_expr("men's dorms \\(",'%',ff,TRUE,TRUE)
  v[4] <- extr_expr('sorority housing \\(','%',ff,TRUE,TRUE)
  v[5] <- extr_expr('fraternity housing \\(','%',ff,TRUE,TRUE)
  v[6] <- extr_expr('apartment for single students \\(','%',ff,TRUE,TRUE)
  v[7] <- extr_expr('special housing for disabled students \\(','%',ff,TRUE,TRUE)
  v[8] <- extr_expr('theme housing \\(','%',ff,TRUE,TRUE)
  v[9] <- extr_expr('wellness housing \\(','%',ff,TRUE,TRUE)
  
  v <- as.numeric(v)
  
  result <- data.frame(name = univ$name[i], data_id = data_id, value = data_value)
  result$value <- as.numeric(result$value)
  result <- rbind(result,
                  data.frame(
                    name = rep(univ$name[i],9),
                    data_id = c('coed','womens','mens','sorority','fraternity','single_apt','disabled','theme','wellness'),
                    value = as.numeric(v)))
  univ.student <- rbind(univ.student, result)
}

write.csv(file='univ.student.csv', univ.student, row.names = FALSE)
save(file = 'USNews_Univ.RData', univ, univ.overview, univ.academics, univ.academics.majors, univ.applying,
     univ.safety,univ.paying, univ.ranking, univ.student)