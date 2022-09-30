options(stringsAsFactors = FALSE)
f <- readLines('2018 Best National Universities _ US News Rankings.html')
length(f)

linenum <- 0
info <- NULL

while(linenum < length(f)){
  
  if(linenum %% 1000 == 0) cat(linenum,'\n')
  
  linenum <- linenum + 1
  if(!grepl('<section class="block-flush"',f[linenum])) next
  
  linenum <- linenum + 1
  
  
  while(!grepl('</section>',f[linenum])){
    
    l <- f[linenum]
    
    if(grepl('<h3 ',l)){
      linenum <- linenum + 1
      uname <- uurl <- f[linenum]
      uname <- gsub('.*">(.*)</a>','\\1',uname)
      uurl <- gsub('.*://(.*)">.*','\\1',uurl)
    }
    if(grepl('<div class="block-normal text-small">',l)){
      linenum <- linenum + 1
      uadr <- f[linenum]
      uadr <- gsub('^\\s+','',uadr)
    }
    if(grepl('<div>$',l)){
      linenum <- linenum + 1
      urank <- f[linenum]
      urank <- gsub('^\\s+#([0-9]+)[^0-9].*','\\1',urank)
    }
    if(grepl('Tuition and Fees',l)){
      ucost <- f[linenum]
      if(grepl('state',ucost)){
        ucost1 <-  gsub(',','',gsub('\\s+<strong>\\$([,0-9]+) \\(out-of-state\\).*','\\1',ucost))
        ucost2 <- gsub(',','',gsub('.*\\$([,0-9]+) \\(in-state\\).*','\\1',ucost))
        utype <- 'state'
      } else {
        ucost <- gsub(',','',gsub('\\s+<strong>\\$(.*)</strong>.*','\\1',ucost))
        ucost1 <- ucost2 <- as.numeric(ucost)
        utype <- 'private'
      }
      
    }
    if(grepl('Enrollment',l)){
      uenroll <- f[linenum]
      uenroll <- gsub(',','',gsub('\\s+<strong>(.*)</strong>.*','\\1',uenroll))
      uenroll <- as.numeric(uenroll)
    }
    linenum <- linenum + 1
  }
  
  res <- data.frame(
    name = uname, url = uurl, addr = uadr, rank = as.numeric(urank), cost = as.numeric(ucost1), 
    cost.instate = as.numeric(ucost2), cost.ostate = as.numeric(ucost1), enroll = uenroll, type = utype
  )
  
  print(str(res))
  
  if(is.null(info)){
    info <- res
  } else {
    info <- rbind(info, res)
  }
  
}
info <- info[info$rank < 231,]
info$state <- gsub('.*,\\s+([^,]+$)','\\1',info$addr)
info$city <- substr(info$addr, 1,nchar(info$addr)-4)

write.csv(file='USNews_Univ_Ranking.csv',info, row.names = FALSE)


# Analysis session --------------------------------------------------------


info <- read.csv('USNews_Univ_Ranking.csv')
info$type <- as.factor(info$type)

library(ggplot2)
library(forcats)

head(info)
nrow(info)

# universities in states
table(info$state)

# universities by type
table(info$type)

# according to state, type
table(info$type, info$state)

# tuition range
summary(info$cost)

# plot

univ.private <- subset(info, type == 'private')
univ.state <- subset(info, type == 'state')

summary(univ.state$cost.instate)
summary(univ.state$cost.ostate)

# histogram

ggplot(info) + geom_bar(aes(x=state), colour='blue', fill='skyblue') + coord_flip()
# stacked bar
ggplot(info) + geom_bar(aes(x=state, fill=type)) +
  coord_flip() + ggtitle('Number of universities in States')
#dodged bar
ggplot(info) + geom_bar(aes(x=state, fill=type), position = 'dodge') +
  coord_flip() + ggtitle('Number of universities in States')

# tuition range vs type
ggplot() + 
  geom_boxplot(data = univ.private,aes(type, cost, color='private')) +
  geom_boxplot(data = univ.state,aes(type, cost.instate, color='in-state')) +
  geom_boxplot(data = univ.state,aes(type, cost.ostate, color='out-of-state')) +
  ggtitle('univeristy tuition') + xlab('type') + ylab('tuition')

# tuition range vs state
ggplot() + 
  geom_boxplot(data = univ.private,aes(state,cost, color='private', group=state)) +
  geom_boxplot(data = univ.state,aes(state,cost.instate, color='in-state', group=state)) +
  geom_boxplot(data = univ.state,aes(state,cost.ostate, color='out-of-state', group=state)) +
  ggtitle('univeristy tuition') + xlab('type') + ylab('tuition')


#state univs in-state cost
ggplot(univ.state) + 
  geom_boxplot(aes(fct_reorder(state,cost.instate), cost.instate, color='in-state')) + 
  geom_boxplot(aes(fct_reorder(state,cost.instate), cost.ostate, color='out-of-state')) + 
  coord_flip()

# average tuition

info.tidy1 <- univ.private[,c('state','type','cost','rank')]

info.tidy2 <- univ.state[,c('state','type','cost.instate','rank')]
info.tidy2$type <- 'in-state'
names(info.tidy2)[3] <- 'cost'

info.tidy3 <- univ.state[,c('state','type','cost.ostate','rank')]
info.tidy3$type <- 'out-of-state'
names(info.tidy3)[3] <- 'cost'

info.tidy <- rbind(info.tidy1, info.tidy2, info.tidy3)
head(info.tidy)
nrow(info.tidy)

# average tuition
avg.tuition <- aggregate(info.tidy$cost, by = (info.tidy[,c('state','type')]), mean)
head(avg.tuition)
names(avg.tuition)[3] <- 'tuition'
head(avg.tuition)

ggplot(avg.tuition) +
  geom_point(aes(x=state,y=tuition, colour = type)) +
  ggtitle('tuition summary') + coord_flip()

# order by average tuition
ggplot(avg.tuition) +
  geom_point(aes(x=fct_reorder(state,tuition), y=tuition, colour = type)) +
  ggtitle('tuition summary') + coord_flip()

# histogram
ggplot(info.tidy) + 
  geom_histogram(aes(cost, fill=type), position = 'dodge', bins=15) +
  ggtitle('tuition (out-of-state) histogram')

# histogram - enrollment
ggplot(info) + 
  geom_histogram(aes(enroll, fill=type), position = 'dodge', bins=15) +
  ggtitle('enrollment histogram')

# rank vs tuition(oos)
ggplot(info.tidy,aes(x=rank, y=cost)) + 
  geom_point() + xlab('rank') + ylab('tutition(out-of-state') +
  ggtitle('rank versus tuition')

ggplot(info.tidy,aes(x=rank, y=cost, color=type)) + 
  geom_point() + xlab('rank') + ylab('tutition(out-of-state') +
  ggtitle('rank versus tuition')

ggplot(info.tidy,aes(x=rank, y=cost, color=type)) + 
  geom_point() + geom_smooth(aes(x=rank, y=cost, group=type), method = lm) +
  xlab('rank') + ylab('tutition(out-of-state') +
  ggtitle('rank versus tuition')

# regression
lm(data = subset(info.tidy,type=='private'), cost ~ rank)
lm(data = subset(info.tidy,type=='in-state'), cost ~ rank)
lm(data = subset(info.tidy,type=='out-of-state'), cost ~ rank)

# cost differences
info$cost.diff <- info$cost.ostate - info$cost.instate
lm(data = subset(info,type=='state'), cost.diff ~ rank)

# is size relevant for the cost difference?
lm1 <- lm(data = subset(info,type=='state'), cost.diff ~ enroll + rank)
summary(lm1)

ggplot(subset(info,type=='state')) + geom_point(aes(y=cost.diff, x = enroll)) +
  geom_smooth(aes(y=cost.diff, x = enroll), method=lm)

ggplot(subset(info,type=='state')) + geom_point(aes(y=cost.diff, x = rank)) +
  geom_smooth(aes(y=cost.diff, x = rank), method=lm)

lm2 <- lm(data = subset(info, type == 'state'), cost.diff ~ rank + enroll + cost)
summary(lm2)

lm3 <- lm(data = subset(info, type == 'state'), cost.diff ~ enroll + cost)
summary(lm3)

