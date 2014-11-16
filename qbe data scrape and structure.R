#### Script info: Get all of GA DOE's QBE PDF
#### and turn it into a neat data frame for analysis
#### this requires a couple other things to like downloading
#### xpdf and pdfbinder (can't figure out how to bind pdfs in R)
#### Author: Chris Gian 
#### Time: "2014-11-16 12:16:16 EST"

### Section Info: This section sets you up!
### 
checkdown<-function(x){
  if(x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x)}}

checkdown('RCurl')
checkdown('gtools')
checkdown('tcltk2')
require(RCurl)  # Rcurl to help us scrape
require(gtools) # used this package to do alphanumeric ordering
require(tcltk2)

# create folders
dir.create("downloaded_pdfs")
dir.create("pdftohtml")
dir.create('output')
dir.create('tools')

# set destination
dest1<-paste(getwd(),'/downloaded_pdfs',sep="")
dest2<-paste(getwd(),'/pdftohtml',sep="")
dest3<-paste(getwd(),'/output',sep="")
dest4<-paste(getwd(),'/tools',sep="")

# following generates pop up dialogues
mywait <- function(x) {
  tt <- tktoplevel()
  tkpack( tkbutton(tt, text=x, command=function()tkdestroy(tt)),
          side='bottom')
  tkbind(tt,'<Key>', function()tkdestroy(tt) )
  tkfocus(tt)
  tkwait.window(tt)
}

# pdf binder
url_binder<-'https://pdfbinder.googlecode.com/files/PDFBinder-v1.2.msi'
download.file(url_binder,paste(dest4,"/binder.msi",sep=""),mode='wb')
shell.exec(paste(dest4,"/binder.msi",sep=""))

# a pop up that asks whether you have finished installing
mywait("A pop up window should have appeared asking to install PDF binder. Click after you finish installing")

url_html<-'http://downloads.sourceforge.net/project/pdftohtml/windows%20binary/pdftohtml-0.38%20win32/pdftohtml-0.38-win32.zip?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Fpdftohtml%2F&ts=1416160943&use_mirror=iweb'
download.file(url_html,paste(dest4,"/html.zip",sep=""),mode='wb')
unzip(zipfile=paste(dest4,"/html.zip",sep=""),exdir = dest2)

### Section Info: scrape pdfs
### Lots of stuff going on here. Essentially, we download pdfs
### fromt the Georgia Dept. of Ed

# get system id's
sys_id_url<-'http://app.doe.k12.ga.us/ows-bin/owa/main_pack_fcl099.entry_form?p_usercode='
sys_id2<-data.frame(readHTMLTable(sys_id_url))[1]



# define years of data you wish to pull
years<-2013:2005

# set the location you wish to save downloaded pdfs

# create a dataframe of a href links for a browser-based solution
print<-data.frame()

# first loop loops through all the years that we've defined
for (i in 1:length(years)){
  
  # This second loop, loops through all the systems that we have defined 
  for (j in 1:nrow(sys_id)){
    
    # add a condition: if years are greater than 2008
    if(years[i]>2008){
      # create the URL
      
      # Set the below url that we will be scraping through
      # how this works is that we loop through the two variables
      # years and systems
      url<-paste(
        'http://rpt.doe.k12.ga.us/genreports/ALLOT',
        years[i],
        'I',
        sys_id[j,],'-0ALL.pdf',sep="")
      
      # create the file destination where we temporarily save
      # the file we have downloaded
      file<-paste(dest1,'/ga',years[i],sys_id[j,],'_qbe.pdf',sep="")
      
      # This is a very important part. try / catch does one thing:
      # if a link is bad it will allow us to skip the URL and continue
      # without interupting our scraping.
      
      err <- try(download.file(url, destfile = file, quiet = TRUE,
                               mode = "wb"))
      # sleep timer, that helps us keep things working
      # without this, for some reason the download chokes and stops
      # I believe this is half a second
      Sys.sleep(.5)
      
      if (class(err) == "try-error"){ # this says that if error next
        next}else{ # if not an error execute the following where down it!
          download.file(url, destfile = file, quiet = TRUE, #silent download
                        mode = "wb")
          print(
            c(paste(
              'year: ',years[i],' system: ',sys_id[j,]
              ,sep=""
            )
            )
          )
          
        }
      Sys.sleep(.5)
    }
    # Same as above, but for years before 2008
    else if(years[i]<=2008)
    {
      # create the URL
      url<-paste(
        'http://rpt.doe.k12.ga.us/genreports/Sys',
        substr(years[i],3,4),
        'Allot',
        sys_id[j,],
        '.pdf',sep="")
      # create the file destination 
      file<-paste(dest1,'/ga',years[i],sys_id[j,],'_qbe.pdf',sep="")
      
      #trycatch
      err <- try(download.file(url, destfile = file, quiet = TRUE,
                               mode = "wb"))
      Sys.sleep(.5)
      
      if (class(err) == "try-error"){
        next}else{
          #down it
          download.file(url, destfile = file, quiet = TRUE,
                        mode = "wb")
          print(
            c(paste(
              'year: ',years[i],' system: ',sys_id[j,]
              ,sep=""
            )
            )
          )
          
        } 
    }
  }
}



### Info: Manual! Here you have to bind your pdfs manual. Sorry!  
### download add execute here https://code.google.com/p/pdfbinder/
### save 2005 - 2006 seperately from 2007-2013

mywait('have you binded the pdfs? If so click to continue.')
### Info: Convert PDF to Dataframe
### This section converts PDFS to HTML to TXT
### (Trust me, this is better structured than PDF to Txt) ###
### before this section, we need to combine the (1600) pdfs into two 
### Pdfs a 2007 - 2013 and a 2005 to 2006
### Get this: http://pdftohtml.sourceforge.net in order to assign th
### exe below!


exe<-paste(dest2,"/pdftohtml.exe",sep="")

# define pdf to convert
dest07_13<-paste(dest2,'/2007-2013.pdf',sep="")
dest05_06<-paste(dest2,'/2005-2006.pdf',sep="")


#check file existence and proceed only if
while(file.exists(paste(dest2,'/2005-2006.pdf',sep=""))==F){
  mywait(paste('bind the pdf,
         name it 2005-2006.pdf,
         save it to ',getwd(),'/pdftohtml',sep=""))}
while(file.exists(paste(dest2,'/2007-2013.pdf',sep=""))==F){
  mywait(paste('bind the pdf,
         name it 2007-2013.pdf,
         save it to ',getwd(),'/pdftohtml',sep=""))
}

# run module "pdftohtml.exe" commands 
system(paste("\"", exe, "\" \"", dest07_13, "\"", sep = ""), wait = T)
system(paste("\"", exe, "\" \"", dest05_06, "\"", sep = ""), wait = T)

# html path
qbe05_06_html<-sub("[.]pdf","s.html",dest05_06)
qbe07_13_html<-sub("[.]pdf","s.html",dest07_13)

# txt path
qbe07_13_txt<-gsub("[.]pdf",".txt",dest07_13)
qbe05_06_txt<-gsub("[.]pdf",".txt",dest05_06)
                  
# rename using paths above
file.rename(qbe05_06_html,qbe05_06_txt)
file.rename(qbe07_13_html,qbe07_13_txt)

# read above
qbe05_06<-read.delim(qbe05_06_txt)
qbe07_13<-read.delim(qbe07_13_txt)

#bind frames
qbe05_13<-rbind(qbe05_06,qbe07_13)

### Info: Initial Cleaning and Data organizing
### A coupel things this section does: Remove tags, remove blank rows
### make it so that every row has its own value (this was super hard)
### and is half of this section. 

# clean data, remove all html tags
step1<-gsub("<([aA-zZ]*)(/?)([aA-zZ]*)>|<A[ ][aA-zZ]*[=]*[0-9]*>","",qbe05_13[,])

# remove preceeding and trailing spaces
# in regex ^ means match beginning
# in $ means match end
step2<-gsub("^\\s+|\\s+$","",step1)

# remove blank rows
step3<-data.frame(step2)
step3<-data.frame(step2[step2!=""])

### begin element seperation
# reassign variable
txt<-step3

# create a new column with just rownames
txt_lab<-cbind(txt,'num'=rownames(txt))

# create a vector of row numbers for numeric
txt_part<-txt_lab[grepl('[0-9]*[A-z]|[A-z]',txt_lab[,1])==F,]

# create a vector of row numbers for alpha AND numeric
txt_part2<-txt_lab[grepl('[0-9]*[A-z]|[A-z]',txt_lab[,1])==T,]

# convert the column with numbers from factors to numeric
txt_part2[,2]<-as.numeric(as.character(txt_part2[,2]))

# find the set of numbers contained in each column for the numeric vector
el_ct<-nchar(gsub("\\S","",txt_part[,1]))+1

# creates a alphabetized and duplicated rownames based on number of elements created
lab_rep<-rep(txt_part[,2],el_ct)

# repeat a rownumber according to the element count. 
# So, "1" with 1 element is just 1, "14" with three elements is "14","14","14"
# sequence letters up to the vec value. 1=A, 2=A,B, etc
#alpha_rep<-seq(.0,.9,length.out=26)[sequence(el_ct)] 
alpha_rep<-letters[sequence(el_ct)] 

# finalize dup
#lab_new<-as.numeric(lab_rep)+as.numeric(alpha_rep) 
lab_new<-paste(lab_rep,alpha_rep,sep="")

### B. Execute element seperation ###
# split the elements according to how many spaces exist
# this should mean that element "012 123" becomes 
# two elements "012" and "123"
split<-unlist(strsplit(as.character(txt_part[,1])," ")) 

### combine A. and B. into a new data frame 
# with only one number per row
txt_part_split<-data.frame("values"=split)
txt_part_split$labels<-lab_new
txt_part_split<-data.frame('values'=split,"label"=lab_new)
names(txt_part2)<-c('values','label') #name the columns

# combine the new numeric data frame and the alphabetic dataframe
txt_part2$label<-paste(txt_part2$label,"a",sep="")
txt_new<-rbind(txt_part_split,txt_part2)            

# sort this new data frame so that it looks like 
# the original except each element has its own row
txt_new<-txt_new[mixedorder(txt_new[,2]),]
txt_new<-data.frame("values"=txt_new$values)
txt_new$label<-row.names(txt_new)

### Info: Building a data set, print a csv
### This section builds a data set. For the moment
### I've only built QBE formula earnings
### The previous section clean data

# QBE Formula Earnings
qbe_form_earn<-txt_new[grepl('QBE FORMULA EARNINGS',txt_new$values),]
qbe_form_earn_rownum<-qbe_form_earn[,2]

# system
system<-data.frame(txt_new[grepl('SYSTEM:\\s|[0-9]{3}\\s[-]\\s[aA-zZ]',txt_new$values),1])
system<-data.frame(apply(system,2,gsub,pattern="SYSTEM:\\s",replace=""))

# year
year<-txt_new[grepl('Initial|Earnings\\sSheet\\sfor\\sFY',txt_new$values),1]
year<-data.frame(gsub("(\\s*)[aA-zZ]*","",year))
# salary
salary<-data.frame(txt_new[as.numeric(qbe_form_earn_rownum)+1,1])
# operating
operating<-data.frame(txt_new[as.numeric(qbe_form_earn_rownum)+2,1])
# qbe earnings values
qbeearnings<-data.frame(txt_new[as.numeric(qbe_form_earn_rownum)+3,1])
# less 5 mills
less5mills<-data.frame(txt_new[as.numeric(qbe_form_earn_rownum)+4,1])
# state funds
statefunds<-data.frame(txt_new[as.numeric(qbe_form_earn_rownum)+5,1])

# QBE data frame
qbe<-data.frame(system,year,salary,operating,qbeearnings,less5mills,statefunds)
names(qbe)<-c('system','year','salary',
              'operating','qbeearnings','less5mills','statefunds')

name<-qbe[,1]
# clean commas
removecomma<-apply(apply(qbe[,-1], 2, gsub, patt=",", replace=""), 2, as.numeric)

# final qbe
qbe<-data.frame(name,removecomma)

destout<-paste(dest3,
            "/qbe_formula_earnings_",substr(Sys.time(),1,10),".csv",sep="")

write.csv(qbe,destout)


