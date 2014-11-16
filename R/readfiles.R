#' Read multiple files at the same time.
#' 
#' @param path Where the data is.
#' @param subject Initials of subjects separated by commas.
#' @param exp Name of the experiment.
#' @param session Number of the session.
#' @export
readfiles <- function(path = 'data', subject = '.', exp = '.',session = '.') {
  subject <- gsub(',', replace = '|', subject)
  exp <- gsub(',', replace = '|', exp)
  session <- gsub(',', replace = '|', session)
  r_exp <- paste('^(', subject,').*', '(',exp,').*', '[',session,']',sep='')
  print(path)
  names_files<-list.files(pattern = r_exp,path = path,full.names = TRUE)
  print(names_files)
  read_data <- function(x) {
    y <- gsub(paste('(', path, '/)|(\\.txt)', sep = ""), '', x)
    subj <- substr(y , 0, 2)
    session <- gsub('[a-zA-Z]', '', y)
    exp <- gsub(paste('(', session, ')|(', subj, ')', sep = '') , '',y)
    d <- read.table(x, header = TRUE)
    d$subject <- toupper(subj)
    d$exp <- exp
    d$session <- session
    return(d)
  }
  data<- do.call('rbind', lapply(names_files, read_data))
  data
}