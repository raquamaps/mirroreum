#!/usr/bin/Rscript

# setting up users

if (!file.exists("credentials.csv")) {
  students <- readLines("researchers.csv")
  # students <- c(students, "MARKUS,SKYTTNER,markus.skyttner@nrm.se")
  users <- read.csv(stringsAsFactors = FALSE, file = textConnection(open = "r", 
    object = gsub("\\s+", " ", students, fixed = TRUE)))
  
  login <- sapply(strsplit(users$Email, "@", fixed = TRUE), "[[", 1)
  pass_url <- paste0("https://www.random.org/passwords/?num=", length(login), "&len=8&format=plain&rnd=new")
  pass <- readLines(pass_url)
  
  users$Login <- login
  users$Pass <- pass
  write.csv(users, file = "credentials.csv", row.names = FALSE)  
}

users <- read.csv("credentials.csv")

base_path <- paste0(getwd(), "/skeleton")
home_base <- paste0(getwd(), "/researchers")

add_user <- paste0("sudo useradd -m -s /bin/bash --skel ", base_path,
  " -d ", paste0(home_base, "/", users$Login),                       
  " -p `openssl passwd -1 ", users$Pass, "` ", users$Login)

del_user <- paste0("sudo deluser --remove-home ", users$Login)

header <- "#!/bin/bash
# execute this script as super user! 
# (ie chmod +x and sudo ./scriptname.sh)
"

bash_add <- paste0(header, 
  paste(collapse = "\n", add_user))

bash_del <- paste0(header, 
  paste(collapse = "\n", del_user))

write(bash_add, file = "user_add.sh")
write(bash_del, file = "user_del.sh")

