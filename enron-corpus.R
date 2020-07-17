if (!file.exists("enron/enron_mail_20150507.tgz")) {
    download.file("https://www.cs.cmu.edu/~./enron/enron_mail_20150507.tar.gz",
                   destfile = "enron/enron-mail-20150507.tgz")
}

if (file.exists("enron/enron-mail-20150507.tgz")) {
    untar("enron/enron-mail-20150507.tgz")
}

emails <- list.files("maildir/", full.names = TRUE, recursive = TRUE)
emails <- emails[grep("/inbox", emails)]
length(emails)

library(tibble)
inboxes <- tibble(
    from = apply(as.data.frame(emails), 1, 
               function(x){readLines(x, warn = FALSE)[3]}),
    to = emails,
    stringsAsFactors = FALSE
)

## Keep only enron.com and strip all but username
library(stringr) # String manipulation
inboxes <- inboxes[grepl("@enron.com", inboxes$from),]
inboxes$from <- str_remove_all(inboxes$from, "From: |@enron.com")
inboxes$to <- str_remove_all(inboxes$to, "maildir//|/inbox/[0-9]*.")

## Create list of usernames
users <- data.frame(user = paste0("maildir/", unique(inboxes$to)))

## Remove those without sent mails
sent <- apply(users, 1, function(x) sum(grepl("sent", dir(x))))
users <- subset(users, sent != 0)

## Replace username with e-mail name
users$mailname <- NA
i < -1
for (i in 1:nrow(users)){
    sentmail <- dir(paste0(users$user[i], "/sent_items/"))
    name <- readLines(paste0(users$user[i], "/sent_items/", sentmail[1]), warn = F)[3]
    name <- str_remove_all(name, "From: |@enron.com")
    users$mailname[i] <- name
}

users$user <- str_remove(users$user, "maildir/")
inboxes <- merge(inboxes, by.x = "to", users, by.y = "user")
inboxes <- tibble(from = inboxes$from,
                  to = inboxes$mailname)

## Only e-mails between users inside the corpus
inboxes <- inboxes[inboxes$from %in% inboxes$to,]

## Remove no.address
inboxes <- subset(inboxes, from != "no.address" & to != "no.address")

## Remove emails to self
inboxes <- subset(inboxes, inboxes$from != inboxes$to)
inboxes
