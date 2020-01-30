ll = readLines("CurrentTags")
odefs = ll[23:37]
tmp = strsplit(odefs, " ")
d = as.data.frame(do.call(rbind, lapply(tmp, `[`, c(1, 3))), stringsAsFactors = FALSE)
d$msg = gsub(".*tag:[^)]+\\) ", "", odefs)
names(d) = c("sha", "tag", "msg")
d$tag = gsub("\\)$", "", d$tag)


tmp = ll[45:109]
log = data.frame(sha = gsub("^([^ ]+) .*", "\\1", tmp),
                 msg = gsub("^[^ ]+ ", "", tmp), stringsAsFactors = FALSE)


m = match(d$msg, log$msg)
# The one that is not present is the one we just re-based.
# That's because we added an extra sentence.
# So
m[is.na(m)] = grep(d$msg[11], log$msg, fixed = TRUE)

d$newSha = log$sha[m]


cat(sprintf("git tag -d %s", d$tag), "", "", sprintf("git tag -a %s %s -m '%s'", d$tag, d$newSha, d$msg), sep = "\n")


