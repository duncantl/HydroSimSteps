ff = list.files("Sandbox5", pattern = "\\.prof$", full = TRUE)
pds = structure(lapply(ff, readProfileData), names = basename(ff))
names(pds) = gsub("\\.prof$", "", basename(ff))

# Reorder based on the order of the tags
tags = system("make tags NO_RANDOM=1", intern = TRUE)
pds = pds[tags]

invisible(mapply(function(f, d) {plot(filterProfileData(d, skip = 6), type = "flame", main = f); readLines(n = 1)}, names(pds), pds))

