user=system('whoami', intern=TRUE)

system("ipcs > foo")

x <- scan("foo", what="", sep="\n")
x <- x[substring(x, 1, 3)=="0x0"]
y <- strsplit(x, " ")

for (i in 1:length(y)) {
  z <- y[[i]][y[[i]]!=""]
  if (z[3]==user) system(paste("ipcrm -m", z[2]))
}

