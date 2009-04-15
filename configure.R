if (Sys.info()[['sysname']] == 'Linux')
{
	file.copy('src/Makevars.linux', 'src/Makevars', overwrite=TRUE)
}
if (Sys.info()[['sysname']] == 'Darwin')
{
	file.copy('src/Makevars.darwin', 'src/Makevars', overwrite=TRUE)
}
# Last and least, copy the windows Makevar file when appropriate
if (Sys.info()[['sysname']] == 'Windows')
{
	file.copy('src/Makevars.windows', 'src/Makevars', overwrite=TRUE)
}
