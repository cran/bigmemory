#!/usr/bin/perl
for $fileName (@ARGV)
{
	$newFileName = $fileName . ".new";
	`sed 's/\\t/  /g' $fileName > $newFileName`;
	$d = `diff $fileName $newFileName`;
	if (length($d) == 0)
	{
		`rm $newFileName`;
	}
	else
	{
		`mv $newFileName $fileName`;
	}
}


