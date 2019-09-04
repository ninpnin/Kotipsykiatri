for filename in $(find . -name "*.txt" -type f -maxdepth 3); do
	#echo $filename; iconv -f ISO_8859-1 -t UTF-8 client.php > client_temp.php && mv -f client_temp.php client.php
	echo $filename
	iconv -f ISO_8859-1 -t UTF-8 $filename > $filename.temp && mv -f ./$filename.temp ./$filename
done