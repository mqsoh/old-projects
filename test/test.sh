#!/bin/sh

test_host="test.local"

test_urls=(
	"http://$test_host/"
	"http://$test_host/content/restful-numeric-id/12345"
	"http://$test_host/content/a-non-numeric-id-should-fail/AbCdefGH"
	"http://$test_host/no/rule/404"
	"http://$test_host/multiple-matched-groups/1234567890/letters/"
	"http://$test_host/query-test/?foo=bar&baz=qux"
	"http://$test_host/com/plica-ted/garbage"
)

for url in ${test_urls[*]}; do
	echo
	echo "========================================"
	echo
	echo "Testing:"
	echo $url
	echo
	curl -s \
	-H "User-Agent: Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.3) Gecko/20100424 Gentoo Firefox/3.6.3" \
	-H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" \
	-H "Accept-Language: en-us,en;q=0.5" \
	-H "Accept-Encoding: deflate" \
	-H "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7" \
	-H "Keep-Alive: 115" \
	-H "Connection: keep-alive" \
	-d "posttest=This is some test POST data." \
	$url

	echo
done
