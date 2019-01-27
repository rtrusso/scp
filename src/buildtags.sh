if [ -x ~/.TAGStmp ]; then rm ~/.TAGStmp; fi
etags -a -f ~/.TAGStmp `find | grep '\.scm$'`
etags --language=scheme -a -f ~/.TAGStmp `find | grep '\.sasm$'`
etags -a -f ~/.TAGStmp `find | grep '\.java$'`
etags -a -f ~/.TAGStmp `find | grep '\.cs$'`
etags -a -f ~/.TAGStmp `find | grep '\.cpp$'`
etags -a -f ~/.TAGStmp `find | grep '\.c$'`
etags -a -f ~/.TAGStmp `find | grep '\.h$'`
etags -a -f ~/.TAGStmp `find | grep '\.asm$'`
mv -f ~/.TAGStmp ~/.TAGS
