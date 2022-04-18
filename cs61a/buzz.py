def fizzbuzz(n):
	count = 1
	while count <= n:
		if count % 5 == 0 and count %3 == 0:
			print ('fizzbuzz')
		elif count%3 == 0 and count%5 != 0:
			print ('fizz')
		elif count % 5 == 0 and count%3 != 0:
			print ('buzz')
#		elif count // 5 != 0 or count //3 != 0:
		#elif count % 5 != 0 or count % 3 != 0:
		else:
			print count
		count += 1