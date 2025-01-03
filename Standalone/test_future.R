library(future)
plan(multicore)

a = future({
			Sys.sleep(2)
			Sys.getpid()
    })
b = future({
			Sys.sleep(2)
			Sys.getpid()
    })
c = future({
			Sys.sleep(2)
			Sys.getpid()
    })
d = future({
			Sys.sleep(2)
			Sys.getpid()
    })
e = future({
			Sys.sleep(2)
			Sys.getpid()
    })
f = future({
			Sys.sleep(2)
			Sys.getpid()
    })
print(paste0("Future a returned: ", value(a)))
print(paste0("Future b returned: ", value(b)))
print(paste0("Future c returned: ", value(c)))
print(paste0("Future d returned: ", value(d)))
print(paste0("Future e returned: ", value(e)))
print(paste0("Future f returned: ", value(f)))
