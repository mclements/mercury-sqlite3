MMC = mmc
PARALLEL = -j $(shell nproc 2>/dev/null || echo 1)

compile: test_sqlite3.m
	@$(MMC) --make $(PARALLEL) test_sqlite3 -lsqlite3
	time ./test_sqlite3

.PHONY = clean
.PHONY += test

test: test_parsing.m
	mmc --make test_parsing && ./test_parsing

clean:
	rm -rf Mercury
	rm -rf test_sqlite3
