MMC = mmc
PARALLEL = -j $(shell nproc 2>/dev/null || echo 1)

compile: test_sqlite3.m
	@$(MMC) --c-debug --make $(PARALLEL) test_sqlite3 -lsqlite3
	time ./test_sqlite3

.PHONY = clean
.PHONY += test
.PHONY += impure

test: test_parsing.m
	mmc --make test_parsing && ./test_parsing

impure: test_sqlite3_impure.m sqlite3_impure.m
	mmc --c-debug --make test_sqlite3_impure -lsqlite3 && ./test_sqlite3_impure

clean:
	rm -rf Mercury
	rm -rf test_sqlite3
