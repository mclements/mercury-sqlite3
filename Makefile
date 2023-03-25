MMC = mmc
PARALLEL = -j $(shell nproc 2>/dev/null || echo 1)
GRADE = asm_fast.gc


compile: test_sqlite3.m
	@$(MMC) --c-debug -s $(GRADE) --make $(PARALLEL) test_sqlite3 -lsqlite3
	time ./test_sqlite3

.PHONY = clean
.PHONY += test
.PHONY += impure

test: test_parsing.m
	mmc --make test_parsing && ./test_parsing

clean:
	rm -rf Mercury
	rm -rf test_sqlite3
