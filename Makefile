BOOK_TESTS_REPO_URL=https://github.com/nlsandler/writing-a-c-compiler-tests.git
BOOK_TESTS_REPO_DIR=writing-a-c-compiler-tests
APP_DIR=./app/build/install/ktc/bin/ktc

.PHONY: all clone_book_tests build book_tests

all: clone_book_tests build

clone_book_tests:
	if [ ! -d "$(BOOK_TESTS_REPO_DIR)" ]; then \
        git clone $(BOOK_TESTS_REPO_URL); \
    else \
        cd $(BOOK_TESTS_REPO_DIR) && git pull; \
    fi

build:
	./gradlew installDist

book_tests: build clone_book_tests
	arch -x86_64 ./$(BOOK_TESTS_REPO_DIR)/test_compiler $(APP_DIR) $(ARGS)