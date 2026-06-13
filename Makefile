all:
	@echo "Something"

uxn_machine:
	@echo "Building UXN machines"
	@cd uxn_source/ && ./build.sh --no-run
	@echo "Adding UXN machines to ./compiler/tal/"
	@cp uxn_source/bin/uxnasm compiler/tal/uxnasm
	@cp uxn_source/bin/uxncli compiler/tal/uxncli

.PHONY: uxn_machine