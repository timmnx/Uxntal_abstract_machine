# Directories
SRC_DIR = progs
BIN_DIR = rom

# List all .tal files in progs/
SRCS = $(wildcard $(SRC_DIR)/*.tal)

# Replace progs/file.tal → rom/file
EXECS = $(patsubst $(SRC_DIR)/%.tal,$(BIN_DIR)/%.rom,$(SRCS))

# Default target
all: $(EXECS)

# Rule to build each executable
$(BIN_DIR)/%.rom: $(SRC_DIR)/%.tal | $(BIN_DIR)
	@echo Compiling $<...
	@./uxnasm $< $@

# Ensure exec directory exists
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Clean up compiled executables
clean:
	rm -f $(BIN_DIR)/*

# Optional: clean everything and remove directory
distclean: clean
	rmdir $(BIN_DIR) 2>/dev/null || true

.PHONY: all clean distclean
