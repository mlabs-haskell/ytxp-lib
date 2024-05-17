.PHONY: usage

usage:
	@echo "usage: make <command>"
	@echo
	@echo "Available commands:"
	@echo ""
	# Code format (works for both ytxp-plutarch and ytxp-convex)
	@echo "    format_lint                                                 -- Formats .hs, .cabal, .nix files and auto-refactors code"
	@echo "    format                                                      -- Formats .hs, .cabal, .nix files"
	@echo "    format_check                                                -- Check formatting of .hs, .cabal, .nix files"

	@echo "    format_haskell                                              -- Formats .hs files"
	@echo "    format_check_haskell                                        -- Check formatting of .hs files"
	@echo "    format_nix                                                  -- Formats .nix files"
	@echo "    format_check_nix                                            -- Check formatting of .nix files"
	@echo "    format_cabal                                                -- Formats .cabal files"
	@echo "    format_check_cabal                                          -- Check formatting of .cabal files"
	@echo "    lint                                                        -- Auto-refactors code"
	@echo "    lint_check                                                  -- Run code linting"
	@echo ""
	# Build
	@echo "    build_all                                                   -- Build all"
	@echo "    build_ytxp-plutarch                                         -- Build ytxp-plutarch"
	@echo "    build_testlib                                               -- Build testlib"
	@echo "    build_pprelude                                              -- Build pprelude"
	@echo "    build_write-config                                          -- Build write-config"
	@echo ""
	# Test
	@echo "    test_serialization                                          -- Test serialization"
	@echo ""
	# Check Typos
	@echo "    typos_check                                                 -- Check typos"
	@echo "    typos_fix                                                   -- Fix typos"
	@echo ""
	# Doc
	@echo "    build_doc                                                   -- Build haddock documentation"
	@echo ""
	# Doc
	@echo "    build_doc                                                   -- Build haddock documentation"
	@echo "    lint_markdown_check                                         -- Check markdownlint suggestions"
	@echo "    lint_markdown                                               -- Apply markdownlint suggestions"
	@echo ""

################################################################################
# Code

# Leave the cabal build directory and the legacy code submodule alone
FIND_EXCLUDE_PATH := -not -path '*/dist-*/*'

# NOTE: we want to apply the same style rule for both ytxp-plutarch and ytxp-convex even if the
# GHC version is different)
FIND_HASKELL_SOURCES := find -name '*.hs' $(FIND_EXCLUDE_PATH)
FIND_NIX_SOURCES := find -name '*.nix' $(FIND_EXCLUDE_PATH)
FIND_CABAL_SOURCES := find -name '*.cabal' $(FIND_EXCLUDE_PATH)
FIND_MARKDOWN_SOURCES := find -name '*.md' $(FIND_EXCLUDE_PATH)

# Runs as command on all results of the `find` call at one.
# e.g.
#   foo found_file_1 found_file_2
find_exec_all_fn = $(1) -exec $(2) {} +

# Runs a command on all results of the `find` call one-by-one
# e.g.
#   foo found_file_1
#   foo found_file_2
find_exec_one_by_one_fn = $(1) | xargs -i $(2) {}


.PHONY: format
format: format_haskell format_nix format_cabal
format_check : format_check_haskell format_check_nix format_check_cabal

# Run fourmolu of .hs files
.PHONY: format_haskell
format_haskell: 
	$(call find_exec_all_fn, $(FIND_HASKELL_SOURCES), fourmolu -i)

.PHONY: format_check_haskell
format_check_haskell:
	$(call find_exec_one_by_one_fn, $(FIND_HASKELL_SOURCES), fourmolu --mode check)

# Run nixpkgs-fmt of .nix files
.PHONY: format_nix
format_nix:
	$(call find_exec_all_fn, $(FIND_NIX_SOURCES), nixpkgs-fmt)

.PHONY: format_check_nix
format_check_nix:
	$(call find_exec_all_fn, $(FIND_NIX_SOURCES), nixpkgs-fmt --check)

# Run cabal-fmt of .cabal files
.PHONY: format_cabal
format_cabal:
	$(call find_exec_all_fn, $(FIND_CABAL_SOURCES), cabal-fmt -i)

.PHONY: format_check_cabal
format_check_cabal:
	$(call find_exec_all_fn, $(FIND_CABAL_SOURCES), cabal-fmt --check)


# Apply hlint suggestions
.PHONY: lint_haskell
lint_haskell:
	$(call find_exec_one_by_one_fn, $(FIND_HASKELL_SOURCES), hlint -j --refactor --refactor-options="-i")

# Check hlint suggestions
.PHONY: lint_haskell_check
lint_haskell_check:
	$(call find_exec_all_fn, $(FIND_HASKELL_SOURCES), hlint -j)

# Apply lint suggestions
.PHONY: lint
lint: lint_haskell lint_markdown

# Apply format and hlint
.PHONY: format_lint
format_lint: format lint

################################################################################
# Build
.PHONY: build_all
build_all:
	cabal build -j all

.PHONY: build_ytxp-plutarch
build_ytxp-plutarch:
	cabal build -j ytxp-plutarch

.PHONY: build_testlib
build_testlib:
	cabal build -j testlib

.PHONY: build_pprelude
build_pprelude:
	cabal build -j pprelude

.PHONY: build_write-config
build_write-config:
	cabal build -j write-config

################################################################################
# Test
.PHONY: test_serialization
test_serialization:
	cabal test -j serialization

################################################################################
# Test
.PHONY: typos_check
typos_check :
	typos -c ../../.typos.toml ../../

.PHONY: typos_fix
typos_fix:
	typos -w -c ../../.typos.toml ../../

################################################################################
# Doc
.PHONY: build_doc
build_doc:
	cabal haddock --haddock-all --haddock-quickjump

# Check markdownlint suggestions
.PHONY: lint_markdown_check
lint_markdown_check: 
	$(call find_exec_all_fn, $(FIND_MARKDOWN_SOURCES), markdownlint)

# Apply markdownlint suggestions.
# NOTE: there are some warnings that cannot be automatically fixed
.PHONY: lint_markdown
lint_markdown: 
	$(call find_exec_all_fn, $(FIND_MARKDOWN_SOURCES), markdownlint -f)
