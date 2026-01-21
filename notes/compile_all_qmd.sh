#!/bin/bash

# Compile all .qmd files in the current directory to HTML
for file in *.qmd; do
    if [ -f "$file" ]; then
        echo "Rendering: $file"
        quarto render "$file"
    fi
done