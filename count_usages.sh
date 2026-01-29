#!/bin/bash

# Output CSV header
echo "module,name,kind,internal_count,src_count,dev_count,test_count,comment"

# Read the definitions file
definitions_file="/tmp/gleam_defs.txt"

# Get all definitions
grep -rE "^pub (fn|type|const) [a-zA-Z_][a-zA-Z0-9_]*" src/ dev/ --include="*.gleam" > "$definitions_file" 2>/dev/null

# Process each line
while IFS= read -r line; do
    # Parse file path
    file_path=$(echo "$line" | cut -d: -f1)

    # Skip if not a .gleam file
    [[ "$file_path" != *.gleam ]] && continue

    # Get the definition part (after line number)
    definition=$(echo "$line" | cut -d: -f2-)

    # Extract kind (fn, type, or const)
    if [[ "$definition" =~ ^pub\ fn\ ([a-zA-Z_][a-zA-Z0-9_]*) ]]; then
        kind="fn"
        name="${BASH_REMATCH[1]}"
    elif [[ "$definition" =~ ^pub\ type\ ([a-zA-Z_][a-zA-Z0-9_]*) ]]; then
        kind="type"
        name="${BASH_REMATCH[1]}"
    elif [[ "$definition" =~ ^pub\ const\ ([a-zA-Z_][a-zA-Z0-9_]*) ]]; then
        kind="const"
        name="${BASH_REMATCH[1]}"
    else
        continue
    fi

    # Convert file path to module path
    module=$(echo "$file_path" | sed 's/^src\///' | sed 's/^dev\//dev\//' | sed 's/\.gleam$//')

    # Count internal usages (within the same file, minus 1 for the definition itself)
    total_in_file=$(grep -cw "$name" "$file_path" 2>/dev/null || echo 0)
    internal_count=$((total_in_file - 1))
    [[ $internal_count -lt 0 ]] && internal_count=0

    # Count usages - search for the name as a word boundary
    # Exclude the definition file for src_count

    # For src, exclude the definition file
    if [[ "$file_path" == src/* ]]; then
        src_count=$(grep -rw "$name" src/ --include="*.gleam" 2>/dev/null | grep -v "^$file_path:" | wc -l | tr -d ' ')
    else
        src_count=$(grep -rw "$name" src/ --include="*.gleam" 2>/dev/null | wc -l | tr -d ' ')
    fi

    # For dev, exclude definition file if it's in dev
    if [[ "$file_path" == dev/* ]]; then
        dev_count=$(grep -rw "$name" dev/ --include="*.gleam" 2>/dev/null | grep -v "^$file_path:" | wc -l | tr -d ' ')
    else
        dev_count=$(grep -rw "$name" dev/ --include="*.gleam" 2>/dev/null | wc -l | tr -d ' ')
    fi

    # Test count
    test_count=$(grep -rw "$name" test/ --include="*.gleam" 2>/dev/null | wc -l | tr -d ' ')

    # Determine comment
    if [[ "$src_count" -eq 0 && "$dev_count" -eq 0 && "$test_count" -eq 0 ]]; then
        if [[ "$internal_count" -gt 0 ]]; then
            comment="internal_only"
        else
            comment="unused"
        fi
    else
        comment=""
    fi

    echo "$module,$name,$kind,$internal_count,$src_count,$dev_count,$test_count,$comment"

done < "$definitions_file"

rm -f "$definitions_file"
