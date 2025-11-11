#!/bin/bash

# Ensure folder and option variables are passed
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <folder> <number_of_steps>"
    exit 1
fi

folder=$1  # Folder path
number_of_steps=$2  # Number of steps
    

# Process files based on option
echo "******* START *******"
echo
for dir in "$folder"/*; do
    if [ -d "$dir" ]; then
        echo
        echo "*****>>> Entering folder: $dir*****"
        echo
        for file in "$dir"/*.lambda; do
            [ -e "$file" ] || continue  # Skip if no .aps files
            echo "filename : $file"
            echo "Result: "
            ./main "$file" "$number_of_steps"
            echo "----------------------------------------------"
        done
        echo "*****>>> Done with folder: $dir*****"
        echo
    fi
done

# Aps files that are there and not in dirs
for file in "$folder"/*.lambda; do
    [ -e "$file" ] || continue
    echo "filename : $file"
    echo "Result:"
    ./main "$file" "$number_of_steps"
    echo "----------------------------------------------"
done
echo "******* END *******"
