#!/bin/bash

# Print message for the user
echo "Select your favorite language"

# Define the list of a menu item with select
select language in C Rust Go Pascal Ada Lua Fortran Bash Exit
do
  if [[ $language == "Exit" ]] 
  then
    exit 0
  else
    echo "Selected language is $language"
  fi
done
