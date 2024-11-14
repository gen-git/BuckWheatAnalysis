#!/bin/sh
items=(
    "map_ssp585_DiffLenSF"
    "map_ssp585_DiffLenFH"
    "map_ssp585_ChangeGSRInteSF"
    "map_ssp585_ChangeGSRInteFH"
)

for item in "${items[@]}" ; do
    convert -resize 75% "Plot/"${item}".tiff" "Plot/"${item}".pdf"
    echo ${item}" converted"
done
