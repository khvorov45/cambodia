PROJ="/home/khvorova/Projects/cambodia"
SHARED="/home/khvorova/vidrlwhoflu/Research"

rsync -rv "$PROJ" "$SHARED" --exclude "renv/library" --exclude ".snakemake"
