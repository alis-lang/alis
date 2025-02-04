for f in semantic-analysis/*; do 
    [[ "$f" == *.alis ]] && echo "$f: $(head -n 1 "$f" | cut -c 21-)";
done
