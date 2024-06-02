for file in good/*.txt; do
    echo "Running test from file: $file"
    ./interpreter "$file"  
done

echo ""
echo ""

for file in bad/*.txt; do
    echo "Running test from file: $file"
    ./interpreter "$file"  
done

echo ""
echo ""

for file in moodle/*.txt; do
    echo "Running test from file: $file"
    ./interpreter "$file"  
done