ghc -o interpreter -package array -package containers -package mtl --make interpreter.hs RunProgram.hs TypeChecker.hs -main-is Interpreter


for file in good_tests/*.txt; do
    echo "Running test from file: $file"
    ./interpreter "$file"  
done

echo ""
echo ""

for file in bad_tests/*.txt; do
    echo "Running test from file: $file"
    ./interpreter "$file"  
done