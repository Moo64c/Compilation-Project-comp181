#!/bin/bash
# build target
total_tests=0
tests_passed=0
tests_failed=0
rm -R testdir
mkdir testdir
for f in $(ls -v tests | grep .in); do
  name=$(echo $f | cut -f 1 -d '.')
  echo '(load "loader.scm") (compile-scheme-file "tests/'$name'.in" "testdir/'$name'.s")' | scheme -q
  echo "Compiling $name...."
  cd testdir
  nasm -f elf64 -F stabs -l $name.lst  $name.s
  gcc -m64 -g -o $name $name.o
  echo "Compiling $name done!"
  ./$name > $name.out

  our=`cat $name.out`
  chez=`cat ../tests/$name.out`

  result=""
  if [ "$our" = "$chez" ]; then
    result="#t"
  else
    result=`echo "(equal? \"$our\" \"$chez\")" | scheme -q`
  fi

  if [ "$result" = "#t" ]; then
      echo -e "\033[1;32mTest $f Passed ☺ \033[0m"
      tests_passed=$((tests_passed+1))
  else
      echo -e "\033[1;31m *** RESULTS DIFFER in $f ☹\033[0m"
      echo "*** scheme output: $chez"
      echo "*** our output: $our"
      tests_failed=$((tests_failed+1))
  fi
  cd ..
done

rm testdir/*.lst testdir/*.o
echo "tests failed: " $tests_failed
