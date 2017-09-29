#!/bin/bash

run_test() {
  file=$1
  out=$(dist/build/runlam/runlam $file);
  if [[ $out ]]
    then echo "$out" > ${file%".lam"}.output;
  fi
}
export -f run_test
find programs -name "*.lam" | xargs -n1 -I{} bash -c 'run_test "$@"' _ {} 

