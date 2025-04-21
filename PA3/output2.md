Running ../testing/arithmetic.cl-type
--
Input a cool file, not ../testing/bad_int_in.cl-input

Running ../testing/case-on-void.cl-type
----------------------------------------------------
1c1
< ERROR: 5: Exception: case on void
---
> ERROR: 5: Exception: case without matching branch

Running ../testing/cases.cl-type
----------------------------------------------------
2c2
< ERROR: 6: Exception: case without matching branch: Int(5)
---
> ERROR: 6: Exception: case without matching branch

Running ../testing/case_variations.cl-type
----------------------------------------------------
./run.sh: line 36: 3545669 Segmentation fault      (core dumped) ./program &> ./our-output.txt
1d0
< Main

Running ../testing/heap-overflow.cl-type
----------------------------------------------------
./run.sh: line 36: 3546712 Segmentation fault      (core dumped) ./program &> ./our-output.txt
1d0
< ERROR: 3: Exception: stack overflow

Running ../testing/hs.cl-type
----------------------------------------------------
./run.sh: line 36: 3546787 Segmentation fault      (core dumped) ./program &> ./our-output.txt
1d0
< 17141611714163171416511714161171416317141653117141611714163171416511714161171416317141653171416117141631714165171416


Running ../testing/no-match-cases.cl-type
----------------------------------------------------
1c1
< ERROR: 5: Exception: case without matching branch: A(...)
---
> ERROR: 5: Exception: case without matching branch

Running ../testing/primes.cl-type
----------------------------------------------------
./run.sh: line 36: 3548128 Segmentation fault      (core dumped) ./program &> ./our-output.txt
1,96d0
< 2 is trivially prime.
< 3 is prime.
< 5 is prime.


Running ../testing/self-case.cl-type
----------------------------------------------------
./run.sh: line 36: 3548627 Segmentation fault      (core dumped) ./program &> ./our-output.txt
1d0
< BCDD
\ No newline at end of file


Running ../testing/sort-list.cl-type
----------------------------------------------------
1
1
1c1
< How many numbers to sort? 0
---
> How many numbers to sort? abort


Running ../testing/special_characters.cl-type
----------------------------------------------------
4,5c4
< \"\
< \
\ No newline at end of file
---
> \"\\n\\t
--

Running ../testing/stack-overflow.cl-type
----------------------------------------------------
./run.sh: line 36: 3549916 Segmentation fault      (core dumped) ./program &> ./our-output.txt
1d0
< ERROR: 11: Exception: stack overflow


Running ../testing/static-void-dispatch.cl-type
----------------------------------------------------
1c1
< ERROR: 6: Exception: static dispatch on void
---
> ERROR: 6: Exception: dispatch on void

