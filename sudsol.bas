********************************************************************************
rem ***                           SUDSOL                                     ***
********************************************************************************

REM  *** Solves sudoku puzzles ***

********************************************************************************
rem ***                      DEFINITIONS AND DECLARATIONS                    ***
********************************************************************************

REM  *** Declarations ***

defdbl a-z

REM  *** Hard coded values/limits ***
const ORDER = 3
const FAIL  = 99
const FALSE = 0
const TRUE  = 1
const VERY  = 2

REM *** Declare puzzle type - result() contains the correct values, scratch() is the tick-off area
type puzzle
  result(1 to ORDER^2, 1 to ORDER^2) as integer
  scratch(1 to ORDER^2, 1 to ORDER^2, 1 to ORDER^2) as integer
end type

REM *** Variables
dim shared reclevel as integer
dim shared verbose as integer
dim temp as string
dim p as puzzle ptr
dim i as integer
dim j as integer
dim k as integer
dim infile as string

REM *** Function / sub declarations ***
declare function set_two(p as puzzle ptr, which as integer) as integer
declare function solve(p as puzzle ptr) as integer
declare function make_certain(p as puzzle ptr) as integer
declare function rule_out(p as puzzle ptr) as integer

********************************************************************************
rem ***                              MAIN PROGRAM                            ***
********************************************************************************

rem *** Read command line options
rem *** -v = verbose
rem *** -d = debug (not documented)
rem *** -? = list help
rem *** -f file = input file

if instr(command$,"-?") then
  print
  print"sudsol [-v] [-f filename]"
  print"  -v verbose	Turns on diagnostic printing"
  print"  -f filename	Specifies the input file name"
  print
  print"  Input files must be .csv format, one row of the puzzle per line."
  end
end if

verbose = FALSE
if instr(command$,"-v") then verbose = TRUE
if instr(command$,"-d") then verbose = VERY

i = instr(command$,"-f")
if (i<>0) then infile$ = mid$(command$,i+3,instr(right$(command$,len(command$)-i-3)," "))

if (infile$="") then input "Type filename for data input (.csv is added) > ",infile$

if (right$(infile$,4)<>".csv") then infile$ = infile$ + ".csv"

rem *** Allocate space and read in grid from file ***

open infile$ for input as #1
p = allocate(len(puzzle)*2)
if (verbose) then print "Reading puzzle from ";infile$
i = 1
j = 1
k = 0
for i = 1 to ORDER^2
  line input #1, temp$
  for j = 1 to ORDER^2
    p->result(i,j) = VAL(temp$)
    if (verbose) then print ;p->result(i,j);" ";
    k = instr(temp$,",")
    temp$ = right$(temp$,len(temp$)-k)
    for k = 1 to ORDER^2
      p->scratch(i,j,k) = 1
    next k
  next j
  if (verbose) then print
next i
close #1

rem *** Set level of recursion (global var)

reclevel = 0

rem *** Call solver as subroutine so we can recurse

if (solve(p) = FAIL) then

  print"Ill-formed problem!"

else

rem *** Finished - print solution
  print"Solution is ..."
  for i = 1 to ORDER*ORDER
    for j = 1 to ORDER*ORDER
      print ;p->result(i,j);" ";
    next j 
    print
  next i
  print

end if

end

********************************************************************************
rem ***                              FUNCTIONS                               ***
********************************************************************************

rem ***
rem *** SOLVE
rem ***
rem *** Attempts to solve the given puzzle, returning a status code when done
rem ***

function solve(p as puzzle ptr) as integer

  dim wkg as puzzle ptr
  dim i as integer
  dim j as integer
  dim k as integer
  dim outcome as integer
  dim acted as integer
  dim holding as integer

rem *** Increment recursion level

  reclevel = reclevel + 1
  if (verbose) then print "Solving at recursion level ";reclevel

rem *** First copy the passed puzzle into a working area (this enables recursion)

  wkg = allocate(len(puzzle)*2)

  for i = 1 to ORDER^2
    for j = 1 to ORDER^2
      wkg->result(i,j) = p->result(i,j)
      for k = 1 to ORDER^2
        wkg->scratch(i,j,k) = p->scratch(i,j,k)
      next k
    next j
  next i

rem *** Continue standard method of elimination

  do

rem *** Propagate known values to eliminate others in boxes, rows and columns
    acted = rule_out(wkg)

rem *** Fill-in any certainties
    outcome = make_certain(wkg)

rem *** If nothing could be ruled out, or the puzzle is solved or unsolvable take remedial action
  loop until (acted=FALSE or outcome=TRUE or outcome=FAIL)

rem *** Elimination method has got stuck (or we've finished successfully or it's unsolvable)

rem *** So try setting a value speculatively and recurse from there

    if (outcome = FALSE) then
      if (verbose) then print"Basic elimination is stuck."
      outcome = TRUE
      holding = set_two(wkg,0)
      if (solve(wkg)<>TRUE) then
rem *** Guess didn't work, so flip to the other option
        set_two(wkg,holding)
        if (solve(wkg)<>TRUE) then
rem *** reclevel > 1 means we're in a recursion so just need to tell the layer above to try something else
rem *** reclevel = 1 means we've failed altogether and that msg will be printed by the main program when we pop-up
          outcome = FAIL
          if (reclevel > 1 and verbose) then print"Second option for square has failed - back-tracking."
        end if
      end if
    end if

rem *** Copy working area to 'good' area if we are going to return success
    if (outcome = TRUE OR (outcome = FAIL AND reclevel = 1)) then
      for i = 1 to ORDER^2
        for j = 1 to ORDER^2
          p->result(i,j) = wkg->result(i,j)
        next j
      next i
    end if

rem *** If outcome = FAIL or TRUE it will be returned and action taken one recursion level above
rem *** Only ever returns FAIL or TRUE because FALSE always causes a recursion

  if (verbose) then print"Solver level ";reclevel;" outcome is ";outcome

  reclevel = reclevel - 1

  solve = outcome

end function

rem ***
rem *** SET_TWO
rem ***
rem *** Finds a square with only two possibilities and assigns one, saving the location
rem *** and both values in case we need to roll-back on failure
rem ***

function set_two(p as puzzle ptr, lastone as integer) as integer

  dim ssum as integer
  dim theone as integer
  dim i as integer
  dim j as integer
  dim k as integer

  if (lastone <> 0) then
    if (verbose) then
      print"Reverting to second option, ";lastone mod 10;", for square (";int(lastone/1000);",";int((lastone-int(lastone/1000)*1000)/100);")"
    end if
    p->result(int(lastone/1000),int((lastone-int(lastone/1000)*1000)/100)) = lastone mod 10
  else
    for i = 1 to ORDER^2
      for j = 1 to ORDER^2
        if (p->result(i,j) = 0) then
          ssum = 0
          theone = 0
          for k = 1 to ORDER^2
            ssum = ssum + p->scratch(i,j,k)
            if (p->scratch(i,j,k)=1) then
              if (theone = 0) then theone = k else theone = theone + 10*k
            end if
          next k
          if (ssum = 2) then
            if (verbose) then
              print"Trying first option, ";int(theone/10);", for square (";i;",";j;"), and remembering ";theone mod 10
            end if
            p->result(i,j) = int(theone/10)
            theone = i*1000 + j*100 + theone
            i = ORDER^2 + 1
            j = ORDER^2 + 1
          end if
        end if
      next j
    next i
  end if

  set_two = theone

end function

rem ***
rem *** MAKE_CERTAIN
rem ***
rem *** Finds squares with only one possibility and assigns the value, returning status to indicate
rem *** whether or not the puzzle is complete (TRUE/FALSE) and if the puzzle is now inconsistent (FAIL)
rem *** meaning that at least one square now has no options available, or a square eliminated down to
rem *** one option cannot be that option as it is a duplicate.
rem ***

function make_certain(p as puzzle ptr) as integer

rem  dim passed as puzzle
  dim temp as integer
  dim theone as integer
  dim finished as integer
  dim i as integer
  dim j as integer
  dim k as integer
  dim x as integer
  dim y as integer

  temp = FALSE
  finished = TRUE
  for i = 1 to ORDER^2
    for j = 1 to ORDER^2
      if (p->result(i,j) = 0) then
        ssum = 0
        theone = 0
        for k = 1 to ORDER^2
          ssum = ssum + p->scratch(i,j,k)
          if (p->scratch(i,j,k)=1) then theone = k
        next k
        if (ssum = 0) then temp = FAIL
        if (ssum = 1) then
rem check it's not a duplicate
          for k = 1 to ORDER^2
            x = int((i-1)/3)*3 + ((k-1) MOD 3) + 1
            y = int((j-1)/3)*3 + int((k-1)/3)  + 1
            if (p->result(i,k) = theone OR p->result(k,j) = theone OR p->result(x,y) = theone) then
              if (verbose) then
                print"Square (";i;",";j;") must be ";theone;" but is a duplicate. Puzzle ill-formed!"
              end if
              temp = FAIL
              theone = 0
              k = ORDER^2+1
            end if
          next k
          p->result(i,j) = theone
        end if
      end if
      if (p->result(i,j) = 0) then finished = FALSE
    next j
  next i

  if (finished = TRUE) then temp = TRUE

  make_certain = temp  

end function

rem ***
rem *** RULE_OUT
rem ***
rem *** Finds squares with values assigned and eliminates that value from all other
rem *** squares in the same row, column and box.  Returns status indicating whether
rem *** any eliminations were made or not (TRUE/FALSE).
rem ***

function rule_out(p as puzzle ptr) as integer

  dim temp as integer
  dim i as integer
  dim j as integer
  dim k as integer
  dim l as integer

  temp = FALSE
  for i = 1 to ORDER^2
    for j = 1 to ORDER^2
      if (p->result(i,j) <> 0) then
        if (verbose=VERY) then print"Eliminating ";p->result(i,j);" from (";i;",";j;")";
        for k = 1 to ORDER^2
          if (p->scratch(i,k,p->result(i,j)) = 1) then
            temp = TRUE
            p->scratch(i,k,p->result(i,j)) = 0
          end if
          if (p->scratch(k,j,p->result(i,j)) = 1) then
            temp = TRUE
            p->scratch(k,j,p->result(i,j)) = 0
          end if
        next k
        if (verbose=VERY) then print ;" and from box at (";int((i-1)/3)*3;",";int((j-1)/3)*3;")"
        for k = 0 to ORDER-1
          for l = 0 to ORDER-1
            if (p->scratch(int((i-1)/3)*3+k+1,int((j-1)/3)*3+l+1,p->result(i,j)) = 1) then
              temp = TRUE
              p->scratch(int((i-1)/3)*3+k+1,int((j-1)/3)*3+l+1,p->result(i,j)) = 0
            end if
          next l
        next k
      end if
    next j
  next i

  rule_out = temp  

end function

