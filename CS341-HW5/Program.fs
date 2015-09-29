//
// F# program to analyze rainfall data.
//
// <<JAMES KLONOWSKI>>
// U. of Illinois, Chicago
// CS341, Spring 2015
// Homework 5
//

#light

// ReadFile: reads a file line by line, returning a list
// of strings, one per line.
let ReadFile filename = 
  [ for line in System.IO.File.ReadLines(filename) -> line ]


// ParseLine: given a line from the rainfall file, parses this
// string into a tuple (year, values) where values is a list of
// the 12 rainfall data values for year.
let ParseLine (line:string) = 
  let strings = line.Split('\t')
  let strlist = Array.toList(strings)
  let year    = System.Int32.Parse(strlist.Head)
  let values  = List.map System.Double.Parse strlist.Tail
  (year, values)

// Given a tuple (year, values), prints these to the console
let PrintYearData (year:int, values:double list) = 
  printfn ""
  printfn "%A: %A" year values


//  Given a list of tuples (year, values), print them all
let rec PrintAll L = 
  if L = [] then printfn ""
  else
    let year, values = L.Head
    PrintAll L.Tail
    printfn "%A: %A" year values

//  Given a list of tuples (month, values), print them all
let rec PrintMonthData L = 
  if L = [] then printfn ""
  else
    let month, values = L.Head
    printfn "%O: %A" month values
    PrintMonthData L.Tail


//  Given a list of strings, parse into a list of tuples (year, monthTuple)
let rec ParseAll lines =
  if lines = [] then []
  else
    let (year, values) = ParseLine lines.Head
    let L = (year, values) :: ParseAll lines.Tail
    L

//  Given a list of tuples (year, monthTuple), return a list of years
let rec parseYears L = 
  if L = [] then []
  else
    let (year, _ ) = L.Head
    let L1 = year :: parseYears L.Tail
    L1

//  Given a list of tuples (year, monthTuple), return a list of tuples of monthly rainfall data
let rec parseMonths L = 
  if L = [] then []
  else
    let (_, values) = L.Head
    let L1 = values :: parseMonths L.Tail
    L1

//  Given a List of doubles, computes the sum
let rec SumYear L = 
  if L = [] then 0.0
  else
    L.Head + SumYear L.Tail

// Computes average given 12 months of doubles
let AverageYear L =
  let sum = SumYear L
  sum/12.0  

// Given a list of lists of floats, returns a list of 12 floats corresponding to the sum of all rainfall for that month
let rec sumMonth (L:float list list) = 
  if L.Tail = [] then L.Head
  else
    let L2 = List.map2(fun x y -> x + y) L.Head L.Tail.Head
    //printfn "L2: %A" L2
    sumMonth (L2 :: L.Tail.Tail)

//  Given a list of lists of floats, returns a list of 12 floats corresponding to the average of all rainfall for that month
let AverageMonth L = 
  let sumList = sumMonth L
  let length = float L.Length
  List.map(fun x -> x/length) sumList

//  
//  Given a list of floats, a tuple (maxSoFar, year, month), and an index, returns the tuple with the max rainfall
//  (I feel I could have executed this more elegantly with higher-order code...)
//
let rec _max L (maxSoFar:float, year:int, month:int) index = 
  if L = [] then (maxSoFar, year, month)
  else if L.Head > maxSoFar then
    _max L.Tail (L.Head, year, (index)) (index+1)
  else
    _max L.Tail (maxSoFar, year, month) (index+1)
  
//  Given a list of tuples (year, data), maps the _max function to return a list of the max rainfall each year
let MaxByYear L = 
  List.map(fun (year:int, data:float list) -> _max data.Tail (data.Head, year, 1) 2) L

//  Given a list of tuples (rainfall, year, month) from all years of data and a tuple to keep track of the previous max, returns the overall max tuple
let rec MaxMonth L (maxSoFar:float, year:int, month:int) =
  if L = [] then (maxSoFar, year, month)
  else
    let v,y,m = L.Head
    if v>maxSoFar then 
      MaxMonth L.Tail (v, y, m)
    else 
      MaxMonth L.Tail (maxSoFar, year, month)

//  
//  Given a list of floats, a tuple (minSoFar, year, month), and an index, returns the tuple with the min rainfall
//  (I feel I could have executed this more elegantly with higher-order code...)
//
let rec _min L (minSoFar:float, year:int, month:int) index = 
  if L = [] then (minSoFar, year, month)
  else if L.Head < minSoFar then
    _min L.Tail (L.Head, year, (index)) (index+1)
  else
    _min L.Tail (minSoFar, year, month) (index+1)

//  Given a list of tuples (year, data), maps the _min function to return a list of the min rainfall each year  
let MinByYear L = 
  List.map(fun (year:int, data:float list) -> _min data.Tail (data.Head, year, 1) 2) L

//  Given a list of tuples (rainfall, year, month) from all years of data and a tuple to keep track of the previous min, returns the overall min tuple
let rec MinMonth L (minSoFar:float, year:int, month:int) =
  if L = [] then (minSoFar, year, month)
  else
    let v,y,m = L.Head
    if v < minSoFar then 
      MinMonth L.Tail (v, y, m)
    else 
      MinMonth L.Tail (minSoFar, year, month)

//  Printing min/max data
let PrintMinMaxValue (value:float, year:int, month:int) =
  printfn " rainfall:\t%.2f, %A/%A" value month year

  

//
// Main:
//
[<EntryPoint>]
let main argv = 
  // read entire file as list of strings:
  let file = ReadFile "rainfall-midway.txt"
  let monthList = ["Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"]
    
  //  parse the entire file into a list of tuples of (year, data)
  let tupleList = ParseAll file
  
  //  create a list containing each year
  let yearList = parseYears tupleList

  //  create a list of lists of each year's monthly rainfall
  let yearDataList = parseMonths tupleList

  //  compute average rainfall per year, and map the output into tuples of the format (year, average)
  let yearAvgData = List.map2(fun x y -> (x, y)) yearList (List.map(fun x -> AverageYear x) yearDataList)

  //  compute average rainfall by month, and map the output into tuples of the format (month, average)
  let av1 = AverageMonth yearDataList
  let monthAvgData = List.map2(fun x y -> (x, y)) monthList av1

  // compute the maximum monthly rainfall over the entire data set
  let maxList = MaxByYear tupleList
  let maxMonth = MaxMonth maxList maxList.Head

  // compute the minimum monthly rainfall over the entire data set
  let minList = MinByYear tupleList
  let minMonth = MinMonth minList minList.Head

  // output
  printfn "** Rainfall Analysis Program **"
  printfn ""
  PrintAll yearAvgData
  printfn ""
  PrintMonthData monthAvgData
  printfn ""


  printf "Max"
  PrintMinMaxValue maxMonth
  printf "Min"
  PrintMinMaxValue minMonth

  // done:
  printfn ""
  printfn ""
  printfn "** Done **"
  0 // return 0 => success