module GiantSquid exposing (..)
import List.Extra exposing (..)
import Array exposing (..)

sampleInput = """
  7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
"""

sampleBoards = """
22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""

rejectEmpty : String -> Bool
rejectEmpty s =
  s /= ""

toInt : String -> Int
toInt e =
  Maybe.withDefault 0 (String.toInt e)

type alias BingoBoard = List (List BingoNumber)

type BingoNumber =
  Marked Int
  | Unmarked Int

bnToInt bingoNumber =
  case bingoNumber of
    Marked x ->
      x
    Unmarked x ->
      x

markRow : Int -> List BingoNumber -> List BingoNumber
markRow num numbers =
  List.map (\e ->
    case e of
      Marked number ->
        Marked number
      Unmarked number ->
        if number == num then
          Marked number
        else
          Unmarked number
  ) numbers

markBoard : Int -> BingoBoard -> BingoBoard
markBoard num board =
  List.map (\row ->
    markRow num row
  ) board

markBoards num boards =
  List.map (\board ->
    markBoard num board
  ) boards

isMarked : BingoNumber -> Bool
isMarked num =
  case num of
      Marked _ ->
        True
      Unmarked _ ->
        False

checkRow : List BingoNumber -> Bool
checkRow row =
  List.all isMarked row



checkBoard : BingoBoard -> Bool
checkBoard board =
  let
    rows = List.map checkRow board
    cols = List.map checkRow (getColumns board)
  in
    List.any identity rows || List.any identity cols

findFirstSolved : List BingoBoard -> Maybe BingoBoard
findFirstSolved boards =
  case boards of
    x :: xs ->
      let
        hasWinningRow = checkBoard x
        hasWinningColumn = checkBoard (getColumns x)
      in
      if hasWinningRow == True || hasWinningColumn == True  then
        Just x
      else
        findFirstSolved xs
    [] ->
      Nothing

allBoardsSolved : List BingoBoard -> Bool
allBoardsSolved boards =
  List.all (\b ->
    let
      hasWinningRow = checkBoard b
      hasWinningColumn = checkBoard (getColumns b)
    in
      hasWinningRow == True || hasWinningColumn == True
  ) boards


parseBoards : String -> List BingoBoard
parseBoards input =
  String.lines input
    |> List.filter rejectEmpty
    |> List.map (\l ->
      String.words l
      |> List.map (\e -> Unmarked (toInt e))
    )
    |> List.Extra.groupsOf 5

parseInput : String -> List Int
parseInput input =
  case List.head (String.words input) of
    Just str ->
      String.split "," str
        |> List.map toInt
    Nothing ->
      []

getColumns : BingoBoard -> BingoBoard
getColumns board =
  List.Extra.transpose board


parsedInput = parseInput sampleInput

solve : List Int -> List BingoBoard -> Maybe Int
solve input boards =
  case input of
    x :: xs ->
      let
        markedBoards = markBoards x boards
      in
      case findFirstSolved markedBoards of
        Just board ->
          Just (calculatePoints x board)
        Nothing ->
          solve xs markedBoards
    [] ->
      Nothing

calculatePoints : Int -> BingoBoard -> Int
calculatePoints num board =
  let
    score = List.foldl (\row acc ->
      let
        unmarked = List.filter (\e -> not (isMarked e)) row
        sum = List.sum (List.map bnToInt unmarked)
      in
        acc + sum
      ) 0 board
  in
    score * num


  -- String.split "\n" input
  --   |> List.map (\s ->
  --     String.split (" ") s
  --       |> List.filter rejectEmpty
  --   )

-- Part 2

type alias Result =
  { board : BingoBoard
    , input : Int
  }

-- Solve all boards, keeping track of last solved
solveAll : List Int -> List BingoBoard -> Result -> Result
solveAll input boards result =
  if allBoardsSolved boards == True then
    result
  else
    case input of
      x :: xs ->
        let
          markedBoards = markBoards x boards
          newUnsolvedBoards = List.filter (\b -> not (checkBoard b)) markedBoards
        in
          case findFirstSolved markedBoards of
            Just board ->
              let
                newResult = { result | board = board, input = x}
              in
                solveAll xs newUnsolvedBoards newResult
            Nothing ->
              solveAll xs newUnsolvedBoards result
      [] ->
        result



realInput = """
  49,48,98,84,71,59,37,36,6,21,46,30,5,33,3,62,63,45,43,35,65,77,57,75,19,44,4,76,88,92,12,27,7,51,14,72,96,9,0,17,83,64,38,95,54,20,1,74,69,80,81,56,10,68,42,15,99,53,93,94,47,13,29,34,60,41,82,90,25,85,78,91,32,70,58,28,61,24,55,87,39,11,79,50,22,8,89,26,16,2,73,23,18,66,52,31,86,97,67,40
"""

realBoards = """
86 46 47 61 57
44 74 17  5 87
78  8 54 55 97
11 90  7 75 70
81 50 84 10 60

47 28 64 52 44
73 48 30 15 53
57 21 78 75 26
51 39 72 18 25
29 76 83 54 82

81  1 18 24 12
 3 38 15 85 50
32 10 74 86 84
30 64 56 79 95
78 94 35 93  8

48 30 79 85 87
66 35 13 17 95
32 22 94 61 20
50 42  0  3 93
69 44 68  1  9

91 79 93 41 33
98 51 39  9 10
24 70 99  2 11
32 13 21  6 68
40 27 48 89  7

40 29 34  1 23
79 36 75 57 95
61 50  4 21 48
54  0 81 98 72
24 30 15 31 52

18 68 17 25 34
97 36 77  6 30
79 72 38 94 60
54 45 16 67 12
58 31 57 71 92

40 58 80 86 85
91 57 51 23 10
61 78  4 36 66
24 41 88 25 99
15 68 12 55 75

82 99 17  5 76
65 42 73 78 61
34 62 14 23 68
 9 79 72 45  0
43 96 11 13 10

98 47 90 14 12
80 63 35 42 11
27 66  1  9 32
17 85 61 71 68
 6 29  7 94 67

38 35 70 18 59
62 54 84 10 27
60 92 90 64 86
25 99 49 43  4
23 50 39 16 40

72  1 73  8 33
86 65 99 49 66
56 79 23 41 46
 4 48 43 55 93
98 63 47 37 30

33 96 72 93 99
30 12 56 46 65
39 40 59 94 50
 0  8 67 27 47
53 57 24 77 42

 9 57  8 28 12
90 81 21 25 51
88 18 78  3 64
20 87 97 45 85
92 40 52 29 17

99 89 15 54 32
93 81 36 14 91
86  7 67 18 92
65 21 55 38  8
12 88 27 90 94

96  0  6 91 44
28 60 10 70 75
69 37 51 21 87
93 59 14 53 15
64 66  9 50 27

33 46 38  2 41
24 51 50 72 57
42 85 99 97 56
35 69 12 86 73
 4 47 34 80 17

70 28 77 73 53
67 94 83 79 82
89  9 96 48 17
47 86 88 12  3
55 39 98 14 90

64 82 85 45 10
27  5 12 72 40
52 31 25 79 65
 6 26  3 43 57
89 49 36 59 35

70  0 58 98 65
54 93 75 14 26
28 69 17 29 78
46 22 47 85 87
44 38 10 11 63

18 52 66 42 58
99 78 44 28 73
24 71  5 14 82
77 35 45 76 19
70 20  0 43 48

42 82 85 87 51
40 49 93 95 74
25 79 37 67 55
26 27 90 47 22
38 50 33 10 75

15 99 60 28 79
94 42 63 20 57
44 55 96 67 53
64  3 29 61 33
51 12 39 97 30

 7 95 28 39 76
87 31 23 47 75
88 10 78 24 20
30 81 22 51 62
53 93 55 38  0

12 99  2 89 17
30 23 92 66 10
39 60 74 82 15
 1 28 49  0 29
90 55  9 69 83

79 44 70 59 88
90  8 81 23  5
40 67 66 55 17
95 61 75 48 91
98 71 24 38 29

95 28  8 76 13
86 21 48  3  6
34 47 31 50  2
52 40 77 60 61
 0 88 87 23 25

63 70 34 91 17
98 49  8  2 14
25 22 92 65 18
78 61 97 73 20
57 83 16  7 68

43 23 70 39 16
 0 60 76  7 58
89 40 38 17  5
86 50 10 77 37
26 65 25 69 92

34 71 92 19 80
93  6 24 42 45
96  9 50 85 21
36 49 13 25 17
20 98 74 70 57

25 96 65 77 30
22 34 41 36 91
62 18 61 15 19
42 74 86 58 97
87 31 53  8 52

40 37 15 53 91
14 11 35 49 55
73 32 83 66 87
98 31 70 58 88
 7 61  8 76 16

27 94 87 57 80
54 35 40 59 72
88 84 70 98 92
37 52 45  7 16
 0 30 12 22 41

44 65 68 14 70
 5 35 17 90  7
56 89 48 84 32
73 69 74 51 72
24 10 94 78 60

81 15  3 42 90
54 52 74 84 71
97 78 20  9  2
59 66  1 91 87
70 56 93 47 37

93 36 19 69 94
17 20 48 58 52
85 57 90 42 14
16 92  4 49 65
22  9  2 24 44

47 99 13 31 62
81 58 88 91 94
29 11 96 95  1
14 20 82 34 37
84 39 76 41 22

 1 74 21  2 67
38 79 96 26 88
19 17 94 71 52
31 28 69  8 51
41 77 45 95 82

24  9 94 69 65
97 84 85 53  5
92 11 61 77  8
21 75 33 57 63
43 68 55 52 93

27 21  8 75 73
 4 53 23 56 47
28 94 50 80 19
89 58 24 12 13
60  5 99 96  9

31 43 59 65 33
51 32 14 58  4
11 41 70 78 12
 1 25 57 80 49
91 66  0 27 17

64 67 37 24 35
 4 22 54 75 21
19 91  9 52 83
20 68 53 12  0
28 76 51 49 89

67 89 14 54 81
 0 59 51 63 56
85 88 95  7 36
40 27 47 86 19
52 92 22 16 30

75 25 39  9 19
59 92 24  6 22
79 73 34 66 49
16 89 56 76 55
 5 45  4 46 62

71 44 63 97 47
27 61 70 52 46
19 80 21 68 65
28 45 84 14 94
38 73 66 78 92

47 45 41 96 54
38 14 62 55 91
 2 11 97 12 51
36 49  3 95 76
 5 75  7 94 87

70 24 93 96 86
49 51 73 50 83
97  0 57 13  9
99 46 22 67 39
56 21 29 52 27

42 82 80 65 19
78 41 56 83 75
51 72 10  1 33
84 63 21 87 86
77 64 31 68  6

60 50 31  5 58
83  9 87 98 13
 4 35 24 33 88
54 59 71 64  3
16 57 48 15 86

45 29 81 25 14
13 21 79 90  0
88 38 56 11 15
47  2 40 35 75
91 28 48 32 98

87 58 78 65 69
89 35 45 26 13
28 61 15  3 44
64 57 92 93 50
90 39  4 70  9

16 35 41 40 81
48 92 94 83 79
54 50 62  8 53
14  5 85 68 22
42 26 33 23 93

 7 13 82 89 49
43 21 79 38 56
 6 31 90 58 81
39 47 77 30 54
23 41 86 19  8

69 20 95 33 63
64 34  4 79 36
13 21 78 56  6
35 44 85 27 76
75 15 14 52 39

42 71 73  1 45
66 75  7 40 54
91 83 65 53 20
34 97 88  5 61
63 82 50 74 38

62 89 40 70 91
84 12 19 96 79
72 15 35 23 14
 4 69  0 55 17
85 90 20 28 13

27 93 23  1 38
67 28 62  9 96
31 35 47 44 88
78 57 53  5 69
91 15 82 75 61

17 44 85 92 94
 0 67  5 50 64
66 65 98 58 56
62  4 57 99 34
83 43 76 12 69

 0 28 13 68 86
84 24 50 32 40
25 71 72 96 94
89  1 64 81 23
97 66  5 15 91

59 67 79 84 44
74 61 81 20 68
24 92 55 99 11
76 60 97 43 66
31 30 89 45 53

 1 97 15 34 85
45 59 54 66 24
53 36 51 58 27
84 83 71  5 95
70  6 65 79 13

84 86  0 25  2
 1 59 92 39 56
17  7 88 78 24
51 87 89 44 31
54 63 50 18 36

98 86 30 70 12
11 52 49 39 14
16 35 56 87 72
85 65 93 92 60
20 43 77 41 79

50 31 71 78 21
70 94 99 35 29
56 58 27 65 28
45 36 47 69 98
 5 48 61 19 93

64 65 86 14 53
 7 43 75 39 38
20 59 80 88 54
12 32 66 34 87
29 15 25 19 45

88 20 42  5 32
56  8 80 15 98
36 99 35 27 16
92 66 75 91 10
81 96 65  0 57

29 78  8 76 41
99 18 60 90 47
50 51 40  2 31
38 70 25 52 39
26 35 84  6 80

 6 68 56 15 53
99 60 69 25  7
65 35  9 11 66
92 85 48 40 97
63 59 57 17 55

46 95 75 99 21
50 24 64 35 63
93 39  3 67 82
41 42 84 15 55
79 81 97 60 17

33 14 60 42 40
76 73 56 71 88
91 41 83 74 16
57 85 35 44 47
99 59 46 12 45

53 83 54 21 68
79 97 85  0 67
41 90 48 95  3
96 70 65 22 25
60 77 33 15 28

17 12  5 51 15
75 92 72 16 65
59 85 29 23 57
14 53 97 68 84
 1 93 49 38 28

27 40 24 12 57
84 13  9 43 31
70 23 51 94 34
 1 80 91 16 29
99 75 49 52 54

52 85 23 72 40
 6 88 16 41 67
53 94  8 32 33
75 62 24 13 64
65  0 60 86 47

 5 28 27 15 41
19 77 38 83 45
32 70 78 26 90
82 80 85 22 84
59 73 24  9 63

29 58 28 82 13
78 55 63 43 51
19 33 90 91 48
93  7 35 22 71
40 95 38 24 46

38 30 13 16 74
69 68 42  6  4
62 82 29 79  1
61  7 15 25 85
 5 66 45 43 90

19 65 12 91 34
17  6 30 32 64
37 53  4 35 62
41 22 13 11 25
60 27 93 76 51

 3 92 25 88 14
40 30 55 10 37
19 94 56 34 74
75 87 80 54 83
 2 20 70 45 16

52 93 87 60 11
82 66 88 59 95
58 31 49 33 28
77 39 43  9 51
20 80 98 47 16

35 48 47 11 82
 8 36 54 20 40
90 95 85  4 66
22 75 64 81 10
27 62 89 30 12

75 40 11 63 19
 4 43  6 93 48
85 58 82 66 52
32 28  0 14 20
78 61 83 95 87

57 79 16 37 33
20 17 27 38 63
35 77 60 97 34
22 78 72 43 26
29 12  9 46 54

26 94 37 57  1
49  6 65 80 55
46 38 33 89 99
42 18 86 97 98
45 76 41  9 12

83 70 31 61 30
16 78 84 12 18
15 65 62 55 98
 6 21 80 41 69
25  2 24 10 79

98 75  5 66 37
90  7 26 61 15
48 70 20 60 41
23 58 82 22 74
80  8 51 67 55

84 86 77 97 28
37 87  2 93  5
16 64 35 61 27
 8  3 36 10 73
31 65 94 63 13

91  9 64 67 19
56 35 11 62 28
 0 65 59 72 45
34 24 51 26 80
93 50 58 53 27

27 54 77 57 94
60 46 55 74 62
16  9 19 48  6
69  1 26 78  2
45 75 41 25 90

45 83 97 81 95
26 64 40 94  7
57 28 86  8 36
98 92 16 13 20
99 79 50 65 51

38  6 96 71 10
51 55  2 44 74
31 61 98 72 73
79 54 91 34 62
88 17 46 45 43

17 18 39 59 26
45 40 91 47 74
46 97 94 12 79
61  7  8 56 50
 0 77 20 57  9

74  4 65  2 23
45 56 90 94 96
80 71 69 86 85
19 78 35 47 98
51 73  6 33 14

 8 60 40  2 37
10 68 44 50 73
69 26  6 52 93
33 65 46 24 11
71 59 15 28 84

57 65 70 98 68
80  3 13 39 20
11 71 47 78 42
31 61 72 86  9
53 43 87 28 77

67 32 59 34 77
29 23 80 27 62
81 97 46 14 42
19 47 44 85 24
53  9 71 37  1

83 13 27 41  9
95 62 65 86 63
 0 17 33 11 76
45 64 39 71 55
84 52 21 59 20

63 45 55 80  3
14 73 47 96 10
82 26 85  0 11
53  6 28 57 60
49 99 18 50 71

30 67 16 22 84
81  4 34 61 65
57 69 51 94 58
 6 89 37 75 47
19 14 97  2 86

64 83  1 66 70
30 82 96  3 67
79 11 22 95 14
87 60  4 15 26
84 69 99 19 74
"""
