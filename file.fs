Largest product in a grid
Submit

 Show HTML problem content 
Problem 11
In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?

let grid =   """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
                49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
                81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
                52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
                22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
                24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
                32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
                67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
                24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
                21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
                78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
                16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
                86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
                19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
                04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
                88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
                04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
                20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
                20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
                01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"""

let size = 20

let digits = 
    grid.Split " " 
    |> Seq.map(fun a -> a.Trim()) 
    |> Seq.filter(fun a -> (Seq.forall Char.IsDigit a) && a <> "") 
    |> Seq.map int
    |> Array.ofSeq 

let table:int[][] = [| 0..size - 1 |] |> Array.map(fun a -> digits[size * a .. size * a + size - 1])

let getColumn (tbl:int[][]) i =  [|0 .. size - 1|] |> Array.map(fun a -> tbl[a][i]) 

let rowMax = 
        table 
        |> Array.map(fun a -> 
                            a 
                            |> Seq.ofArray
                            |> Seq.windowed 4
                            |> Seq.map (Array.reduce (*))
                            |> Seq.max
        )
        |> Array.max

let colMax = 
    [| 0.. size - 1|]
    |> Array.map(fun a -> (getColumn table a) )
    |> Array.map(fun a -> 
                        a 
                        |> Seq.ofArray
                        |> Seq.windowed 4
                        |> Seq.map (Array.reduce (*))
                        |> Seq.max
    )
    |> Array.max



let diagLeftToRight (i,j) = 
    (i,j) 
    |> Seq.unfold(fun (x,y) ->
                    if x < 20 && y < 20 then
                        Some((x,y),(x+1,y+1))
                    else 
                        None) 
    |> Seq.map(fun (x,y) ->  table[x][y] )
    |> Seq.windowed 4
    |> Seq.map(fun a -> 
                    if a.Length > 0 then 
                        a |> Array.reduce (*) 
                    else 
                        0) 
    |> List.ofSeq
    

let diagRightToLeft (i,j) = 
    (i,j) 
    |> Seq.unfold(fun (x,y) ->
                    if x < 20 && y < 20 && x >= 0 && y >= 0 then
                        Some((x,y),(x-1,y+1))
                    else 
                        None) 
    |> Seq.map(fun (x,y) ->  table[x][y] )
    |> Seq.windowed 4
    |> Seq.map(fun a -> 
                    if a.Length > 0 then 
                        a |> Array.reduce (*) 
                    else 
                        0) 
    |> List.ofSeq

[
    colMax 
    rowMax
    ([0..size - 1] |> List.map(fun a -> diagLeftToRight (0,a)) |> List.concat |> List.max)
    ([0..size - 1] |> List.map(fun a -> diagLeftToRight (a,0))  |> List.concat |> List.max)
    ([0..size - 1] |> List.map(fun a -> diagRightToLeft (a,0))  |> List.concat |> List.max)
    ([0..size - 1] |> List.map(fun a -> diagRightToLeft (size - 1,a))  |> List.concat |> List.max)
]
|> List.max
|> printfn "%A"
            
//70600674


(*
Longest Collatz sequence
Problem 14
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*)

// Few seconds?

let collatz n = 
    let rec collatz_inner (n:int64) (acc:int64): int64 = 
        if n = 1L then 
            acc + 1L
        elif n % 2L = 0L then 
            collatz_inner (n / 2L) (acc + 1L)
        else     
            collatz_inner (3L * n + 1L) (acc + 1L)

    collatz_inner n 0L

[1L..1_000_000L] 
|> List.map(fun a -> (a, collatz a) )
|> List.sortByDescending(fun b -> snd b)
|> List.head
|> fst
|> printfn "%A"

(*
Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
Split second
*)

"37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690".Split "\n" 
|> Array.map(fun a -> BigInteger.Parse(a))
|> Array.reduce (+)
|> string
|> Seq.take 10
|> Seq.map string 
|> String.concat ""
|> printfn "%A" 


(*
215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 21000?

*)
// Split second

(bigint 2 ** 1000)
|> string 
|> Array.ofSeq
|> Array.map (string >> int)
|> Array.sum
|> printfn "%A"

(*
The four adjacent digits in the 1000-digit number that have the greatest product are 9 × 9 × 8 × 9 = 5832.

73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450

Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?
*)
// Split second

"73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"
|> Array.ofSeq 
|> Array.filter(fun a -> List.contains a ['0'..'9'])
|> Array.map(fun b -> int64 (string b))
|> Seq.ofArray
|> Seq.windowed 13
|> Seq.map(fun c -> c |> Array.reduce (*))
|> Seq.max
|> printfn "%A"

(*
Special Pythagorean triplet
Problem 9
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*)
// Split second
1 
|> Seq.unfold(fun a -> 
      if a < 1000 then 
        let mySeq = 
            a 
            |> Seq.unfold(fun b -> 
                if b < 1000 then 
                    let c = 1000 - a - b          
                    Some((a,b,c) , b + 1)
                else 
                    None
                
                    )
        Some((a,mySeq), a + 1)
      else 
        None)
|> Seq.collect(fun a -> snd a)
|> Seq.filter(fun (a,b,c) -> c > b && b > a )
|> Seq.filter(fun (a,b,c) -> a * a + b * b = c * c )
|> Seq.take 1
|> Seq.map(fun (a,b,c) -> a * b * c )
|> Seq.head
|> printfn "%A"
  
(*

10001st prime
Problem 7
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
*)
let isPrime i =
    let rec isPrime_inner x acc:bool = 
        if x = 1L then 
            true 
        elif acc = x then 
            true 
        else 
            if x % acc = 0L then 
                false 
            else 
                isPrime_inner x (acc + 1L)
    isPrime_inner i 2L



let a = Seq.initInfinite(fun a -> a) 
        |> Seq.filter(fun c -> c > 1) 
        |> Seq.filter(fun b -> isPrime b) 
        |> Seq.take 10001
        |> List.ofSeq
        |> List.rev
        |> List.head
printfn "%A" a


(*
Sum square difference
Problem 6
The sum of the squares of the first ten natural numbers is,

The square of the sum of the first ten natural numbers is,

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is
*)

let lst = [1..100]

let b = lst |> List.map(fun a ->  a * a) |> List.sum
let c = lst |> List.sum 
let ans = (c * c) - b
printfn "%A" ans

(*
Smallest multiple
Problem 5
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*)
let isDivisible i max =
    let rec isDivisible_inner x acc max =
        if max = acc then 
            true 
        elif x % acc = 0 then 
            isDivisible_inner x (acc + 1) max
        else
            false
    isDivisible_inner i 1 max


let a = Seq.initInfinite(fun a -> a) |> Seq.filter(fun b -> b > 0) |> Seq.filter(fun c -> isDivisible c 20) |> Seq.take 1
//232792560
// 15 seconds
printfn "%A" a


(*

    Largest palindrome product
    Submit

    Problem 4
    A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

    Find the largest palindrome made from the product of two 3-digit numbers.
*)

let rec palin (x:string):bool = 
    printfn "%A" x
    if x.Length = 1 then 
        true 
    elif x.Length = 2  && x.[0] = x.[x.Length - 1] then
        true 
    else 
        // Check first and last
        if x.[0] <> x.[x.Length - 1] then 
            false 
        else 
            palin x.[1..x.Length - 2] 
            

printfn "%A" (palin "3301033")


// Divisible by two 3 digit numbers
let divisible x = 
    let rec divisible_inner (x:int) (acc:int): bool = // 9999 91 
        if acc.ToString().Length  > 3 then 
            false 
        else 
            let remainder =  x % acc 
    
            if remainder = 0 && acc.ToString().Length = 3 then
                let otherDigit = x / acc

                if otherDigit.ToString().Length = 3 then

                    if otherDigit * acc = x then
                        printfn "%A" acc
                        printfn "Other digit is %A" otherDigit
                        true 
                    else 
                        divisible_inner x (acc + 1)
                else 
                    divisible_inner x (acc + 1) 
            else 
                divisible_inner x (acc + 1)
    
    divisible_inner x 100



let b = seq {998001..-1.. 1} |> Seq.filter(fun a -> palin (string a)) |> Seq.filter(fun a -> divisible a) |> Seq.take 1
printfn "Result%A" b
(*
Split second to run

913
Other digit is 993
seq [906609]
*)




(*
Largest prime factor
Problem 3
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*)
let isPrime i =
    let rec isPrime_inner x acc:bool = 
        if x = 1L then 
            true 
        elif acc = x then 
            true 
        else 
            if x % acc = 0L then 
                false 
            else 
                isPrime_inner x (acc + 1L)
    isPrime_inner i 2L


let largestPrimeFactor i = 
    let rec largestPrimeFactor_inner x acc =
        if x = 1L || x = acc then 
          acc 
        elif isPrime acc then 
            if x % acc = 0L then 
                largestPrimeFactor_inner (x/acc) (acc + 1L)
            else 
                largestPrimeFactor_inner (x) (acc + 1L)
        else 
            largestPrimeFactor_inner (x) (acc + 1L)
    largestPrimeFactor_inner i 1L
            
let result = largestPrimeFactor 600851475143L

printfn "%A" result
//6857 
// Took about 10 seconds to run





(*
Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*)

let result = [1..999] |> List.filter(fun a -> a % 3 = 0 || a % 5 = 0) |> List.reduce (+) //(fun acc i -> acc + i)
printfn "%A" result
//233168



(*
Problem 2
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
*)
let fib x = 
    let rec fib_tail (i:int) (a_acc:int) (b_acc:int) (sum:int): int = 
        match i with 
        | 0 -> sum//a_acc 
        | 1 -> fib_tail(i - 1) (b_acc) (a_acc) (sum)
        | _ -> fib_tail (i - 1) (b_acc) (b_acc + a_acc) (if b_acc % 2= 0 && b_acc < 4000000 then b_acc + sum else sum)
  
    fib_tail x 1 2 0   //  "fib_tail x 1 2" allows for the first term to start at 1 as defined in sequence in question. (1, 2, 3, 5, 8)
    
printfn "%A" (fib 35)
