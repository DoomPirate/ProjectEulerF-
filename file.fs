
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
