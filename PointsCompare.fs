//Compare 2 sets of points 
module PointsCompare

open System.IO

let eps = 0.1
let eps' = 0.1

type Point = float*float

type Segment =
    {
        sqrLength: float
        fstPoint: Point
        sndPoint: Point
    } 
    static member (==) (a:Segment, b:Segment) = abs(a.sqrLength - b.sqrLength) < eps' 
    static member (==>) (a:Segment, b:Segment) = a.sqrLength - b.sqrLength > eps' 

let toPoint (str:string) =
    (float <| str.Substring(0, str.IndexOf(' ')), float <| str.Substring(str.IndexOf(' ') + 1))

let readSet (file:StreamReader) k = [| for i in 1 .. k ->  toPoint <| file.ReadLine() |]
   
let sqrDistance (x, y) (u, v) = (x - u)*(x - u) + (y - v)*(y - v) 

let countSegments (a:Point[]) k = 
    [| for i in 0 .. k - 2 do 
           for j in i + 1 .. k - 1 -> 
               { sqrLength = sqrDistance a.[i] a.[j]; fstPoint = a.[i]; sndPoint = a.[j] } |]    

let calcTransformation (x1, y1) (x2, y2) (u1, v1) (u2, v2) =
    let x = x1 - x2
    let y = y1 - y2
    let u = u1 - u2
    let v = v1 - v2

    let sin = (y*u - x*v) / (u*u + v*v)
    let cos = (x*u + y*v) / (u*u + v*v)

    let a = x1 - u1 * cos + v1 * sin
    let b = y1 - u1 * sin - v1 * cos

    (sin, cos, a, b)

let (===) (x, y) (u, v) = eps > sqrDistance (x, y) (u, v)
let (=!) x y = eps > x - y

let transformation (x:Point[]) (sin, cos, a, b) n =
    let transformation' (u, v) = (u * cos - v * sin + a, u * sin + v * cos + b)
    Array.map transformation' x

let countPoints (a:Point[]) (b:Point[]) m n =
    let mutable count = 0
    for i in 0 .. n - 1 do
        let findPoints l' r' f =
            let rec binSearchL l r =
                let mid = (l + r) / 2

                if (l > r) then None
                else 
                    if f b.[i] =! f a.[l] then Some l
                    else 
                        if f b.[i] =! f a.[mid] then binSearchL (l + 1) mid
                        else 
                            if f b.[i] < f a.[mid]  then binSearchL l (mid - 1)
                            else binSearchL (mid + 1) r

            let rec binSearchR l r =
                let mid = (l + r) / 2

                if (l > r) then None
                else 
                    if f b.[i] =! f a.[r] then Some r
                    else 
                        if f b.[i] =! f a.[mid] then binSearchR mid (r - 1)
                        else 
                            if f b.[i] < f a.[mid]  then binSearchR l (mid - 1)
                            else binSearchR (mid + 1) r

            (binSearchL l' r', binSearchR l' r')
        match findPoints 0 (m - 1) fst with 
        | (Some l1, Some r1) -> match findPoints l1 r1 snd with
                                | (Some l2, Some r2) -> 
                                    if Array.exists (fun x -> b.[i] === x) (Array.sub a l2 (r2 - l2 + 1)) then 
                                        count <- count + 1
                                | _ -> ()
        | _ -> ()

    count

[<EntryPoint>]
let main args = 
    use input = new StreamReader("input.txt")
    
    let n = int <| input.ReadLine()
    let set1 = readSet input n
    let m = int <| input.ReadLine()
    let set2 = Array.sort <| readSet input m

    let segments1 = Array.sort <| countSegments set1 n
    let segments2 = Array.sort <| countSegments set2 m

    let n' = n * (n - 1) / 2
    let m' = m * (m - 1) / 2

    let mutable maxPoints = 
        if n = 1 || m = 1 then min m n
        else 0
    let mutable start = 0 
    for i in 0 .. n' - 1 do
        while start < m' && segments1.[i] ==> segments2.[start] do
            start <- start + 1
        
        let mutable j = start
        while j < m' && segments1.[i] == segments2.[j] do
            let transform = calcTransformation segments2.[j].fstPoint segments2.[j].sndPoint
         
            let set' = transformation set1 (transform segments1.[i].fstPoint segments1.[i].sndPoint) n
            maxPoints <- max maxPoints (countPoints set2 set' m n)

            let set' = transformation set1 (transform segments1.[i].sndPoint segments1.[i].fstPoint) n
            maxPoints <- max maxPoints (countPoints set2 set' m n)
      
            j <- j + 1
    
    printfn "%A" maxPoints
    
    System.Console.ReadKey() |> ignore
    0