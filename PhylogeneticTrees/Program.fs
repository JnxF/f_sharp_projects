System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

open System
type Id = string
type Gen = string
type Kmer = Map<string, int>

type Species = {
    Id: Id
    Gen: Gen
}
 
let kmer g k = 
    let n = String.length g 
    let getChunk i = g.[i..i+k-1]

    [0..n-k]
    |> List.map getChunk 
    |> List.groupBy id
    |> List.map (fun (word, sq) -> word, Seq.length sq)
    |> Map.ofSeq : Kmer

let intersectionkmers (kmer1:Kmer) (kmer2:Kmer) =
    let relateToOtherOrDestroy (str, i) =
        if kmer1.ContainsKey str
        then min (int(kmer1.Item str)) (int(i))
        else 0

    Map.toList kmer2 |>
    List.sumBy relateToOtherOrDestroy

let unionkmers kmer1 kmer2 =
    let k1 = Map.toList kmer1
    let k2 = Map.toList kmer2

    let sumk1 = List.sumBy snd k1
    let sumk2 = List.sumBy snd k2
    let common = 
        List.allPairs k1 k2
        |> List.filter (fun ((s1, _), (s2, _)) -> s1 = s2)
        |> List.sumBy (fun ((_, a), (_, b)) -> a + b - (max a b))

    sumk1 + sumk2 - common

let speciesDistance s1 s2 k =
    let k1 = kmer s1.Gen k
    let k2 = kmer s2.Gen k

    let unionSize = double(unionkmers k1 k2)
    let interSize = double(intersectionkmers k1 k2)

    (1.0 - (interSize / unionSize)) * 100.0

let species = [
    {Id="a"; Gen="AAAAGATGACCAGCGTAATG"};
    {Id="b"; Gen="GCAACCTTTGTGGGCGCAGT"};
    {Id="c"; Gen="ACGATTTGCGTAAGCTATGT"};
    {Id="d"; Gen="GCTCCTGTCCGTTTCAGCCG"};
    {Id="e"; Gen="TACACCATTAACCGGTGGGG"};
]
 
let string2species (text:string) =
    text.Split ' '
    |> Seq.toList
    |> List.filter (String.IsNullOrEmpty >> not)
    |> fun x -> {Id=x.[0]; Gen=x.[1]}

let tablaDistancias k =
    for s1 in species do
        printf "%s:" s1.Id
        for s2 in species do
            if s1 < s2 then
                let d = speciesDistance s1 s2 k
                printf " %s (%f)" s2.Id d
        printfn ""

type Tree = 
    | Empty
    | Node of value: string * left: Tree * right: Tree

let rec clusterDistance (t1 : Tree) (t2 : Tree) (distances: string->string->float) =
    match (t1, t2) with
    | Empty, Empty -> 0.0
    | Node (a, Empty, Empty), Node (b, Empty, Empty) -> distances a b
    | Node (_, a, b), d -> ((clusterDistance a d distances) + (clusterDistance b d distances))/2.0
    | d, Node (_, a, b) -> ((clusterDistance a d distances) + (clusterDistance b d distances))/2.0

let decorar f a b v =
    fun x y -> if (x = a && y = b) || (x = b && y = a) then v else f x y
    
let distances (species : Species list) k =
    let pairSpecies = seq {
        let n = List.length species
        for i in [0..n-1] do
            for j in [i+1..n-1] do
                yield (species.[i], species.[j])
    }
    
    let decoracion acc (a, b) = decorar acc a b (speciesDistance a b k)
    let emptyFunction _ _ = 0.0

    pairSpecies
    |> Seq.fold decoracion emptyFunction

let WPGMA (sp:Species list) k =
    let clusters =
        sp
        |> List.map (fun x -> x.Id)
        |> List.sort
        |> List.map (fun x -> Node (x, Empty, Empty))

    // Get closest ones
    let closests =
        List.allPairs sp sp
        |> List.filter (fun (s1, s2) -> s1 <> s2)
        |> List.map (fun (s1, s2) -> ((s1, s2), speciesDistance s1 s2 k))
        |> List.minBy snd
        |> fst

    let it1, it2 = closests

    let withNewPair = 
        clusters
        |> List.filter (fun x -> x <> Node(it1.Id, Empty, Empty))
        |> List.filter (fun x -> x <> Node(it2.Id, Empty, Empty))
        |> List.append [Node(it1.Id + it2.Id, Node(it1.Id, Empty, Empty), Node(it2.Id, Empty, Empty))]
    
    withNewPair

[<EntryPoint>]
let main argv =
    let k = 3
    tablaDistancias k
    
    0