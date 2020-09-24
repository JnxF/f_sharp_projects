open System

type FrequencyTable = Map<char, int>

type Language = {
    Id: string
    FrequencyTable: (char * int) list
}

type NodeInfo = {
    Tag: string
    Frequency: int
}

type TreeCode = 
    | Leaf of NodeInfo
    | Branch of NodeInfo * TreeCode * TreeCode

let spanish = {
    Id = "Spanish"
    FrequencyTable = [
        ('a', 30);
        ('b', 12);
        ('c', 18);
        ('d', 15);
        ('e', 25)
    ]
}

let language2forest (l : Language) =
    l.FrequencyTable
    |> List.map (fun (c, i) -> Leaf {Tag=string(c); Frequency=i})


let rec reduce (l : TreeCode list) =
    let getFrequencies et =
        match et with
        | Leaf x -> (x.Frequency, x.Tag)
        | Branch (x, _, _) -> (x.Frequency, x.Tag)
    
    let sorted = List.sortBy getFrequencies l

    let m1 = sorted.[0]
    let m2 = sorted.[1]

    let listWithoutMinimums =
        l
        |> List.filter (fun x -> x <> m1)
        |> List.filter (fun x -> x <> m2)

    let joinNodeInfos et1 et2 =
        let n1, n2 =
            match (et1, et2) with
            | (Leaf x, Leaf y) -> (x, y)
            | (Leaf x, Branch(y, _, _)) -> (x, y)
            | (Branch(x, _, _), Leaf y) -> (x, y)
            | (Branch(x, _, _), Branch(y, _, _)) -> (x, y)

        let joinTags n1 n2 = if n1.Tag < n2.Tag then n1.Tag + n2.Tag else n2.Tag + n1.Tag
        
        {Tag = joinTags n1 n2; Frequency=n1.Frequency + n2.Frequency}
    
    let res = listWithoutMinimums |> List.append [
        Branch (joinNodeInfos m1 m2, m1, m2)
    ]

    match res with
    | [x] -> x
    | _ -> reduce res

let rec encoderToList enc =
    let rec encodeNode prefix n =
        match n with
        | Leaf n -> [(n.Tag.[0], prefix)]
        | Branch (n, l, r) ->
            encodeNode (prefix + "0") l @ encodeNode (prefix + "1") r
            
    encodeNode "" enc

let encodeGivenStringLanguage (s:string) (l:Language) =
    let languageencoder = l |> language2forest |> reduce |> encoderToList
    
    let char2optionMappedString x =
        List.tryFind (fun (c, _) -> c = x) languageencoder
        |> Option.map snd

    let reduceCharOptions = fun (acc:string option) (x:string option) ->
            match (acc, x) with
            | Some(a), Some(b) -> Some (a + b)
            | _, _ -> None

    s
    |> Seq.toList
    |> List.map char2optionMappedString
    |> List.fold reduceCharOptions (Some "")

let decodeGivenStringLanguage (l:Language) (s:string) =
    let tree = l |> language2forest |> reduce
    let chars = Seq.toList s
    let compute chars tree =
        let rec computeR chars actual raiz prefijo =
            match chars, actual with
            | [], t when t = raiz -> prefijo
            // Altura 1
            | '0'::xs, Branch (_, Leaf n, _) ->
                computeR xs raiz raiz (Option.map (fun x -> x + n.Tag) prefijo)
            | '1'::xs, Branch (_, _, Leaf n) ->
                computeR xs raiz raiz (Option.map (fun x -> x + n.Tag) prefijo)
            // Altura n
            | x::xs, Branch(_, l, r) ->
                if x = '0'
                then computeR xs l raiz prefijo
                else computeR xs r raiz prefijo
            | _, _ -> None

        computeR chars tree tree (Some "")

    if List.forall (fun x -> x = '0' || x = '1') chars 
    then compute chars tree
    else None

let readLanguage() =
    let name = Console.ReadLine()
    let s = int(Console.ReadLine())
    let mutable frequencyTable = []
    for i in 1..s do
        let line = (Console.ReadLine()).Split " "
        frequencyTable <- (line.[0].[0], int(line.[1])) :: frequencyTable
    
    {Id = name; FrequencyTable = frequencyTable}

let tablaFrec name langs =
    langs
    |> List.tryFind (fun x -> x.Id = name)
    |> Option.map (fun l -> l.FrequencyTable)
    |> Option.map (fun l -> List.sortBy fst l)
    |> Option.map (List.iter (fun (a, b) -> printfn "%c %i" a b))
    |> ignore

[<EntryPoint>]
let main argv =
    let n = int(Console.ReadLine())
    let mutable languages = []
    for _ in 1..n do
        languages <- readLanguage() :: languages

    while true do
        let line = Console.ReadLine()
        let splitted = List.ofSeq (line.Split " ")
        let command = splitted.Head
        let rest = splitted.Tail

        let action =
            match command with
            | "tabla_frec" -> tablaFrec rest.[0] languages
            | _ -> printfn "hi"

        action
    0