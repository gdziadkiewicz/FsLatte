namespace FsLatte.Semantics
open FsLatte.Model.Abs

module internal EntryPointChecker =
    let checkMain (program:Program) : string list =
        let (Program topdefs) = program
        let functions = topdefs |> List.map (fun (FnDef (a,(Ident b),c,_) ) -> (a,b,c))
        let mains = functions |> List.filter (fun (_,name,_) -> name = "main")

        if List.isEmpty mains then
            sprintf "There is no main function defined." |> List.singleton
        elif mains |> List.exists (fun (t,_,args) ->  t = Int && args |> List.isEmpty) |> not then
            sprintf "main function has invalid signature." |> List.singleton
        else
            []