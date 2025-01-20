namespace FsLatte.Semantics
open FsLatte.Model.Abs

module internal FunctionHelper =
    let getFunctionFromProgram (program:Program) =
        let (Program topdefs) = program
        topdefs |> List.map (fun (FnDef (a,(Ident b),c,_) ) -> (a,b,c))
