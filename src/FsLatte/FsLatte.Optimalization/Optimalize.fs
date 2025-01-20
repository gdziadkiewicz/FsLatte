namespace FsLatte.Optimalization
open FsLatte.Model.Abs

module public Optimalize =
    let computeConsts (prog:Program) : Program =
        prog |> ConstOptimalization.optimalize