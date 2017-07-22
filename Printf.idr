data Format = Number Format
            | Str Format
            | Chr Format
            | Doubl Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Chr fmt) = (ch : Char) -> PrintfType fmt
PrintfType (Doubl fmt) = (d : Double) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \num => printfFmt fmt (acc ++ show num)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Chr fmt) acc = \chr => printfFmt fmt (acc ++ cast chr)
printfFmt (Doubl fmt) acc = \dbl => printfFmt fmt (acc ++ show dbl)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc


toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Doubl (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                            Lit lit chars' => Lit (strCons c lit) chars'
                            fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

TupleVect : (n : Nat) -> (a : Type) -> Type
TupleVect Z a = ()
TupleVect (S k) a = (a, TupleVect k a)

test : TupleVect 4 Nat
test = (1,2,3,4,())
