decrypt[keyword_, enc_] := Module[
  {
   map = Table[Mod[i + j, 10], {j, 9, 0, -1}, {i, 0, 9}][[{10, 1, 2, 
      3, 4, 5, 6, 7, 8, 9}]],
   i = Flatten@
     IntegerDigits[ToExpression[StringReplace[enc, Whitespace -> ""]],
       10],
   k = Flatten@IntegerDigits[ToCharacterCode[keyword, "ASCII"] + 100]
   },
  Module[
   {
    li = Length[i], lk = Length[k]
    },
   Module[
    {
     newk = k[[Mod[Range[li], lk, 1]]]
     },
    Module[
     {
      position = 1 + Table[{newk[[j]], i[[j]]}, {j, 1, li}]
      },
     dnc = map[[First[#], Last[#]]] & /@ position;
     dnc1 = FromDigits[#] & /@ Partition[dnc, 3];
     outputdnc = FromCharacterCode[dnc1 - 100];
     ]
    ]
   ]
  ]
