encrypt[keyword_, object_] := Module[
  {
   map = Table[Mod[i + j, 10], {i, 0, 9}, {j, 0, 9}],
   i = Flatten@IntegerDigits[ToCharacterCode[object, "ASCII"] + 100],
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
     Module[
      {
       
       enc = map[[First[#], Last[#]]] & /@ position
       },
      Module[
       {
        enc1 = FromDigits[#] & /@ Partition[enc, 3]
        },
       encstring = 
        StringReplace[
         ToString[enc], {"{" -> "", "}" -> "", "," -> "", 
          Whitespace -> ""}];
       encstring1 = 
        StringReplace[
         ToString[enc1], {"{" -> "", "}" -> "", Whitespace -> ""}];
       outputenc = FromCharacterCode[enc1];
       ]
      ]
     ]
    ]
   ]
  ]
