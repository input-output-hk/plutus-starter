(program
  (let
    (nonrec)
    (typebind (tyvardecl ScriptContext (type)) (all a (type) (fun a a)))
    (datatypebind
      (datatype
        (tyvardecl Bool (type))

        Bool_match
        (vardecl True Bool) (vardecl False Bool)
      )
    )
    (lam
      hs
      (con bytestring)
      (lam
        cs
        (con bytestring)
        (lam
          ds
          ScriptContext
          [
            [
              [
                { (builtin ifThenElse) Bool }
                [ [ (builtin equalsByteString) hs ] [ (builtin sha2_256) cs ] ]
              ]
              True
            ]
            False
          ]
        )
      )
    )
  )
)