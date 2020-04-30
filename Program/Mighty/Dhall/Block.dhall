let Domain = Text
let Route = ./Route.dhall
in
  { domains : List Domain
  , routes : List Route
  }
