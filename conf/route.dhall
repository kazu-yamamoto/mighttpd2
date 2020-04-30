let Route =  ../Program/Mighty/Dhall/Route.dhall
in
  [ { domains =
      [ "localhost"
      , "www.example.com"
      ]
    , routes =
      [ Route.Cgi { src = "/~alice/cgi-bin/", dst = "/home/alice/public_html/cgi-bin/" }
      , Route.File { src = "/~alice/", dst = "/home/alice/public_html/" }
      , Route.Cgi { src = "/cgi-bin/", dst = "/export/cgi-bin/" }
      , Route.RevProxy { src = "/app/cal/", dst = "/calendar/", domain = "example.net", port = 80 }
      , Route.RevProxy { src = "/app/wiki/", dst = "/", domain = "127.0.0.1", port = 3000 }
      , Route.File { src = "/", dst = "/export/www/" }
      ]
    }
  ]
