< File : { src : Text, dst : Text }
  | Redirect : { src : Text, dst : Text }
  | Cgi : { src : Text, dst : Text }
  | RevProxy : { src : Text, dst : Text, domain : Text, port : Natural }
  >