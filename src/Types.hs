module Types where

import Network (Socket)

data Service = HttpOnly Socket | HttpsOnly Socket | HttpAndHttps Socket Socket
