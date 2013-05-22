module ConfigSpec where

import Config.Internal
import Test.Hspec

spec :: Spec
spec = do
    describe "parseConfig" $ do
        it "parses example.conf correctly" $ do
            res <- parseConfig "conf/example.conf"
            res `shouldBe` ans

ans :: [(String, ConfValue)]
ans = [("Port",CV_Int 80),("Debug_Mode",CV_Bool True),("User",CV_String "root"),("Group",CV_String "root"),("Pid_File",CV_String "/var/run/mighty.pid"),("Logging",CV_Bool True),("Log_File",CV_String "/var/log/mighty"),("Log_File_Size",CV_Int 16777216),("Log_Backup_Number",CV_Int 10),("Index_File",CV_String "index.html"),("Index_Cgi",CV_String "index.cgi"),("Status_File_Dir",CV_String "/usr/local/share/mighty/status"),("Connection_Timeout",CV_Int 30),("Fd_Cache_Duration",CV_Int 10),("Worker_Processes",CV_Int 1),("Tls_Port",CV_Int 443),("Tls_Cert_File",CV_String "certificate.pem"),("Tls_Key_file",CV_String "key.pem"),("Service",CV_Int 0)]
