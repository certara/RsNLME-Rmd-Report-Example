
$INSTALLDIR="C:\Program Files\Certara\NLME_Engine"
if($INSTALLDIR -eq "" -or $INSTALLDIR -eq $null)
{
  "Installation directory is not specified"
  exit 1
}
powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR\generic_run.ps1 local_mpi $INSTALLDIR $shared_directory C:\Users\jcraig\Downloads\WebinarFeb2022\TwoCptIV C:\Users\jcraig\Downloads\WebinarFeb2022\TwoCptIV\jobControlFile.txt 4 WorkFlow
