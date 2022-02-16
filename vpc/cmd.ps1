
$INSTALLDIR="C:\Program Files\Certara\NLME_Engine"
if($INSTALLDIR -eq "" -or $INSTALLDIR -eq $null)
{
  "Installation directory is not specified"
  exit 1
}
powershell -noninteractive -executionpolicy remotesigned -File $INSTALLDIR\generic_run.ps1 none $INSTALLDIR $shared_directory C:\Users\jcraig\Downloads\WebinarFeb2022\vpc C:\Users\jcraig\Downloads\WebinarFeb2022\vpc\jobControlFile.txt 1 WorkFlow
