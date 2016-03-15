osql -E -S ".\sqlexpress" -Q "RESTORE DATABASE [EME] FROM  DISK = N'C:\sistemas\Versiones\EME\BackupBD\EME.bak' WITH  FILE = 1,  MOVE N'futura_Data' TO N'c:\Program Files\Microsoft SQL Server\MSSQL10_50.SQLEXPRESS\MSSQL\DATA\EME.mdf',  MOVE N'futura_Log' TO N'c:\Program Files\Microsoft SQL Server\MSSQL10_50.SQLEXPRESS\MSSQL\DATA\EME.ldf',  NOUNLOAD,  REPLACE,  STATS = 5"
pause
