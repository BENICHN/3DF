for /f "tokens=2 delims==" %%I in ('wmic os get localdatetime /format:list') do set datetime=%%I
set datetime=%datetime:~0,8%-%datetime:~8,6%
cd ..\3DFH
stack test --test-arguments "--output ../Build/test/3DFH-test--%datetime%.html"
cd ..\Build