dotnet tool restore
dotnet paket restore
dotnet test
$current = get-location
set-location tests/ExpectoTests
npm install
npm test
set-location $current
$current = get-location
set-location tests/MochaTests
npm install
npm test
set-location $current
