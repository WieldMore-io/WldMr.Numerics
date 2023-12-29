#!/usr/bin/env pwsh
remove-item -rec tests/MochaTests/node_modules -erroraction silentlycontinue
remove-item -rec tests/MochaTests/build -erroraction silentlycontinue
remove-item -rec tests/ExpectoTests/node_modules -erroraction silentlycontinue
remove-item -rec tests/ExpectoTests/build -erroraction silentlycontinue
remove-item -rec js-build -erroraction silentlycontinue
remove-item -rec py-build -erroraction silentlycontinue
remove-item -rec node_modules -erroraction silentlycontinue
remove-item -rec .paket -erroraction silentlycontinue
remove-item -rec paket-files -erroraction silentlycontinue
gci -recurse . -directory | where name -match "^(bin|obj)$" | select fullname | foreach { remove-item -rec -force $_.FullName}
