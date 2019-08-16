  ## Test environments
  
  * local: mingw32-3.6.1
  * travis: 3.4, oldrel, release, devel
  * r-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
  * win-builder: windows-x86_64-devel
  
  ## R CMD check results
  
  R CMD check NOTES highlight possible misspelled words in DESCRIPTION: 
  
  pibble (11:193)
  tibble (11:174)
  
  This is expected. The tibble subclass used in this package ('pibble') is a slight variation of the spelling of 'tibble'. 
  
  This is a first submission.
  
