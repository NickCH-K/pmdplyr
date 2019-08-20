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
    
  ## Resubmission 
  
  This is a resubmission. In this version I have:
  
  - Replaced COC link in README.md with a valid URL, to address the following CRAN feedback:

  > Found the following (possibly) invalid file URI:
  > URI: .github/CODE_OF_CONDUCT.md
  > From: README.md  
  
  Previous submission only replaced URL in README.Rmd without fixing the knitted Markdown file. 

  > If there are references describing the methods in your package, please
  > add these in the description field of your DESCRIPTION file in the form
  > authors (year) <doi:...>
  > authors (year) <arXiv:...>
  > authors (year, ISBN:...)
  > or if those are not available: authors (year) <https:...>
  > with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
  > auto-linking.
  
  References are not relevant for the methods implemented in this package.
